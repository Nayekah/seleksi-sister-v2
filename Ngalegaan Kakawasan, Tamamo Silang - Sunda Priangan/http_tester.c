#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#define BUFFER_SIZE 8192
#define DEFAULT_HOST "w1ntr.space"
#define DEFAULT_PORT 80

typedef struct {
    char *method;
    char *path;
    char *headers;
    char *body;
    char *description;
} test_request;

struct server_config {
    char hostname[256];
    int port;
} config = {"w1ntr.space", 80};

int connect_to_server() {
    struct hostent *host_entry;
    struct sockaddr_in server_addr;
    int sock;

    host_entry = gethostbyname(config.hostname);
    if (host_entry == NULL) {
        printf("Failed to resolve hostname: %s\n", config.hostname);
        return -1;
    }

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        perror("Socket creation failed");
        return -1;
    }
    
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(config.port);
    memcpy(&server_addr.sin_addr, host_entry->h_addr_list[0], host_entry->h_length);
    
    if (connect(sock, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
        perror("Connection failed");
        close(sock);
        return -1;
    }
    
    return sock;
}

void parse_response_headers(const char* response, ssize_t total_bytes) {
    printf("\n=== RESPONSE HEADER ANALYSIS ===\n");
    
    const char* header_end = strstr(response, "\r\n\r\n");
    if (!header_end) {
        printf("No headers found or malformed response\n");
        return;
    }
    
    char status_line[256] = {0};
    const char* first_crlf = strstr(response, "\r\n");
    if (first_crlf) {
        int status_len = first_crlf - response;
        if (status_len < 255) {
            strncpy(status_line, response, status_len);
            printf("Status Line: %s\n", status_line);
        }
    }
    
    const char* current = response + strlen(status_line) + 2;
    while (current < header_end) {
        const char* next_crlf = strstr(current, "\r\n");
        if (!next_crlf) break;
        
        int header_len = next_crlf - current;
        char header[512] = {0};
        if (header_len < 511) {
            strncpy(header, current, header_len);
            printf("Header: %s\n", header);
        }
        
        current = next_crlf + 2;
    }
    
    const char* body_start = header_end + 4;
    int header_length = body_start - response;
    int body_size = total_bytes - header_length;
    
    printf("Body Size: %d bytes (calculated from total %zd bytes - %d header bytes)\n", 
           body_size, total_bytes, header_length);
}

void send_request_with_redirect(test_request *req, int max_redirects) {
    char current_hostname[256];
    int current_port;
    char current_path[512];
    int redirect_count = 0;
    
    strncpy(current_hostname, config.hostname, sizeof(current_hostname));
    current_port = config.port;
    strncpy(current_path, req->path, sizeof(current_path));
    
    while (redirect_count <= max_redirects) {
        printf("\n========================================================\n");
        if (redirect_count == 0) {
            printf("TEST: %s\n", req->description);
        } else {
            printf("FOLLOWING REDIRECT #%d\n", redirect_count);
        }
        printf("REQUEST: %s %s\n", req->method, current_path);
        printf("TARGET: http://%s:%d%s\n", current_hostname, current_port, current_path);
        printf("========================================================\n");
        
        struct hostent *host_entry = gethostbyname(current_hostname);
        if (host_entry == NULL) {
            printf("Failed to resolve hostname: %s\n", current_hostname);
            return;
        }

        int sock = socket(AF_INET, SOCK_STREAM, 0);
        if (sock < 0) {
            perror("Socket creation failed");
            return;
        }
        
        struct sockaddr_in server_addr;
        server_addr.sin_family = AF_INET;
        server_addr.sin_port = htons(current_port);
        memcpy(&server_addr.sin_addr, host_entry->h_addr_list[0], host_entry->h_length);
        
        if (connect(sock, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
            perror("Connection failed");
            close(sock);
            return;
        }
        
        char request[BUFFER_SIZE];
        int content_length = req->body ? strlen(req->body) : 0;
        
        if (content_length > 0) {
            snprintf(request, sizeof(request),
                "%s %s HTTP/1.1\r\n"
                "Host: %s\r\n"
                "User-Agent: W1ntr-HTTPTester/1.0\r\n"
                "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n"
                "Accept-Language: en-US,en;q=0.5\r\n"
                "Connection: close\r\n"
                "Content-Type: application/x-www-form-urlencoded\r\n"
                "Content-Length: %d\r\n"
                "%s"
                "\r\n"
                "%s",
                req->method, current_path, current_hostname,
                content_length,
                req->headers ? req->headers : "",
                req->body);
        } else {
            snprintf(request, sizeof(request),
                "%s %s HTTP/1.1\r\n"
                "Host: %s\r\n"
                "User-Agent: W1ntr-HTTPTester/1.0\r\n"
                "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n"
                "Accept-Language: en-US,en;q=0.5\r\n"
                "Connection: close\r\n"
                "%s"
                "\r\n",
                req->method, current_path, current_hostname,
                req->headers ? req->headers : "");
        }
        
        printf("=== SENDING REQUEST ===\n");
        printf("%s", request);
        printf("=== END REQUEST ===\n");
        
        if (send(sock, request, strlen(request), 0) < 0) {
            perror("Send failed");
            close(sock);
            return;
        }
        
        char response[BUFFER_SIZE];
        memset(response, 0, sizeof(response));
        ssize_t total_received = 0;
        ssize_t bytes_received;
        
        while ((bytes_received = recv(sock, response + total_received, 
                                     sizeof(response) - total_received - 1, 0)) > 0) {
            total_received += bytes_received;
            if (total_received >= sizeof(response) - 1) {
                break;
            }
        }
        
        close(sock);
        
        if (total_received <= 0) {
            printf("No response received\n");
            return;
        }
        
        response[total_received] = '\0';
        printf("\n=== FULL RESPONSE (%zd bytes total) ===\n", total_received);
        
        if (total_received > 1000) {
            printf("%.1000s...\n[Response truncated - total size: %zd bytes]\n", 
                   response, total_received);
        } else {
            printf("Raw response (%zd bytes):\n", total_received);
            for (int i = 0; i < total_received; i++) {
                if (response[i] == '\0') {
                    printf("[NULL]");
                } else if (response[i] >= 32 && response[i] < 127) {
                    printf("%c", response[i]);
                } else if (response[i] == '\r') {
                    printf("\\r");
                } else if (response[i] == '\n') {
                    printf("\\n\n");
                } else {
                    printf("[%02X]", (unsigned char)response[i]);
                }
            }
            printf("\n");
        }
        
        parse_response_headers(response, total_received);
        
        // Check for redirect
        int is_redirect = 0;
        char *location = NULL;
        
        if (strstr(response, "HTTP/1.1 301") || strstr(response, "HTTP/1.1 302") ||
            strstr(response, "HTTP/1.1 303") || strstr(response, "HTTP/1.1 307") ||
            strstr(response, "HTTP/1.1 308")) {
            is_redirect = 1;
            
            char *loc_start = strstr(response, "Location: ");
            if (loc_start) {
                loc_start += 10; // Skip "Location: "
                char *loc_end = strstr(loc_start, "\r\n");
                if (loc_end) {
                    int loc_len = loc_end - loc_start;
                    location = malloc(loc_len + 1);
                    strncpy(location, loc_start, loc_len);
                    location[loc_len] = '\0';
                }
            }
        }
        
        printf("\n=== SERVER BEHAVIOR ANALYSIS ===\n");
        if (strstr(response, "HTTP/1.1 200 OK")) {
            printf("✅ HTTP/1.1 200 OK response\n");
        } else if (strstr(response, "HTTP/1.1 404")) {
            printf("⚠️  HTTP/1.1 404 Not Found\n");
        } else if (is_redirect) {
            printf("🔄 HTTP redirect response\n");
            if (location) {
                printf("   Redirect to: %s\n", location);
            }
        } else {
            printf("❓ Unexpected status code\n");
        }
        
        if (strstr(response, "Connection: close")) {
            printf("✅ Connection: close header present\n");
        }
        
        if (strstr(response, "Content-Type:")) {
            printf("✅ Content-Type header present\n");
        }
        
        // Handle redirect
        if (is_redirect && location && redirect_count < max_redirects) {
            printf("\n🔄 Following redirect to: %s\n", location);
            
            // Parse redirect URL
            if (strstr(location, "https://")) {
                // HTTPS redirect - we can't follow with plain sockets
                printf("⚠️  Cannot follow HTTPS redirect with plain HTTP socket.\n");
                printf("💡 Use curl to test HTTPS: curl -L %s\n", location);
                if (location) free(location);
                return;
            } else if (strstr(location, "http://")) {
                // Parse full HTTP URL
                char *url_start = location + 7; // Skip "http://"
                char *path_start = strchr(url_start, '/');
                
                if (path_start) {
                    *path_start = '\0';
                    strncpy(current_hostname, url_start, sizeof(current_hostname));
                    strncpy(current_path, path_start + 1, sizeof(current_path));
                    current_port = 80;
                    
                    // Check for port in hostname
                    char *port_start = strchr(current_hostname, ':');
                    if (port_start) {
                        *port_start = '\0';
                        current_port = atoi(port_start + 1);
                    }
                } else {
                    strncpy(current_hostname, url_start, sizeof(current_hostname));
                    strcpy(current_path, "/");
                }
            } else {
                // Relative redirect
                strncpy(current_path, location, sizeof(current_path));
            }
            
            redirect_count++;
            if (location) free(location);
            continue;
        }
        
        // Final response analysis
        if (strcmp(req->method, "POST") == 0 && strcmp(req->path, "/calculate") == 0) {
            char name1[64] = {0};
            char name2[64] = {0};
            
            if (req->body) {
                char *name1_start = strstr(req->body, "name1=");
                char *name2_start = strstr(req->body, "name2=");
                
                if (name1_start) {
                    name1_start += 6;
                    char *name1_end = strchr(name1_start, '&');
                    int name1_len = name1_end ? (name1_end - name1_start) : strlen(name1_start);
                    if (name1_len < 63) {
                        strncpy(name1, name1_start, name1_len);
                        name1[name1_len] = '\0';
                    }
                }
                
                if (name2_start) {
                    name2_start += 6;
                    char *name2_end = strchr(name2_start, '&');
                    int name2_len = name2_end ? (name2_end - name2_start) : strlen(name2_start);
                    if (name2_len < 63) {
                        strncpy(name2, name2_start, name2_len);
                        name2[name2_len] = '\0';
                    }
                }
                
                if (strlen(name1) > 0 && strlen(name2) > 0) {
                    if (strstr(response, name1) && strstr(response, name2)) {
                        printf("✅ Love calculation working - names '%s' and '%s' found in response\n", name1, name2);
                    } else {
                        printf("⚠️  Love calculation may not be working - names '%s' and '%s' not found in response\n", name1, name2);
                    }
                } else {
                    if (strstr(response, "%") && (strstr(response, "Love") || strstr(response, "Match"))) {
                        printf("✅ Love calculation appears to be working - percentage and love-related text found\n");
                    } else {
                        printf("⚠️  Love calculation may not be working - no percentage or love text found\n");
                    }
                }
            }
        }
        
        if (location) free(location);
        break; // Exit loop if not redirecting
    }
    
    if (redirect_count > max_redirects) {
        printf("⚠️  Too many redirects (%d), stopping\n", max_redirects);
    }
}

void send_request_and_analyze(test_request *req) {
    send_request_with_redirect(req, 5); // Follow up to 5 redirects
}


void test_custom_headers() {
    printf("\n========================================================\n");
    printf("TESTING CUSTOM HEADERS HANDLING\n");
    printf("========================================================\n");
    
    test_request custom_tests[] = {
        {"GET", "/main", "X-Custom-Header: test-value\r\n", NULL, "Custom header test"},
        {"GET", "/calculate", "Authorization: Bearer token123\r\n", NULL, "Authorization header test"},
        {"POST", "/calculate", "X-CSRF-Token: abc123\r\nReferer: http://w1ntr.space/\r\n", 
         "name1=TestUser&name2=TestPartner", "Multiple custom headers with POST"},
        {"GET", "/notes", "Cookie: sessionid=12345; csrftoken=abcde\r\n", NULL, "Cookie header test"},
    };
    
    int num_tests = sizeof(custom_tests) / sizeof(custom_tests[0]);
    for (int i = 0; i < num_tests; i++) {
        send_request_and_analyze(&custom_tests[i]);
        sleep(1);
    }
}

void print_usage(char *program_name) {
    printf("\nUsage: %s [options] [test_number|test_name]\n", program_name);
    printf("\nOptions:\n");
    printf("  -h HOST     Target hostname (default: w1ntr.space)\n");
    printf("  -p PORT     Target port (default: 80 for HTTP)\n");
    printf("  --help      Show this help\n");
    printf("\nAvailable tests:\n");
    printf("  1 - GET /main\n");
    printf("  2 - GET /calculate\n");
    printf("  3 - POST /calculate\n");
    printf("  4 - GET /notes\n");
    printf("  5 - POST /notes\n");
    printf("  6 - PUT /notes\n");
    printf("  7 - DELETE /notes\n");
    printf("  8 - GET /eta.mp4\n");
    printf("  9 - GET /nonexistent (404 test)\n");
    printf("  all - Run all basic tests\n");
    printf("  headers - Test custom headers\n");
    printf("  comprehensive - Run all tests\n");
    printf("\nNote: This tester uses HTTP port 80.\n");
    printf("For HTTPS testing, use: curl -I https://w1ntr.space/main\n");
    printf("\nExamples:\n");
    printf("  %s 1                     # Test main page\n", program_name);
    printf("  %s all                   # Test all endpoints\n", program_name);
}

int main(int argc, char *argv[]) {
    printf("W1ntr.space HTTP Server Tester\n");
    
    int arg_index = 1;
    while (arg_index < argc && argv[arg_index][0] == '-') {
        if (strcmp(argv[arg_index], "-h") == 0 && arg_index + 1 < argc) {
            strncpy(config.hostname, argv[arg_index + 1], sizeof(config.hostname) - 1);
            arg_index += 2;
        } else if (strcmp(argv[arg_index], "-p") == 0 && arg_index + 1 < argc) {
            config.port = atoi(argv[arg_index + 1]);
            arg_index += 2;
        } else if (strcmp(argv[arg_index], "--help") == 0) {
            print_usage(argv[0]);
            return 0;
        } else {
            printf("Unknown option: %s\n", argv[arg_index]);
            print_usage(argv[0]);
            return 1;
        }
    }
    
    printf("Testing server at %s:%d\n", config.hostname, config.port);
    
    test_request basic_tests[] = {
        {"GET", "/main", NULL, NULL, "Main page"},
        {"GET", "/calculate", NULL, NULL, "Love calculator page"},
        {"POST", "/calculate", NULL, "name1=Alice&name2=Bob", "Love calculation"},
        {"GET", "/notes", NULL, NULL, "Notes page"},
        {"POST", "/notes", NULL, "content=Test+note+content", "Create note"},
        {"PUT", "/notes", NULL, "content=Updated+note+content", "Update note"},
        {"DELETE", "/notes", NULL, NULL, "Delete note"},
        {"GET", "/eta.mp4", NULL, NULL, "Video file request"},
        {"GET", "/nonexistent", NULL, NULL, "404 Not Found test"}
    };
    
    int num_basic_tests = sizeof(basic_tests) / sizeof(basic_tests[0]);
    
    if (arg_index >= argc) {
        printf("Running basic connectivity test...\n");
        send_request_and_analyze(&basic_tests[0]);
        printf("\nFor more tests, try: %s --help\n", argv[0]);
    } else {
        char *test_arg = argv[arg_index];
        
        if (strcmp(test_arg, "all") == 0) {
            for (int i = 0; i < num_basic_tests; i++) {
                send_request_and_analyze(&basic_tests[i]);
                sleep(1);
            }
        } else if (strcmp(test_arg, "headers") == 0) {
            test_custom_headers();
        } else if (strcmp(test_arg, "comprehensive") == 0) {
            printf("Running comprehensive test suite...\n");
            for (int i = 0; i < num_basic_tests; i++) {
                send_request_and_analyze(&basic_tests[i]);
                sleep(1);
            }
            test_custom_headers();
        } else {
            int test_num = atoi(test_arg);
            if (test_num >= 1 && test_num <= num_basic_tests) {
                send_request_and_analyze(&basic_tests[test_num - 1]);
            } else {
                printf("Invalid test. Use --help for available options.\n");
                return 1;
            }
        }
    }
    
    return 0;
}