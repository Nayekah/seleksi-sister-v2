#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define BUFFER_SIZE 8192
#define SERVER_PORT 6969
#define SERVER_IP "127.0.0.1"

typedef struct {
    char *method;
    char *path;
    char *headers;
    char *body;
    char *description;
} test_request;

int connect_to_server() {
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        perror("Socket creation failed");
        return -1;
    }
    
    struct sockaddr_in server_addr;
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(SERVER_PORT);
    server_addr.sin_addr.s_addr = inet_addr(SERVER_IP);
    
    if (connect(sock, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
        perror("Connection failed");
        close(sock);
        return -1;
    }
    
    return sock;
}

void parse_response_headers(const char* response, ssize_t total_bytes) {
    printf("\n=== RESPONSE HEADER ANALYSIS ===\n");
    
    // Find end of headers
    const char* header_end = strstr(response, "\r\n\r\n");
    if (!header_end) {
        printf("No headers found or malformed response\n");
        return;
    }
    
    // Parse status line
    char status_line[256] = {0};
    const char* first_crlf = strstr(response, "\r\n");
    if (first_crlf) {
        int status_len = first_crlf - response;
        if (status_len < 255) {
            strncpy(status_line, response, status_len);
            printf("Status Line: %s\n", status_line);
        }
    }
    
    // Parse individual headers
    const char* current = response + strlen(status_line) + 2; // Skip status + CRLF
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
    
    // Calculate body size correctly - FIXED!
    const char* body_start = header_end + 4;
    int header_length = body_start - response;
    int body_size = total_bytes - header_length;
    
    printf("Body Size: %d bytes (calculated from total %zd bytes - %d header bytes)\n", 
           body_size, total_bytes, header_length);
}

void send_request_and_analyze(test_request *req) {
    printf("\n" "========================================================\n");
    printf("TEST: %s\n", req->description);
    printf("REQUEST: %s %s\n", req->method, req->path);
    printf("========================================================\n");
    
    int sock = connect_to_server();
    if (sock < 0) {
        printf("Failed to connect to server\n");
        return;
    }
    
    // Build HTTP request
    char request[BUFFER_SIZE];
    int content_length = req->body ? strlen(req->body) : 0;
    
    // Build request with all headers
    snprintf(request, sizeof(request),
        "%s %s HTTP/1.1\r\n"
        "Host: localhost:%d\r\n"
        "User-Agent: HTTPTester/1.0\r\n"
        "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n"
        "Accept-Language: en-US,en;q=0.5\r\n"
        "Accept-Encoding: gzip, deflate\r\n"
        "Connection: close\r\n"
        "%s"
        "%s"
        "%s"
        "\r\n"
        "%s",
        req->method, req->path, SERVER_PORT,
        req->headers ? req->headers : "",
        content_length > 0 ? "Content-Type: application/x-www-form-urlencoded\r\n" : "",
        content_length > 0 ? "Content-Length: " : "",
        req->body ? req->body : "");
    
    // Add content-length value if needed
    if (content_length > 0) {
        char temp[BUFFER_SIZE];
        snprintf(temp, sizeof(temp),
            "%s %s HTTP/1.1\r\n"
            "Host: localhost:%d\r\n"
            "User-Agent: HTTPTester/1.0\r\n"
            "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n"
            "Accept-Language: en-US,en;q=0.5\r\n"
            "Accept-Encoding: gzip, deflate\r\n"
            "Connection: close\r\n"
            "%s"
            "Content-Type: application/x-www-form-urlencoded\r\n"
            "Content-Length: %d\r\n"
            "\r\n"
            "%s",
            req->method, req->path, SERVER_PORT,
            req->headers ? req->headers : "",
            content_length,
            req->body);
        strcpy(request, temp);
    }
    
    printf("=== SENDING REQUEST ===\n");
    printf("%s", request);
    printf("=== END REQUEST ===\n");
    
    // Send request
    if (send(sock, request, strlen(request), 0) < 0) {
        perror("Send failed");
        close(sock);
        return;
    }
    
    // Receive response - potentially multiple recv calls for large responses
    char response[BUFFER_SIZE];
    memset(response, 0, sizeof(response));
    ssize_t total_received = 0;
    ssize_t bytes_received;
    
    // Read response in chunks until connection closes
    while ((bytes_received = recv(sock, response + total_received, 
                                 sizeof(response) - total_received - 1, 0)) > 0) {
        total_received += bytes_received;
        if (total_received >= sizeof(response) - 1) {
            break; // Buffer full
        }
    }
    
    if (total_received > 0) {
        response[total_received] = '\0'; // Null terminate for string functions
        printf("\n=== FULL RESPONSE (%zd bytes total) ===\n", total_received);
        
        // Show first part of response for readability
        if (total_received > 1000) {
            printf("%.1000s...\n[Response truncated for readability - total size: %zd bytes]\n", 
                   response, total_received);
        } else {
            // For smaller responses, show everything but handle null bytes
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
        
        // Analyze headers with correct body size calculation
        parse_response_headers(response, total_received);
        
        // Test specific server behavior
        printf("\n=== SERVER BEHAVIOR ANALYSIS ===\n");
        if (strstr(response, "HTTP/1.1 200 OK")) {
            printf("✅ Correct HTTP/1.1 200 response\n");
        } else if (strstr(response, "HTTP/1.1 404")) {
            printf("✅ Correct HTTP/1.1 404 response\n");
        } else if (strstr(response, "HTTP/1.1 302")) {
            printf("✅ Correct HTTP/1.1 302 redirect response\n");
        } else {
            printf("⚠️  Unexpected status code\n");
        }
        
        if (strstr(response, "Connection: close")) {
            printf("✅ Connection: close header present\n");
        } else {
            printf("⚠️  Missing Connection: close header\n");
        }
        
        if (strstr(response, "Content-Type:")) {
            printf("✅ Content-Type header present\n");
        } else {
            printf("⚠️  Missing Content-Type header\n");
        }
        
        // Additional analysis for POST /calculate
        if (strcmp(req->method, "POST") == 0 && strcmp(req->path, "/calculate") == 0) {
            // Extract names from the POST body to check for them in response
            char name1[64] = {0};
            char name2[64] = {0};
            
            if (req->body) {
                // Parse name1=XXX&name2=YYY format
                char *name1_start = strstr(req->body, "name1=");
                char *name2_start = strstr(req->body, "name2=");
                
                if (name1_start) {
                    name1_start += 6; // Skip "name1="
                    char *name1_end = strchr(name1_start, '&');
                    int name1_len = name1_end ? (name1_end - name1_start) : strlen(name1_start);
                    if (name1_len < 63) {
                        strncpy(name1, name1_start, name1_len);
                        name1[name1_len] = '\0';
                    }
                }
                
                if (name2_start) {
                    name2_start += 6; // Skip "name2="
                    char *name2_end = strchr(name2_start, '&');
                    int name2_len = name2_end ? (name2_end - name2_start) : strlen(name2_start);
                    if (name2_len < 63) {
                        strncpy(name2, name2_start, name2_len);
                        name2[name2_len] = '\0';
                    }
                }
                
                // Check if both names appear in the response
                if (strlen(name1) > 0 && strlen(name2) > 0) {
                    if (strstr(response, name1) && strstr(response, name2)) {
                        printf("✅ Love calculation working - names '%s' and '%s' found in response\n", name1, name2);
                    } else {
                        printf("⚠️  Love calculation may not be working - names '%s' and '%s' not found in response\n", name1, name2);
                    }
                } else {
                    // Fallback: look for any percentage pattern
                    if (strstr(response, "%") && (strstr(response, "Love") || strstr(response, "Match"))) {
                        printf("✅ Love calculation appears to be working - percentage and love-related text found\n");
                    } else {
                        printf("⚠️  Love calculation may not be working - no percentage or love text found\n");
                    }
                }
            }
        }
        
    } else {
        printf("No response received or connection closed immediately\n");
    }
    
    close(sock);
}

void test_custom_headers() {
    printf("\n" "========================================================\n");
    printf("TESTING CUSTOM HEADERS HANDLING\n");
    printf("========================================================\n");
    
    test_request custom_tests[] = {
        {"GET", "/main", "X-Custom-Header: test-value\r\n", NULL, "Custom header test"},
        {"GET", "/calculate", "Authorization: Bearer token123\r\n", NULL, "Authorization header test"},
        {"POST", "/calculate", "X-CSRF-Token: abc123\r\nReferer: http://localhost:6969/\r\n", 
         "name1=TestUser&name2=TestPartner", "Multiple custom headers with POST"},
        {"GET", "/notes", "Cookie: sessionid=12345; csrftoken=abcde\r\n", NULL, "Cookie header test"},
    };
    
    int num_tests = sizeof(custom_tests) / sizeof(custom_tests[0]);
    for (int i = 0; i < num_tests; i++) {
        send_request_and_analyze(&custom_tests[i]);
        sleep(1);
    }
}

int main(int argc, char *argv[]) {
    printf("Fixed HTTP Server Request Tester\n");
    printf("Testing server at %s:%d\n", SERVER_IP, SERVER_PORT);
    
    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
        printf("\nUsage: %s [test_number|option]\n", argv[0]);
        printf("Available tests:\n");
        printf("  1 - GET /main (main page)\n");
        printf("  2 - GET /calculate (love calculator page)\n");
        printf("  3 - POST /calculate (love calculation)\n");
        printf("  4 - GET /notes (notes page)\n");
        printf("  5 - POST /notes (create note)\n");
        printf("  6 - PUT /notes (update note)\n");
        printf("  7 - DELETE /notes (delete note)\n");
        printf("  8 - GET /eta.mp4 (video file)\n");
        printf("  9 - GET /nonexistent (404 test)\n");
        printf("  all - Run all basic tests\n");
        printf("  headers - Test custom headers\n");
        printf("  methods - Test all HTTP methods\n");
        printf("  comprehensive - Run all tests including headers\n");
        return 0;
    }
    
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
    
    if (argc > 1) {
        if (strcmp(argv[1], "all") == 0) {
            for (int i = 0; i < num_basic_tests; i++) {
                send_request_and_analyze(&basic_tests[i]);
                sleep(1);
            }
        } else if (strcmp(argv[1], "headers") == 0) {
            test_custom_headers();
        } else if (strcmp(argv[1], "methods") == 0) {
            printf("Testing HTTP methods with detailed analysis...\n");
            for (int i = 0; i < 7; i++) { // Skip video and 404 tests
                send_request_and_analyze(&basic_tests[i]);
                sleep(1);
            }
        } else if (strcmp(argv[1], "comprehensive") == 0) {
            printf("Running comprehensive test suite...\n");
            for (int i = 0; i < num_basic_tests; i++) {
                send_request_and_analyze(&basic_tests[i]);
                sleep(1);
            }
            test_custom_headers();
        } else {
            int test_num = atoi(argv[1]);
            if (test_num >= 1 && test_num <= num_basic_tests) {
                send_request_and_analyze(&basic_tests[test_num - 1]);
            } else {
                printf("Invalid test number. Use --help for available tests.\n");
                return 1;
            }
        }
    } else {
        printf("\nRunning basic connectivity test with header analysis...\n");
        send_request_and_analyze(&basic_tests[0]);
        
        printf("\n\nTo run specific tests, use:\n");
        printf("  %s [test_number]     - Run specific test (1-%d)\n", argv[0], num_basic_tests);
        printf("  %s all              - Run all basic tests\n", argv[0]);
        printf("  %s headers          - Test custom headers\n", argv[0]);
        printf("  %s methods          - Test HTTP methods\n", argv[0]);
        printf("  %s comprehensive    - Run everything\n", argv[0]);
        printf("  %s --help           - Show detailed help\n", argv[0]);
    }
    
    return 0;
}