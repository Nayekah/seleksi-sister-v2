#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <errno.h>

#define MAX_NOTE_LENGTH 250
#define NOTES_DIR "./notes_data"

int ensure_notes_directory() {
    struct stat st = {0};
    if (stat(NOTES_DIR, &st) == -1) {
        return mkdir(NOTES_DIR, 0755);
    }
    return 0;
}

void get_note_file_path(const char *client_id, char *path, size_t path_size) {
    if (client_id && strlen(client_id) > 0) {
        snprintf(path, path_size, "%s/note_%s.txt", NOTES_DIR, client_id);
    } else {
        snprintf(path, path_size, "%s/note_default.txt", NOTES_DIR);
    }
}

void read_note(const char *client_id) {
    char note_file[512];
    get_note_file_path(client_id, note_file, sizeof(note_file));
    
    FILE *file = fopen(note_file, "r");
    if (!file) {
        printf("No notes yet. Click \"Create Note\" to add your first note!");
        return;
    }
    
    char buffer[MAX_NOTE_LENGTH + 1];
    memset(buffer, 0, sizeof(buffer));
    size_t read_bytes = fread(buffer, 1, MAX_NOTE_LENGTH, file);
    
    fclose(file);
    
    if (read_bytes == 0) {
        printf("No notes yet. Click \"Create Note\" to add your first note!");
    } else {
        buffer[read_bytes] = '\0';
        for (int i = read_bytes - 1; i >= 0; i--) {
            if (buffer[i] == '\n' || buffer[i] == '\r') {
                buffer[i] = '\0';
            } else {
                break;
            }
        }
        printf("%s", buffer);
    }
}

int write_note(const char *client_id, const char *content) {
    if (ensure_notes_directory() != 0) {
        return 1;
    }
    
    char note_file[512];
    get_note_file_path(client_id, note_file, sizeof(note_file));
    
    FILE *file = fopen(note_file, "w");
    if (!file) {
        return 1;
    }
    
    size_t content_len = strlen(content);
    if (content_len > MAX_NOTE_LENGTH) {
        content_len = MAX_NOTE_LENGTH;
    }
    
    fwrite(content, 1, content_len, file);
    fclose(file);
    return 0;
}

int delete_note(const char *client_id) {
    char note_file[512];
    get_note_file_path(client_id, note_file, sizeof(note_file));
    
    if (unlink(note_file) != 0) {
        if (errno == ENOENT) {
            return 0;
        } else {
            return 1;
        }
    }
    return 0;
}

void url_decode(char *dst, const char *src) {
    char *p = dst;
    char code[3];
    
    while (*src) {
        if (*src == '%') {
            if (src[1] && src[2]) {
                code[0] = src[1];
                code[1] = src[2];
                code[2] = '\0';
                *p++ = (char)strtol(code, NULL, 16);
                src += 3;
            } else {
                *p++ = *src++;
            }
        } else if (*src == '+') {
            *p++ = ' ';
            src++;
        } else {
            *p++ = *src++;
        }
    }
    *p = '\0';
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("ERROR: No operation specified");
        return 1;
    }
    
    const char *operation = argv[1];
    const char *client_id = (argc > 2) ? argv[2] : "default";
    
    if (strcmp(operation, "READ") == 0) {
        read_note(client_id);
        return 0;
    }
    
    if (strcmp(operation, "CREATE") == 0 || strcmp(operation, "UPDATE") == 0) {
        if (argc < 3) {
            printf("ERROR: No content provided");
            return 1;
        }
        
        const char *content = (argc > 3) ? argv[3] : argv[2];
        char decoded_content[MAX_NOTE_LENGTH + 1];
        url_decode(decoded_content, content);
        
        if (strlen(decoded_content) > MAX_NOTE_LENGTH) {
            printf("ERROR: Note too long (max %d characters)", MAX_NOTE_LENGTH);
            return 1;
        }
        
        if (strlen(decoded_content) == 0) {
            printf("ERROR: Empty note content");
            return 1;
        }
        
        if (write_note(client_id, decoded_content) != 0) {
            printf("ERROR: Failed to save note");
            return 1;
        }
        
        printf("SUCCESS");
        return 0;
    }
    
    if (strcmp(operation, "DELETE") == 0) {
        if (delete_note(client_id) != 0) {
            printf("ERROR: Failed to delete note");
            return 1;
        }
        
        printf("SUCCESS");
        return 0;
    }
    
    printf("ERROR: Unknown operation");
    return 1;
}