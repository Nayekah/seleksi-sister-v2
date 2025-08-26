SECTION .data
    ; Server Configuration
    port            dw 6969
    backlog         equ 5
    buffer_size     equ 65536
    
    ; Socket Structure for bind()
    sockaddr:
        sin_family  dw 2        ; AF_INET
        sin_port    dw 0x391b   ; port 6969 in network byte order
        sin_addr    dd 0        ; INADDR_ANY
        sin_zero    times 8 db 0

    ; HTTP Response Templates
    http_200        db 'HTTP/1.1 200 OK', 13, 10
                    db 'Content-Type: text/html', 13, 10
                    db 'Connection: close', 13, 10, 13, 10, 0
    
    http_200_video  db 'HTTP/1.1 200 OK', 13, 10
                    db 'Content-Type: video/mp4', 13, 10
                    db 'Accept-Ranges: bytes', 13, 10
                    db 'Connection: close', 13, 10, 13, 10, 0
    
    http_404        db 'HTTP/1.1 404 Not Found', 13, 10
                    db 'Content-Type: text/html', 13, 10
                    db 'Connection: close', 13, 10, 13, 10
                    db '<html><body><h1>404 Not Found</h1></body></html>', 0

    ; File paths
    main_file       db './www/index.html', 0
    love_file       db './www/love.html', 0
    notes_file      db './www/notes.html', 0
    www_root        db './www/', 0
    
    ; External programs
    lovecalc_prog   db './lovecalc', 0
    notes_prog      db './notes', 0

    ; Template placeholders and replacements
    placeholder     db 'PLACEHOLDER_FOR_RESULT_CONTENT_HERE_123456789', 0
    default_content db 'Enter two names above and click Calculate Love!', 0

SECTION .bss
    socket_fd       resq 1
    client_fd       resq 1
    client_id_str   resb 32
    buffer          resb 65536
    file_buffer     resb 65536
    name1_buffer    resb 64
    name2_buffer    resb 64
    result_content_buffer resb 1024
    notes_content_buffer resb 512
    temp_buffer     resb 512
    exec_result     resb 256
    notes_exec_result resb 256       ; NEW: Separate buffer for notes
    form_content_buffer resb 8192

SECTION .text
    global _start

_start:
    mov     rax, 41                     ; sys_socket
    mov     rdi, 2                      ; AF_INET
    mov     rsi, 1                      ; SOCK_STREAM
    mov     rdx, 6                      ; IPPROTO_TCP
    syscall
    test    rax, rax
    js      exit_error
    mov     [socket_fd], rax

    ; Set socket options SO_REUSEADDR
    mov     rax, 54                     ; sys_setsockopt
    mov     rdi, [socket_fd]
    mov     rsi, 1                      ; SOL_SOCKET
    mov     rdx, 2                      ; SO_REUSEADDR
    mov     r10, 1
    push    r10
    mov     r10, rsp
    mov     r8, 4
    syscall
    add     rsp, 8

bind_socket:
    mov     rax, 49                     ; sys_bind
    mov     rdi, [socket_fd]
    mov     rsi, sockaddr
    mov     rdx, 16
    syscall
    test    rax, rax
    js      exit_error

listen_socket:
    mov     rax, 50                     ; sys_listen
    mov     rdi, [socket_fd]
    mov     rsi, backlog
    syscall
    test    rax, rax
    js      exit_error

accept_loop:
    mov     rax, 43                     ; sys_accept
    mov     rdi, [socket_fd]
    mov     rsi, 0
    mov     rdx, 0
    syscall
    test    rax, rax
    js      accept_loop
    mov     [client_fd], rax

    ; Fork for each request
    mov     rax, 57                     ; sys_fork
    syscall
    test    rax, rax
    jz      handle_client
    
    ; Parent process - close client socket and continue
    mov     rax, 3                      ; sys_close
    mov     rdi, [client_fd]
    syscall
    
    ; Reap zombie children
    mov     rax, 61                     ; sys_wait4
    mov     rdi, -1
    mov     rsi, 0
    mov     rdx, 1                      ; WNOHANG
    mov     r10, 0
    syscall
    
    jmp     accept_loop

handle_client:
    ; Close server socket in child
    mov     rax, 3
    mov     rdi, [socket_fd]
    syscall

    ; Clear buffer
    mov     rdi, buffer
    mov     rcx, buffer_size
    xor     rax, rax
    rep     stosb

    ; Read request
    mov     rax, 0                      ; sys_read
    mov     rdi, [client_fd]
    mov     rsi, buffer
    mov     rdx, buffer_size - 1
    syscall
    test    rax, rax
    jle     .close_client
    
    ; Null terminate buffer
    mov     byte [buffer + rax], 0

    ; Parse and handle request
    call    parse_and_handle_request

.close_client:
    mov     rax, 3                      ; sys_close
    mov     rdi, [client_fd]
    syscall

    ; Exit child process
    mov     rax, 60                     ; sys_exit
    mov     rdi, 0
    syscall

parse_and_handle_request:
    ; Find first space to get method end
    mov     rsi, buffer
.find_method_end:
    lodsb
    cmp     al, ' '
    jne     .find_method_end

    ; Now at start of path, check routing
    call    check_request_routing
    ret

check_request_routing:
    ; Check method type
    mov     rdx, buffer
    
    ; Check if it's a POST request
    cmp     dword [rdx], 'POST'
    je      .handle_post_request
    
    ; Check if it's a PUT request
    cmp     dword [rdx], 'PUT '
    je      .handle_put_request
    
    ; Check if it's a DELETE request
    cmp     byte [rdx], 'D'
    jne     .handle_get_request
    cmp     byte [rdx + 1], 'E'
    jne     .handle_get_request
    cmp     byte [rdx + 2], 'L'
    jne     .handle_get_request
    cmp     byte [rdx + 3], 'E'
    jne     .handle_get_request
    cmp     byte [rdx + 4], 'T'
    jne     .handle_get_request
    cmp     byte [rdx + 5], 'E'
    jne     .handle_get_request
    cmp     byte [rdx + 6], ' '
    je      .handle_delete_request
    
.handle_get_request:
    ; Check for GET /main
    cmp     byte [rsi], '/'
    jne     .try_files
    cmp     byte [rsi + 1], 'm'
    jne     .check_calculate
    cmp     byte [rsi + 2], 'a'
    jne     .check_calculate
    cmp     byte [rsi + 3], 'i'
    jne     .check_calculate
    cmp     byte [rsi + 4], 'n'
    jne     .check_calculate
    
    mov     al, [rsi + 5]
    cmp     al, ' '
    je      serve_main_page
    cmp     al, '?'
    je      serve_main_page
    cmp     al, 0
    je      serve_main_page

.check_calculate:
    ; Check for GET /calculate
    cmp     byte [rsi], '/'
    jne     .check_notes
    cmp     byte [rsi + 1], 'c'
    jne     .check_notes
    cmp     byte [rsi + 2], 'a'
    jne     .check_notes
    cmp     byte [rsi + 3], 'l'
    jne     .check_notes
    cmp     byte [rsi + 4], 'c'
    jne     .check_notes
    cmp     byte [rsi + 5], 'u'
    jne     .check_notes
    cmp     byte [rsi + 6], 'l'
    jne     .check_notes
    cmp     byte [rsi + 7], 'a'
    jne     .check_notes
    cmp     byte [rsi + 8], 't'
    jne     .check_notes
    cmp     byte [rsi + 9], 'e'
    jne     .check_notes
    
    mov     al, [rsi + 10]
    cmp     al, ' '
    je      serve_calculate_page
    cmp     al, '?'
    je      serve_calculate_page
    cmp     al, 0
    je      serve_calculate_page

.check_notes:
    ; Check for GET /notes
    cmp     byte [rsi], '/'
    jne     .check_eta_video
    cmp     byte [rsi + 1], 'n'
    jne     .check_eta_video
    cmp     byte [rsi + 2], 'o'
    jne     .check_eta_video
    cmp     byte [rsi + 3], 't'
    jne     .check_eta_video
    cmp     byte [rsi + 4], 'e'
    jne     .check_eta_video
    cmp     byte [rsi + 5], 's'
    jne     .check_eta_video
    
    mov     al, [rsi + 6]
    cmp     al, ' '
    je      serve_notes_page
    cmp     al, '?'
    je      serve_notes_page
    cmp     al, 0
    je      serve_notes_page

.check_eta_video:
    ; Check for GET /eta.mp4
    cmp     byte [rsi], '/'
    jne     .try_files
    cmp     byte [rsi + 1], 'e'
    jne     .try_files
    cmp     byte [rsi + 2], 't'
    jne     .try_files
    cmp     byte [rsi + 3], 'a'
    jne     .try_files
    cmp     byte [rsi + 4], '.'
    jne     .try_files
    cmp     byte [rsi + 5], 'm'
    jne     .try_files
    cmp     byte [rsi + 6], 'p'
    jne     .try_files
    cmp     byte [rsi + 7], '4'
    jne     .try_files
    
    mov     al, [rsi + 8]
    cmp     al, ' '
    je      serve_eta_video
    cmp     al, '?'
    je      serve_eta_video
    cmp     al, 0
    je      serve_eta_video
    jmp     .try_files

.handle_post_request:
    ; Check if path is /calculate or /notes
    cmp     byte [rsi], '/'
    jne     .try_files
    
    ; Check for /calculate
    cmp     byte [rsi + 1], 'c'
    jne     .check_post_notes
    cmp     byte [rsi + 2], 'a'
    jne     .check_post_notes
    cmp     byte [rsi + 3], 'l'
    jne     .check_post_notes
    cmp     byte [rsi + 4], 'c'
    jne     .check_post_notes
    cmp     byte [rsi + 5], 'u'
    jne     .check_post_notes
    cmp     byte [rsi + 6], 'l'
    jne     .check_post_notes
    cmp     byte [rsi + 7], 'a'
    jne     .check_post_notes
    cmp     byte [rsi + 8], 't'
    jne     .check_post_notes
    cmp     byte [rsi + 9], 'e'
    jne     .check_post_notes
    
    mov     al, [rsi + 10]
    cmp     al, ' '
    je      handle_love_calculation
    cmp     al, '?'
    je      handle_love_calculation
    cmp     al, 0
    je      handle_love_calculation

.check_post_notes:
    ; Check for /notes
    cmp     byte [rsi + 1], 'n'
    jne     .try_files
    cmp     byte [rsi + 2], 'o'
    jne     .try_files
    cmp     byte [rsi + 3], 't'
    jne     .try_files
    cmp     byte [rsi + 4], 'e'
    jne     .try_files
    cmp     byte [rsi + 5], 's'
    jne     .try_files
    
    mov     al, [rsi + 6]
    cmp     al, ' '
    je      handle_notes_create
    cmp     al, '?'
    je      handle_notes_create
    cmp     al, 0
    je      handle_notes_create
    jmp     .try_files

.handle_put_request:
    ; Check if path is /notes
    cmp     byte [rsi], '/'
    jne     .try_files
    cmp     byte [rsi + 1], 'n'
    jne     .try_files
    cmp     byte [rsi + 2], 'o'
    jne     .try_files
    cmp     byte [rsi + 3], 't'
    jne     .try_files
    cmp     byte [rsi + 4], 'e'
    jne     .try_files
    cmp     byte [rsi + 5], 's'
    jne     .try_files
    
    mov     al, [rsi + 6]
    cmp     al, ' '
    je      handle_notes_update
    cmp     al, '?'
    je      handle_notes_update
    cmp     al, 0
    je      handle_notes_update
    jmp     .try_files

.handle_delete_request:
    ; Check if path is /notes
    cmp     byte [rsi], '/'
    jne     .try_files
    cmp     byte [rsi + 1], 'n'
    jne     .try_files
    cmp     byte [rsi + 2], 'o'
    jne     .try_files
    cmp     byte [rsi + 3], 't'
    jne     .try_files
    cmp     byte [rsi + 4], 'e'
    jne     .try_files
    cmp     byte [rsi + 5], 's'
    jne     .try_files
    
    mov     al, [rsi + 6]
    cmp     al, ' '
    je      handle_notes_delete
    cmp     al, '?'
    je      handle_notes_delete
    cmp     al, 0
    je      handle_notes_delete
    jmp     .try_files

.try_files:
    call    build_file_path_from_request
    call    serve_file_or_404
    ret

serve_main_page:
    mov     rdi, main_file
    call    serve_file_or_404
    ret

serve_calculate_page:
    call    load_love_file
    test    r8, r8
    jz      send_404
    
    call    replace_placeholder_with_default
    call    send_200_header
    call    send_file_content
    ret

serve_notes_page:
    ; Generate unique client ID per request
    call    generate_client_id
    
    call    load_notes_file
    test    r8, r8
    jz      send_404
    
    call    load_current_note
    call    find_and_replace_notes_placeholder
    call    send_200_header
    call    send_file_content
    ret

handle_love_calculation:
    call    parse_post_data
    call    call_love_calculator
    call    load_love_file
    test    r8, r8
    jz      send_404
    
    call    replace_placeholder_with_result
    call    send_200_header
    call    send_file_content
    ret

handle_notes_create:
    ; Generate unique client ID per request
    call    generate_client_id
    
    call    parse_notes_post_data
    call    call_notes_create
    call    redirect_to_notes
    ret

handle_notes_update:
    ; Generate unique client ID per request
    call    generate_client_id
    
    call    parse_notes_post_data
    call    call_notes_update
    call    redirect_to_notes
    ret

handle_notes_delete:
    ; Generate unique client ID per request
    call    generate_client_id
    
    call    call_notes_delete
    call    redirect_to_notes
    ret

serve_eta_video:
    mov     rdi, .eta_video_path
    call    serve_video_file
    ret

.eta_video_path db './www/eta.mp4', 0

serve_video_file:
    mov     rax, 2                      ; sys_open
    mov     rsi, 0                      ; O_RDONLY
    syscall
    test    rax, rax
    js      send_404

    mov     r9, rax
    call    send_video_header
    
.read_chunk:
    mov     rdi, file_buffer
    mov     rcx, 65536
    xor     rax, rax
    rep     stosb
    
    mov     rax, 0                      ; sys_read
    mov     rdi, r9
    mov     rsi, file_buffer
    mov     rdx, 65536
    syscall
    
    test    rax, rax
    jle     .done_reading
    
    mov     r8, rax
    mov     rax, 1                      ; sys_write
    mov     rdi, [client_fd]
    mov     rsi, file_buffer
    mov     rdx, r8
    syscall
    
    test    rax, rax
    jg      .read_chunk

.done_reading:
    mov     rax, 3                      ; sys_close
    mov     rdi, r9
    syscall
    ret

serve_file_or_404:
    mov     rax, 2                      ; sys_open
    mov     rsi, 0                      ; O_RDONLY
    syscall
    test    rax, rax
    js      send_404

    mov     rdi, rax
    push    rdi
    
    mov     rdi, file_buffer
    mov     rcx, 65536
    xor     rax, rax
    rep     stosb
    
    pop     rdi
    push    rdi
    mov     rax, 0                      ; sys_read
    mov     rsi, file_buffer
    mov     rdx, 65535
    syscall
    mov     r8, rax
    
    mov     byte [file_buffer + r8], 0
    
    pop     rdi
    push    r8
    mov     rax, 3                      ; sys_close
    syscall
    pop     r8

    call    send_200_header
    call    send_file_content
    ret

load_love_file:
    mov     rax, 2                      ; sys_open
    mov     rdi, love_file
    mov     rsi, 0                      ; O_RDONLY
    syscall
    test    rax, rax
    js      .error
    
    mov     rdi, rax
    push    rdi
    
    push    rdi
    mov     rdi, file_buffer
    mov     rcx, 65536
    xor     rax, rax
    rep     stosb
    pop     rdi
    
    mov     rax, 0                      ; sys_read
    mov     rsi, file_buffer
    mov     rdx, 65535
    syscall
    mov     r8, rax
    
    mov     byte [file_buffer + r8], 0
    
    pop     rdi
    mov     rax, 3                      ; sys_close
    syscall
    ret

.error:
    mov     r8, 0
    ret

load_notes_file:
    mov     rax, 2                      ; sys_open
    mov     rdi, notes_file
    mov     rsi, 0                      ; O_RDONLY
    syscall
    test    rax, rax
    js      .error
    
    mov     rdi, rax
    push    rdi
    
    push    rdi
    mov     rdi, file_buffer
    mov     rcx, 65536
    xor     rax, rax
    rep     stosb
    pop     rdi
    
    mov     rax, 0                      ; sys_read
    mov     rsi, file_buffer
    mov     rdx, 65535
    syscall
    mov     r8, rax
    
    mov     byte [file_buffer + r8], 0
    
    pop     rdi
    mov     rax, 3                      ; sys_close
    syscall
    ret

.error:
    mov     r8, 0
    ret

load_current_note:
    mov     rdi, notes_content_buffer
    mov     rcx, 512
    xor     rax, rax
    rep     stosb
    
    call    call_notes_read
    
    mov     rsi, notes_exec_result      ; Use separate buffer
    mov     rdi, notes_content_buffer
    call    copy_string_simple
    ret

parse_notes_post_data:
    mov     rdi, form_content_buffer
    mov     rcx, 8192
    xor     rax, rax
    rep     stosb
    
    mov     rsi, buffer
.find_body:
    cmp     word [rsi], 0x0A0D
    jne     .next_char
    cmp     word [rsi + 2], 0x0A0D
    je      .found_body
.next_char:
    inc     rsi
    cmp     rsi, buffer + 4000
    jl      .find_body
    ret

.found_body:
    add     rsi, 4
    call    extract_content_from_form
    ret

extract_content_from_form:
    mov     rsi, buffer
.scan:
    cmp     byte [rsi], 0
    je      .not_found
    
    cmp     byte [rsi], 'c'
    jne     .next
    cmp     byte [rsi + 1], 'o'
    jne     .next
    cmp     byte [rsi + 2], 'n'
    jne     .next
    cmp     byte [rsi + 3], 't'
    jne     .next
    cmp     byte [rsi + 4], 'e'
    jne     .next
    cmp     byte [rsi + 5], 'n'
    jne     .next
    cmp     byte [rsi + 6], 't'
    jne     .next
    cmp     byte [rsi + 7], '='
    jne     .next
    
    add     rsi, 8
    mov     rdi, form_content_buffer
    call    extract_url_encoded_value
    ret

.next:
    inc     rsi
    cmp     rsi, buffer + 4000
    jl      .scan
.not_found:
    ret

extract_url_encoded_value:
    xor     rcx, rcx
.loop:
    mov     al, [rsi]
    
    cmp     al, '&'
    je      .done
    cmp     al, ' '
    je      .done
    cmp     al, 13
    je      .done
    cmp     al, 10
    je      .done
    cmp     al, 0
    je      .done
    
    cmp     al, '+'
    jne     .store_char
    mov     al, ' '
    
.store_char:
    mov     [rdi + rcx], al
    inc     rsi
    inc     rcx
    cmp     rcx, 8191
    jl      .loop

.done:
    mov     byte [rdi + rcx], 0
    ret

generate_client_id:
    ; Clear client_id_str buffer
    mov     rdi, client_id_str
    mov     rcx, 32
    xor     rax, rax
    rep     stosb
    
    ; Get current time in seconds
    mov     rax, 201                    ; sys_time
    mov     rdi, 0
    syscall
    
    ; Truncate to 10-minute intervals for session persistence
    ; This gives each "session" a 10-minute window
    mov     rbx, 600                    ; 600 seconds = 10 minutes
    xor     rdx, rdx
    div     rbx
    ; Now rax contains the 10-minute interval number
    
    mov     rdi, client_id_str
    call    int_to_string_at_pos
    ret

call_notes_read:
    call    execute_notes_program_read
    ret

call_notes_create:
    call    execute_notes_program_create
    ret

call_notes_update:
    call    execute_notes_program_update
    ret

call_notes_delete:
    call    execute_notes_program_delete
    ret

execute_notes_program_read:
    mov     rax, 2                      ; sys_open
    mov     rdi, .temp_file
    mov     rsi, 0x241                  ; O_WRONLY | O_CREAT | O_TRUNC
    mov     rdx, 0644o
    syscall
    test    rax, rax
    js      .use_default_result
    
    mov     rdi, rax
    mov     rax, 3                      ; sys_close
    syscall
    
    mov     rax, 57                     ; sys_fork
    syscall
    test    rax, rax
    jz      .child_read
    
    mov     rax, 61                     ; sys_wait4
    mov     rdi, -1
    mov     rsi, 0
    mov     rdx, 0
    mov     r10, 0
    syscall
    
    call    read_notes_temp_result
    ret

.child_read:
    mov     rax, 2                      ; sys_open
    mov     rdi, .temp_file
    mov     rsi, 0x241                  ; O_WRONLY | O_CREAT | O_TRUNC
    mov     rdx, 0644o
    syscall
    
    mov     rdi, rax
    mov     rax, 33                     ; sys_dup2
    mov     rsi, 1                      ; stdout
    syscall
    
    mov     rax, 3
    syscall
    
    sub     rsp, 32
    mov     qword [rsp], notes_prog
    mov     qword [rsp + 8], .read_cmd
    mov     qword [rsp + 16], client_id_str
    mov     qword [rsp + 24], 0
    
    mov     rax, 59                     ; sys_execve
    mov     rdi, notes_prog
    mov     rsi, rsp
    mov     rdx, 0
    syscall
    
    add     rsp, 32
    mov     rax, 60                     ; sys_exit
    mov     rdi, 1
    syscall

.use_default_result:
    mov     rsi, .default_notes
    mov     rdi, notes_exec_result      ; Use separate buffer
    call    copy_string_simple
    ret

.temp_file      db '/tmp/notes_result', 0
.read_cmd       db 'READ', 0
.default_notes  db 'No notes yet. Click "Create Note" to add your first note!', 0

execute_notes_program_create:
    call    execute_notes_program_with_content
    ret

execute_notes_program_update:
    call    execute_notes_program_with_content
    ret

execute_notes_program_with_content:
    mov     rax, 57                     ; sys_fork
    syscall
    test    rax, rax
    jz      .child_create
    
    mov     rax, 61                     ; sys_wait4
    mov     rdi, -1
    mov     rsi, 0
    mov     rdx, 0
    mov     r10, 0
    syscall
    ret

.child_create:
    sub     rsp, 40
    mov     qword [rsp], notes_prog
    mov     qword [rsp + 8], .create_cmd
    mov     qword [rsp + 16], client_id_str
    mov     qword [rsp + 24], form_content_buffer
    mov     qword [rsp + 32], 0
    
    mov     rax, 59                     ; sys_execve
    mov     rdi, notes_prog
    mov     rsi, rsp
    mov     rdx, 0
    syscall
    
    add     rsp, 40
    mov     rax, 60                     ; sys_exit
    mov     rdi, 1
    syscall

.create_cmd db 'CREATE', 0

execute_notes_program_delete:
    mov     rax, 57                     ; sys_fork
    syscall
    test    rax, rax
    jz      .child_delete
    
    mov     rax, 61                     ; sys_wait4
    mov     rdi, -1
    mov     rsi, 0
    mov     rdx, 0
    mov     r10, 0
    syscall
    ret

.child_delete:
    sub     rsp, 32
    mov     qword [rsp], notes_prog
    mov     qword [rsp + 8], .delete_cmd
    mov     qword [rsp + 16], client_id_str
    mov     qword [rsp + 24], 0
    
    mov     rax, 59                     ; sys_execve
    mov     rdi, notes_prog
    mov     rsi, rsp
    mov     rdx, 0
    syscall
    
    add     rsp, 32
    mov     rax, 60                     ; sys_exit
    mov     rdi, 1
    syscall

.delete_cmd db 'DELETE', 0

read_notes_temp_result:
    mov     rdi, notes_exec_result      ; Use separate buffer
    mov     rcx, 256
    xor     rax, rax
    rep     stosb
    
    mov     rax, 2                      ; sys_open
    mov     rdi, execute_notes_program_read.temp_file
    mov     rsi, 0                      ; O_RDONLY
    syscall
    test    rax, rax
    js      .use_default
    
    mov     rdi, rax
    push    rdi
    mov     rax, 0                      ; sys_read
    mov     rsi, notes_exec_result      ; Use separate buffer
    mov     rdx, 255
    syscall
    
    mov     r9, rax
    pop     rdi
    push    r9
    mov     rax, 3                      ; sys_close
    syscall
    pop     r9
    
    test    r9, r9
    jle     .use_default
    
    mov     byte [notes_exec_result + r9], 0
    
    mov     rdi, notes_exec_result
    add     rdi, r9
    dec     rdi
.remove_newlines:
    cmp     rdi, notes_exec_result
    jl      .done_cleanup
    mov     al, [rdi]
    cmp     al, 10
    je      .remove_char
    cmp     al, 13
    je      .remove_char
    jmp     .done_cleanup
.remove_char:
    mov     byte [rdi], 0
    dec     rdi
    jmp     .remove_newlines
.done_cleanup:
    ret

.use_default:
    mov     rsi, execute_notes_program_read.default_notes
    mov     rdi, notes_exec_result      ; Use separate buffer
    call    copy_string_simple
    ret

redirect_to_notes:
    mov     rsi, .redirect_header
    call    send_string
    ret

.redirect_header:
    db 'HTTP/1.1 302 Found', 13, 10
    db 'Location: /notes', 13, 10
    db 'Connection: close', 13, 10, 13, 10, 0

build_file_path_from_request:
    mov     rsi, www_root
    mov     rdi, temp_buffer
    call    copy_string

    mov     rdx, rsi
    cmp     byte [rdx], '/'
    jne     .copy_path
    inc     rdx

.copy_path:
    push    rdx
    mov     rcx, 0
.find_end:
    mov     al, [rdx + rcx]
    cmp     al, ' '
    je      .path_end
    cmp     al, '?'
    je      .path_end
    cmp     al, 0
    je      .path_end
    inc     rcx
    jmp     .find_end

.path_end:
    pop     rdx
    mov     byte [rdx + rcx], 0
    mov     rsi, rdx
    call    append_string
    mov     rdi, temp_buffer
    ret

parse_post_data:
    call    clear_name_buffers
    
    mov     rsi, buffer
.find_body:
    cmp     word [rsi], 0x0A0D
    jne     .next_char
    cmp     word [rsi + 2], 0x0A0D
    je      .found_body
.next_char:
    inc     rsi
    cmp     rsi, buffer + 4000
    jl      .find_body
    ret

.found_body:
    add     rsi, 4
    call    extract_names_from_form
    ret

clear_name_buffers:
    mov     rdi, name1_buffer
    mov     rcx, 64
    xor     rax, rax
    rep     stosb
    
    mov     rdi, name2_buffer
    mov     rcx, 64
    xor     rax, rax
    rep     stosb
    ret

extract_names_from_form:
    mov     rdi, name1_buffer
    mov     rcx, 64
    xor     rax, rax
    rep     stosb
    
    mov     rdi, name2_buffer
    mov     rcx, 64
    xor     rax, rax
    rep     stosb
    
    mov     rsi, buffer
    call    simple_find_name1
    
    mov     rsi, buffer
    call    simple_find_name2
    
    call    set_default_names_if_empty
    ret

simple_find_name1:
.scan:
    cmp     byte [rsi], 0
    je      .not_found
    
    cmp     byte [rsi], 'n'
    jne     .next
    cmp     byte [rsi + 1], 'a'
    jne     .next
    cmp     byte [rsi + 2], 'm'
    jne     .next
    cmp     byte [rsi + 3], 'e'
    jne     .next
    cmp     byte [rsi + 4], '1'
    jne     .next
    cmp     byte [rsi + 5], '='
    jne     .next
    
    add     rsi, 6
    mov     rdi, name1_buffer
    call    extract_simple_value
    ret

.next:
    inc     rsi
    cmp     rsi, buffer + 4000
    jl      .scan
.not_found:
    ret

simple_find_name2:
.scan:
    cmp     byte [rsi], 0
    je      .not_found
    
    cmp     byte [rsi], 'n'
    jne     .next
    cmp     byte [rsi + 1], 'a'
    jne     .next
    cmp     byte [rsi + 2], 'm'
    jne     .next
    cmp     byte [rsi + 3], 'e'
    jne     .next
    cmp     byte [rsi + 4], '2'
    jne     .next
    cmp     byte [rsi + 5], '='
    jne     .next
    
    add     rsi, 6
    mov     rdi, name2_buffer
    call    extract_simple_value
    ret

.next:
    inc     rsi
    cmp     rsi, buffer + 4000
    jl      .scan
.not_found:
    ret

extract_simple_value:
    xor     rcx, rcx
.loop:
    mov     al, [rsi]
    
    cmp     al, '&'
    je      .done
    cmp     al, ' '
    je      .done
    cmp     al, 13
    je      .done
    cmp     al, 10
    je      .done
    cmp     al, 0
    je      .done
    
    cmp     al, '+'
    jne     .store_char
    mov     al, ' '
    
.store_char:
    mov     [rdi + rcx], al
    inc     rsi
    inc     rcx
    cmp     rcx, 63
    jl      .loop

.done:
    mov     byte [rdi + rcx], 0
    ret

set_default_names_if_empty:
    cmp     byte [name1_buffer], 0
    jne     .check_name2
    
    mov     rsi, .default_name1
    mov     rdi, name1_buffer
    call    copy_string_simple

.check_name2:
    cmp     byte [name2_buffer], 0
    jne     .done
    
    mov     rsi, .default_name2
    mov     rdi, name2_buffer
    call    copy_string_simple

.done:
    ret

.default_name1 db 'User1', 0
.default_name2 db 'User2', 0

copy_string_simple:
.loop:
    lodsb
    stosb
    test    al, al
    jnz     .loop
    ret

call_love_calculator:
    mov     rax, 2                      ; sys_open
    mov     rdi, .temp_file
    mov     rsi, 0x241                  ; O_WRONLY | O_CREAT | O_TRUNC
    mov     rdx, 0644o
    syscall
    test    rax, rax
    js      .use_default_result
    
    mov     rdi, rax
    mov     rax, 3                      ; sys_close
    syscall
    
    mov     rax, 57                     ; sys_fork
    syscall
    test    rax, rax
    jz      .child_process
    
    mov     rax, 61                     ; sys_wait4
    mov     rdi, -1
    mov     rsi, 0
    mov     rdx, 0
    mov     r10, 0
    syscall
    
    call    read_temp_file_result
    ret

.child_process:
    mov     rax, 2                      ; sys_open
    mov     rdi, .temp_file
    mov     rsi, 0x241                  ; O_WRONLY | O_CREAT | O_TRUNC
    mov     rdx, 0644o
    syscall
    
    mov     rdi, rax
    mov     rax, 33                     ; sys_dup2
    mov     rsi, 1                      ; stdout
    syscall
    
    mov     rax, 3
    syscall
    
    sub     rsp, 32
    mov     qword [rsp], lovecalc_prog
    mov     qword [rsp + 8], name1_buffer
    mov     qword [rsp + 16], name2_buffer
    mov     qword [rsp + 24], 0
    
    mov     rax, 59                     ; sys_execve
    mov     rdi, lovecalc_prog
    mov     rsi, rsp
    mov     rdx, 0
    syscall
    
    add     rsp, 32
    mov     rax, 60                     ; sys_exit
    mov     rdi, 1
    syscall

.use_default_result:
    mov     rsi, .default_result
    mov     rdi, exec_result
    call    copy_string
    ret

.temp_file      db '/tmp/love_result', 0
.default_result db 'Error|Error|50|Try Again!', 0

read_temp_file_result:
    mov     rdi, exec_result
    mov     rcx, 256
    xor     rax, rax
    rep     stosb
    
    mov     rax, 2                      ; sys_open
    mov     rdi, call_love_calculator.temp_file  ; FIXED: Use correct file path
    mov     rsi, 0                      ; O_RDONLY
    syscall
    test    rax, rax
    js      .use_default
    
    mov     rdi, rax
    push    rdi
    mov     rax, 0                      ; sys_read
    mov     rsi, exec_result
    mov     rdx, 255
    syscall
    
    mov     r9, rax                     ; Store bytes read
    pop     rdi
    mov     rax, 3                      ; sys_close
    syscall
    
    ; FIXED: Null terminate at correct position
    mov     byte [exec_result + r9], 0
    ret

.use_default:
    mov     rsi, call_love_calculator.default_result
    mov     rdi, exec_result
    call    copy_string
    ret

replace_placeholder_with_result:
    call    build_result_html
    call    find_and_replace_placeholder
    ret

replace_placeholder_with_default:
    mov     rdi, result_content_buffer
    mov     rcx, 1024
    xor     rax, rax
    rep     stosb
    
    mov     rsi, default_content
    mov     rdi, result_content_buffer
.copy_default:
    lodsb
    test    al, al
    jz      .copy_done
    stosb
    jmp     .copy_default
.copy_done:
    
    call    find_and_replace_placeholder
    ret

build_result_html:
    mov     rdi, result_content_buffer
    mov     rcx, 1024
    xor     rax, rax
    rep     stosb
    
    mov     rdi, result_content_buffer
    
    mov     rsi, name1_buffer
    call    append_string_simple
    
    mov     al, ' '
    stosb
    mov     al, '+'
    stosb
    mov     al, ' '
    stosb
    
    mov     rsi, name2_buffer
    call    append_string_simple
    
    mov     al, ':'
    stosb
    mov     al, ' '
    stosb
    
    call    extract_and_add_percentage
    
    mov     al, '%'
    stosb
    mov     al, ' '
    stosb
    mov     al, '-'
    stosb
    mov     al, ' '
    stosb
    
    call    extract_and_add_message
    ret

append_string_simple:
.loop:
    lodsb
    test    al, al
    jz      .done
    stosb
    jmp     .loop
.done:
    ret

extract_and_add_percentage:
    mov     rsi, exec_result
    
.skip1:
    lodsb
    cmp     al, 0
    je      .use_default
    cmp     al, '|'
    jne     .skip1
    
.skip2:
    lodsb
    cmp     al, 0
    je      .use_default
    cmp     al, '|'
    jne     .skip2
    
.copy_percent:
    lodsb
    cmp     al, '|'
    je      .done_percent
    cmp     al, 0
    je      .done_percent
    stosb
    jmp     .copy_percent

.use_default:
    mov     al, '5'
    stosb
    mov     al, '0'
    stosb
    ret

.done_percent:
    ret

extract_and_add_message:
    mov     rsi, exec_result
    
    xor     rcx, rcx
.find_third:
    lodsb
    cmp     al, 0
    je      .use_default_msg
    cmp     al, '|'
    jne     .find_third
    inc     rcx
    cmp     rcx, 3
    jl      .find_third
    
.copy_msg:
    lodsb
    cmp     al, 0
    je      .done_msg
    cmp     al, 13
    je      .done_msg
    cmp     al, 10
    je      .done_msg
    stosb
    jmp     .copy_msg

.use_default_msg:
    mov     rsi, .default_message
    call    append_string_simple
    ret

.done_msg:
    ret

.default_message db 'Good Match!', 0

find_and_replace_placeholder:
    mov     rsi, file_buffer
    
.scan_file:
    cmp     byte [rsi], 0
    je      .not_found
    
    cmp     byte [rsi], 'P'
    jne     .next_char
    
    push    rsi
    mov     rdi, placeholder
    mov     rcx, 45
    repe    cmpsb
    pop     rsi
    jne     .next_char
    
    mov     rdi, rsi
    mov     al, ' '
    mov     rcx, 45
    rep     stosb
    
    mov     rdi, rsi
    mov     rsi, result_content_buffer
    
.copy_result:
    lodsb
    test    al, al
    jz      .replacement_done
    stosb
    jmp     .copy_result

.next_char:
    inc     rsi
    jmp     .scan_file

.replacement_done:
.not_found:
    ret

find_and_replace_notes_placeholder:
    mov     rsi, file_buffer
    
.scan_file:
    cmp     byte [rsi], 0
    je      .not_found
    
    cmp     byte [rsi], 'P'
    jne     .next_char
    
    push    rsi
    mov     rdi, placeholder
    mov     rcx, 45
    repe    cmpsb
    pop     rsi
    jne     .next_char
    
    mov     rdi, rsi
    mov     al, ' '
    mov     rcx, 45
    rep     stosb
    
    mov     rdi, rsi
    mov     rsi, notes_content_buffer
    
.copy_notes:
    lodsb
    test    al, al
    jz      .replacement_done
    stosb
    jmp     .copy_notes

.next_char:
    inc     rsi
    jmp     .scan_file

.replacement_done:
.not_found:
    ret

int_to_string_at_pos:
    ; Convert integer in rax to string at current rdi position
    mov     rbx, 10
    mov     rcx, 0
    
    ; Handle zero case
    test    rax, rax
    jnz     .count_digits
    mov     byte [rdi], '0'
    inc     rdi
    ret
    
.count_digits:
    mov     r8, rax
    mov     r9, 0
.count:
    xor     rdx, rdx
    div     rbx
    inc     r9
    test    rax, rax
    jnz     .count
    
    ; Convert from right to left
    mov     rax, r8
    add     rdi, r9
    dec     rdi
    
.convert_loop:
    xor     rdx, rdx
    div     rbx
    add     dl, '0'
    mov     [rdi], dl
    dec     rdi
    test    rax, rax
    jnz     .convert_loop
    
    add     rdi, r9
    inc     rdi
    ret

copy_string:
.loop:
    lodsb
    stosb
    test    al, al
    jnz     .loop
    dec     rdi
    ret

append_string:
.find_end:
    cmp     byte [rdi], 0
    je      .append
    inc     rdi
    jmp     .find_end
.append:
    call    copy_string
    ret

send_200_header:
    mov     rsi, http_200
    call    send_string
    ret

send_video_header:
    mov     rsi, http_200_video
    call    send_string
    ret

send_404:
    mov     rsi, http_404
    call    send_string
    ret

send_file_content:
    mov     rax, 1                      ; sys_write
    mov     rdi, [client_fd]
    mov     rsi, file_buffer
    mov     rdx, r8
    syscall
    ret

send_string:
    push    rsi
    mov     rdi, rsi
    call    string_length
    mov     rdx, rcx
    pop     rsi
    
    mov     rax, 1                      ; sys_write
    mov     rdi, [client_fd]
    syscall
    ret

string_length:
    xor     rcx, rcx
.loop:
    cmp     byte [rdi + rcx], 0
    je      .done
    inc     rcx
    jmp     .loop
.done:
    ret

exit_error:
    mov     rax, 60                     ; sys_exit
    mov     rdi, 1
    syscall