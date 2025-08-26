# Ngalegaan Kakawasan, Tamamo Silang - Sunda Priangan

> Seleksi Asisten Laboratorium Sistem Paralel dan Terdistribusi 2025
<p align="center">
    <img src="https://github.com/user-attachments/assets/9f821cf0-87ee-4977-b762-59754039ef4c">
</p>
    <h3 align="center">Will you make this HTTP web server for me? Ehe~ :begging:</h3>

---

## About <a name="about"></a>

<p align="justify">This project implements a simple web server running with x86-64 assembly architecture on port 6969</p>

---

## Implementations <a name="algorithms"></a>

- **Port listening**  
  The server is listening to port 6969
     ```bash
        ; Socket Structure for bind()
        sockaddr:
            sin_family  dw 2        ; AF_INET
            sin_port    dw 0x391b   ; port 6969 in network byte order
            sin_addr    dd 0        ; INADDR_ANY
            sin_zero    times 8 db 0
   ```
<br/>

- **Fork each process for each requests**  
  The relevant code:
     ```bash
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
   ```
<br/>

- **Basic HTTP request(GET, POST, PUT, DELETE), file processing, and routing**  
  For testing the http request, you can run `make test`, or you can manually make manual test case using `curl`

  GET main
  <img width="2559" height="1390" alt="image" src="https://github.com/user-attachments/assets/0664433c-9577-438b-905f-4995d66f0a23" />

  You can play video (w/ sound) in the /main route with html rendering

<br/>

  POST /calculate
  <img width="2559" height="1382" alt="image" src="https://github.com/user-attachments/assets/a1168974-e7dd-4b90-b4c0-34aacca9955c" />
  <img width="2558" height="1379" alt="image" src="https://github.com/user-attachments/assets/518e51d5-5505-49ae-9d63-5cee557b2dff" />

  You can use love calculator to calculate your love w/ your crush :3 with C binary linking for calculator, rendered with html

<br/>

  PUT, POST, DELETE /notes
  <img width="2556" height="1377" alt="image" src="https://github.com/user-attachments/assets/58a5d7c3-882d-4e10-ab9b-f2b2239cbd3f" />
  <img width="2559" height="1378" alt="image" src="https://github.com/user-attachments/assets/07251f36-df2b-4449-9737-f58e94f83246" />
  <img width="2557" height="1376" alt="image" src="https://github.com/user-attachments/assets/99f4e60e-4b76-4e19-9e1c-49f7136a5f9b" />
  <img width="2559" height="1382" alt="image" src="https://github.com/user-attachments/assets/b548df8a-d89b-44d3-8116-a11f5ec7c907" />

  Making new note with POST, edit with PUT, and delete with DELETE. Notes saved in the memory with .txt format







- **Bonus**  
  Implementing

### Network Configurations
   ```bash
       netmask 255.255.255.0
       network 192.168.1.0
       broadcast 192.168.1.255
   ```
--- 

## How to Run

### Requirements
- g++/c++
- Make
- nasm (Assembly compiler)
- ld (GNU linker)
- gcc (GNU compiler collection)
- Docker
- VPS (Virtual Private Server, if you want to deploy it)

### Running the Application
1. Clone this repo
   ```bash
   https://github.com/Nayekah/seleksi-sister.git
   ```

2. Navigate to the cloned repository.
   ```bash
   cd seleksi-sister
   cd "Bagian-B"
   cd "Mandelbrot of Madness"
   cd src
   ```
   
3. Do
   ```dotenv
   make cli (to make cli binary)
   make gui (to make gui binary)

   # Or

   make all (to make both)
   ```
   
4. To run in cli mode, do:
   ```bash
   make run-cli # default

   # Or

   ./mandelbrot [width] [height] [iterations]
   ```

5. To run in gui mode, do:
   ```bash
   make run-gui # default

   # Or

   ./mandelbrot_gui
   ```

6. Cheatsheets for GUI
   ```bash
   left mouse drag         : zoom
   right mouse drag        : pan
   'R' or 's'              : Reset to original position
   'S' or 's'              : Save the current mandelbrot set view
   'J' or 'j'              : Save the current position julia set
   mouse scroll            : zoom in/out (alternative)
   escape button (esc)     : return to input window
   ```


> [!IMPORTANT]
> You can access the web server on https://w1ntr.space/main

