FROM debian:bookworm-slim

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    build-essential nasm \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY server.asm ./
COPY *.c ./
COPY www/ ./www/

RUN set -eux; \
    echo "Building assembly server..."; \
    nasm -felf64 server.asm -o server.o; \
    ld server.o -o http_server; \
    echo "Building C programs..."; \
    gcc -O2 lovecalc.c -o lovecalc; \
    gcc -O2 notes.c -o notes; \
    echo "Setting permissions..."; \
    chmod +x http_server lovecalc notes; \
    echo "Build completed successfully"; \
    ls -la

RUN mkdir -p ./notes_data

EXPOSE 6969

CMD ["./http_server"]