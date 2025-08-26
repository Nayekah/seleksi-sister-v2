FROM debian:bookworm-slim

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    build-essential nasm \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY server.asm ./
COPY ./*.c ./

COPY www/ ./www/

# build
RUN set -eux; \
    nasm -felf64 server.asm -o server.o; \
    ld server.o -o http_server; \
    gcc -O2 lovecalc.c -o lovecalc; \
    gcc -O2 notes.c -o notes

EXPOSE 6969
CMD ["/app/http_server"]
