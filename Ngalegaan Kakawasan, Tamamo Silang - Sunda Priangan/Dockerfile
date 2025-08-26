# Multi-stage build for Assembly HTTP Server
FROM ubuntu:22.04 AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y \
    nasm \
    gcc \
    make \
    libc6-dev \
    && rm -rf /var/lib/apt/lists/*

# Create working directory
WORKDIR /app

# Copy source files
COPY server.asm .
COPY lovecalc.c .
COPY notes.c .
COPY http_tester.c .
COPY Makefile .

# Copy www directory with HTML files and video
COPY www/ ./www/

# Copy shell scripts
COPY notes.sh .

# Build the application
RUN make clean && make all

# Runtime stage
FROM ubuntu:22.04 AS runtime

# Install minimal runtime dependencies
RUN apt-get update && apt-get install -y \
    libc6 \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create app user for security
RUN useradd -r -u 1001 -g root appuser

# Create working directory
WORKDIR /app

# Copy built binaries from builder stage
COPY --from=builder /app/http_server .
COPY --from=builder /app/lovecalc .
COPY --from=builder /app/notes .
COPY --from=builder /app/http_tester .

# Copy www directory with HTML files
COPY --from=builder /app/www ./www

# Create notes_data directory with proper permissions
RUN mkdir -p notes_data && chown -R appuser:root notes_data

# Make binaries executable
RUN chmod +x http_server lovecalc notes http_tester

# Change ownership of app directory
RUN chown -R appuser:root /app

# Create health check script
RUN echo '#!/bin/bash\ncurl -f http://localhost:6969/main > /dev/null 2>&1 || exit 1' > /app/healthcheck.sh \
    && chmod +x /app/healthcheck.sh

# Switch to non-root user
USER appuser

# Expose port
EXPOSE 6969

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD /app/healthcheck.sh

# Start the server
CMD ["./http_server"]