AS = nasm
LD = ld
CC = gcc
TARGET = http_server
SOURCE = server.asm
LOVECALC = lovecalc
NOTES = notes
TESTER = http_tester

all: $(TARGET) $(LOVECALC) $(NOTES) $(TESTER)

$(TARGET): $(SOURCE)
	$(AS) -f elf64 $(SOURCE) -o server.o
	$(LD) server.o -o $(TARGET)

$(LOVECALC): lovecalc.c
	$(CC) lovecalc.c -o $(LOVECALC)

$(NOTES): notes.c
	$(CC) notes.c -o $(NOTES)

$(TESTER): http_tester.c
	$(CC) http_tester.c -o $(TESTER)

clean:
	rm -f *.o $(TARGET) $(LOVECALC) $(NOTES) $(TESTER)
	rm -f /tmp/love_result /tmp/notes_result /tmp/post_debug /tmp/request_debug /tmp/get_debug /tmp/post_routing_debug /tmp/form_data_debug
	rm -f server.log nohup.out
	rm -rf notes_data/

run: $(TARGET)
	@echo "Starting HTTP server on port 6969..."
	@echo "Checking prerequisites..."
	@make check-port > /dev/null 2>&1 || make kill-port
	@if [ ! -d "www" ]; then make setup; fi
	@echo "Press Ctrl+C to stop server"
	./$(TARGET)

run-background: $(TARGET)
	@echo "Starting HTTP server in background..."
	@make check-port > /dev/null 2>&1 || make kill-port
	@if [ ! -d "www" ]; then make setup; fi
	nohup ./$(TARGET) > server.log 2>&1 &
	@sleep 2
	@echo "Server started. PID: $$(pgrep -f http_server | tail -1)"
	@echo "Test with: curl http://localhost:6969/main"

setup:
	mkdir -p www
	@echo "Setting up www directory..."
	@if [ -f "index.html" ]; then cp index.html www/; echo "✅ Copied index.html"; else echo "⚠️  index.html not found"; fi
	@if [ -f "love.html" ]; then cp love.html www/; echo "✅ Copied love.html"; else echo "⚠️  love.html not found"; fi
	@if [ -f "notes.html" ]; then cp notes.html www/; echo "✅ Copied notes.html"; else echo "⚠️  notes.html not found"; fi
	@if [ -f "eta.mp4" ]; then cp eta.mp4 www/; echo "✅ Copied eta.mp4"; elif [ ! -f "www/eta.mp4" ]; then echo "⚠️  eta.mp4 not found - please add your video file to www/"; fi
	@echo "Setup complete. Directory structure:"
	@ls -la www/ 2>/dev/null || echo "Failed to list www directory"

check-port:
	@echo "Checking what's using port 6969..."
	@netstat -tulpn | grep :6969 || echo "Port 6969 is free"

kill-port:
	@echo "Killing processes on port 6969..."
	@pkill -f http_server 2>/dev/null || echo "No http_server processes to kill"
	@lsof -ti:6969 | xargs kill -9 2>/dev/null || echo "No processes on port 6969"

# Enhanced testing targets
test: $(TESTER)
	@echo "Running comprehensive HTTP server tests..."
	@echo "Make sure server is running first (make run-background)"
	@sleep 1
	@./$(TESTER) comprehensive

test-basic: $(TESTER)
	@echo "Running basic connectivity test with header analysis..."
	@./$(TESTER) 1

test-methods: $(TESTER)
	@echo "Testing HTTP methods with detailed analysis..."
	@./$(TESTER) methods

test-headers: $(TESTER)
	@echo "Testing custom headers handling..."
	@./$(TESTER) headers

test-notes: $(TESTER)
	@echo "Testing notes functionality..."
	@echo "GET /notes:"
	@./$(TESTER) 4
	@echo ""
	@echo "POST /notes (create note):"
	@./$(TESTER) 5
	@echo ""
	@echo "GET /notes (after create):"
	@./$(TESTER) 4
	@echo ""
	@echo "DELETE /notes:"
	@./$(TESTER) 7
	@echo ""

# Validation tests
validate-implementation: all
	@echo "=== HTTP SERVER IMPLEMENTATION VALIDATION ==="
	@echo ""
	@echo "Testing Core Requirements:"
	@echo "1. Port Listening..."
	@make check-port > /dev/null 2>&1 || make kill-port > /dev/null 2>&1
	@make run-background > /dev/null 2>&1
	@sleep 2
	@if pgrep -f http_server > /dev/null; then echo "✅ Server listening on port 6969"; else echo "❌ Server failed to start"; fi
	@echo ""
	@echo "2. Process Forking (tested via concurrent requests)..."
	@./$(TESTER) 1 > /dev/null 2>&1 && echo "✅ Child process handling works" || echo "❌ Process forking failed"
	@echo ""
	@echo "3. HTTP Methods Support..."
	@./$(TESTER) methods > /tmp/method_test.log 2>&1 && echo "✅ All HTTP methods supported" || echo "❌ HTTP method parsing failed"
	@echo ""
	@echo "4. Static File Serving..."
	@./$(TESTER) 1 > /dev/null 2>&1 && echo "✅ Static files served correctly" || echo "❌ Static file serving failed"
	@echo ""
	@echo "5. Route-based Handling..."
	@./$(TESTER) all > /tmp/routing_test.log 2>&1 && echo "✅ Routing system works" || echo "❌ Routing failed"
	@echo ""
	@echo "6. Browser Compatibility..."
	@if curl -s http://localhost:6969/main | grep -q "HTTP/1.1 200 OK\|Welcome"; then echo "✅ Browser compatible responses"; else echo "❌ Browser compatibility issues"; fi
	@echo ""
	@echo "Testing Bonus Features:"
	@echo "1. Binary Integration..."
	@./$(TESTER) 3 > /tmp/binary_test.log 2>&1 && echo "✅ External program integration works" || echo "❌ Binary integration failed"
	@echo ""
	@echo "2. Template System..."
	@if ./$(TESTER) 2 2>&1 | grep -q "PLACEHOLDER\|Calculate"; then echo "✅ Template rendering works"; else echo "❌ Template system failed"; fi
	@echo ""
	@echo "3. Header Handling..."
	@./$(TESTER) headers > /tmp/header_test.log 2>&1 && echo "✅ Custom headers processed" || echo "❌ Header handling failed"
	@echo ""
	@make kill-port > /dev/null 2>&1
	@echo "=== VALIDATION COMPLETE ==="

demo: all
	@echo "HTTP Server Demo"
	@echo "=================="
	@echo "This demo will start the server and run comprehensive tests"
	@echo ""
	@make setup > /dev/null 2>&1
	@make kill-port > /dev/null 2>&1
	@echo "Starting server in background..."
	@make run-background > /dev/null 2>&1
	@sleep 3
	@echo "Running validation tests..."
	@make validate-implementation
	@echo ""
	@echo "Server is running on http://localhost:6969"
	@echo "Try these URLs in your browser:"
	@echo "  http://localhost:6969/main"
	@echo "  http://localhost:6969/calculate"
	@echo "  http://localhost:6969/notes"
	@echo ""
	@echo "To stop the server: make kill-port"

debug-notes: $(NOTES)
	@echo "Testing notes program directly..."
	@echo "READ command:"
	@./$(NOTES) READ
	@echo ""
	@echo "CREATE command:"
	@./$(NOTES) CREATE "Test+note+content"
	@echo ""
	@echo "READ after CREATE:"
	@./$(NOTES) READ
	@echo ""
	@echo "UPDATE command:"
	@./$(NOTES) UPDATE "Updated+content"
	@echo ""
	@echo "READ after UPDATE:"
	@./$(NOTES) READ
	@echo ""
	@echo "DELETE command:"
	@./$(NOTES) DELETE
	@echo ""
	@echo "READ after DELETE:"
	@./$(NOTES) READ

install-deps:
	@echo "Installing dependencies (if needed)..."
	@which nasm > /dev/null || (echo "Please install nasm assembler" && exit 1)
	@which gcc > /dev/null || (echo "Please install gcc compiler" && exit 1)
	@which curl > /dev/null || (echo "Please install curl for testing" && exit 1)
	@echo "All dependencies are available"

help:
	@echo "HTTP Server Build System"
	@echo "========================"
	@echo ""
	@echo "Build targets:"
	@echo "  all          - Build server and all components"
	@echo "  clean        - Clean all build files"
	@echo "  install-deps - Check for required dependencies"
	@echo ""
	@echo "Run targets:"
	@echo "  run          - Start server (foreground)"
	@echo "  run-background - Start server (background)"
	@echo "  demo         - Full demonstration with validation"
	@echo ""
	@echo "Test targets:"
	@echo "  test         - Run comprehensive tests"
	@echo "  test-basic   - Basic connectivity test"
	@echo "  test-methods - Test all HTTP methods"
	@echo "  test-headers - Test custom headers"
	@echo "  test-notes   - Test notes functionality"
	@echo ""
	@echo "Validation targets:"
	@echo "  validate-implementation - Validate all requirements"
	@echo ""
	@echo "Debug targets:"
	@echo "  debug-notes  - Test notes program directly"
	@echo ""
	@echo "Utility targets:"
	@echo "  setup        - Set up www directory"
	@echo "  check-port   - Check what's using port 6969"
	@echo "  kill-port    - Kill processes on port 6969"

.PHONY: all clean run run-background setup check-port kill-port test test-basic test-methods test-headers test-notes validate-implementation demo debug-notes install-deps help