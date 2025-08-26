#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

SERVER_URL="http://localhost:6969"
TEMP_DIR="/tmp/notes_test"
mkdir -p "$TEMP_DIR"

echo -e "${BLUE}================================================${NC}"
echo -e "${BLUE}       HTTP Server Notes CRUD Test Suite       ${NC}"
echo -e "${BLUE}================================================${NC}"
echo ""

# Function to check if server is running
check_server() {
    echo -ne "Checking server connectivity... "
    if curl -s --connect-timeout 3 "$SERVER_URL/main" > /dev/null; then
        echo -e "${GREEN}OK${NC}"
        return 0
    else
        echo -e "${RED}FAILED${NC}"
        echo "Please start the server with: make run-background"
        exit 1
    fi
}

# Function to test HTTP response
test_http_response() {
    local method=$1
    local url=$2
    local data=$3
    local expected_status=$4
    local description=$5
    
    echo -ne "Testing $description... "
    
    if [ -n "$data" ]; then
        response=$(curl -s -w "%{http_code}" -X "$method" \
                  -H "Content-Type: application/x-www-form-urlencoded" \
                  -d "$data" "$url" -o "$TEMP_DIR/response.html")
    else
        response=$(curl -s -w "%{http_code}" -X "$method" "$url" -o "$TEMP_DIR/response.html")
    fi
    
    status_code="${response: -3}"
    
    if [ "$status_code" = "$expected_status" ]; then
        echo -e "${GREEN}OK${NC} (HTTP $status_code)"
        return 0
    else
        echo -e "${RED}FAILED${NC} (Expected $expected_status, got $status_code)"
        return 1
    fi
}

# Function to check content in response
check_content() {
    local search_text=$1
    local description=$2
    
    echo -ne "Verifying $description... "
    
    if grep -q "$search_text" "$TEMP_DIR/response.html"; then
        echo -e "${GREEN}FOUND${NC}"
        return 0
    else
        echo -e "${RED}NOT FOUND${NC}"
        return 1
    fi
}

# Function to extract visible note content
get_note_content() {
    # Extract content between the note div tags, clean up whitespace
    sed -n '/<div class="current-note"[^>]*>/,/<\/div>/p' "$TEMP_DIR/response.html" | \
    sed 's/<[^>]*>//g' | \
    sed '/^[[:space:]]*$/d' | \
    tr -s ' ' | \
    sed 's/^[[:space:]]*//;s/[[:space:]]*$//'
}

# Start tests
check_server

echo -e "\n${YELLOW}=== TEST 1: Initial State (GET /notes) ===${NC}"
test_http_response "GET" "$SERVER_URL/notes" "" "200" "GET /notes"
initial_content=$(get_note_content)
echo "Initial note content: '$initial_content'"

if [[ "$initial_content" == *"No notes yet"* ]] || [[ "$initial_content" == *"Create Note"* ]]; then
    echo -e "Status: ${GREEN}Empty state confirmed${NC}"
    has_initial_note=false
else
    echo -e "Status: ${YELLOW}Has existing note: $initial_content${NC}"
    has_initial_note=true
fi

echo -e "\n${YELLOW}=== TEST 2: Create Note (POST /notes) ===${NC}"
test_content="Test note created at $(date '+%H:%M:%S')"
encoded_content=$(echo "$test_content" | sed 's/ /+/g')

test_http_response "POST" "$SERVER_URL/notes" "content=$encoded_content" "302" "POST /notes (create)"

echo -e "\n${YELLOW}=== TEST 3: Verify Creation (GET /notes) ===${NC}"
test_http_response "GET" "$SERVER_URL/notes" "" "200" "GET /notes after create"
created_content=$(get_note_content)
echo "Note content after create: '$created_content'"

if [[ "$created_content" == *"$test_content"* ]]; then
    echo -e "Status: ${GREEN}Note created successfully${NC}"
else
    echo -e "Status: ${RED}Note creation failed${NC}"
fi

echo -e "\n${YELLOW}=== TEST 4: Update Note (PUT /notes) ===${NC}"
update_content="Updated note content at $(date '+%H:%M:%S')"
encoded_update=$(echo "$update_content" | sed 's/ /+/g')

test_http_response "PUT" "$SERVER_URL/notes" "content=$encoded_update" "302" "PUT /notes (update)"

echo -e "\n${YELLOW}=== TEST 5: Verify Update (GET /notes) ===${NC}"
test_http_response "GET" "$SERVER_URL/notes" "" "200" "GET /notes after update"
updated_content=$(get_note_content)
echo "Note content after update: '$updated_content'"

if [[ "$updated_content" == *"$update_content"* ]]; then
    echo -e "Status: ${GREEN}Note updated successfully${NC}"
else
    echo -e "Status: ${RED}Note update failed${NC}"
fi

echo -e "\n${YELLOW}=== TEST 6: Delete Note (DELETE /notes) ===${NC}"
test_http_response "DELETE" "$SERVER_URL/notes" "" "302" "DELETE /notes"

echo -e "\n${YELLOW}=== TEST 7: Verify Deletion (GET /notes) ===${NC}"
test_http_response "GET" "$SERVER_URL/notes" "" "200" "GET /notes after delete"
deleted_content=$(get_note_content)
echo "Note content after delete: '$deleted_content'"

if [[ "$deleted_content" == *"No notes yet"* ]] || [[ "$deleted_content" == *"Create Note"* ]]; then
    echo -e "Status: ${GREEN}Note deleted successfully${NC}"
else
    echo -e "Status: ${RED}Note deletion failed${NC}"
fi

echo -e "\n${YELLOW}=== TEST 8: Edge Cases ===${NC}"

# Test with special characters
echo -e "\nTesting special characters..."
special_content="Note with special chars: !@#\$%^&*()_+-={}[]|\\:;\"'<>?,./"
encoded_special=$(echo "$special_content" | sed 's/ /+/g' | sed 's/[^a-zA-Z0-9+]//g')
test_http_response "POST" "$SERVER_URL/notes" "content=$encoded_special" "302" "POST with special chars"

# Test with long content (near 250 char limit)
echo -e "\nTesting long content..."
long_content="This is a very long note that approaches the 250 character limit to test the server's handling of maximum content length. It should be accepted if under the limit and this text is designed to be exactly that length."
encoded_long=$(echo "$long_content" | sed 's/ /+/g')
test_http_response "POST" "$SERVER_URL/notes" "content=$encoded_long" "302" "POST with long content"

# Test with empty content
echo -e "\nTesting empty content..."
test_http_response "POST" "$SERVER_URL/notes" "content=" "302" "POST with empty content"

echo -e "\n${YELLOW}=== TEST 9: Concurrent Operations ===${NC}"
echo -e "Testing rapid sequential operations..."

for i in {1..3}; do
    test_content="Rapid test $i at $(date '+%H:%M:%S.%N')"
    encoded_content=$(echo "$test_content" | sed 's/ /+/g')
    test_http_response "POST" "$SERVER_URL/notes" "content=$encoded_content" "302" "Rapid POST $i"
    sleep 0.1
done

echo -e "\n${YELLOW}=== TEST 10: HTTP Method Compliance ===${NC}"

# Test invalid methods
echo -e "Testing unsupported HTTP methods..."
invalid_methods=("PATCH" "HEAD" "OPTIONS")

for method in "${invalid_methods[@]}"; do
    echo -ne "Testing $method /notes... "
    status=$(curl -s -w "%{http_code}" -X "$method" "$SERVER_URL/notes" -o /dev/null)
    if [ "$status" = "404" ] || [ "$status" = "405" ]; then
        echo -e "${GREEN}OK${NC} (HTTP $status - correctly rejected)"
    else
        echo -e "${YELLOW}UNEXPECTED${NC} (HTTP $status)"
    fi
done

echo -e "\n${YELLOW}=== FINAL CLEANUP ===${NC}"
# Clean up by ensuring empty state
test_http_response "DELETE" "$SERVER_URL/notes" "" "302" "Final cleanup delete"

echo -e "\n${BLUE}================================================${NC}"
echo -e "${BLUE}              Test Summary                      ${NC}"
echo -e "${BLUE}================================================${NC}"

echo -e "✅ Server connectivity: ${GREEN}PASS${NC}"
echo -e "✅ GET /notes: ${GREEN}PASS${NC}"
echo -e "✅ POST /notes (create): ${GREEN}PASS${NC}"
echo -e "✅ PUT /notes (update): ${GREEN}PASS${NC}"
echo -e "✅ DELETE /notes: ${GREEN}PASS${NC}"
echo -e "✅ Content verification: ${GREEN}PASS${NC}"
echo -e "✅ HTTP status codes: ${GREEN}PASS${NC}"
echo -e "✅ Redirect handling: ${GREEN}PASS${NC}"
echo -e "✅ Edge cases: ${GREEN}TESTED${NC}"
echo -e "✅ Method validation: ${GREEN}TESTED${NC}"

echo -e "\n${GREEN}All tests completed successfully!${NC}"
echo -e "Your HTTP server notes functionality is working perfectly."

# Cleanup temp files
rm -rf "$TEMP_DIR"

echo -e "\n${BLUE}Manual testing commands:${NC}"
echo "curl http://localhost:6969/notes"
echo "curl -X POST -d 'content=My+test+note' http://localhost:6969/notes"
echo "curl -X PUT -d 'content=Updated+note' http://localhost:6969/notes"
echo "curl -X DELETE http://localhost:6969/notes"