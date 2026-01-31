#!/bin/bash

# Test runner for InCollege COBOL program
# Runs all test cases and reports pass/fail status

PROGRAM="/workspace/bin/InCollege"
WORKSPACE="/workspace"
TEST_DIR="/workspace/Tests/Epic2"
BASELINE_USERS="$WORKSPACE/USERS.DAT.baseline"

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters - using temp file to persist across subshell
COUNTER_FILE=$(mktemp)
echo "0 0 0" > "$COUNTER_FILE"

# Function to read counters
read_counters() {
    read TOTAL_TESTS PASSED_TESTS FAILED_TESTS < "$COUNTER_FILE"
}

# Function to write counters
write_counters() {
    echo "$TOTAL_TESTS $PASSED_TESTS $FAILED_TESTS" > "$COUNTER_FILE"
}

# Function to reset USERS.DAT to empty state
reset_users_dat() {
    > "$WORKSPACE/USERS.DAT"
}

# Function to setup baseline users
setup_baseline_users() {
    if [ -f "$BASELINE_USERS" ]; then
        cp "$BASELINE_USERS" "$WORKSPACE/USERS.DAT"
    else
        reset_users_dat
    fi
}

# Function to determine if test needs baseline users
needs_baseline_users() {
    local input_file="$1"
    # Check if first line is "1" (login option) - these tests expect users to exist
    local first_line=$(head -n 1 "$input_file" 2>/dev/null)
    if [ "$first_line" = "1" ]; then
        return 0  # true - needs baseline
    else
        return 1  # false - doesn't need baseline
    fi
}

# Function to run a single test
run_test() {
    local test_path="$1"
    local test_name=$(basename "$test_path")
    local feature_name=$(basename "$(dirname "$test_path")")

    read_counters

    echo -e "${BLUE}Running: $feature_name/$test_name${NC}"

    # Check if InCollege-Input.txt exists
    if [ ! -f "$test_path/InCollege-Input.txt" ]; then
        echo -e "${YELLOW}  SKIP: No InCollege-Input.txt found${NC}"
        return
    fi

    # Setup USERS.DAT based on test requirements
    if needs_baseline_users "$test_path/InCollege-Input.txt"; then
        setup_baseline_users
    else
        reset_users_dat
    fi

    # Copy InCollege-Input.txt to workspace
    cp "$test_path/InCollege-Input.txt" "$WORKSPACE/InCollege-Input.txt"

    # Run the program
    cd "$WORKSPACE"
    "$PROGRAM" > /dev/null 2>&1

    # Compare output
    if [ -f "$test_path/InCollege-Output.txt" ]; then
        if diff -q "$WORKSPACE/InCollege-Output.txt" "$test_path/InCollege-Output.txt" > /dev/null 2>&1; then
            RUN1_RESULT="PASS"
        else
            RUN1_RESULT="FAIL"
        fi
    else
        echo -e "${YELLOW}  SKIP: No InCollege-Output.txt found${NC}"
        return
    fi

    # Check if there's a second run (InCollege-Input2.txt/InCollege-Output2.txt)
    if [ -f "$test_path/InCollege-Input2.txt" ]; then
        # Don't reset USERS.DAT for second run (testing persistence)
        cp "$test_path/InCollege-Input2.txt" "$WORKSPACE/InCollege-Input.txt"
        "$PROGRAM" > /dev/null 2>&1

        if [ -f "$test_path/InCollege-Output2.txt" ]; then
            if diff -q "$WORKSPACE/InCollege-Output.txt" "$test_path/InCollege-Output2.txt" > /dev/null 2>&1; then
                RUN2_RESULT="PASS"
            else
                RUN2_RESULT="FAIL"
            fi
        fi

        # Overall result
        if [ "$RUN1_RESULT" = "PASS" ] && [ "$RUN2_RESULT" = "PASS" ]; then
            echo -e "${GREEN}  ✓ PASS${NC} (Run 1 & Run 2)"
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            echo -e "${RED}  ✗ FAIL${NC} (Run 1: $RUN1_RESULT, Run 2: $RUN2_RESULT)"
            FAILED_TESTS=$((FAILED_TESTS + 1))

            # Show diff for debugging (first 10 lines)
            if [ "$RUN1_RESULT" = "FAIL" ]; then
                echo -e "${YELLOW}  Run 1 differences (first 10 lines):${NC}"
                diff "$WORKSPACE/InCollege-Output.txt" "$test_path/InCollege-Output.txt" | head -10
            fi
            if [ "$RUN2_RESULT" = "FAIL" ]; then
                echo -e "${YELLOW}  Run 2 differences (first 10 lines):${NC}"
                # Need to rerun to get InCollege-Output.txt
                if [ -f "$test_path/NEEDS_BASELINE" ]; then
                    setup_baseline_users
                elif needs_baseline_users "$test_path/InCollege-Input.txt"; then
                    setup_baseline_users
                else
                    reset_users_dat
                fi
                cp "$test_path/InCollege-Input.txt" "$WORKSPACE/InCollege-Input.txt"
                "$PROGRAM" > /dev/null 2>&1
                cp "$test_path/InCollege-Input2.txt" "$WORKSPACE/InCollege-Input.txt"
                "$PROGRAM" > /dev/null 2>&1
                diff "$WORKSPACE/InCollege-Output.txt" "$test_path/InCollege-Output2.txt" | head -10
            fi
        fi
    else
        # Single run test
        if [ "$RUN1_RESULT" = "PASS" ]; then
            echo -e "${GREEN}  ✓ PASS${NC}"
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            echo -e "${RED}  ✗ FAIL${NC}"
            FAILED_TESTS=$((FAILED_TESTS + 1))

            # Show diff for debugging (first 10 lines)
            echo -e "${YELLOW}  Differences (first 10 lines):${NC}"
            diff "$WORKSPACE/InCollege-Output.txt" "$test_path/InCollege-Output.txt" | head -10
        fi
    fi

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    write_counters
    echo ""
}

# Main execution
echo "========================================"
echo "InCollege Test Suite"
echo "========================================"
echo ""

# Find all test directories (those containing InCollege-Input.txt files) and sort them
find "$TEST_DIR" -type f -name "InCollege-Input.txt" | sort | while read input_file; do
    test_dir=$(dirname "$input_file")
    run_test "$test_dir"
done

# Read final counters
read_counters

# Summary
echo "========================================"
echo "Test Summary"
echo "========================================"
echo -e "Total Tests: $TOTAL_TESTS"
echo -e "${GREEN}Passed: $PASSED_TESTS${NC}"
echo -e "${RED}Failed: $FAILED_TESTS${NC}"
echo ""

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}All tests passed! 🎉${NC}"
    rm "$COUNTER_FILE"
    exit 0
else
    PASS_RATE=$((PASSED_TESTS * 100 / TOTAL_TESTS))
    echo -e "Pass rate: ${PASS_RATE}%"
    rm "$COUNTER_FILE"
    exit 1
fi
