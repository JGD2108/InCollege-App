#!/bin/bash

# Test runner for InCollege COBOL program
# Runs all test cases and reports pass/fail status

PROGRAM="/workspace/bin/InCollege"
WORKSPACE="/workspace"
TEST_DIRS=("/workspace/Tests/Epic3")
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

# Function to blank profile/education/experience data files
reset_extra_dat_files() {
    for f in "PROFILES.DAT" "EDUCATION.DAT" "EXPERIENCE.DAT"; do
        : > "$WORKSPACE/$f"
    done
}

# Function to run a single test
run_test() {
    local test_path="$1"
    local test_name=$(basename "$test_path")
    local feature_name=$(basename "$(dirname "$test_path")")
    local input_file=""
    local output_file=""
    local input2_file=""
    local output2_file=""
    local ws_input_file=""
    local ws_output_file=""

    read_counters

    echo -e "${BLUE}Running: $feature_name/$test_name${NC}"

    # Skip tests that are marked as needing baselines
    if [ -f "$test_path/NEEDS_BASELINE" ]; then
        echo -e "${YELLOW}  SKIP: Baseline output pending${NC}"
        return
    fi

    # Determine input/output filenames (DAT vs TXT)
    if [ -f "$test_path/InCollege-Input.txt" ]; then
        input_file="$test_path/InCollege-Input.txt"
        output_file="$test_path/InCollege-Output.txt"
        input2_file="$test_path/InCollege-Input2.txt"
        output2_file="$test_path/InCollege-Output2.txt"
        ws_input_file="$WORKSPACE/InCollege-Input.txt"
        ws_output_file="$WORKSPACE/InCollege-Output.txt"
    elif [ -f "$test_path/INPUT.DAT" ]; then
        input_file="$test_path/INPUT.DAT"
        output_file="$test_path/OUTPUT.DAT"
        input2_file="$test_path/INPUT2.DAT"
        output2_file="$test_path/OUTPUT2.DAT"
        ws_input_file="$WORKSPACE/INPUT.DAT"
        ws_output_file="$WORKSPACE/OUTPUT.DAT"
    else
        echo -e "${YELLOW}  SKIP: No input file found${NC}"
        return
    fi

    # Setup USERS.DAT based on test requirements
    # Allow a test-suite-provided USERS.DAT in the parent directory (e.g. Tests/Epic3/USERS.DAT).
    # If present, copy it into the workspace and do not overwrite it with baseline/reset.
    TEST_PARENT_DIR=$(dirname "$test_path")
    if [ -f "$TEST_PARENT_DIR/USERS.DAT" ]; then
        cp "$TEST_PARENT_DIR/USERS.DAT" "$WORKSPACE/USERS.DAT"
        TEST_USERS_PROVIDED=1
    elif [ -f "$test_root/USERS.DAT" ]; then
        # Fallback to Epic level USERS.DAT
        cp "$test_root/USERS.DAT" "$WORKSPACE/USERS.DAT"
        TEST_USERS_PROVIDED=1
    else
        TEST_USERS_PROVIDED=0
    fi

    if [ "$TEST_USERS_PROVIDED" -eq 1 ]; then
        # Use the provided USERS.DAT; do not reset or overwrite for this test.
        :
    else
        if needs_baseline_users "$input_file"; then
            setup_baseline_users
        else
            reset_users_dat
        fi
    fi

    # Copy input file to workspace
    cp "$input_file" "$ws_input_file"

    # Run the program
    cd "$WORKSPACE"
    "$PROGRAM" > /dev/null 2>&1

    # Save actual output for Run 1 into the test directory for inspection
    ACTUAL1_FILE="$test_path/ACTUAL-$(basename "$ws_output_file")"
    cp "$ws_output_file" "$ACTUAL1_FILE" 2>/dev/null || true

    # Compare output
    if [ -f "$output_file" ]; then
        if diff -q "$ws_output_file" "$output_file" > /dev/null 2>&1; then
            RUN1_RESULT="PASS"
        else
            RUN1_RESULT="FAIL"
        fi
    else
        echo -e "${YELLOW}  SKIP: No output file found${NC}"
        return
    fi

    # Check if there's a second run
    if [ -f "$input2_file" ]; then
        # Don't reset USERS.DAT for second run (testing persistence)
        cp "$input2_file" "$ws_input_file"
        "$PROGRAM" > /dev/null 2>&1

            # Save actual output for Run 2 into the test directory for inspection
            ACTUAL2_FILE="$test_path/ACTUAL2-$(basename "$ws_output_file")"
            cp "$ws_output_file" "$ACTUAL2_FILE" 2>/dev/null || true

        if [ -f "$output2_file" ]; then
            if diff -q "$ws_output_file" "$output2_file" > /dev/null 2>&1; then
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
                diff "$ws_output_file" "$output_file" | head -10
            fi
            if [ "$RUN2_RESULT" = "FAIL" ]; then
                echo -e "${YELLOW}  Run 2 differences (first 10 lines):${NC}"
                # Need to rerun to get fresh output
                if needs_baseline_users "$input_file"; then
                    setup_baseline_users
                else
                    reset_users_dat
                fi
                cp "$input_file" "$ws_input_file"
                "$PROGRAM" > /dev/null 2>&1
                cp "$input2_file" "$ws_input_file"
                "$PROGRAM" > /dev/null 2>&1
                diff "$ws_output_file" "$output2_file" | head -10
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
            diff "$ws_output_file" "$output_file" | head -10
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
# Ensure USERS.DAT starts from baseline on every run
# Blank profile/education/experience files at start
reset_extra_dat_files
echo "Blanked PROFILES.DAT, EDUCATION.DAT, EXPERIENCE.DAT."

# Ensure USERS.DAT starts from baseline on every run
setup_baseline_users
echo "Users database reset from baseline."
# Do not restore USERS.DAT on exit; leave DAT files as-is after run

# Find all test directories (those containing input files) and sort them
for test_root in "${TEST_DIRS[@]}"; do
    if [ -d "$test_root" ]; then
        # If the test directory contains shared DAT baseline files (for example
        # PROFILES.DAT, EDUCATION.DAT, EXPERIENCE.DAT), copy them into the
        # workspace so the tests in this directory can use that data.
        # NEW CODE - Add USERS.DAT to the shared files:
        for _dat in "USERS.DAT" "PROFILES.DAT" "EDUCATION.DAT" "EXPERIENCE.DAT"; do
            if [ -f "$test_root/$_dat" ]; then
                cp "$test_root/$_dat" "$WORKSPACE/$_dat"
            fi
        done

        find "$test_root" -type f \( -name "INPUT.DAT" -o -name "InCollege-Input.txt" \) | sort -u | while read input_file; do
            test_dir=$(dirname "$input_file")
            run_test "$test_dir"
        done
    fi
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
