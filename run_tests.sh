#!/bin/bash

# Test runner for InCollege COBOL program
# Runs all test cases and reports pass/fail status

PROGRAM="/workspace/bin/InCollege"
WORKSPACE="/workspace"
TEST_DIRS=("/workspace/Tests/Epic4")
CURRENT_TEST_ROOT=""

# Allow overriding test root from command line
if [ -n "$1" ]; then
    TEST_DIRS=("$1")
fi

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

# Resolve fixture path from test root.
# Priority:
# 1) <test_root>/<DAT name>
resolve_fixture_file() {
    local test_root="$1"
    local dat_name="$2"
    local fixture_file="$test_root/$dat_name"

    if [ -f "$fixture_file" ]; then
        echo "$fixture_file"
        return
    fi

    echo ""
}

# Function to setup users fixture
setup_users_dat() {
    local users_fixture
    users_fixture=$(resolve_fixture_file "$CURRENT_TEST_ROOT" "USERS.DAT")
    if [ -n "$users_fixture" ]; then
        cp "$users_fixture" "$WORKSPACE/USERS.DAT"
    else
        reset_users_dat
    fi
}

# Function to seed or blank a DAT file from test-root fixtures
seed_or_blank_dat() {
    local dat_name="$1"
    local fixture_file
    fixture_file=$(resolve_fixture_file "$CURRENT_TEST_ROOT" "$dat_name")
    if [ -n "$fixture_file" ]; then
        cp "$fixture_file" "$WORKSPACE/$dat_name"
    else
        : > "$WORKSPACE/$dat_name"
    fi
}

# Seed DAT file with per-test override support:
# 1) <test_path>/<DAT name>
# 2) <test_root>/<DAT name>
# 3) blank file
seed_dat_for_test() {
    local test_path="$1"
    local dat_name="$2"
    if [ -f "$test_path/$dat_name" ]; then
        cp "$test_path/$dat_name" "$WORKSPACE/$dat_name"
    else
        seed_or_blank_dat "$dat_name"
    fi
}

# Function to determine if test needs baseline users
needs_baseline_users() {
    local input_file="$1"
    # Check if first line is "1" (login option) - these tests expect users to exist
    local first_line=$(head -n 1 "$input_file" 2>/dev/null | tr -d '\r')
    if [ "$first_line" = "1" ]; then
        return 0  # true - needs baseline
    else
        return 1  # false - doesn't need baseline
    fi
}

# Function to blank profile/education/experience data files
reset_extra_dat_files() {
    for f in "PROFILES.DAT" "EDUCATION.DAT" "EXPERIENCE.DAT" "CONNECTIONS.DAT"; do
        seed_or_blank_dat "$f"
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

    # Reset DAT fixtures for each test so tests stay isolated.
    seed_dat_for_test "$test_path" "PROFILES.DAT"
    seed_dat_for_test "$test_path" "EDUCATION.DAT"
    seed_dat_for_test "$test_path" "EXPERIENCE.DAT"
    seed_dat_for_test "$test_path" "CONNECTIONS.DAT"

    # Setup USERS.DAT based on test requirements
    if needs_baseline_users "$input_file"; then
        if [ -f "$test_path/USERS.DAT" ]; then
            cp "$test_path/USERS.DAT" "$WORKSPACE/USERS.DAT"
        else
            setup_users_dat
        fi
    else
        reset_users_dat
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
                seed_dat_for_test "$test_path" "PROFILES.DAT"
                seed_dat_for_test "$test_path" "EDUCATION.DAT"
                seed_dat_for_test "$test_path" "EXPERIENCE.DAT"
                seed_dat_for_test "$test_path" "CONNECTIONS.DAT"
                if needs_baseline_users "$input_file"; then
                    if [ -f "$test_path/USERS.DAT" ]; then
                        cp "$test_path/USERS.DAT" "$WORKSPACE/USERS.DAT"
                    else
                        setup_users_dat
                    fi
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
# Find all test directories (those containing input files) and sort them
for test_root in "${TEST_DIRS[@]}"; do
    if [ -d "$test_root" ]; then
        CURRENT_TEST_ROOT="$test_root"
        reset_extra_dat_files
        echo "Loaded DAT fixtures from: $CURRENT_TEST_ROOT"
        setup_users_dat
        echo "Users database reset from fixture file."
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
