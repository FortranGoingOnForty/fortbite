#!/bin/bash

# FORTBITE Test Runner Script
# Runs all unit and integration tests

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "================================"
echo "    FORTBITE Test Suite"
echo "================================"
echo

# Create test results directory
mkdir -p test_results

# Track overall results
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
FAILED_LIST=""

# Function to run a test
run_test() {
    local test_name=$1
    local test_exec=$2
    local test_type=$3

    echo -n "Running $test_type test: $test_name ... "

    if [ -f "$test_exec" ]; then
        if $test_exec > "test_results/${test_name}.log" 2>&1; then
            echo -e "${GREEN}PASSED${NC}"
            ((PASSED_TESTS++))
        else
            echo -e "${RED}FAILED${NC}"
            ((FAILED_TESTS++))
            FAILED_LIST="$FAILED_LIST\n  - $test_name"
            echo "  Error output:"
            tail -n 5 "test_results/${test_name}.log" | sed 's/^/    /'
        fi
    else
        echo -e "${YELLOW}NOT FOUND${NC} (skipping)"
    fi
    ((TOTAL_TESTS++))
}

# Build tests if needed
if [ "$1" != "--no-build" ]; then
    echo "Building tests..."
    make test-build || exit 1
    echo
fi

# Run unit tests
echo "=== Unit Tests ==="
run_test "arithmetic" "build/tests/unit/test_arithmetic" "unit"
run_test "functions" "build/tests/unit/test_functions" "unit"
run_test "complex" "build/tests/unit/test_complex" "unit"
run_test "matrix" "build/tests/unit/test_matrix" "unit"
echo

# Run integration tests
echo "=== Integration Tests ==="
run_test "expressions" "build/tests/integration/test_expressions" "integration"
run_test "variables" "build/tests/integration/test_variables" "integration"
echo

# Run end-to-end tests using test scripts
echo "=== End-to-End Tests ==="
if [ -d "tests/fixtures" ]; then
    for fixture in tests/fixtures/*.fb; do
        if [ -f "$fixture" ]; then
            test_name=$(basename "$fixture" .fb)
            echo -n "Running E2E test: $test_name ... "

            expected_output="${fixture%.fb}.expected"
            if [ -f "$expected_output" ]; then
                if ./build/bin/fortbite < "$fixture" > "test_results/${test_name}.out" 2>&1; then
                    # Strip prompts and compare output
                    grep -v "fortbite>" "test_results/${test_name}.out" | \
                    grep -v "======" | \
                    grep -v "Type" | \
                    grep -v "Use" | \
                    grep -v "Goodbye" > "test_results/${test_name}.clean"

                    if diff -q "$expected_output" "test_results/${test_name}.clean" > /dev/null; then
                        echo -e "${GREEN}PASSED${NC}"
                        ((PASSED_TESTS++))
                    else
                        echo -e "${RED}FAILED${NC} (output mismatch)"
                        ((FAILED_TESTS++))
                        FAILED_LIST="$FAILED_LIST\n  - E2E: $test_name"
                    fi
                else
                    echo -e "${RED}FAILED${NC} (execution error)"
                    ((FAILED_TESTS++))
                    FAILED_LIST="$FAILED_LIST\n  - E2E: $test_name"
                fi
            else
                echo -e "${YELLOW}NO EXPECTED OUTPUT${NC} (skipping)"
            fi
            ((TOTAL_TESTS++))
        fi
    done
fi
echo

# Summary
echo "================================"
echo "         Test Summary"
echo "================================"
echo -e "Total:  $TOTAL_TESTS tests"
echo -e "Passed: ${GREEN}$PASSED_TESTS${NC} tests"
echo -e "Failed: ${RED}$FAILED_TESTS${NC} tests"

if [ $FAILED_TESTS -gt 0 ]; then
    echo
    echo -e "${RED}Failed tests:${NC}"
    echo -e "$FAILED_LIST"
    exit 1
else
    echo
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
fi