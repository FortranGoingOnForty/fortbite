# Makefile for FORTBITE - Modern Fortran Calculator
# Author: espadonne (mfw)

# Compiler and flags
FC = gfortran
FFLAGS = -Wall -Wextra -fcheck=all -g -O2 -std=f2008
LDFLAGS = 
LIBS = 

# Directories
SRCDIR = src
BUILDDIR = build
MODDIR = $(BUILDDIR)/mod
BINDIR = $(BUILDDIR)/bin

# Target executable
TARGET = $(BINDIR)/fortbite

# Source files (order matters for dependencies)
SOURCES = $(SRCDIR)/fortbite_precision_m.f90 \
          $(SRCDIR)/fortbite_types_m.f90 \
          $(SRCDIR)/fortbite_matrix_m.f90 \
          $(SRCDIR)/fortbite_functions_m.f90 \
          $(SRCDIR)/fortbite_arithmetic_m.f90 \
          $(SRCDIR)/fortbite_ast_m.f90 \
          $(SRCDIR)/fortbite_lexer_m.f90 \
          $(SRCDIR)/fortbite_parser_m.f90 \
          $(SRCDIR)/fortbite_evaluator_m.f90 \
          $(SRCDIR)/fortbite_io_m.f90 \
          $(SRCDIR)/fortbite.f90

# Object files
OBJECTS = $(SOURCES:$(SRCDIR)/%.f90=$(BUILDDIR)/%.o)

# Default target
all: directories $(TARGET)

# Create necessary directories
directories:
	@mkdir -p $(BUILDDIR) $(MODDIR) $(BINDIR)

# Link the executable
$(TARGET): $(OBJECTS)
	$(FC) $(LDFLAGS) -J$(MODDIR) -o $@ $^ $(LIBS)
	@echo "Built FORTBITE successfully!"

# Compile Fortran source files
$(BUILDDIR)/%.o: $(SRCDIR)/%.f90
	$(FC) $(FFLAGS) -J$(MODDIR) -c $< -o $@

# Dependencies (manually specified for now)
$(BUILDDIR)/fortbite_types_m.o: $(BUILDDIR)/fortbite_precision_m.o
$(BUILDDIR)/fortbite_matrix_m.o: $(BUILDDIR)/fortbite_types_m.o
$(BUILDDIR)/fortbite_functions_m.o: $(BUILDDIR)/fortbite_types_m.o
$(BUILDDIR)/fortbite_arithmetic_m.o: $(BUILDDIR)/fortbite_precision_m.o $(BUILDDIR)/fortbite_types_m.o $(BUILDDIR)/fortbite_matrix_m.o
$(BUILDDIR)/fortbite_ast_m.o: $(BUILDDIR)/fortbite_types_m.o
$(BUILDDIR)/fortbite_lexer_m.o: $(BUILDDIR)/fortbite_types_m.o
$(BUILDDIR)/fortbite_parser_m.o: $(BUILDDIR)/fortbite_types_m.o $(BUILDDIR)/fortbite_ast_m.o
$(BUILDDIR)/fortbite_evaluator_m.o: $(BUILDDIR)/fortbite_types_m.o $(BUILDDIR)/fortbite_ast_m.o $(BUILDDIR)/fortbite_arithmetic_m.o $(BUILDDIR)/fortbite_functions_m.o $(BUILDDIR)/fortbite_matrix_m.o
$(BUILDDIR)/fortbite_io_m.o: $(BUILDDIR)/fortbite_precision_m.o $(BUILDDIR)/fortbite_types_m.o $(BUILDDIR)/fortbite_lexer_m.o $(BUILDDIR)/fortbite_parser_m.o $(BUILDDIR)/fortbite_evaluator_m.o $(BUILDDIR)/fortbite_ast_m.o
$(BUILDDIR)/fortbite.o: $(BUILDDIR)/fortbite_precision_m.o $(BUILDDIR)/fortbite_types_m.o $(BUILDDIR)/fortbite_io_m.o

# Clean up
clean:
	rm -rf $(BUILDDIR)

# Run the program
run: $(TARGET)
	./$(TARGET)

# Install (optional)
install: $(TARGET)
	cp $(TARGET) /usr/local/bin/fortbite

# Test compilation
TEST_FRAMEWORK = tests/test_framework.f90
TEST_SOURCES = tests/unit/test_arithmetic.f90 \
               tests/unit/test_functions.f90 \
               tests/integration/test_expressions.f90

TEST_OBJECTS = $(TEST_SOURCES:tests/%.f90=$(BUILDDIR)/tests/%.o) \
               $(BUILDDIR)/tests/test_framework.o

TEST_EXECUTABLES = $(TEST_SOURCES:tests/%.f90=$(BUILDDIR)/tests/%)

# Build test framework
$(BUILDDIR)/tests/test_framework.o: $(TEST_FRAMEWORK) directories
	@mkdir -p $(BUILDDIR)/tests/unit $(BUILDDIR)/tests/integration
	$(FC) $(FFLAGS) -J$(MODDIR) -c $< -o $@

# Library objects (all except main program)
LIB_OBJECTS = $(filter-out $(BUILDDIR)/fortbite.o, $(OBJECTS))

# Build test executables
$(BUILDDIR)/tests/unit/%.o: tests/unit/%.f90 $(BUILDDIR)/tests/test_framework.o $(LIB_OBJECTS)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) -c $< -o $@

$(BUILDDIR)/tests/integration/%.o: tests/integration/%.f90 $(BUILDDIR)/tests/test_framework.o $(LIB_OBJECTS)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) -c $< -o $@

$(BUILDDIR)/tests/unit/%: $(BUILDDIR)/tests/unit/%.o $(BUILDDIR)/tests/test_framework.o $(LIB_OBJECTS)
	$(FC) $(LDFLAGS) -J$(MODDIR) -o $@ $^

$(BUILDDIR)/tests/integration/%: $(BUILDDIR)/tests/integration/%.o $(BUILDDIR)/tests/test_framework.o $(LIB_OBJECTS)
	$(FC) $(LDFLAGS) -J$(MODDIR) -o $@ $^

# Test targets
test-build: directories $(TARGET) $(TEST_EXECUTABLES)
	@echo "Test build complete!"

test: test-build
	@./tests/run_tests.sh --no-build

test-unit: test-build
	@echo "Running unit tests..."
	@$(BUILDDIR)/tests/unit/test_arithmetic || true
	@$(BUILDDIR)/tests/unit/test_functions || true

test-integration: test-build
	@echo "Running integration tests..."
	@$(BUILDDIR)/tests/integration/test_expressions || true

test-verbose: test-build
	@for test in $(TEST_EXECUTABLES); do \
		echo "=== Running $$test ==="; \
		$$test || true; \
		echo; \
	done

# Clean including tests
clean-all: clean
	rm -rf test_results

# Help
help:
	@echo "FORTBITE Makefile targets:"
	@echo "  all           - Build the executable (default)"
	@echo "  clean         - Remove build files"
	@echo "  clean-all     - Remove build files and test results"
	@echo "  run           - Build and run FORTBITE"
	@echo "  install       - Install to /usr/local/bin"
	@echo "  test          - Run all tests"
	@echo "  test-unit     - Run unit tests only"
	@echo "  test-integration - Run integration tests only"
	@echo "  test-verbose  - Run all tests with full output"
	@echo "  test-build    - Build test executables only"
	@echo "  help          - Show this help message"

.PHONY: all clean clean-all run install help directories test test-unit test-integration test-verbose test-build