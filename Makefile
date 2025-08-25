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
$(BUILDDIR)/fortbite_evaluator_m.o: $(BUILDDIR)/fortbite_types_m.o $(BUILDDIR)/fortbite_ast_m.o $(BUILDDIR)/fortbite_arithmetic_m.o $(BUILDDIR)/fortbite_functions_m.o
$(BUILDDIR)/fortbite_io_m.o: $(BUILDDIR)/fortbite_precision_m.o $(BUILDDIR)/fortbite_types_m.o
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

# Help
help:
	@echo "FORTBITE Makefile targets:"
	@echo "  all      - Build the executable (default)"
	@echo "  clean    - Remove build files"
	@echo "  run      - Build and run FORTBITE"
	@echo "  install  - Install to /usr/local/bin"
	@echo "  help     - Show this help message"

.PHONY: all clean run install help directories