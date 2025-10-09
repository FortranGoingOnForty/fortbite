# FORTBITE Developer Guide

This guide is for developers who want to understand, modify, or extend FORTBITE's codebase.

## Architecture Overview

FORTBITE follows a modular, layered architecture with clear separation of concerns:

```
┌─────────────────────────────────────┐
│            User Interface          │
│         (fortbite_io_m)            │
├─────────────────────────────────────┤
│           Evaluator                 │
│      (fortbite_evaluator_m)        │
├─────────────────────────────────────┤
│    Parser          │    Functions   │
│ (fortbite_parser_m)│(fortbite_funcs_m)│
├─────────────────────┼─────────────────┤
│         AST         │   Arithmetic    │
│  (fortbite_ast_m)  │(fortbite_arith_m)│
├─────────────────────┼─────────────────┤
│        Lexer        │     Matrix      │
│ (fortbite_lexer_m) │(fortbite_matrix_m)│
├─────────────────────────────────────┤
│           Core Types                │
│        (fortbite_types_m)          │
├─────────────────────────────────────┤
│          Precision                  │
│      (fortbite_precision_m)        │
└─────────────────────────────────────┘
```

## Module Reference

### Core Modules

#### `fortbite_precision_m.f90`
**Purpose:** Defines precision parameters and floating-point kinds
```fortran
integer, parameter :: sp = real32  ! Single precision
integer, parameter :: dp = real64  ! Double precision  
integer, parameter :: qp = real128 ! Quad precision (if available)
```

#### `fortbite_types_m.f90`
**Purpose:** Core data types and error handling
```fortran
type :: value_t              ! Universal value container
type :: variable_t           ! Variable storage with linked list
type :: token_t              ! Lexical tokens
type :: fortbite_error_t     ! Unified error handling
```

**Key functions:**
- Matrix creation: `create_zeros_matrix()`, `create_ones_matrix()`, `create_eye_matrix()`
- Value operations: `create_scalar()`, `create_complex()`, `destroy_value()`
- Error handling: `set_fortbite_error()`, `create_error_value()`

### Parsing Pipeline

#### `fortbite_lexer_m.f90`
**Purpose:** Tokenizes input strings into mathematical tokens
```fortran
function tokenize(input) result(tokens)
```

**Token types:** Numbers, identifiers, operators, parentheses, brackets, etc.

#### `fortbite_ast_m.f90`  
**Purpose:** Abstract Syntax Tree definition and memory management
```fortran
type :: ast_node_t
    integer :: node_type        ! AST_LITERAL, AST_BINARY_OP, etc.
    character(len=:), allocatable :: identifier
    real(real64) :: value
    type(ast_node_t), pointer :: left, right
    ! ... other fields
end type
```

#### `fortbite_parser_m.f90`
**Purpose:** Builds ASTs from token streams using recursive descent
```fortran
function parse_expression(tokens, error) result(root)
```

**Grammar hierarchy:**
1. Assignment (`x := expr`)
2. Logical operations
3. Arithmetic operations (`+`, `-`, `*`, `/`, `**`)
4. Function calls
5. Primary expressions (numbers, variables, parentheses)

### Evaluation System

#### `fortbite_evaluator_m.f90`
**Purpose:** Executes ASTs and manages variables
```fortran
function evaluate_expression(node, context, error) result(value)
```

**Key patterns:**
- Visitor pattern for AST traversal
- Type checking and promotion
- Error propagation
- Variable context management

**Recent improvements:**
- Unified error handling with `quick_error()` helper
- Consolidated function dispatch with helper functions
- Streamlined argument validation

#### `fortbite_arithmetic_m.f90`
**Purpose:** Basic arithmetic operations with type promotion
```fortran
function add_values(left, right) result(value)
function multiply_values(left, right) result(value)
! etc.
```

### Mathematical Functions

#### `fortbite_functions_m.f90`
**Purpose:** Advanced mathematical functions
```fortran
function eval_trigonometric(func_name, arg) result(value)
function eval_hyperbolic(func_name, arg) result(value)
function eval_logarithmic(func_name, arg) result(value)
function eval_exponential(func_name, arg) result(value)
function eval_statistical(func_name, arg) result(value)
function eval_special(func_name, arg) result(value)
function eval_complex_functions(func_name, arg) result(value)
```

#### `fortbite_matrix_m.f90`
**Purpose:** Linear algebra operations
```fortran
function matrix_transpose(matrix) result(result)
function matrix_determinant(matrix) result(det)
function matrix_inverse(matrix) result(inv)
function matrix_solve(A, b) result(x)
```

### User Interface

#### `fortbite_io_m.f90`
**Purpose:** REPL loop and user interaction
```fortran
subroutine repl_loop(context)
function evaluate_math_expression(input, context) result(success)
```

## Code Quality Improvements

### Recent Refactoring (Completed)

1. **Eliminated Code Duplication**
   - Created `validate_function_args()` helper
   - Added `validate_matrix_dims()` for dimension checking
   - Reduced ~120 lines of duplicate validation code

2. **Unified Function Dispatch**
   - Added `eval_matrix_creation()` and `eval_matrix_operation()` helpers
   - Simplified case statements from 60+ lines to 1-2 lines
   - Maintained backward compatibility

3. **Streamlined Matrix Creation**
   - Created `setup_matrix_base()` helper function
   - Reduced duplicate setup code across all matrix creation functions
   - All matrix functions now use consistent patterns

4. **Enhanced Error Handling**
   - Added unified `fortbite_error_t` type
   - Created `create_error_value()` and `quick_error()` helpers
   - Replaced 15+ instances of hardcoded error returns

## Development Guidelines

### Coding Standards

1. **Naming Conventions**
   - Modules: `fortbite_*_m.f90`
   - Functions: `verb_noun()` (e.g., `create_matrix()`)
   - Types: `*_t` suffix (e.g., `value_t`)
   - Constants: `UPPER_CASE`

2. **Error Handling**
   - Always use unified error types
   - Prefer helper functions over manual error setting
   - Include descriptive error messages
   - Clean up resources on error paths

3. **Memory Management**
   - Use allocatable arrays instead of pointers where possible
   - Always pair allocate/deallocate
   - Implement cleanup routines for complex types
   - Use `destroy_*()` functions consistently

4. **Documentation**
   - Document all public interfaces with `!>` comments
   - Include usage examples for complex functions
   - Explain non-obvious algorithms
   - Keep README and guides up to date

### Adding New Functions

1. **Mathematical Functions**
   ```fortran
   ! In fortbite_functions_m.f90
   case ('your_func')
       if (validate_args(...)) then
           result_val = create_scalar(your_calculation(x))
       else
           result_val = create_error_value()
       end if
   ```

2. **Matrix Operations**  
   ```fortran
   ! In fortbite_matrix_m.f90
   function matrix_your_operation(matrix) result(result)
       type(value_t), intent(in) :: matrix
       type(value_t) :: result
       
       ! Validate matrix input
       ! Perform operation
       ! Return result
   end function
   ```

3. **Register in Evaluator**
   ```fortran
   ! In fortbite_evaluator_m.f90
   case ('your_func')
       value = eval_your_category(node%function_name, args(1))
   ```

### Testing Strategy

#### Unit Tests (Recommended)
```fortran
program test_arithmetic
    use fortbite_arithmetic_m
    implicit none
    
    call test_addition()
    call test_multiplication()
    
contains
    subroutine test_addition()
        ! Test cases
    end subroutine
end program
```

#### Integration Tests
```bash
# Test mathematical functions
echo "sin(pi/2)" | ./fortbite | grep "1.0"
echo "det([1,2;3,4])" | ./fortbite | grep "\-2"
```

### Performance Considerations

1. **Memory Usage**
   - Large matrices can consume significant memory
   - Consider implementing matrix size limits
   - Use efficient algorithms (O(n³) → O(n²·⁸) where possible)

2. **Precision vs Speed**
   - High-precision mode is slower
   - Consider lazy precision conversion
   - Cache frequently used constants

3. **Function Dispatch**
   - Current string-based dispatch is readable but not fastest
   - Consider hash tables for very large function sets
   - Profile before optimizing

## Future Enhancements

### Planned Features
- **Automated Testing Suite** (in progress)
- **Package Management** (fpm.toml ready)
- **Extended Function Library** (statistical distributions, etc.)
- **Performance Optimization** (BLAS integration)

### Extension Points
- **Custom Functions**: Easy to add via `fortbite_functions_m.f90`
- **New Data Types**: Extend `value_t` for intervals, polynomials, etc.
- **Alternative UIs**: Replace `fortbite_io_m.f90` for GUI/web interfaces
- **File I/O**: Add import/export capabilities

## Debugging Tips

1. **Build with Debug Info**
   ```bash
   make clean && make FFLAGS="-g -fcheck=all -fbacktrace"
   ```

2. **Enable Warnings**
   ```bash
   make FFLAGS="-Wall -Wextra -Wconversion"
   ```

3. **Memory Debugging**
   ```bash
   valgrind --tool=memcheck ./build/bin/fortbite
   ```

4. **AST Debugging**
   - Add print statements in evaluator
   - Trace recursive descent parsing
   - Verify token sequences

## Contributing

1. **Fork and Branch**
   - Create feature branches
   - Use descriptive commit messages
   - Include tests for new functionality

2. **Code Review Checklist**
   - [ ] Follows naming conventions
   - [ ] Includes error handling
   - [ ] Has documentation comments
   - [ ] Memory is properly managed
   - [ ] Tests pass (when implemented)

3. **Performance Testing**
   - Profile before/after changes
   - Test with large matrices
   - Verify memory usage patterns

---

*This developer guide provides the foundation for understanding and extending FORTBITE. For questions or clarifications, please refer to the source code comments and existing patterns.*