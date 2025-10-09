# FORTBITE Quick Reference Card

## Basic Operations
```
2 + 3       # Addition
5 - 1       # Subtraction  
4 * 6       # Multiplication
8 / 2       # Division
2**8        # Power
17 % 5      # Modulo
```

## Variables & Precision
```
x := 42             # Variable assignment
pi::50              # High precision (50 digits)
precision           # Show current precision
```

## Mathematical Functions

### Trigonometric
```
sin(x)  cos(x)  tan(x)      # Basic trig
asin(x) acos(x) atan(x)     # Inverse trig  
sec(x)  csc(x)  cot(x)      # Additional trig
```

### Hyperbolic
```
sinh(x) cosh(x) tanh(x)     # Basic hyperbolic
asinh(x) acosh(x) atanh(x)  # Inverse hyperbolic
sech(x) csch(x) coth(x)     # Additional hyperbolic
```

### Logarithmic & Exponential
```
log(x)   ln(x)      # Natural log
log10(x) lg(x)      # Base-10 log
log2(x)             # Base-2 log
exp(x)   exp2(x)    # Exponential
sqrt(x)  abs(x)     # Square root, absolute
```

### Special Functions
```
factorial(n)  gamma(x)     # Factorial, Gamma
erf(x)       erfc(x)       # Error functions
ceil(x)      floor(x)      # Ceiling, Floor
round(x)     frac(x)       # Round, Fractional part
```

## Complex Numbers
```
3+4i                # Complex literal
(3,4)               # Alternative syntax
cmplx(3,4)          # Function form

real(z)   imag(z)   # Real/imaginary parts
conj(z)   abs(z)    # Conjugate, magnitude
arg(z)              # Phase angle
```

## Matrix Operations

### Creation
```
[1,2;3,4]          # Matrix literal
zeros(3,3)         # 3×3 zero matrix
ones(2,4)          # 2×4 ones matrix  
eye(5)             # 5×5 identity matrix
```

### Operations
```
transpose(A)       # Matrix transpose
det(A)            # Determinant
inv(A)            # Matrix inverse
trace(A)          # Trace (diagonal sum)
rank(A)           # Matrix rank
solve(A,b)        # Solve Ax = b
```

### Statistics
```
sum(M)            # Sum all elements
mean(M)           # Average
std(M)            # Standard deviation
```

## Built-in Constants
```
pi                # 3.14159...
e                 # 2.71828...  
i                 # Imaginary unit
```

## Commands
```
help              # Show help
exit              # Quit program
clear             # Clear screen
precision         # Show precision info
info              # System information
```

## Syntax Rules
- `:=` for assignment
- `::` for precision control
- `;` separates matrix rows
- `,` separates matrix columns  
- `i` suffix for imaginary numbers
- `()` for function calls and grouping
- `[]` for matrix literals

## Examples
```
# Scientific calculation
g := 9.81
velocity := sqrt(2 * g * height)

# Linear algebra  
A := [2,1;1,3]
b := [5;7]
x := solve(A,b)

# Complex analysis
z := exp(i * pi/4)
magnitude := abs(z)

# High precision
pi_precise := pi::100
```