# FORTBITE Usage Guide

**FORTBITE** (Fortran-based Operations on Real-Time Backend for Interactive Technical Expression) is a high-precision mathematical calculator built with modern Fortran. It provides advanced mathematical capabilities including matrix operations, complex numbers, and high-precision arithmetic.

## Table of Contents
- [Getting Started](#getting-started)
- [Basic Arithmetic](#basic-arithmetic)
- [Variables](#variables)
- [Precision Control](#precision-control)
- [Mathematical Functions](#mathematical-functions)
- [Complex Numbers](#complex-numbers)
- [Matrix Operations](#matrix-operations)
- [Advanced Features](#advanced-features)
- [Command Reference](#command-reference)

## Getting Started

### Building FORTBITE
```bash
make clean && make
```

### Running FORTBITE
```bash
./build/bin/fortbite
```

### Basic Usage
FORTBITE uses an interactive REPL (Read-Eval-Print Loop). Simply type mathematical expressions and press Enter:

```
fortbite> 2 + 3
5
fortbite> sin(pi/4)
0.7071067811865476
fortbite> exit
```

## Basic Arithmetic

### Operators
- `+` Addition
- `-` Subtraction
- `*` Multiplication
- `/` Division
- `**` Exponentiation (power)
- `%` Modulo

### Examples
```
fortbite> 15 + 25
40
fortbite> 10 - 3.5
6.5
fortbite> 4 * 7
28
fortbite> 22 / 7
3.142857142857143
fortbite> 2**10
1024
fortbite> 17 % 5
2
```

### Order of Operations
FORTBITE follows standard mathematical precedence:
1. Parentheses `()`
2. Exponentiation `**`
3. Multiplication `*`, Division `/`, Modulo `%`
4. Addition `+`, Subtraction `-`

```
fortbite> 2 + 3 * 4
14
fortbite> (2 + 3) * 4
20
fortbite> 2**3**2
512
```

## Variables

### Variable Assignment
Use `:=` to assign values to variables:

```
fortbite> x := 42
42
fortbite> y := x * 2
84
fortbite> radius := 5.0
5
fortbite> area := pi * radius**2
78.53981633974483
```

### Variable Names
- Must start with a letter
- Can contain letters, numbers, and underscores
- Case-sensitive

```
fortbite> my_var := 10
10
fortbite> MyVar := 20
20
fortbite> coefficient_1 := 3.14
3.14
```

## Precision Control

### Setting Precision
Use `::` to specify the number of decimal digits for high-precision calculations:

```
fortbite> pi
3.141592653589793
fortbite> pi::50
3.1415926535897932384626433832795028841971693993751
fortbite> 1/3::25
0.3333333333333333333333333
```

### Current Precision
Check current precision settings:
```
fortbite> precision
Current precision: Double Precision
Decimal digits: 15
Exponent range: ±307
```

## Mathematical Functions

### Trigonometric Functions
```
fortbite> sin(pi/6)
0.49999999999999994
fortbite> cos(0)
1
fortbite> tan(pi/4)
0.9999999999999999
```

**Available functions:**
- `sin(x)`, `cos(x)`, `tan(x)` - Basic trigonometric functions
- `asin(x)`, `acos(x)`, `atan(x)` - Inverse trigonometric functions
- `sec(x)`, `csc(x)`, `cot(x)` - Additional trigonometric functions

**Alternative names:**
- `arcsin(x)` = `asin(x)`
- `arccos(x)` = `acos(x)` 
- `arctan(x)` = `atan(x)`

### Hyperbolic Functions
```
fortbite> sinh(1)
1.1752011936438014
fortbite> cosh(0)
1
fortbite> tanh(2)
0.9640275800758169
```

**Available functions:**
- `sinh(x)`, `cosh(x)`, `tanh(x)` - Basic hyperbolic functions
- `asinh(x)`, `acosh(x)`, `atanh(x)` - Inverse hyperbolic functions
- `sech(x)`, `csch(x)`, `coth(x)` - Additional hyperbolic functions

### Logarithmic and Exponential Functions
```
fortbite> log(e)
1
fortbite> log10(100)
2
fortbite> exp(1)
2.718281828459045
fortbite> sqrt(16)
4
```

**Available functions:**
- `log(x)`, `ln(x)` - Natural logarithm
- `log10(x)`, `lg(x)` - Base-10 logarithm
- `log2(x)` - Base-2 logarithm
- `exp(x)` - Exponential function (e^x)
- `exp2(x)` - Base-2 exponential (2^x)
- `exp10(x)` - Base-10 exponential (10^x)
- `expm1(x)` - exp(x) - 1 (accurate for small x)

### Special Functions
```
fortbite> factorial(5)
120
fortbite> gamma(4)
6
fortbite> erf(1)
0.8427007929497149
fortbite> abs(-5)
5
```

**Available functions:**
- `factorial(n)`, `fact(n)` - Factorial function
- `gamma(x)` - Gamma function
- `lgamma(x)`, `loggamma(x)` - Log-gamma function
- `erf(x)` - Error function
- `erfc(x)` - Complementary error function
- `abs(x)` - Absolute value
- `sqrt(x)` - Square root
- `ceil(x)`, `ceiling(x)` - Ceiling function
- `floor(x)` - Floor function
- `round(x)`, `nint(x)` - Round to nearest integer
- `frac(x)`, `fraction(x)` - Fractional part

## Complex Numbers

### Creating Complex Numbers
Multiple ways to create complex numbers:

```
fortbite> 3+4i
3+4i
fortbite> (3,4)
3+4i
fortbite> cmplx(3,4)
3+4i
```

### Complex Functions
```
fortbite> z := 3+4i
3+4i
fortbite> real(z)
3
fortbite> imag(z)
4
fortbite> abs(z)
5
fortbite> conj(z)
3-4i
fortbite> arg(z)
0.9272952180016122
```

**Available complex functions:**
- `real(z)`, `re(z)` - Real part
- `imag(z)`, `im(z)` - Imaginary part
- `conj(z)`, `conjugate(z)` - Complex conjugate
- `abs(z)`, `cabs(z)`, `modulus(z)` - Magnitude/modulus
- `arg(z)`, `phase(z)`, `angle(z)` - Phase angle

### Complex Arithmetic
```
fortbite> (3+4i) + (1+2i)
4+6i
fortbite> (3+4i) * (1+2i)
-5+10i
fortbite> (3+4i) / (1+2i)
2.2-0.4i
```

## Matrix Operations

### Creating Matrices

#### Matrix Literals
```
fortbite> [1,2;3,4]
[2x2 matrix]
  1  2
  3  4
```

#### Special Matrices
```
fortbite> zeros(3,3)
[3x3 matrix]
  0  0  0
  0  0  0
  0  0  0

fortbite> ones(2,3)
[2x3 matrix]
  1  1  1
  1  1  1

fortbite> eye(3)
[3x3 matrix]
  1  0  0
  0  1  0
  0  0  1
```

**Matrix creation functions:**
- `zeros(n)` - n×n zero matrix
- `zeros(m,n)` - m×n zero matrix
- `ones(n)` - n×n ones matrix  
- `ones(m,n)` - m×n ones matrix
- `eye(n)` - n×n identity matrix

### Matrix Operations
```
fortbite> A := [1,2;3,4]
[2x2 matrix]
  1  2
  3  4

fortbite> transpose(A)
[2x2 matrix]
  1  3
  2  4

fortbite> det(A)
-2

fortbite> inv(A)
[2x2 matrix]
  -2   1
  1.5  -0.5
```

**Available matrix functions:**
- `transpose(A)`, `trans(A)` - Matrix transpose
- `det(A)`, `determinant(A)` - Determinant
- `inv(A)`, `inverse(A)` - Matrix inverse
- `trace(A)` - Matrix trace (sum of diagonal elements)
- `rank(A)` - Matrix rank
- `solve(A,b)` - Solve linear system Ax = b

### Matrix Statistics
```
fortbite> M := [1,2,3;4,5,6]
[2x3 matrix]
  1  2  3
  4  5  6

fortbite> sum(M)
21
fortbite> mean(M)
3.5
fortbite> std(M)
1.8708286933869707
```

**Statistical functions:**
- `sum(M)` - Sum of all elements
- `mean(M)`, `average(M)` - Mean of all elements
- `std(M)`, `stddev(M)` - Standard deviation

## Advanced Features

### Built-in Constants
```
fortbite> pi
3.141592653589793
fortbite> e
2.718281828459045
fortbite> i
0+1i
```

### Chaining Operations
```
fortbite> result := sin(pi/4) + cos(pi/4)
1.4142135623730951
fortbite> matrix_result := det(inv([1,2;3,4]))
-0.5
```

### High-Precision Calculations
```
fortbite> pi::100
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679

fortbite> x := 1/3::50
0.33333333333333333333333333333333333333333333333333

fortbite> y := x * 3::50
0.99999999999999999999999999999999999999999999999999
```

## Command Reference

### Interactive Commands
- `help` - Display help information
- `exit` - Exit FORTBITE
- `clear` - Clear screen (if supported)
- `precision` - Show current precision settings
- `info` - Display system information

### Special Syntax
- `:=` - Variable assignment
- `::` - Precision specification
- `;` - Matrix row separator in literals
- `,` - Matrix column separator in literals
- `i` - Imaginary unit suffix
- `()` - Function calls and grouping
- `[]` - Matrix literals

## Examples and Use Cases

### Scientific Calculations
```fortran
! Calculate projectile motion
fortbite> g := 9.81
9.81
fortbite> v0 := 50
50  
fortbite> angle := 45 * pi/180
0.7853981633974483
fortbite> time_flight := 2 * v0 * sin(angle) / g
7.214390675673431
fortbite> max_height := (v0 * sin(angle))**2 / (2*g)
63.775510204081634
```

### Linear Algebra
```fortran
! Solve system of equations: 2x + 3y = 8, x - y = 1
fortbite> A := [2,3;1,-1]
[2x2 matrix]
  2   3
  1  -1
fortbite> b := [8;1] 
[2x1 matrix]
  8
  1
fortbite> solution := solve(A,b)
[2x1 matrix]
  2.2
  1.2
```

### Complex Analysis
```fortran
! Euler's formula verification: e^(iπ) + 1 = 0
fortbite> result := exp(i*pi) + 1
1.2246467991473532e-16+0i
! (Very close to zero, within numerical precision)
```

### Statistical Analysis
```fortran
! Analyze dataset
fortbite> data := [1,2,3,4,5,6,7,8,9,10]
[1x10 matrix]
  1  2  3  4  5  6  7  8  9  10
fortbite> data_mean := mean(data)
5.5
fortbite> data_std := std(data)
3.0276503540974917
```

## Tips and Best Practices

1. **Use descriptive variable names** for complex calculations
2. **Specify precision** when you need more than 15 decimal digits
3. **Check matrix dimensions** before operations (FORTBITE will warn you)
4. **Use parentheses** to clarify complex expressions
5. **Store intermediate results** in variables for multi-step calculations

## Error Handling

FORTBITE provides helpful error messages for common mistakes:

```
fortbite> sqrt(-1)
Evaluation error: sqrt() argument must be non-negative

fortbite> solve([1,2])
Evaluation error: solve() expects 2 arguments: solve(A, b)

fortbite> unknown_func(5)
Evaluation error: Unknown function: unknown_func
```

## Limitations

- Matrix operations are limited to reasonable sizes (memory dependent)
- Complex numbers use double precision internally
- Some functions may have domain restrictions (e.g., sqrt of negative numbers)
- High precision mode may be slower for complex calculations

---

*FORTBITE - High-Precision Calculator, Modern Fortran Edition*
*Built with modern Fortran 2008+ standards*