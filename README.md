# fortbite

A command-line calculator written in Fortran that handles arbitrary precision arithmetic, complex numbers, and matrices.

## Building

```bash
make
```

## Running

```bash
./build/bin/fortbite
```

## Basic Usage

### Arithmetic
```
fortbite> 2 + 3
5.0000000000000000

fortbite> 10 - 4 * 2
2.0000000000000000

fortbite> (10 - 4) * 2
12.000000000000000

fortbite> 2^8
256.00000000000000

fortbite> 27^(1/3)
3.0000000000000000
```

### Precision Control

Specify output precision with `::digits`
```
fortbite> pi
3.1415926535897931

fortbite> pi::10
3.1415926536

fortbite> pi::30
3.141592653589793115997963468544

fortbite> e::15
2.718281828459045
```

### Variables
```
fortbite> x := 42
42.000000000000000

fortbite> y := x * 2
84.000000000000000

fortbite> x + y
126.00000000000000
```

### Functions

#### Trigonometric
```
fortbite> sin(pi/2)
1.0000000000000000

fortbite> cos(0)
1.0000000000000000

fortbite> atan(1) * 4
3.1415926535897931
```

#### Logarithmic and Exponential
```
fortbite> log(e)
1.0000000000000000

fortbite> log10(1000)
3.0000000000000000

fortbite> exp(1)
2.7182818284590451

fortbite> 2^10
1024.0000000000000
```

#### Special Functions
```
fortbite> sqrt(2)
1.4142135623730951

fortbite> factorial(5)
120.00000000000000

fortbite> floor(3.7)
3.0000000000000000

fortbite> ceil(3.2)
4.0000000000000000
```

### Complex Numbers
```
fortbite> (3 + 4i) * 2
6.0000000000000000+8.0000000000000000i

fortbite> abs(3 + 4i)
5.0000000000000000

fortbite> conj(3 + 4i)
3.0000000000000000-4.0000000000000000i
```

### Matrices
```
fortbite> [1,2;3,4]
[2x2 matrix]
  1.0000000000000000  2.0000000000000000
  3.0000000000000000  4.0000000000000000

fortbite> A := [1,2;3,4]
[2x2 matrix]
  1.0000000000000000  2.0000000000000000
  3.0000000000000000  4.0000000000000000

fortbite> det(A)
-2.0000000000000000

fortbite> inv(A)
[2x2 matrix]
  -2.0000000000000000  1.0000000000000000
  1.5000000000000000  -0.50000000000000000

fortbite> eye(3)
[3x3 matrix]
  1.0000000000000000  0.0000000000000000  0.0000000000000000
  0.0000000000000000  1.0000000000000000  0.0000000000000000
  0.0000000000000000  0.0000000000000000  1.0000000000000000
```

## Available Functions

### Math Functions
- Trigonometric: `sin`, `cos`, `tan`, `asin`, `acos`, `atan`
- Hyperbolic: `sinh`, `cosh`, `tanh`
- Logarithmic: `log`, `ln`, `log10`, `log2`
- Exponential: `exp`, `exp2`, `exp10`
- Rounding: `floor`, `ceil`, `round`
- Other: `sqrt`, `abs`, `factorial`

### Complex Functions
- `real` or `re` - real part
- `imag` or `im` - imaginary part
- `conj` - complex conjugate
- `arg` - phase angle
- `cabs` - complex absolute value

### Matrix Functions
- `zeros(m,n)` - zero matrix
- `ones(m,n)` - matrix of ones
- `eye(n)` - identity matrix
- `det` - determinant
- `inv` - inverse
- `transpose` - matrix transpose

## Constants
- `pi` - 3.14159...
- `e` - 2.71828...
- `i` - imaginary unit

## Operators
- Basic: `+`, `-`, `*`, `/`
- Power: `^` or `**`
- Assignment: `:=`
- Precision: `::`

## Commands
- `help` - show command list
- `exit` or `quit` - exit the calculator
- `clear` - clear screen
- `vars` - list defined variables

## Testing

Run the test suite:
```bash
make test
```

Run specific test categories:
```bash
make test-unit        # Unit tests only
make test-integration # Integration tests only
```

## Notes

- Calculations use double precision by default
- The `::` precision operator only affects output formatting, not internal precision
- Variable names are case-sensitive
- Matrix operations require compatible dimensions