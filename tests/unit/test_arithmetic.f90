!> Unit tests for arithmetic operations
program test_arithmetic
    use test_framework
    use fortbite_types_m, only: value_t, create_scalar, VALUE_SCALAR
    use fortbite_arithmetic_m, only: add_values, subtract_values, multiply_values, &
                                    divide_values, power_values, negate_value
    use iso_fortran_env, only: real64
    implicit none

    type(test_suite) :: suite
    type(value_t) :: a, b, result
    real(real64) :: expected

    ! Initialize test suite
    suite = init_test_suite('Arithmetic Operations')

    ! Test addition
    call add_test_case(suite, 'Addition: 2 + 3 = 5')
    a = create_scalar(2.0_real64)
    b = create_scalar(3.0_real64)
    result = add_values(a, b)
    call assert_equals(suite, result%scalar_val, 5.0_real64)

    call add_test_case(suite, 'Addition: -5 + 3 = -2')
    a = create_scalar(-5.0_real64)
    b = create_scalar(3.0_real64)
    result = add_values(a, b)
    call assert_equals(suite, result%scalar_val, -2.0_real64)

    call add_test_case(suite, 'Addition: 0.1 + 0.2')
    a = create_scalar(0.1_real64)
    b = create_scalar(0.2_real64)
    result = add_values(a, b)
    call assert_near(suite, result%scalar_val, 0.3_real64, 1.0e-10_real64)

    ! Test subtraction
    call add_test_case(suite, 'Subtraction: 10 - 3 = 7')
    a = create_scalar(10.0_real64)
    b = create_scalar(3.0_real64)
    result = subtract_values(a, b)
    call assert_equals(suite, result%scalar_val, 7.0_real64)

    call add_test_case(suite, 'Subtraction: -5 - 3 = -8')
    a = create_scalar(-5.0_real64)
    b = create_scalar(3.0_real64)
    result = subtract_values(a, b)
    call assert_equals(suite, result%scalar_val, -8.0_real64)

    ! Test multiplication
    call add_test_case(suite, 'Multiplication: 4 * 5 = 20')
    a = create_scalar(4.0_real64)
    b = create_scalar(5.0_real64)
    result = multiply_values(a, b)
    call assert_equals(suite, result%scalar_val, 20.0_real64)

    call add_test_case(suite, 'Multiplication: -3 * 7 = -21')
    a = create_scalar(-3.0_real64)
    b = create_scalar(7.0_real64)
    result = multiply_values(a, b)
    call assert_equals(suite, result%scalar_val, -21.0_real64)

    call add_test_case(suite, 'Multiplication: -2 * -3 = 6')
    a = create_scalar(-2.0_real64)
    b = create_scalar(-3.0_real64)
    result = multiply_values(a, b)
    call assert_equals(suite, result%scalar_val, 6.0_real64)

    ! Test division
    call add_test_case(suite, 'Division: 15 / 3 = 5')
    a = create_scalar(15.0_real64)
    b = create_scalar(3.0_real64)
    result = divide_values(a, b)
    call assert_equals(suite, result%scalar_val, 5.0_real64)

    call add_test_case(suite, 'Division: 7 / 2 = 3.5')
    a = create_scalar(7.0_real64)
    b = create_scalar(2.0_real64)
    result = divide_values(a, b)
    call assert_equals(suite, result%scalar_val, 3.5_real64)

    call add_test_case(suite, 'Division: -12 / 4 = -3')
    a = create_scalar(-12.0_real64)
    b = create_scalar(4.0_real64)
    result = divide_values(a, b)
    call assert_equals(suite, result%scalar_val, -3.0_real64)

    ! Test power operations
    call add_test_case(suite, 'Power: 2^3 = 8')
    a = create_scalar(2.0_real64)
    b = create_scalar(3.0_real64)
    result = power_values(a, b)
    call assert_equals(suite, result%scalar_val, 8.0_real64)

    call add_test_case(suite, 'Power: 5^2 = 25')
    a = create_scalar(5.0_real64)
    b = create_scalar(2.0_real64)
    result = power_values(a, b)
    call assert_equals(suite, result%scalar_val, 25.0_real64)

    call add_test_case(suite, 'Power: 4^0.5 = 2')
    a = create_scalar(4.0_real64)
    b = create_scalar(0.5_real64)
    result = power_values(a, b)
    call assert_equals(suite, result%scalar_val, 2.0_real64)

    call add_test_case(suite, 'Power: 10^-1 = 0.1')
    a = create_scalar(10.0_real64)
    b = create_scalar(-1.0_real64)
    result = power_values(a, b)
    call assert_near(suite, result%scalar_val, 0.1_real64, 1.0e-10_real64)

    ! Test negation
    call add_test_case(suite, 'Negation: -(5) = -5')
    a = create_scalar(5.0_real64)
    result = negate_value(a)
    call assert_equals(suite, result%scalar_val, -5.0_real64)

    call add_test_case(suite, 'Negation: -(-3) = 3')
    a = create_scalar(-3.0_real64)
    result = negate_value(a)
    call assert_equals(suite, result%scalar_val, 3.0_real64)

    ! Test special cases
    call add_test_case(suite, 'Addition with zero: 5 + 0 = 5')
    a = create_scalar(5.0_real64)
    b = create_scalar(0.0_real64)
    result = add_values(a, b)
    call assert_equals(suite, result%scalar_val, 5.0_real64)

    call add_test_case(suite, 'Multiplication by zero: 5 * 0 = 0')
    a = create_scalar(5.0_real64)
    b = create_scalar(0.0_real64)
    result = multiply_values(a, b)
    call assert_equals(suite, result%scalar_val, 0.0_real64)

    call add_test_case(suite, 'Multiplication by one: 7 * 1 = 7')
    a = create_scalar(7.0_real64)
    b = create_scalar(1.0_real64)
    result = multiply_values(a, b)
    call assert_equals(suite, result%scalar_val, 7.0_real64)

    ! Run the test suite
    call run_test_suite(suite)

end program test_arithmetic