!> Unit tests for mathematical functions
program test_functions
    use test_framework
    use fortbite_types_m, only: value_t, create_scalar, create_complex, VALUE_SCALAR, VALUE_COMPLEX
    use fortbite_functions_m, only: eval_trigonometric, eval_hyperbolic, eval_logarithmic, &
                                   eval_exponential, eval_statistical, eval_special, &
                                   eval_complex_functions
    use iso_fortran_env, only: real64
    implicit none

    type(test_suite) :: suite
    type(value_t) :: arg, result
    real(real64) :: pi

    ! Initialize test suite
    suite = init_test_suite('Mathematical Functions')
    pi = 4.0_real64 * atan(1.0_real64)

    ! Test trigonometric functions
    call add_test_case(suite, 'sin(0) = 0')
    arg = create_scalar(0.0_real64)
    result = eval_trigonometric('sin', arg)
    call assert_near(suite, result%scalar_val, 0.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'sin(pi/2) = 1')
    arg = create_scalar(pi/2.0_real64)
    result = eval_trigonometric('sin', arg)
    call assert_near(suite, result%scalar_val, 1.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'cos(0) = 1')
    arg = create_scalar(0.0_real64)
    result = eval_trigonometric('cos', arg)
    call assert_near(suite, result%scalar_val, 1.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'cos(pi) = -1')
    arg = create_scalar(pi)
    result = eval_trigonometric('cos', arg)
    call assert_near(suite, result%scalar_val, -1.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'tan(0) = 0')
    arg = create_scalar(0.0_real64)
    result = eval_trigonometric('tan', arg)
    call assert_near(suite, result%scalar_val, 0.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'tan(pi/4) = 1')
    arg = create_scalar(pi/4.0_real64)
    result = eval_trigonometric('tan', arg)
    call assert_near(suite, result%scalar_val, 1.0_real64, 1.0e-10_real64)

    ! Test inverse trigonometric functions
    call add_test_case(suite, 'asin(0) = 0')
    arg = create_scalar(0.0_real64)
    result = eval_trigonometric('asin', arg)
    call assert_near(suite, result%scalar_val, 0.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'asin(1) = pi/2')
    arg = create_scalar(1.0_real64)
    result = eval_trigonometric('asin', arg)
    call assert_near(suite, result%scalar_val, pi/2.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'acos(1) = 0')
    arg = create_scalar(1.0_real64)
    result = eval_trigonometric('acos', arg)
    call assert_near(suite, result%scalar_val, 0.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'atan(1) = pi/4')
    arg = create_scalar(1.0_real64)
    result = eval_trigonometric('atan', arg)
    call assert_near(suite, result%scalar_val, pi/4.0_real64, 1.0e-10_real64)

    ! Test hyperbolic functions
    call add_test_case(suite, 'sinh(0) = 0')
    arg = create_scalar(0.0_real64)
    result = eval_hyperbolic('sinh', arg)
    call assert_near(suite, result%scalar_val, 0.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'cosh(0) = 1')
    arg = create_scalar(0.0_real64)
    result = eval_hyperbolic('cosh', arg)
    call assert_near(suite, result%scalar_val, 1.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'tanh(0) = 0')
    arg = create_scalar(0.0_real64)
    result = eval_hyperbolic('tanh', arg)
    call assert_near(suite, result%scalar_val, 0.0_real64, 1.0e-10_real64)

    ! Test logarithmic functions
    call add_test_case(suite, 'log(e) = 1')
    arg = create_scalar(exp(1.0_real64))
    result = eval_logarithmic('log', arg)
    call assert_near(suite, result%scalar_val, 1.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'ln(e) = 1')
    arg = create_scalar(exp(1.0_real64))
    result = eval_logarithmic('ln', arg)
    call assert_near(suite, result%scalar_val, 1.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'log10(10) = 1')
    arg = create_scalar(10.0_real64)
    result = eval_logarithmic('log10', arg)
    call assert_near(suite, result%scalar_val, 1.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'log10(100) = 2')
    arg = create_scalar(100.0_real64)
    result = eval_logarithmic('log10', arg)
    call assert_near(suite, result%scalar_val, 2.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'log2(8) = 3')
    arg = create_scalar(8.0_real64)
    result = eval_logarithmic('log2', arg)
    call assert_near(suite, result%scalar_val, 3.0_real64, 1.0e-10_real64)

    ! Test exponential functions
    call add_test_case(suite, 'exp(0) = 1')
    arg = create_scalar(0.0_real64)
    result = eval_exponential('exp', arg)
    call assert_near(suite, result%scalar_val, 1.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'exp(1) = e')
    arg = create_scalar(1.0_real64)
    result = eval_exponential('exp', arg)
    call assert_near(suite, result%scalar_val, exp(1.0_real64), 1.0e-10_real64)

    call add_test_case(suite, 'exp2(3) = 8')
    arg = create_scalar(3.0_real64)
    result = eval_exponential('exp2', arg)
    call assert_near(suite, result%scalar_val, 8.0_real64, 1.0e-10_real64)

    call add_test_case(suite, 'exp10(2) = 100')
    arg = create_scalar(2.0_real64)
    result = eval_exponential('exp10', arg)
    call assert_near(suite, result%scalar_val, 100.0_real64, 1.0e-10_real64)

    ! Test special functions
    call add_test_case(suite, 'floor(3.7) = 3')
    arg = create_scalar(3.7_real64)
    result = eval_special('floor', arg)
    call assert_equals(suite, result%scalar_val, 3.0_real64)

    call add_test_case(suite, 'floor(-2.3) = -3')
    arg = create_scalar(-2.3_real64)
    result = eval_special('floor', arg)
    call assert_equals(suite, result%scalar_val, -3.0_real64)

    call add_test_case(suite, 'ceil(3.2) = 4')
    arg = create_scalar(3.2_real64)
    result = eval_special('ceil', arg)
    call assert_equals(suite, result%scalar_val, 4.0_real64)

    call add_test_case(suite, 'ceil(-2.7) = -2')
    arg = create_scalar(-2.7_real64)
    result = eval_special('ceil', arg)
    call assert_equals(suite, result%scalar_val, -2.0_real64)

    call add_test_case(suite, 'round(3.5) = 4')
    arg = create_scalar(3.5_real64)
    result = eval_special('round', arg)
    call assert_equals(suite, result%scalar_val, 4.0_real64)

    call add_test_case(suite, 'round(3.4) = 3')
    arg = create_scalar(3.4_real64)
    result = eval_special('round', arg)
    call assert_equals(suite, result%scalar_val, 3.0_real64)

    call add_test_case(suite, 'factorial(5) = 120')
    arg = create_scalar(5.0_real64)
    result = eval_special('factorial', arg)
    call assert_equals(suite, result%scalar_val, 120.0_real64)

    call add_test_case(suite, 'factorial(0) = 1')
    arg = create_scalar(0.0_real64)
    result = eval_special('factorial', arg)
    call assert_equals(suite, result%scalar_val, 1.0_real64)

    ! Test complex functions
    call add_test_case(suite, 'real(3+4i) = 3')
    arg = create_complex(3.0_real64, 4.0_real64)
    result = eval_complex_functions('real', arg)
    call assert_equals(suite, result%scalar_val, 3.0_real64)

    call add_test_case(suite, 'imag(3+4i) = 4')
    arg = create_complex(3.0_real64, 4.0_real64)
    result = eval_complex_functions('imag', arg)
    call assert_equals(suite, result%scalar_val, 4.0_real64)

    call add_test_case(suite, 'abs(3+4i) = 5')
    arg = create_complex(3.0_real64, 4.0_real64)
    result = eval_complex_functions('cabs', arg)
    call assert_equals(suite, result%scalar_val, 5.0_real64)

    call add_test_case(suite, 'arg(1+i) = pi/4')
    arg = create_complex(1.0_real64, 1.0_real64)
    result = eval_complex_functions('arg', arg)
    call assert_near(suite, result%scalar_val, pi/4.0_real64, 1.0e-10_real64)

    ! Run the test suite
    call run_test_suite(suite)

end program test_functions