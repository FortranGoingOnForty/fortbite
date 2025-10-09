!> Integration tests for complete expression evaluation
program test_expressions
    use test_framework
    use fortbite_types_m, only: value_t, token_t, print_value
    use fortbite_lexer_m, only: tokenize, free_tokens
    use fortbite_parser_m, only: parse_expression, parse_error_t
    use fortbite_evaluator_m, only: evaluate_expression, evaluation_context_t, &
                                   evaluation_error_t, create_context
    use fortbite_ast_m, only: ast_node_t, free_ast
    use iso_fortran_env, only: real64
    implicit none

    type(test_suite) :: suite
    type(evaluation_context_t) :: context
    type(value_t) :: result
    real(real64) :: expected

    ! Initialize test suite
    suite = init_test_suite('Expression Evaluation')
    context = create_context()

    ! Test simple arithmetic expressions
    call test_expression(suite, context, '2 + 3', 5.0_real64, 'Simple addition')
    call test_expression(suite, context, '10 - 4', 6.0_real64, 'Simple subtraction')
    call test_expression(suite, context, '3 * 7', 21.0_real64, 'Simple multiplication')
    call test_expression(suite, context, '15 / 3', 5.0_real64, 'Simple division')
    call test_expression(suite, context, '2 ^ 3', 8.0_real64, 'Power operation')
    call test_expression(suite, context, '2 ** 3', 8.0_real64, 'Power with ** operator')

    ! Test operator precedence
    call test_expression(suite, context, '2 + 3 * 4', 14.0_real64, 'Precedence: multiplication before addition')
    call test_expression(suite, context, '(2 + 3) * 4', 20.0_real64, 'Parentheses override precedence')
    call test_expression(suite, context, '10 - 2 * 3', 4.0_real64, 'Precedence: multiplication before subtraction')
    call test_expression(suite, context, '2 * 3 + 4 * 5', 26.0_real64, 'Multiple multiplications and addition')
    call test_expression(suite, context, '2 ^ 3 * 4', 32.0_real64, 'Power before multiplication')
    call test_expression(suite, context, '2 * 3 ^ 2', 18.0_real64, 'Power before multiplication (2)')

    ! Test nested expressions
    call test_expression(suite, context, '((2 + 3) * 4) / 2', 10.0_real64, 'Nested parentheses')
    call test_expression(suite, context, '2 * (3 + (4 * 5))', 46.0_real64, 'Multiple nesting levels')

    ! Test unary operators
    call test_expression(suite, context, '-5', -5.0_real64, 'Unary minus')
    call test_expression(suite, context, '+5', 5.0_real64, 'Unary plus')
    call test_expression(suite, context, '-(2 + 3)', -5.0_real64, 'Unary minus with expression')
    call test_expression(suite, context, '-2 * 3', -6.0_real64, 'Unary minus with multiplication')
    call test_expression(suite, context, '2 * -3', -6.0_real64, 'Multiplication with unary minus')

    ! Test floating point expressions
    call test_expression_near(suite, context, '0.1 + 0.2', 0.3_real64, 1.0e-10_real64, 'Floating point addition')
    call test_expression(suite, context, '3.5 * 2', 7.0_real64, 'Floating point multiplication')
    call test_expression(suite, context, '7.5 / 2.5', 3.0_real64, 'Floating point division')

    ! Test mathematical constants
    call test_expression_near(suite, context, 'pi', 3.141592653589793_real64, 1.0e-10_real64, 'Pi constant')
    call test_expression_near(suite, context, 'e', 2.718281828459045_real64, 1.0e-10_real64, 'E constant')
    call test_expression_near(suite, context, '2 * pi', 6.283185307179586_real64, 1.0e-10_real64, 'Expression with pi')

    ! Test precision specification (our recently fixed feature!)
    call test_precision(suite, context, 'pi::10', 10, 'Pi with 10 digit precision')
    call test_precision(suite, context, 'e::5', 5, 'E with 5 digit precision')

    ! Run the test suite
    call run_test_suite(suite)

contains

    !> Test an expression evaluation
    subroutine test_expression(suite, context, expr_str, expected, test_name)
        type(test_suite), intent(inout) :: suite
        type(evaluation_context_t), intent(inout) :: context
        character(len=*), intent(in) :: expr_str
        real(real64), intent(in) :: expected
        character(len=*), intent(in) :: test_name

        type(token_t), allocatable :: tokens(:)
        type(ast_node_t), pointer :: ast
        type(parse_error_t) :: parse_err
        type(evaluation_error_t) :: eval_err
        type(value_t) :: result

        call add_test_case(suite, test_name)

        ! Tokenize
        tokens = tokenize(expr_str)

        ! Parse
        ast => parse_expression(tokens, parse_err)
        if (parse_err%has_error) then
            call assert_true(suite, .false., 'Parse error: ' // trim(parse_err%message))
            call free_tokens(tokens)
            return
        end if

        ! Evaluate
        result = evaluate_expression(ast, context, eval_err)
        if (eval_err%has_error) then
            call assert_true(suite, .false., 'Evaluation error: ' // trim(eval_err%message))
        else
            call assert_equals(suite, result%scalar_val, expected)
        end if

        ! Clean up
        call free_ast(ast)
        call free_tokens(tokens)
    end subroutine test_expression

    !> Test an expression evaluation with tolerance
    subroutine test_expression_near(suite, context, expr_str, expected, tolerance, test_name)
        type(test_suite), intent(inout) :: suite
        type(evaluation_context_t), intent(inout) :: context
        character(len=*), intent(in) :: expr_str
        real(real64), intent(in) :: expected, tolerance
        character(len=*), intent(in) :: test_name

        type(token_t), allocatable :: tokens(:)
        type(ast_node_t), pointer :: ast
        type(parse_error_t) :: parse_err
        type(evaluation_error_t) :: eval_err
        type(value_t) :: result

        call add_test_case(suite, test_name)

        ! Tokenize
        tokens = tokenize(expr_str)

        ! Parse
        ast => parse_expression(tokens, parse_err)
        if (parse_err%has_error) then
            call assert_true(suite, .false., 'Parse error: ' // trim(parse_err%message))
            call free_tokens(tokens)
            return
        end if

        ! Evaluate
        result = evaluate_expression(ast, context, eval_err)
        if (eval_err%has_error) then
            call assert_true(suite, .false., 'Evaluation error: ' // trim(eval_err%message))
        else
            call assert_near(suite, result%scalar_val, expected, tolerance)
        end if

        ! Clean up
        call free_ast(ast)
        call free_tokens(tokens)
    end subroutine test_expression_near

    !> Test precision specification
    subroutine test_precision(suite, context, expr_str, expected_precision, test_name)
        type(test_suite), intent(inout) :: suite
        type(evaluation_context_t), intent(inout) :: context
        character(len=*), intent(in) :: expr_str
        integer, intent(in) :: expected_precision
        character(len=*), intent(in) :: test_name

        type(token_t), allocatable :: tokens(:)
        type(ast_node_t), pointer :: ast
        type(parse_error_t) :: parse_err
        type(evaluation_error_t) :: eval_err
        type(value_t) :: result

        call add_test_case(suite, test_name)

        ! Tokenize
        tokens = tokenize(expr_str)

        ! Parse
        ast => parse_expression(tokens, parse_err)
        if (parse_err%has_error) then
            call assert_true(suite, .false., 'Parse error: ' // trim(parse_err%message))
            call free_tokens(tokens)
            return
        end if

        ! Evaluate
        result = evaluate_expression(ast, context, eval_err)
        if (eval_err%has_error) then
            call assert_true(suite, .false., 'Evaluation error: ' // trim(eval_err%message))
        else
            ! Check that precision_kind was set correctly
            call assert_true(suite, result%precision_kind == expected_precision, &
                'Precision specification stored correctly')
        end if

        ! Clean up
        call free_ast(ast)
        call free_tokens(tokens)
    end subroutine test_precision

end program test_expressions