!> Expression evaluator module for FORTBITE
!>
!> Evaluates Abstract Syntax Trees, performing mathematical operations
!> with proper type promotion and precision handling.
module fortbite_evaluator_m
    use fortbite_types_m, only: value_t, variable_t, VALUE_SCALAR, VALUE_COMPLEX, VALUE_MATRIX, &
                               create_scalar, create_complex, create_matrix, print_value, is_zero, is_real, &
                               create_zeros_matrix, create_ones_matrix, create_eye_matrix
    use fortbite_ast_m, only: ast_node_t, ast_node_ptr_t, AST_LITERAL, AST_IDENTIFIER, AST_BINARY_OP, &
                             AST_UNARY_OP, AST_FUNCTION_CALL, AST_ASSIGNMENT, AST_PRECISION_SPEC, AST_MATRIX_LITERAL, &
                             OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_POW, OP_MOD, &
                             OP_UNARY_PLUS, OP_UNARY_MINUS
    use fortbite_arithmetic_m, only: add_values, subtract_values, multiply_values, &
                                    divide_values, power_values, negate_value, abs_value
    use fortbite_matrix_m, only: matrix_transpose, matrix_determinant, matrix_inverse, &
                                matrix_element_access, matrix_solve, matrix_rank, matrix_trace
    use iso_fortran_env, only: real64
    implicit none
    private
    
    public :: evaluate_expression, evaluation_context_t, evaluation_error_t
    public :: create_context, destroy_context, set_variable, get_variable
    
    !> Evaluation context (variable storage)
    type :: evaluation_context_t
        type(variable_t), pointer :: variables => null()
        integer :: default_precision = 15
    end type evaluation_context_t
    
    !> Evaluation error information
    type :: evaluation_error_t
        logical :: has_error = .false.
        character(len=200) :: message = ''
    end type evaluation_error_t
    
contains

    !> Create a new evaluation context
    function create_context() result(context)
        type(evaluation_context_t) :: context
        
        ! Initialize with default precision
        context%default_precision = 15
        nullify(context%variables)
    end function create_context
    
    !> Destroy evaluation context and free variables
    subroutine destroy_context(context)
        type(evaluation_context_t), intent(inout) :: context
        
        call free_variables(context%variables)
    end subroutine destroy_context
    
    !> Set a variable in the context
    subroutine set_variable(context, name, value)
        type(evaluation_context_t), intent(inout) :: context
        character(len=*), intent(in) :: name
        type(value_t), intent(in) :: value
        
        type(variable_t), pointer :: var, current
        
        ! Look for existing variable
        current => context%variables
        do while (associated(current))
            if (current%name == name) then
                current%value = value
                return
            end if
            current => current%next
        end do
        
        ! Create new variable
        allocate(var)
        var%name = trim(name)
        var%value = value
        var%next => context%variables
        context%variables => var
    end subroutine set_variable
    
    !> Get a variable from the context
    function get_variable(context, name, value) result(found)
        type(evaluation_context_t), intent(in) :: context
        character(len=*), intent(in) :: name
        type(value_t), intent(out) :: value
        logical :: found
        
        type(variable_t), pointer :: current
        
        found = .false.
        current => context%variables
        
        do while (associated(current))
            if (current%name == name) then
                value = current%value
                found = .true.
                return
            end if
            current => current%next
        end do
    end function get_variable
    
    !> Evaluate an AST expression
    recursive function evaluate_expression(node, context, error) result(value)
        type(ast_node_t), pointer, intent(in) :: node
        type(evaluation_context_t), intent(inout) :: context
        type(evaluation_error_t), intent(out), optional :: error
        type(value_t) :: value
        
        type(evaluation_error_t) :: local_error
        
        local_error%has_error = .false.
        
        if (.not. associated(node)) then
            call set_eval_error(local_error, 'Null AST node')
            value = create_scalar(0.0_real64)  ! Default value
            if (present(error)) error = local_error
            return
        end if
        
        select case (node%node_type)
        case (AST_LITERAL)
            value = evaluate_literal(node, local_error)
            
        case (AST_IDENTIFIER)
            value = evaluate_identifier(node, context, local_error)
            
        case (AST_BINARY_OP)
            value = evaluate_binary_op(node, context, local_error)
            
        case (AST_UNARY_OP)
            value = evaluate_unary_op(node, context, local_error)
            
        case (AST_FUNCTION_CALL)
            value = evaluate_function_call(node, context, local_error)
            
        case (AST_ASSIGNMENT)
            value = evaluate_assignment(node, context, local_error)
            
        case (AST_PRECISION_SPEC)
            value = evaluate_precision_spec(node, context, local_error)
            
        case (AST_MATRIX_LITERAL)
            value = evaluate_matrix_literal(node, local_error)
            
        case default
            call set_eval_error(local_error, 'Unknown AST node type')
            value = create_scalar(0.0_real64)
        end select
        
        if (present(error)) error = local_error
    end function evaluate_expression
    
    !> Evaluate a literal value
    function evaluate_literal(node, error) result(value)
        type(ast_node_t), pointer, intent(in) :: node
        type(evaluation_error_t), intent(out) :: error
        type(value_t) :: value
        
        error%has_error = .false.
        value = node%literal_value
    end function evaluate_literal
    
    !> Evaluate an identifier (variable or constant)
    function evaluate_identifier(node, context, error) result(value)
        type(ast_node_t), pointer, intent(in) :: node
        type(evaluation_context_t), intent(inout) :: context
        type(evaluation_error_t), intent(out) :: error
        type(value_t) :: value
        
        logical :: found
        
        error%has_error = .false.
        
        ! First check for mathematical constants
        if (node%identifier == 'pi') then
            value = create_scalar(4.0_real64 * atan(1.0_real64))  ! Precise π
            return
        else if (node%identifier == 'e') then
            value = create_scalar(exp(1.0_real64))  ! Precise e
            return
        else if (node%identifier == 'i') then
            value = create_complex(0.0_real64, 1.0_real64)  ! Imaginary unit
            return
        end if
        
        ! Check for matrix creation shortcuts
        if (node%identifier == 'zeros2') then
            value = create_zeros_matrix(2, 2)
            return
        else if (node%identifier == 'ones2') then
            value = create_ones_matrix(2, 2)
            return
        else if (node%identifier == 'ones3') then
            value = create_ones_matrix(3, 3)
            return
        else if (node%identifier == 'eye2') then
            value = create_eye_matrix(2)
            return
        else if (node%identifier == 'testmat') then
            ! Create a test matrix [[1,2],[3,4]]
            value = create_matrix(reshape([1.0_real64, 3.0_real64, 2.0_real64, 4.0_real64], [2, 2]))
            return
        end if
        
        ! Check user-defined variables
        found = get_variable(context, node%identifier, value)
        
        if (.not. found) then
            call set_eval_error(error, 'Undefined variable: ' // node%identifier)
            value = create_scalar(0.0_real64)
        end if
    end function evaluate_identifier
    
    !> Evaluate a binary operation
    recursive function evaluate_binary_op(node, context, error) result(value)
        type(ast_node_t), pointer, intent(in) :: node
        type(evaluation_context_t), intent(inout) :: context
        type(evaluation_error_t), intent(out) :: error
        type(value_t) :: value
        
        type(value_t) :: left_val, right_val
        type(evaluation_error_t) :: left_error, right_error
        
        error%has_error = .false.
        
        ! Evaluate operands
        left_val = evaluate_expression(node%left, context, left_error)
        if (left_error%has_error) then
            error = left_error
            return
        end if
        
        right_val = evaluate_expression(node%right, context, right_error)
        if (right_error%has_error) then
            error = right_error
            return
        end if
        
        ! Perform operation
        select case (node%operator)
        case (OP_ADD)
            value = add_values(left_val, right_val)
        case (OP_SUB)
            value = subtract_values(left_val, right_val)
        case (OP_MUL)
            value = multiply_values(left_val, right_val)
        case (OP_DIV)
            value = divide_values(left_val, right_val)
        case (OP_POW)
            value = power_values(left_val, right_val)
        case (OP_MOD)
            ! Modulo operation (for now, only on real numbers)
            if (left_val%value_type == VALUE_SCALAR .and. right_val%value_type == VALUE_SCALAR) then
                value = create_scalar(mod(left_val%scalar_val, right_val%scalar_val))
            else
                call set_eval_error(error, 'Modulo operation only supported for real numbers')
                value = create_scalar(0.0_real64)
            end if
        case default
            call set_eval_error(error, 'Unknown binary operator')
            value = create_scalar(0.0_real64)
        end select
    end function evaluate_binary_op
    
    !> Evaluate a unary operation
    recursive function evaluate_unary_op(node, context, error) result(value)
        type(ast_node_t), pointer, intent(in) :: node
        type(evaluation_context_t), intent(inout) :: context
        type(evaluation_error_t), intent(out) :: error
        type(value_t) :: value
        
        type(value_t) :: operand_val
        type(evaluation_error_t) :: operand_error
        
        error%has_error = .false.
        
        ! Evaluate operand
        operand_val = evaluate_expression(node%operand, context, operand_error)
        if (operand_error%has_error) then
            error = operand_error
            return
        end if
        
        ! Perform operation
        select case (node%operator)
        case (OP_UNARY_PLUS)
            value = operand_val  ! Unary plus doesn't change the value
        case (OP_UNARY_MINUS)
            value = negate_value(operand_val)
        case default
            call set_eval_error(error, 'Unknown unary operator')
            value = create_scalar(0.0_real64)
        end select
    end function evaluate_unary_op
    
    !> Evaluate a function call
    recursive function evaluate_function_call(node, context, error) result(value)
        type(ast_node_t), pointer, intent(in) :: node
        type(evaluation_context_t), intent(inout) :: context
        type(evaluation_error_t), intent(out) :: error
        type(value_t) :: value
        
        type(value_t), allocatable :: args(:)
        type(evaluation_error_t) :: arg_error
        integer :: i
        real(real64) :: x, result_val
        
        error%has_error = .false.
        
        ! Evaluate arguments
        if (node%arg_count > 0) then
            allocate(args(node%arg_count))
            do i = 1, node%arg_count
                args(i) = evaluate_expression(node%arguments(i)%ptr, context, arg_error)
                if (arg_error%has_error) then
                    error = arg_error
                    return
                end if
            end do
        end if
        
        ! Call function
        select case (node%function_name)
        case ('sin')
            if (node%arg_count /= 1) then
                call set_eval_error(error, 'sin() expects 1 argument')
                value = create_scalar(0.0_real64)
                return
            end if
            if (args(1)%value_type == VALUE_SCALAR) then
                value = create_scalar(sin(args(1)%scalar_val))
            else
                call set_eval_error(error, 'sin() expects a real argument')
                value = create_scalar(0.0_real64)
            end if
            
        case ('cos')
            if (node%arg_count /= 1) then
                call set_eval_error(error, 'cos() expects 1 argument')
                value = create_scalar(0.0_real64)
                return
            end if
            if (args(1)%value_type == VALUE_SCALAR) then
                value = create_scalar(cos(args(1)%scalar_val))
            else
                call set_eval_error(error, 'cos() expects a real argument')
                value = create_scalar(0.0_real64)
            end if
            
        case ('tan')
            if (node%arg_count /= 1) then
                call set_eval_error(error, 'tan() expects 1 argument')
                value = create_scalar(0.0_real64)
                return
            end if
            if (args(1)%value_type == VALUE_SCALAR) then
                value = create_scalar(tan(args(1)%scalar_val))
            else
                call set_eval_error(error, 'tan() expects a real argument')
                value = create_scalar(0.0_real64)
            end if
            
        case ('log')
            if (node%arg_count /= 1) then
                call set_eval_error(error, 'log() expects 1 argument')
                value = create_scalar(0.0_real64)
                return
            end if
            if (args(1)%value_type == VALUE_SCALAR) then
                x = args(1)%scalar_val
                if (x > 0.0_real64) then
                    value = create_scalar(log(x))
                else
                    call set_eval_error(error, 'log() argument must be positive')
                    value = create_scalar(0.0_real64)
                end if
            else
                call set_eval_error(error, 'log() expects a real argument')
                value = create_scalar(0.0_real64)
            end if
            
        case ('exp')
            if (node%arg_count /= 1) then
                call set_eval_error(error, 'exp() expects 1 argument')
                value = create_scalar(0.0_real64)
                return
            end if
            if (args(1)%value_type == VALUE_SCALAR) then
                value = create_scalar(exp(args(1)%scalar_val))
            else
                call set_eval_error(error, 'exp() expects a real argument')
                value = create_scalar(0.0_real64)
            end if
            
        case ('sqrt')
            if (node%arg_count /= 1) then
                call set_eval_error(error, 'sqrt() expects 1 argument')
                value = create_scalar(0.0_real64)
                return
            end if
            if (args(1)%value_type == VALUE_SCALAR) then
                x = args(1)%scalar_val
                if (x >= 0.0_real64) then
                    value = create_scalar(sqrt(x))
                else
                    call set_eval_error(error, 'sqrt() argument must be non-negative')
                    value = create_scalar(0.0_real64)
                end if
            else
                call set_eval_error(error, 'sqrt() expects a real argument')
                value = create_scalar(0.0_real64)
            end if
            
        case ('abs')
            if (node%arg_count /= 1) then
                call set_eval_error(error, 'abs() expects 1 argument')
                value = create_scalar(0.0_real64)
                return
            end if
            value = abs_value(args(1))
            
        ! Matrix creation functions
        case ('zeros')
            if (node%arg_count == 1) then
                ! zeros(n) - square matrix
                if (args(1)%value_type == VALUE_SCALAR) then
                    if (args(1)%scalar_val > 0 .and. args(1)%scalar_val == int(args(1)%scalar_val)) then
                        value = create_zeros_matrix(int(args(1)%scalar_val), int(args(1)%scalar_val))
                    else
                        call set_eval_error(error, 'zeros() size must be a positive integer')
                        value = create_scalar(0.0_real64)
                    end if
                else
                    call set_eval_error(error, 'zeros() expects numeric size argument')
                    value = create_scalar(0.0_real64)
                end if
            else if (node%arg_count == 2) then
                ! zeros(m,n) - rectangular matrix
                if (args(1)%value_type == VALUE_SCALAR .and. args(2)%value_type == VALUE_SCALAR) then
                    if (args(1)%scalar_val > 0 .and. args(1)%scalar_val == int(args(1)%scalar_val) .and. &
                        args(2)%scalar_val > 0 .and. args(2)%scalar_val == int(args(2)%scalar_val)) then
                        value = create_zeros_matrix(int(args(1)%scalar_val), int(args(2)%scalar_val))
                    else
                        call set_eval_error(error, 'zeros() sizes must be positive integers')
                        value = create_scalar(0.0_real64)
                    end if
                else
                    call set_eval_error(error, 'zeros() expects numeric size arguments')
                    value = create_scalar(0.0_real64)
                end if
            else
                call set_eval_error(error, 'zeros() expects 1 or 2 arguments')
                value = create_scalar(0.0_real64)
            end if
            
        case ('ones')
            if (node%arg_count == 1) then
                ! ones(n) - square matrix
                if (args(1)%value_type == VALUE_SCALAR) then
                    if (args(1)%scalar_val > 0 .and. args(1)%scalar_val == int(args(1)%scalar_val)) then
                        value = create_ones_matrix(int(args(1)%scalar_val), int(args(1)%scalar_val))
                    else
                        call set_eval_error(error, 'ones() size must be a positive integer')
                        value = create_scalar(0.0_real64)
                    end if
                else
                    call set_eval_error(error, 'ones() expects numeric size argument')
                    value = create_scalar(0.0_real64)
                end if
            else if (node%arg_count == 2) then
                ! ones(m,n) - rectangular matrix
                if (args(1)%value_type == VALUE_SCALAR .and. args(2)%value_type == VALUE_SCALAR) then
                    if (args(1)%scalar_val > 0 .and. args(1)%scalar_val == int(args(1)%scalar_val) .and. &
                        args(2)%scalar_val > 0 .and. args(2)%scalar_val == int(args(2)%scalar_val)) then
                        value = create_ones_matrix(int(args(1)%scalar_val), int(args(2)%scalar_val))
                    else
                        call set_eval_error(error, 'ones() sizes must be positive integers')
                        value = create_scalar(0.0_real64)
                    end if
                else
                    call set_eval_error(error, 'ones() expects numeric size arguments')
                    value = create_scalar(0.0_real64)
                end if
            else
                call set_eval_error(error, 'ones() expects 1 or 2 arguments')
                value = create_scalar(0.0_real64)
            end if
            
        case ('eye')
            if (node%arg_count /= 1) then
                call set_eval_error(error, 'eye() expects 1 argument')
                value = create_scalar(0.0_real64)
                return
            end if
            if (args(1)%value_type == VALUE_SCALAR) then
                if (args(1)%scalar_val > 0 .and. args(1)%scalar_val == int(args(1)%scalar_val)) then
                    value = create_eye_matrix(int(args(1)%scalar_val))
                else
                    call set_eval_error(error, 'eye() size must be a positive integer')
                    value = create_scalar(0.0_real64)
                end if
            else
                call set_eval_error(error, 'eye() expects numeric size argument')
                value = create_scalar(0.0_real64)
            end if
            
        ! Matrix functions
        case ('transpose', 'trans')
            if (node%arg_count /= 1) then
                call set_eval_error(error, 'transpose() expects 1 argument')
                value = create_scalar(0.0_real64)
                return
            end if
            if (args(1)%value_type == VALUE_MATRIX) then
                value = matrix_transpose(args(1))
            else
                call set_eval_error(error, 'transpose() expects a matrix argument')
                value = create_scalar(0.0_real64)
            end if
            
        case ('det', 'determinant')
            if (node%arg_count /= 1) then
                call set_eval_error(error, 'det() expects 1 argument')
                value = create_scalar(0.0_real64)
                return
            end if
            if (args(1)%value_type == VALUE_MATRIX) then
                value = create_scalar(matrix_determinant(args(1)))
            else
                call set_eval_error(error, 'det() expects a matrix argument')
                value = create_scalar(0.0_real64)
            end if
            
        case ('inv', 'inverse')
            if (node%arg_count /= 1) then
                call set_eval_error(error, 'inv() expects 1 argument')
                value = create_scalar(0.0_real64)
                return
            end if
            if (args(1)%value_type == VALUE_MATRIX) then
                value = matrix_inverse(args(1))
            else
                call set_eval_error(error, 'inv() expects a matrix argument')
                value = create_scalar(0.0_real64)
            end if
            
        case ('solve')
            ! Solve linear system Ax = b
            if (node%arg_count /= 2) then
                call set_eval_error(error, 'solve() expects 2 arguments: solve(A, b)')
                value = create_scalar(0.0_real64)
                return
            end if
            if (args(1)%value_type == VALUE_MATRIX .and. args(2)%value_type == VALUE_MATRIX) then
                value = matrix_solve(args(1), args(2))
            else
                call set_eval_error(error, 'solve() expects matrix arguments')
                value = create_scalar(0.0_real64)
            end if
            
        case ('rank')
            ! Calculate matrix rank
            if (args(1)%value_type == VALUE_MATRIX) then
                value = create_scalar(real(matrix_rank(args(1)), real64))
            else
                call set_eval_error(error, 'rank() expects a matrix argument')
                value = create_scalar(0.0_real64)
            end if
            
        case ('trace')
            ! Calculate matrix trace (sum of diagonal elements)
            if (args(1)%value_type == VALUE_MATRIX) then
                value = create_scalar(matrix_trace(args(1)))
            else
                call set_eval_error(error, 'trace() expects a matrix argument')
                value = create_scalar(0.0_real64)
            end if
            
        case default
            call set_eval_error(error, 'Unknown function: ' // node%function_name)
            value = create_scalar(0.0_real64)
        end select
        
        if (allocated(args)) deallocate(args)
    end function evaluate_function_call
    
    !> Evaluate an assignment
    recursive function evaluate_assignment(node, context, error) result(value)
        type(ast_node_t), pointer, intent(in) :: node
        type(evaluation_context_t), intent(inout) :: context
        type(evaluation_error_t), intent(out) :: error
        type(value_t) :: value
        
        type(evaluation_error_t) :: expr_error
        character(len=:), allocatable :: var_name
        
        error%has_error = .false.
        
        if (.not. associated(node%left) .or. node%left%node_type /= AST_IDENTIFIER) then
            call set_eval_error(error, 'Left side of assignment must be a variable')
            value = create_scalar(0.0_real64)
            return
        end if
        
        var_name = node%left%identifier
        
        ! Evaluate the right-hand side expression
        value = evaluate_expression(node%right, context, expr_error)
        if (expr_error%has_error) then
            error = expr_error
            return
        end if
        
        ! Store the variable
        call set_variable(context, var_name, value)
    end function evaluate_assignment
    
    !> Evaluate a precision specification
    recursive function evaluate_precision_spec(node, context, error) result(value)
        type(ast_node_t), pointer, intent(in) :: node
        type(evaluation_context_t), intent(inout) :: context
        type(evaluation_error_t), intent(out) :: error
        type(value_t) :: value
        
        type(evaluation_error_t) :: expr_error
        
        error%has_error = .false.
        
        ! For now, just evaluate the expression (ignore precision specification)
        ! TODO: Implement actual precision control in Phase 3
        value = evaluate_expression(node%expression, context, expr_error)
        if (expr_error%has_error) then
            error = expr_error
            return
        end if
        
        ! Could modify precision here based on node%precision_digits
        ! For now, just return the value as-is
    end function evaluate_precision_spec
    
    !> Evaluate a matrix literal
    function evaluate_matrix_literal(node, error) result(value)
        type(ast_node_t), pointer, intent(in) :: node
        type(evaluation_error_t), intent(out) :: error
        type(value_t) :: value
        
        error%has_error = .false.
        
        if (allocated(node%matrix_elements)) then
            value = create_matrix(node%matrix_elements)
        else
            call set_eval_error(error, 'Invalid matrix literal')
            value = create_scalar(0.0_real64)
        end if
    end function evaluate_matrix_literal
    
    !> Set an evaluation error
    subroutine set_eval_error(error, message)
        type(evaluation_error_t), intent(out) :: error
        character(len=*), intent(in) :: message
        
        error%has_error = .true.
        error%message = trim(message)
    end subroutine set_eval_error
    
    !> Free variable linked list
    recursive subroutine free_variables(var)
        type(variable_t), pointer, intent(inout) :: var
        
        if (associated(var)) then
            call free_variables(var%next)
            deallocate(var)
        end if
        nullify(var)
    end subroutine free_variables
    
end module fortbite_evaluator_m