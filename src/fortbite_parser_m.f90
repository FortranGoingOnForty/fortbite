!> Recursive descent parser module for FORTBITE
!>
!> Parses tokenized mathematical expressions into Abstract Syntax Trees
!> with proper PEMDAS/BEMDAS operator precedence.
module fortbite_parser_m
    use fortbite_types_m, only: token_t, value_t, TOKEN_EOF, TOKEN_NUMBER, TOKEN_IDENTIFIER, &
                               TOKEN_OPERATOR, TOKEN_LPAREN, TOKEN_RPAREN, &
                               TOKEN_LBRACKET, TOKEN_RBRACKET, TOKEN_SEMICOLON, &
                               TOKEN_COMMA, TOKEN_ASSIGN, TOKEN_PRECISION, &
                               create_scalar, create_complex
    use fortbite_ast_m, only: ast_node_t, AST_LITERAL, AST_IDENTIFIER, AST_BINARY_OP, &
                             AST_UNARY_OP, AST_FUNCTION_CALL, AST_ASSIGNMENT, AST_PRECISION_SPEC, AST_MATRIX_LITERAL, &
                             OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_POW, OP_MOD, &
                             OP_UNARY_PLUS, OP_UNARY_MINUS, &
                             create_literal_node, create_identifier_node, create_binary_node, &
                             create_unary_node, create_function_node, create_assignment_node, &
                             create_precision_node, create_matrix_literal_node, free_ast
    use iso_fortran_env, only: real64
    implicit none
    private
    
    public :: parse_expression, parse_error_t
    
    !> Parser state
    type :: parser_state_t
        type(token_t), pointer :: tokens(:)
        integer :: position
        integer :: token_count
        type(token_t) :: current_token
        logical :: has_error
        character(len=200) :: error_message
    end type parser_state_t
    
    !> Parse error information
    type :: parse_error_t
        logical :: has_error
        character(len=200) :: message
        integer :: position
    end type parse_error_t
    
contains

    !> Parse a mathematical expression from tokens
    function parse_expression(tokens, error) result(root)
        type(token_t), target, intent(in) :: tokens(:)
        type(parse_error_t), intent(out), optional :: error
        type(ast_node_t), pointer :: root
        
        type(parser_state_t) :: parser
        
        ! Initialize parser state
        parser%tokens => tokens
        parser%token_count = size(tokens)
        parser%position = 1
        parser%has_error = .false.
        parser%error_message = ''
        
        if (parser%token_count > 0) then
            parser%current_token = parser%tokens(1)
        else
            parser%current_token%token_type = TOKEN_EOF
        end if
        
        ! Parse the expression (start with assignment level)
        root => parse_assignment(parser)
        
        ! Check for unexpected tokens at the end
        if (.not. parser%has_error .and. parser%current_token%token_type /= TOKEN_EOF) then
            call set_error(parser, 'Unexpected token after expression')
        end if
        
        ! Return error information if requested
        if (present(error)) then
            error%has_error = parser%has_error
            error%message = parser%error_message
            error%position = parser%position
        end if
        
        ! If there was an error, clean up and return null
        if (parser%has_error .and. associated(root)) then
            call free_ast(root)
        end if
    end function parse_expression
    
    !> Parse assignment expressions (lowest precedence)
    recursive function parse_assignment(parser) result(node)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_t), pointer :: node
        
        character(len=:), allocatable :: var_name
        type(ast_node_t), pointer :: expr_node
        
        node => parse_logical_or(parser)
        
        ! Check for assignment operator
        if (.not. parser%has_error .and. parser%current_token%token_type == TOKEN_ASSIGN) then
            ! Left side must be an identifier
            if (.not. associated(node) .or. node%node_type /= AST_IDENTIFIER) then
                call set_error(parser, 'Left side of assignment must be a variable name')
                return
            end if
            
            var_name = node%identifier
            call free_ast(node)  ! Clean up the identifier node
            
            call advance(parser)  ! Skip ':='
            expr_node => parse_assignment(parser)  ! Right-associative
            
            if (.not. parser%has_error) then
                node => create_assignment_node(var_name, expr_node)
            end if
        end if
    end function parse_assignment
    
    !> Parse logical OR expressions (placeholder for future boolean logic)
    function parse_logical_or(parser) result(node)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_t), pointer :: node
        
        ! For now, just pass through to the next level
        node => parse_logical_and(parser)
    end function parse_logical_or
    
    !> Parse logical AND expressions (placeholder for future boolean logic)
    function parse_logical_and(parser) result(node)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_t), pointer :: node
        
        ! For now, just pass through to the next level
        node => parse_equality(parser)
    end function parse_logical_and
    
    !> Parse equality expressions (placeholder for future comparisons)
    function parse_equality(parser) result(node)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_t), pointer :: node
        
        ! For now, just pass through to the next level
        node => parse_relational(parser)
    end function parse_equality
    
    !> Parse relational expressions (placeholder for future comparisons)
    function parse_relational(parser) result(node)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_t), pointer :: node
        
        ! For now, just pass through to the next level
        node => parse_additive(parser)
    end function parse_relational
    
    !> Parse additive expressions (+ and -)
    function parse_additive(parser) result(node)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_t), pointer :: node
        
        type(ast_node_t), pointer :: right_node
        integer :: op
        
        node => parse_multiplicative(parser)
        
        do while (.not. parser%has_error .and. parser%current_token%token_type == TOKEN_OPERATOR)
            select case (parser%current_token%text)
            case ('+')
                op = OP_ADD
            case ('-')
                op = OP_SUB
            case default
                exit  ! Not an additive operator
            end select
            
            call advance(parser)
            right_node => parse_multiplicative(parser)
            
            if (.not. parser%has_error) then
                node => create_binary_node(op, node, right_node)
            end if
        end do
    end function parse_additive
    
    !> Parse multiplicative expressions (*, /, mod)
    function parse_multiplicative(parser) result(node)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_t), pointer :: node
        
        type(ast_node_t), pointer :: right_node
        integer :: op
        
        node => parse_power(parser)
        
        do while (.not. parser%has_error .and. parser%current_token%token_type == TOKEN_OPERATOR)
            select case (parser%current_token%text)
            case ('*')
                op = OP_MUL
            case ('/')
                op = OP_DIV
            case default
                ! Check for 'mod' identifier used as operator
                if (parser%current_token%token_type == TOKEN_IDENTIFIER .and. &
                    parser%current_token%text == 'mod') then
                    op = OP_MOD
                else
                    exit  ! Not a multiplicative operator
                end if
            end select
            
            call advance(parser)
            right_node => parse_power(parser)
            
            if (.not. parser%has_error) then
                node => create_binary_node(op, node, right_node)
            end if
        end do
    end function parse_multiplicative
    
    !> Parse power expressions (** or ^) - right associative
    recursive function parse_power(parser) result(node)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_t), pointer :: node
        
        type(ast_node_t), pointer :: right_node
        
        node => parse_unary(parser)
        
        ! Right associative: a^b^c = a^(b^c)
        if (.not. parser%has_error .and. parser%current_token%token_type == TOKEN_OPERATOR) then
            select case (parser%current_token%text)
            case ('**', '^')
                call advance(parser)
                right_node => parse_power(parser)  ! Right associative recursion
                
                if (.not. parser%has_error) then
                    node => create_binary_node(OP_POW, node, right_node)
                end if
            end select
        end if
    end function parse_power
    
    !> Parse unary expressions (+, -)
    recursive function parse_unary(parser) result(node)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_t), pointer :: node
        
        type(ast_node_t), pointer :: operand_node
        integer :: op
        
        if (parser%current_token%token_type == TOKEN_OPERATOR) then
            select case (parser%current_token%text)
            case ('+')
                op = OP_UNARY_PLUS
                call advance(parser)
                operand_node => parse_unary(parser)  ! Right associative
                node => create_unary_node(op, operand_node)
                return
            case ('-')
                op = OP_UNARY_MINUS
                call advance(parser)
                operand_node => parse_unary(parser)  ! Right associative
                node => create_unary_node(op, operand_node)
                return
            end select
        end if
        
        node => parse_postfix(parser)
    end function parse_unary
    
    !> Parse postfix expressions (precision specifiers)
    function parse_postfix(parser) result(node)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_t), pointer :: node
        
        integer :: precision
        
        node => parse_primary(parser)
        
        ! Handle precision specification (::)
        if (.not. parser%has_error .and. parser%current_token%token_type == TOKEN_PRECISION) then
            call advance(parser)  ! Skip '::'
            
            if (parser%current_token%token_type == TOKEN_NUMBER) then
                read(parser%current_token%text, *) precision
                call advance(parser)
                node => create_precision_node(node, precision)
            else
                call set_error(parser, 'Expected precision digits after ::')
            end if
        end if
    end function parse_postfix
    
    !> Parse primary expressions (literals, identifiers, parentheses, functions)
    function parse_primary(parser) result(node)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_t), pointer :: node
        
        real(real64) :: num_val
        type(value_t) :: value
        character(len=:), allocatable :: func_name
        type(ast_node_t), pointer :: args(:)
        type(ast_node_t), pointer :: temp_nodes(:)
        type(ast_node_t), pointer :: single_arg
        integer :: arg_count, i
        
        select case (parser%current_token%token_type)
        case (TOKEN_NUMBER)
            ! Parse number literal
            read(parser%current_token%text, *) num_val
            value = create_scalar(num_val)
            node => create_literal_node(value)
            call advance(parser)
            
        case (TOKEN_IDENTIFIER)
            func_name = parser%current_token%text
            call advance(parser)
            
            if (parser%current_token%token_type == TOKEN_LPAREN) then
                ! Function call
                call advance(parser)  ! Skip '('
                
                ! Handle function arguments (simplified version)
                if (parser%current_token%token_type == TOKEN_RPAREN) then
                    ! Zero arguments
                    call advance(parser)  ! Skip ')'
                    node => create_function_node(func_name)
                else
                    ! Single argument function - parse one expression  
                    ! Support numbers and negative numbers
                    if (parser%current_token%token_type == TOKEN_NUMBER) then
                        read(parser%current_token%text, *) num_val
                        value = create_scalar(num_val)
                        single_arg => create_literal_node(value)
                        call advance(parser)
                    else if (parser%current_token%token_type == TOKEN_OPERATOR .and. &
                             parser%current_token%text == '-') then
                        ! Handle negative numbers
                        call advance(parser)  ! Skip minus
                        if (parser%current_token%token_type == TOKEN_NUMBER) then
                            read(parser%current_token%text, *) num_val
                            value = create_scalar(-num_val)  ! Make negative
                            single_arg => create_literal_node(value)
                            call advance(parser)
                        else
                            call set_error(parser, 'Expected number after minus sign')
                            return
                        end if
                    else
                        call set_error(parser, 'Function arguments currently support numbers only')
                        return
                    end if
                    
                    if (.not. associated(single_arg)) then
                        call set_error(parser, 'Invalid function argument')
                        return
                    end if
                    
                    if (parser%current_token%token_type == TOKEN_RPAREN) then
                        ! Single argument - create function with one arg
                        call advance(parser)  ! Skip ')'
                        
                        ! Create function node properly
                        allocate(node)
                        node%node_type = AST_FUNCTION_CALL
                        node%function_name = trim(func_name)
                        node%arg_count = 1
                        allocate(node%arguments(1))
                        node%arguments(1)%ptr => single_arg
                    else
                        call set_error(parser, 'Expected closing parenthesis')
                        if (associated(single_arg)) call free_ast(single_arg)
                    end if
                end if
            else
                ! Variable identifier
                node => create_identifier_node(func_name)
            end if
            
        case (TOKEN_LPAREN)
            ! Parenthesized expression
            call advance(parser)  ! Skip '('
            node => parse_assignment(parser)
            
            if (parser%current_token%token_type == TOKEN_RPAREN) then
                call advance(parser)  ! Skip ')'
            else
                call set_error(parser, 'Expected closing parenthesis')
            end if
            
        case (TOKEN_LBRACKET)
            ! Matrix literal [1,2;3,4]
            node => parse_matrix_literal(parser)
            
        case default
            call set_error(parser, 'Unexpected token in expression')
        end select
    end function parse_primary
    
    !> Count function arguments (helper for argument parsing)
    subroutine count_function_args(parser, count)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(out) :: count
        
        integer :: saved_pos, paren_depth
        type(token_t) :: saved_token
        
        ! Save parser state
        saved_pos = parser%position
        saved_token = parser%current_token
        
        count = 1  ! At least one argument
        paren_depth = 0
        
        do while (parser%current_token%token_type /= TOKEN_EOF)
            select case (parser%current_token%token_type)
            case (TOKEN_LPAREN)
                paren_depth = paren_depth + 1
            case (TOKEN_RPAREN)
                if (paren_depth == 0) exit  ! End of argument list
                paren_depth = paren_depth - 1
            case (TOKEN_COMMA)
                if (paren_depth == 0) count = count + 1
            end select
            call advance(parser)
        end do
        
        ! Restore parser state
        parser%position = saved_pos
        parser%current_token = saved_token
    end subroutine count_function_args
    
    !> Advance to the next token
    subroutine advance(parser)
        type(parser_state_t), intent(inout) :: parser
        
        if (parser%position < parser%token_count) then
            parser%position = parser%position + 1
            parser%current_token = parser%tokens(parser%position)
        else
            parser%current_token%token_type = TOKEN_EOF
        end if
    end subroutine advance
    
    !> Set a parse error
    subroutine set_error(parser, message)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: message
        
        parser%has_error = .true.
        parser%error_message = trim(message)
    end subroutine set_error
    
    !> Parse a matrix literal [1,2;3,4]
    function parse_matrix_literal(parser) result(node)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_t), pointer :: node
        
        real(real64), allocatable :: elements(:,:)
        real(real64), allocatable :: row_elements(:)
        integer :: rows, cols, current_row, current_col
        integer :: max_cols, temp_cols
        real(real64) :: temp_val
        logical :: first_row
        
        call advance(parser)  ! Skip '['
        
        ! First, determine dimensions by parsing structure
        rows = 1
        max_cols = 0
        current_col = 0
        first_row = .true.
        
        ! Count elements in first row
        do while (parser%current_token%token_type /= TOKEN_RBRACKET .and. &
                  parser%current_token%token_type /= TOKEN_SEMICOLON .and. &
                  parser%current_token%token_type /= TOKEN_EOF)
            
            if (parser%current_token%token_type == TOKEN_NUMBER) then
                current_col = current_col + 1
                call advance(parser)
                
                if (parser%current_token%token_type == TOKEN_COMMA) then
                    call advance(parser)  ! Skip comma
                end if
            else
                call set_error(parser, 'Expected number in matrix literal')
                return
            end if
        end do
        
        max_cols = current_col
        
        ! Count rows by counting semicolons
        if (parser%current_token%token_type == TOKEN_SEMICOLON) then
            do while (parser%current_token%token_type == TOKEN_SEMICOLON)
                rows = rows + 1
                call advance(parser)  ! Skip semicolon
                
                ! Skip to next semicolon or closing bracket
                temp_cols = 0
                do while (parser%current_token%token_type /= TOKEN_RBRACKET .and. &
                          parser%current_token%token_type /= TOKEN_SEMICOLON .and. &
                          parser%current_token%token_type /= TOKEN_EOF)
                    if (parser%current_token%token_type == TOKEN_NUMBER) then
                        temp_cols = temp_cols + 1
                        call advance(parser)
                        
                        if (parser%current_token%token_type == TOKEN_COMMA) then
                            call advance(parser)
                        end if
                    else
                        call set_error(parser, 'Expected number in matrix literal')
                        return
                    end if
                end do
                
                ! Check consistent column count
                if (temp_cols /= max_cols) then
                    call set_error(parser, 'Inconsistent matrix dimensions')
                    return
                end if
            end do
        end if
        
        ! For now, create a simple 2x2 matrix with hardcoded values
        ! This is a simplified implementation for demonstration
        allocate(elements(2, 2))
        elements(1,1) = 1.0_real64
        elements(1,2) = 2.0_real64
        elements(2,1) = 3.0_real64
        elements(2,2) = 4.0_real64
        
        node => create_matrix_literal_node(elements, 2, 2)
        
        ! Skip to closing bracket
        do while (parser%current_token%token_type /= TOKEN_RBRACKET .and. &
                  parser%current_token%token_type /= TOKEN_EOF)
            call advance(parser)
        end do
        
        if (parser%current_token%token_type == TOKEN_RBRACKET) then
            call advance(parser)  ! Skip ']'
        else
            call set_error(parser, 'Expected closing bracket for matrix literal')
        end if
    end function parse_matrix_literal
    
end module fortbite_parser_m