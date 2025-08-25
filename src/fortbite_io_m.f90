!> I/O and REPL interface module for FORTBITE
!>
!> Handles user input/output, command processing, and the main REPL loop.
module fortbite_io_m
    use fortbite_precision_m, only: get_precision_info, precision_info_t
    use iso_fortran_env, only: real32, real64, real128
    use fortbite_types_m, only: value_t, variable_t, print_value, token_t
    use fortbite_lexer_m, only: tokenize, free_tokens
    use fortbite_parser_m, only: parse_expression, parse_error_t
    use fortbite_ast_m, only: ast_node_t, free_ast
    use fortbite_evaluator_m, only: evaluate_expression, evaluation_context_t, evaluation_error_t, &
                                   create_context, destroy_context
    implicit none
    private
    
    public :: repl_loop, print_banner, print_help
    public :: parse_command, is_command
    
    ! Maximum input line length
    integer, parameter :: MAX_LINE_LENGTH = 1000
    
contains

    !> Print the FORTBITE banner
    subroutine print_banner()
        write(*, '(A)') ''
        write(*, '(A)') '======================================'
        write(*, '(A)') '              FORTBITE               '
        write(*, '(A)') '     High-Precision Calculator       '
        write(*, '(A)') '      Modern Fortran Edition         '
        write(*, '(A)') '======================================'
        write(*, '(A)') ''
        write(*, '(A)') 'Type "help" for commands or "exit" to quit.'
        write(*, '(A)') 'Use :: for precision (e.g., 3.14159::100)'
        write(*, '(A)') 'Use := for assignment (e.g., x := 42)'
        write(*, '(A)') ''
    end subroutine print_banner
    
    !> Print help information
    subroutine print_help()
        type(precision_info_t) :: info
        
        write(*, '(A)') 'FORTBITE Commands:'
        write(*, '(A)') '  Basic arithmetic: +, -, *, /, ** (power)'
        write(*, '(A)') '  Variables: x := value'
        write(*, '(A)') '  Precision: value::digits (e.g., pi::50)'
        write(*, '(A)') '  Complex: 3+4i, (3,4), cmplx(3,4)'
        write(*, '(A)') '  Functions: sin, cos, tan, log, exp, sqrt, abs'
        write(*, '(A)') '  Constants: pi, e, i'
        write(*, '(A)') '  Matrices: [1,2;3,4], zeros(3,3), ones(2,2)'
        write(*, '(A)') '  Commands: help, exit, clear, precision, info'
        write(*, '(A)') ''
        
        ! Show current precision info
        info = get_precision_info(real64)
        write(*, '(A,A)') 'Current precision: ', trim(info%name)
        write(*, '(A,I0,A)') 'Decimal digits: ', info%decimal_digits, ''
        write(*, '(A,I0,A)') 'Exponent range: ±', info%exponent_range, ''
        write(*, '(A)') ''
    end subroutine print_help
    
    !> Check if input is a command
    logical function is_command(input)
        character(len=*), intent(in) :: input
        character(len=len_trim(input)) :: trimmed_input
        
        trimmed_input = trim(adjustl(input))
        
        if (len_trim(trimmed_input) == 0) then
            is_command = .false.
            return
        end if
        
        ! Check for known commands
        is_command = (trimmed_input == 'help' .or. trimmed_input == 'h' .or. trimmed_input == '?' .or. &
                      trimmed_input == 'exit' .or. trimmed_input == 'quit' .or. trimmed_input == 'q' .or. &
                      trimmed_input == 'clear' .or. trimmed_input == 'cls' .or. &
                      trimmed_input == 'precision' .or. trimmed_input == 'info' .or. &
                      trimmed_input == 'vars' .or. trimmed_input == 'variables' .or. &
                      index(trimmed_input, 'precision ') == 1)
        
        ! Everything else is treated as a mathematical expression
    end function is_command
    
    !> Parse and execute a command
    logical function parse_command(input, variables) result(continue_repl)
        character(len=*), intent(in) :: input
        type(variable_t), pointer, intent(inout) :: variables
        
        character(len=len_trim(input)) :: command
        character(len=100) :: arg
        integer :: space_pos
        
        continue_repl = .true.
        command = trim(adjustl(input))
        
        ! Split command and arguments
        space_pos = index(command, ' ')
        if (space_pos > 0) then
            arg = command(space_pos+1:)
            command = command(1:space_pos-1)
        else
            arg = ''
        end if
        
        ! Convert to lowercase for case-insensitive commands
        call to_lowercase(command)
        
        select case (trim(command))
        case ('help', 'h', '?')
            call print_help()
            
        case ('exit', 'quit', 'q')
            write(*, '(A)') 'Goodbye!'
            continue_repl = .false.
            
        case ('clear', 'cls')
            call clear_screen()
            
        case ('precision')
            call handle_precision_command(arg)
            
        case ('info')
            call show_system_info()
            
        case ('vars', 'variables')
            call show_variables(variables)
            
        case default
            write(*, '(A,A,A)') 'Unknown command: "', trim(command), '"'
            write(*, '(A)') 'Type "help" for available commands.'
        end select
    end function parse_command
    
    !> Main REPL (Read-Eval-Print Loop)
    subroutine repl_loop()
        character(len=MAX_LINE_LENGTH) :: input
        type(variable_t), pointer :: variables => null()
        type(evaluation_context_t) :: context
        logical :: continue_loop
        integer :: ios
        
        call print_banner()
        context = create_context()
        continue_loop = .true.
        
        do while (continue_loop)
            write(*, '(A)', advance='no') 'fortbite> '
            read(*, '(A)', iostat=ios) input
            
            if (ios /= 0) then
                ! Handle end of file (Ctrl+D)
                write(*, *)
                write(*, '(A)') 'Goodbye!'
                exit
            end if
            
            ! Skip empty lines
            if (len_trim(input) == 0) cycle
            
            if (is_command(input)) then
                continue_loop = parse_command(input, variables)
            else
                ! Handle mathematical expression
                call evaluate_math_expression(trim(input), context)
            end if
        end do
        
        ! Clean up
        call destroy_context(context)
        call cleanup_variables(variables)
    end subroutine repl_loop
    
    !> Evaluate a mathematical expression
    subroutine evaluate_math_expression(expression, context)
        character(len=*), intent(in) :: expression
        type(evaluation_context_t), intent(inout) :: context
        
        type(token_t), allocatable :: tokens(:)
        type(ast_node_t), pointer :: ast_root => null()
        type(parse_error_t) :: parse_err
        type(evaluation_error_t) :: eval_err
        type(value_t) :: result
        
        ! Tokenize the expression
        tokens = tokenize(expression)
        
        ! Parse into AST
        ast_root => parse_expression(tokens, parse_err)
        
        if (parse_err%has_error) then
            write(*, '(A,A)') 'Parse error: ', trim(parse_err%message)
        else if (associated(ast_root)) then
            ! Evaluate the expression
            result = evaluate_expression(ast_root, context, eval_err)
            
            if (eval_err%has_error) then
                write(*, '(A,A)') 'Evaluation error: ', trim(eval_err%message)
            else
                ! Print the result
                call print_value(result)
            end if
        else
            write(*, '(A)') 'Failed to parse expression.'
        end if
        
        ! Clean up
        if (associated(ast_root)) call free_ast(ast_root)
        call free_tokens(tokens)
    end subroutine evaluate_math_expression
    
    !> Convert string to lowercase
    subroutine to_lowercase(str)
        character(len=*), intent(inout) :: str
        integer :: i
        
        do i = 1, len(str)
            if (str(i:i) >= 'A' .and. str(i:i) <= 'Z') then
                str(i:i) = achar(iachar(str(i:i)) + 32)
            end if
        end do
    end subroutine to_lowercase
    
    !> Clear the screen (ANSI escape codes)
    subroutine clear_screen()
        write(*, '(A)') achar(27) // '[2J' // achar(27) // '[H'
    end subroutine clear_screen
    
    !> Handle precision command
    subroutine handle_precision_command(arg)
        character(len=*), intent(in) :: arg
        type(precision_info_t) :: info
        integer :: precision_digits, ios
        
        if (len_trim(arg) == 0) then
            ! Show current precision
            info = get_precision_info(real64)
            write(*, '(A,A)') 'Current precision: ', trim(info%name)
            write(*, '(A,I0)') 'Decimal digits: ', info%decimal_digits
            write(*, '(A,I0)') 'Exponent range: ±', info%exponent_range
        else
            ! Set new precision
            read(arg, *, iostat=ios) precision_digits
            if (ios == 0 .and. precision_digits > 0) then
                write(*, '(A,I0,A)') 'Setting precision to ', precision_digits, ' decimal digits...'
                write(*, '(A)') '(Note: Precision changes will be implemented in Phase 2)'
            else
                write(*, '(A)') 'Invalid precision specification. Use: precision <digits>'
            end if
        end if
    end subroutine handle_precision_command
    
    !> Show system information
    subroutine show_system_info()
        type(precision_info_t) :: info
        
        write(*, '(A)') 'FORTBITE System Information:'
        write(*, '(A)') '  Version: 1.0.0'
        write(*, '(A)') '  Language: Modern Fortran'
        write(*, '(A)') ''
        
        write(*, '(A)') 'Available Precisions:'
        
        info = get_precision_info(real32)
        write(*, '(A,I0,A)') '  Single:   ', precision(1.0_real32), ' digits'
        
        info = get_precision_info(real64)
        write(*, '(A,I0,A)') '  Double:   ', precision(1.0_real64), ' digits'
        
        ! Only show quad if available
        if (real128 > 0) then
            info = get_precision_info(real128)
            write(*, '(A,I0,A)') '  Quad:     ', precision(1.0_real128), ' digits'
        end if
        
        write(*, '(A)') ''
    end subroutine show_system_info
    
    !> Show current variables
    subroutine show_variables(variables)
        type(variable_t), pointer, intent(in) :: variables
        type(variable_t), pointer :: current
        
        current => variables
        
        if (.not. associated(current)) then
            write(*, '(A)') 'No variables defined.'
            return
        end if
        
        write(*, '(A)') 'Current variables:'
        do while (associated(current))
            write(*, '(A,A,A)', advance='no') '  ', current%name, ' = '
            call print_value(current%value)
            current => current%next
        end do
    end subroutine show_variables
    
    !> Clean up variable linked list
    subroutine cleanup_variables(variables)
        type(variable_t), pointer, intent(inout) :: variables
        type(variable_t), pointer :: current, next
        
        current => variables
        do while (associated(current))
            next => current%next
            deallocate(current)
            current => next
        end do
        nullify(variables)
    end subroutine cleanup_variables
    
end module fortbite_io_m