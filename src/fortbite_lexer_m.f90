!> Lexical analyzer module for FORTBITE
!>
!> Tokenizes mathematical expressions into a stream of tokens for parsing.
!> Handles numbers, identifiers, operators, and special syntax like :: and :=
module fortbite_lexer_m
    use fortbite_types_m, only: token_t, TOKEN_EOF, TOKEN_NUMBER, TOKEN_IDENTIFIER, &
                               TOKEN_OPERATOR, TOKEN_LPAREN, TOKEN_RPAREN, &
                               TOKEN_LBRACKET, TOKEN_RBRACKET, TOKEN_SEMICOLON, &
                               TOKEN_COMMA, TOKEN_ASSIGN, TOKEN_PRECISION
    use iso_fortran_env, only: real64
    implicit none
    private
    
    public :: tokenize, free_tokens, print_tokens
    
    ! Lexer state
    type :: lexer_state_t
        character(len=:), allocatable :: input
        integer :: position
        integer :: length
        character :: current_char
    end type lexer_state_t
    
contains

    !> Tokenize a mathematical expression
    function tokenize(expression) result(tokens)
        character(len=*), intent(in) :: expression
        type(token_t), allocatable :: tokens(:)
        
        type(lexer_state_t) :: lexer
        type(token_t) :: current_token
        integer :: token_count, capacity
        
        ! Initialize lexer state
        lexer%input = trim(adjustl(expression))
        lexer%length = len_trim(lexer%input)
        lexer%position = 1
        
        if (lexer%length == 0) then
            allocate(tokens(1))
            tokens(1)%token_type = TOKEN_EOF
            return
        end if
        
        lexer%current_char = lexer%input(1:1)
        
        ! Dynamic token array
        capacity = max(16, lexer%length / 2)
        allocate(tokens(capacity))
        token_count = 0
        
        ! Main tokenization loop
        do
            call skip_whitespace(lexer)
            if (lexer%current_char == char(0) .or. lexer%position > lexer%length) exit
            
            current_token = next_token(lexer)
            
            ! Resize array if needed
            if (token_count >= capacity) then
                capacity = capacity * 2
                call resize_token_array(tokens, capacity)
            end if
            
            token_count = token_count + 1
            tokens(token_count) = current_token
            
            if (current_token%token_type == TOKEN_EOF) exit
        end do
        
        ! Add EOF token if not already present
        if (token_count == 0 .or. tokens(token_count)%token_type /= TOKEN_EOF) then
            if (token_count >= capacity) then
                capacity = capacity + 1
                call resize_token_array(tokens, capacity)
            end if
            token_count = token_count + 1
            tokens(token_count)%token_type = TOKEN_EOF
            tokens(token_count)%position = lexer%position
        end if
        
        ! Trim array to actual size
        call resize_token_array(tokens, token_count)
    end function tokenize
    
    !> Get the next token from the input stream
    function next_token(lexer) result(token)
        type(lexer_state_t), intent(inout) :: lexer
        type(token_t) :: token
        
        call skip_whitespace(lexer)
        
        if (lexer%position > lexer%length) then
            token%token_type = TOKEN_EOF
            token%position = lexer%position
            return
        end if
        
        token%position = lexer%position
        
        select case (lexer%current_char)
        case ('0':'9', '.')
            token = read_number(lexer)
        case ('a':'z', 'A':'Z', '_')
            token = read_identifier(lexer)
        case ('(')
            token%token_type = TOKEN_LPAREN
            token%text = '('
            call advance(lexer)
        case (')')
            token%token_type = TOKEN_RPAREN
            token%text = ')'
            call advance(lexer)
        case ('[')
            token%token_type = TOKEN_LBRACKET
            token%text = '['
            call advance(lexer)
        case (']')
            token%token_type = TOKEN_RBRACKET
            token%text = ']'
            call advance(lexer)
        case (';')
            token%token_type = TOKEN_SEMICOLON
            token%text = ';'
            call advance(lexer)
        case (',')
            token%token_type = TOKEN_COMMA
            token%text = ','
            call advance(lexer)
        case (':')
            token = read_colon_operator(lexer)
        case ('+', '-', '/', '^', '=', '<', '>', '!')
            token = read_operator(lexer)
        case ('*')
            token = read_power_or_multiply(lexer)
        case default
            ! Unknown character - create error token
            token%token_type = TOKEN_EOF  ! We'll use this as error for now
            token%text = lexer%current_char
            call advance(lexer)
        end select
    end function next_token
    
    !> Read a numeric token (integer or real)
    function read_number(lexer) result(token)
        type(lexer_state_t), intent(inout) :: lexer
        type(token_t) :: token
        
        integer :: start_pos
        logical :: has_dot, has_exp
        
        start_pos = lexer%position
        has_dot = .false.
        has_exp = .false.
        
        ! Handle leading decimal point
        if (lexer%current_char == '.') then
            has_dot = .true.
            call advance(lexer)
            if (.not. is_digit(lexer%current_char)) then
                ! Just a dot, not a number
                token%token_type = TOKEN_OPERATOR
                token%text = '.'
                return
            end if
        end if
        
        ! Read digits
        do while (is_digit(lexer%current_char))
            call advance(lexer)
        end do
        
        ! Handle decimal point
        if (lexer%current_char == '.' .and. .not. has_dot) then
            has_dot = .true.
            call advance(lexer)
            do while (is_digit(lexer%current_char))
                call advance(lexer)
            end do
        end if
        
        ! Handle scientific notation
        if ((lexer%current_char == 'e' .or. lexer%current_char == 'E') .and. .not. has_exp) then
            has_exp = .true.
            call advance(lexer)
            if (lexer%current_char == '+' .or. lexer%current_char == '-') then
                call advance(lexer)
            end if
            do while (is_digit(lexer%current_char))
                call advance(lexer)
            end do
        end if
        
        token%token_type = TOKEN_NUMBER
        token%text = lexer%input(start_pos:lexer%position-1)
    end function read_number
    
    !> Read an identifier or keyword
    function read_identifier(lexer) result(token)
        type(lexer_state_t), intent(inout) :: lexer
        type(token_t) :: token
        
        integer :: start_pos
        
        start_pos = lexer%position
        
        ! Read alphanumeric characters and underscores
        do while (is_alphanumeric(lexer%current_char))
            call advance(lexer)
        end do
        
        token%token_type = TOKEN_IDENTIFIER
        token%text = lexer%input(start_pos:lexer%position-1)
    end function read_identifier
    
    !> Read colon-based operators (: := ::)
    function read_colon_operator(lexer) result(token)
        type(lexer_state_t), intent(inout) :: lexer
        type(token_t) :: token
        
        integer :: start_pos
        
        start_pos = lexer%position
        call advance(lexer)  ! Skip first ':'
        
        if (lexer%current_char == '=') then
            ! Assignment operator :=
            call advance(lexer)
            token%token_type = TOKEN_ASSIGN
            token%text = ':='
        else if (lexer%current_char == ':') then
            ! Precision operator ::
            call advance(lexer)
            token%token_type = TOKEN_PRECISION
            token%text = '::'
        else
            ! Just a colon (should be an error in math expressions)
            token%token_type = TOKEN_OPERATOR
            token%text = ':'
        end if
    end function read_colon_operator
    
    !> Read mathematical operators
    function read_operator(lexer) result(token)
        type(lexer_state_t), intent(inout) :: lexer
        type(token_t) :: token
        
        integer :: start_pos
        
        start_pos = lexer%position
        
        select case (lexer%current_char)
        case ('+', '-', '/', '^')
            token%text = lexer%current_char
            call advance(lexer)
        case ('*')
            call advance(lexer)
            if (lexer%current_char == '*') then
                ! Power operator **
                call advance(lexer)
                token%text = '**'
            else
                token%text = '*'
            end if
        case ('=')
            call advance(lexer)
            if (lexer%current_char == '=') then
                call advance(lexer)
                token%text = '=='
            else
                token%text = '='
            end if
        case ('<')
            call advance(lexer)
            if (lexer%current_char == '=') then
                call advance(lexer)
                token%text = '<='
            else
                token%text = '<'
            end if
        case ('>')
            call advance(lexer)
            if (lexer%current_char == '=') then
                call advance(lexer)
                token%text = '>='
            else
                token%text = '>'
            end if
        case ('!')
            call advance(lexer)
            if (lexer%current_char == '=') then
                call advance(lexer)
                token%text = '!='
            else
                token%text = '!'
            end if
        case default
            token%text = lexer%current_char
            call advance(lexer)
        end select
        
        token%token_type = TOKEN_OPERATOR
    end function read_operator
    
    !> Handle * vs ** power operator
    function read_power_or_multiply(lexer) result(token)
        type(lexer_state_t), intent(inout) :: lexer
        type(token_t) :: token
        
        call advance(lexer)  ! Skip first '*'
        
        if (lexer%current_char == '*') then
            ! Power operator **
            call advance(lexer)
            token%token_type = TOKEN_OPERATOR
            token%text = '**'
        else
            ! Multiply operator *
            token%token_type = TOKEN_OPERATOR
            token%text = '*'
        end if
    end function read_power_or_multiply
    
    !> Skip whitespace characters
    subroutine skip_whitespace(lexer)
        type(lexer_state_t), intent(inout) :: lexer
        
        do while (is_whitespace(lexer%current_char) .and. lexer%position <= lexer%length)
            call advance(lexer)
        end do
    end subroutine skip_whitespace
    
    !> Advance to the next character
    subroutine advance(lexer)
        type(lexer_state_t), intent(inout) :: lexer
        
        lexer%position = lexer%position + 1
        if (lexer%position <= lexer%length) then
            lexer%current_char = lexer%input(lexer%position:lexer%position)
        else
            lexer%current_char = char(0)  ! EOF marker
        end if
    end subroutine advance
    
    !> Check if character is a digit
    logical function is_digit(c)
        character, intent(in) :: c
        is_digit = (c >= '0' .and. c <= '9')
    end function is_digit
    
    !> Check if character is alphabetic
    logical function is_alpha(c)
        character, intent(in) :: c
        is_alpha = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z') .or. c == '_'
    end function is_alpha
    
    !> Check if character is alphanumeric
    logical function is_alphanumeric(c)
        character, intent(in) :: c
        is_alphanumeric = is_alpha(c) .or. is_digit(c)
    end function is_alphanumeric
    
    !> Check if character is whitespace
    logical function is_whitespace(c)
        character, intent(in) :: c
        is_whitespace = (c == ' ' .or. c == char(9) .or. c == char(10) .or. c == char(13))
    end function is_whitespace
    
    !> Resize token array
    subroutine resize_token_array(tokens, new_size)
        type(token_t), allocatable, intent(inout) :: tokens(:)
        integer, intent(in) :: new_size
        
        type(token_t), allocatable :: temp(:)
        integer :: old_size
        
        if (.not. allocated(tokens)) then
            allocate(tokens(new_size))
            return
        end if
        
        old_size = size(tokens)
        if (new_size == old_size) return
        
        allocate(temp(min(old_size, new_size)))
        temp = tokens(1:min(old_size, new_size))
        
        deallocate(tokens)
        allocate(tokens(new_size))
        
        if (new_size >= old_size) then
            tokens(1:old_size) = temp
        else
            tokens = temp(1:new_size)
        end if
        
        deallocate(temp)
    end subroutine resize_token_array
    
    !> Free token array memory
    subroutine free_tokens(tokens)
        type(token_t), allocatable, intent(inout) :: tokens(:)
        
        if (allocated(tokens)) deallocate(tokens)
    end subroutine free_tokens
    
    !> Print tokens for debugging
    subroutine print_tokens(tokens)
        type(token_t), intent(in) :: tokens(:)
        integer :: i
        
        write(*, '(A)') 'Tokens:'
        do i = 1, size(tokens)
            if (tokens(i)%token_type == TOKEN_EOF) then
                write(*, '(A,I0,A)') '  [', i, '] EOF'
                exit
            end if
            write(*, '(A,I0,A,A,A,A,A,I0)') '  [', i, '] ', &
                get_token_type_name(tokens(i)%token_type), ' "', &
                tokens(i)%text, '" @', tokens(i)%position
        end do
    end subroutine print_tokens
    
    !> Get human-readable token type name
    function get_token_type_name(token_type) result(name)
        integer, intent(in) :: token_type
        character(len=12) :: name
        
        select case (token_type)
        case (TOKEN_EOF)
            name = 'EOF'
        case (TOKEN_NUMBER)
            name = 'NUMBER'
        case (TOKEN_IDENTIFIER)
            name = 'IDENTIFIER'
        case (TOKEN_OPERATOR)
            name = 'OPERATOR'
        case (TOKEN_LPAREN)
            name = 'LPAREN'
        case (TOKEN_RPAREN)
            name = 'RPAREN'
        case (TOKEN_LBRACKET)
            name = 'LBRACKET'
        case (TOKEN_RBRACKET)
            name = 'RBRACKET'
        case (TOKEN_SEMICOLON)
            name = 'SEMICOLON'
        case (TOKEN_COMMA)
            name = 'COMMA'
        case (TOKEN_ASSIGN)
            name = 'ASSIGN'
        case (TOKEN_PRECISION)
            name = 'PRECISION'
        case default
            name = 'UNKNOWN'
        end select
    end function get_token_type_name
    
end module fortbite_lexer_m