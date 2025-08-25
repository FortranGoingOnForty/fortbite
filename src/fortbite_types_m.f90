!> Core data types module for FORTBITE
!>
!> Defines the fundamental data types used throughout FORTBITE:
!> scalars, complex numbers, matrices, and variables.
module fortbite_types_m
    use fortbite_precision_m, only: dp, sp, qp
    use iso_fortran_env, only: real64
    use iso_fortran_env, only: int32, int64
    implicit none
    private
    
    ! Public types
    public :: value_t, variable_t, token_t
    public :: value_type_enum, token_type_enum
    public :: VALUE_UNDEFINED, VALUE_SCALAR, VALUE_COMPLEX, VALUE_MATRIX, VALUE_UNIT
    public :: TOKEN_EOF, TOKEN_NUMBER, TOKEN_IDENTIFIER, TOKEN_OPERATOR
    public :: TOKEN_LPAREN, TOKEN_RPAREN, TOKEN_LBRACKET, TOKEN_RBRACKET
    public :: TOKEN_SEMICOLON, TOKEN_COMMA, TOKEN_ASSIGN, TOKEN_PRECISION
    public :: assignment(=)
    
    ! Public procedures
    public :: create_scalar, create_complex, create_matrix
    public :: create_zeros_matrix, create_ones_matrix, create_eye_matrix, create_diag_matrix
    public :: create_complex_matrix
    public :: destroy_value, copy_value, print_value
    public :: is_zero, is_real, get_real_part, get_imag_part
    
    !> Enumeration for value types
    enum, bind(c)
        enumerator :: VALUE_UNDEFINED = 0
        enumerator :: VALUE_SCALAR = 1
        enumerator :: VALUE_COMPLEX = 2  
        enumerator :: VALUE_MATRIX = 3
        enumerator :: VALUE_UNIT = 4
    end enum
    integer, parameter :: value_type_enum = kind(VALUE_UNDEFINED)
    
    !> Enumeration for token types
    enum, bind(c)
        enumerator :: TOKEN_EOF = 0
        enumerator :: TOKEN_NUMBER = 1
        enumerator :: TOKEN_IDENTIFIER = 2
        enumerator :: TOKEN_OPERATOR = 3
        enumerator :: TOKEN_LPAREN = 4
        enumerator :: TOKEN_RPAREN = 5
        enumerator :: TOKEN_LBRACKET = 6
        enumerator :: TOKEN_RBRACKET = 7
        enumerator :: TOKEN_SEMICOLON = 8
        enumerator :: TOKEN_COMMA = 9
        enumerator :: TOKEN_ASSIGN = 10
        enumerator :: TOKEN_PRECISION = 11
    end enum
    integer, parameter :: token_type_enum = kind(TOKEN_EOF)
    
    !> Core value type - can represent scalars, complex numbers, matrices
    type :: value_t
        integer(value_type_enum) :: value_type = VALUE_UNDEFINED
        integer :: precision_kind = real64
        
        ! Union-like storage for different value types
        real(real64) :: scalar_val = 0.0_real64
        complex(real64) :: complex_val = (0.0_real64, 0.0_real64)
        real(real64), allocatable :: matrix_val(:,:)
        complex(real64), allocatable :: complex_matrix_val(:,:)
        
        ! Matrix dimensions
        integer :: rows = 0
        integer :: cols = 0
        logical :: is_complex_matrix = .false.
    end type value_t
    
    !> Variable storage type
    type :: variable_t
        character(len=:), allocatable :: name
        type(value_t) :: value
        type(variable_t), pointer :: next => null()
    end type variable_t
    
    !> Token for lexical analysis
    type :: token_t
        integer(token_type_enum) :: token_type = TOKEN_EOF
        character(len=:), allocatable :: text
        integer :: position = 0
    end type token_t
    
    ! Generic interface for assignment
    interface assignment(=)
        module procedure assign_value
    end interface
    
contains

    !> Create a scalar value
    function create_scalar(val, precision_kind) result(value)
        real(real64), intent(in) :: val
        integer, intent(in), optional :: precision_kind
        type(value_t) :: value
        
        value%value_type = VALUE_SCALAR
        value%precision_kind = real64
        if (present(precision_kind)) value%precision_kind = precision_kind
        value%scalar_val = val
    end function create_scalar
    
    !> Create a complex value
    function create_complex(real_part, imag_part, precision_kind) result(value)
        real(real64), intent(in) :: real_part, imag_part
        integer, intent(in), optional :: precision_kind
        type(value_t) :: value
        
        value%value_type = VALUE_COMPLEX
        value%precision_kind = real64
        if (present(precision_kind)) value%precision_kind = precision_kind
        value%complex_val = cmplx(real_part, imag_part, kind=real64)
    end function create_complex
    
    !> Create a matrix value
    function create_matrix(matrix_data, precision_kind) result(value)
        real(real64), intent(in) :: matrix_data(:,:)
        integer, intent(in), optional :: precision_kind
        type(value_t) :: value
        
        value%value_type = VALUE_MATRIX
        value%precision_kind = real64
        if (present(precision_kind)) value%precision_kind = precision_kind
        
        value%rows = size(matrix_data, 1)
        value%cols = size(matrix_data, 2)
        value%is_complex_matrix = .false.
        
        allocate(value%matrix_val(value%rows, value%cols))
        value%matrix_val = matrix_data
    end function create_matrix
    
    !> Create a complex matrix value
    function create_complex_matrix(matrix_data, precision_kind) result(value)
        complex(real64), intent(in) :: matrix_data(:,:)
        integer, intent(in), optional :: precision_kind
        type(value_t) :: value
        
        value%value_type = VALUE_MATRIX
        value%precision_kind = real64
        if (present(precision_kind)) value%precision_kind = precision_kind
        
        value%rows = size(matrix_data, 1)
        value%cols = size(matrix_data, 2)
        value%is_complex_matrix = .true.
        
        allocate(value%complex_matrix_val(value%rows, value%cols))
        value%complex_matrix_val = matrix_data
    end function create_complex_matrix
    
    !> Create a zeros matrix
    function create_zeros_matrix(rows, cols, precision_kind) result(value)
        integer, intent(in) :: rows, cols
        integer, intent(in), optional :: precision_kind
        type(value_t) :: value
        
        value%value_type = VALUE_MATRIX
        value%precision_kind = real64
        if (present(precision_kind)) value%precision_kind = precision_kind
        
        value%rows = rows
        value%cols = cols
        value%is_complex_matrix = .false.
        
        allocate(value%matrix_val(rows, cols))
        value%matrix_val = 0.0_real64
    end function create_zeros_matrix
    
    !> Create a ones matrix
    function create_ones_matrix(rows, cols, precision_kind) result(value)
        integer, intent(in) :: rows, cols
        integer, intent(in), optional :: precision_kind
        type(value_t) :: value
        
        value%value_type = VALUE_MATRIX
        value%precision_kind = real64
        if (present(precision_kind)) value%precision_kind = precision_kind
        
        value%rows = rows
        value%cols = cols
        value%is_complex_matrix = .false.
        
        allocate(value%matrix_val(rows, cols))
        value%matrix_val = 1.0_real64
    end function create_ones_matrix
    
    !> Create an identity matrix
    function create_eye_matrix(size, precision_kind) result(value)
        integer, intent(in) :: size
        integer, intent(in), optional :: precision_kind
        type(value_t) :: value
        integer :: i
        
        value%value_type = VALUE_MATRIX
        value%precision_kind = real64
        if (present(precision_kind)) value%precision_kind = precision_kind
        
        value%rows = size
        value%cols = size
        value%is_complex_matrix = .false.
        
        allocate(value%matrix_val(size, size))
        value%matrix_val = 0.0_real64
        
        ! Set diagonal elements to 1
        do i = 1, size
            value%matrix_val(i, i) = 1.0_real64
        end do
    end function create_eye_matrix
    
    !> Create a diagonal matrix from a vector
    function create_diag_matrix(diagonal_elements, precision_kind) result(value)
        real(real64), intent(in) :: diagonal_elements(:)
        integer, intent(in), optional :: precision_kind
        type(value_t) :: value
        integer :: i, n
        
        n = size(diagonal_elements)
        
        value%value_type = VALUE_MATRIX
        value%precision_kind = real64
        if (present(precision_kind)) value%precision_kind = precision_kind
        
        value%rows = n
        value%cols = n
        value%is_complex_matrix = .false.
        
        allocate(value%matrix_val(n, n))
        value%matrix_val = 0.0_real64
        
        ! Set diagonal elements
        do i = 1, n
            value%matrix_val(i, i) = diagonal_elements(i)
        end do
    end function create_diag_matrix
    
    !> Destroy/deallocate a value
    subroutine destroy_value(value)
        type(value_t), intent(inout) :: value
        
        if (allocated(value%matrix_val)) deallocate(value%matrix_val)
        if (allocated(value%complex_matrix_val)) deallocate(value%complex_matrix_val)
        
        value%value_type = VALUE_UNDEFINED
        value%rows = 0
        value%cols = 0
    end subroutine destroy_value
    
    !> Copy a value (deep copy)
    function copy_value(source) result(dest)
        type(value_t), intent(in) :: source
        type(value_t) :: dest
        
        dest%value_type = source%value_type
        dest%precision_kind = source%precision_kind
        dest%rows = source%rows
        dest%cols = source%cols
        dest%is_complex_matrix = source%is_complex_matrix
        
        select case (source%value_type)
        case (VALUE_SCALAR)
            dest%scalar_val = source%scalar_val
        case (VALUE_COMPLEX)
            dest%complex_val = source%complex_val
        case (VALUE_MATRIX)
            if (allocated(source%matrix_val)) then
                allocate(dest%matrix_val(dest%rows, dest%cols))
                dest%matrix_val = source%matrix_val
            end if
            if (allocated(source%complex_matrix_val)) then
                allocate(dest%complex_matrix_val(dest%rows, dest%cols))
                dest%complex_matrix_val = source%complex_matrix_val
            end if
        end select
    end function copy_value
    
    !> Print a value to standard output
    subroutine print_value(value)
        type(value_t), intent(in) :: value
        
        select case (value%value_type)
        case (VALUE_SCALAR)
            write(*, '(G0)') value%scalar_val
        case (VALUE_COMPLEX)
            if (aimag(value%complex_val) >= 0.0_real64) then
                write(*, '(G0,"+",G0,"i")') real(value%complex_val), aimag(value%complex_val)
            else
                write(*, '(G0,G0,"i")') real(value%complex_val), aimag(value%complex_val)
            end if
        case (VALUE_MATRIX)
            call print_matrix(value)
        case default
            write(*, '(A)') 'Undefined value'
        end select
    end subroutine print_value
    
    !> Print a matrix value
    subroutine print_matrix(value)
        type(value_t), intent(in) :: value
        integer :: i, j
        
        write(*, '(A,I0,"x",I0,A)') '[', value%rows, value%cols, ' matrix]'
        
        if (value%rows <= 10 .and. value%cols <= 10) then
            do i = 1, value%rows
                write(*, '(A)', advance='no') '  '
                do j = 1, value%cols
                    if (value%is_complex_matrix) then
                        write(*, '(SP,G0,G0,"i")', advance='no') &
                            real(value%complex_matrix_val(i,j)), &
                            aimag(value%complex_matrix_val(i,j))
                    else
                        write(*, '(G0)', advance='no') value%matrix_val(i,j)
                    end if
                    if (j < value%cols) write(*, '(A)', advance='no') '  '
                end do
                write(*, *)  ! New line
            end do
        end if
    end subroutine print_matrix
    
    !> Check if a value is zero
    logical function is_zero(value)
        type(value_t), intent(in) :: value
        
        select case (value%value_type)
        case (VALUE_SCALAR)
            is_zero = (abs(value%scalar_val) < epsilon(value%scalar_val))
        case (VALUE_COMPLEX)
            is_zero = (abs(value%complex_val) < epsilon(real(value%complex_val)))
        case default
            is_zero = .false.
        end select
    end function is_zero
    
    !> Check if a value is purely real
    logical function is_real(value)
        type(value_t), intent(in) :: value
        
        select case (value%value_type)
        case (VALUE_SCALAR)
            is_real = .true.
        case (VALUE_COMPLEX)
            is_real = (abs(aimag(value%complex_val)) < epsilon(real(value%complex_val)))
        case default
            is_real = .false.
        end select
    end function is_real
    
    !> Get real part of a value
    function get_real_part(value) result(real_val)
        type(value_t), intent(in) :: value
        real(real64) :: real_val
        
        select case (value%value_type)
        case (VALUE_SCALAR)
            real_val = value%scalar_val
        case (VALUE_COMPLEX)
            real_val = real(value%complex_val)
        case default
            real_val = 0.0_real64
        end select
    end function get_real_part
    
    !> Get imaginary part of a value
    function get_imag_part(value) result(imag_val)
        type(value_t), intent(in) :: value
        real(real64) :: imag_val
        
        select case (value%value_type)
        case (VALUE_SCALAR)
            imag_val = 0.0_real64
        case (VALUE_COMPLEX)
            imag_val = aimag(value%complex_val)
        case default
            imag_val = 0.0_real64
        end select
    end function get_imag_part
    
    !> Assignment operator for values
    subroutine assign_value(lhs, rhs)
        type(value_t), intent(out) :: lhs
        type(value_t), intent(in) :: rhs
        
        ! Simple assignment - let Fortran handle the copying
        lhs%value_type = rhs%value_type
        lhs%precision_kind = rhs%precision_kind
        lhs%scalar_val = rhs%scalar_val
        lhs%complex_val = rhs%complex_val
        lhs%rows = rhs%rows
        lhs%cols = rhs%cols
        lhs%is_complex_matrix = rhs%is_complex_matrix
        
        ! For now, we'll handle matrix copying manually when needed
    end subroutine assign_value
    
end module fortbite_types_m