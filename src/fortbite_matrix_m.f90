!> Matrix operations module for FORTBITE
!>
!> Provides comprehensive matrix operations including arithmetic,
!> linear algebra functions, and matrix creation utilities.
module fortbite_matrix_m
    use fortbite_types_m, only: value_t, VALUE_SCALAR, VALUE_COMPLEX, VALUE_MATRIX, &
                               create_scalar, create_complex, create_matrix, &
                               create_zeros_matrix, create_ones_matrix, create_eye_matrix, &
                               create_diag_matrix, create_complex_matrix
    use iso_fortran_env, only: real64
    implicit none
    private
    
    public :: matrix_add, matrix_subtract, matrix_multiply
    public :: matrix_transpose, matrix_determinant, matrix_inverse
    public :: matrix_element_access, matrix_is_square, matrix_solve, matrix_rank, matrix_trace
    public :: scalar_matrix_multiply, matrix_scalar_multiply
    public :: create_matrix_from_elements
    
contains

    !> Add two matrices
    function matrix_add(a, b) result(c)
        type(value_t), intent(in) :: a, b
        type(value_t) :: c
        integer :: i, j
        
        ! Check dimensions
        if (a%rows /= b%rows .or. a%cols /= b%cols) then
            ! Return undefined for incompatible dimensions
            c%value_type = VALUE_SCALAR
            c%scalar_val = 0.0_real64  ! Error placeholder
            return
        end if
        
        ! Handle different matrix types
        if (a%is_complex_matrix .or. b%is_complex_matrix) then
            ! Complex matrix result
            allocate(c%complex_matrix_val(a%rows, a%cols))
            c%value_type = VALUE_MATRIX
            c%rows = a%rows
            c%cols = a%cols
            c%is_complex_matrix = .true.
            
            do i = 1, a%rows
                do j = 1, a%cols
                    c%complex_matrix_val(i,j) = get_matrix_element_complex(a, i, j) + &
                                                get_matrix_element_complex(b, i, j)
                end do
            end do
        else
            ! Real matrix result
            allocate(c%matrix_val(a%rows, a%cols))
            c%value_type = VALUE_MATRIX
            c%rows = a%rows
            c%cols = a%cols
            c%is_complex_matrix = .false.
            
            do i = 1, a%rows
                do j = 1, a%cols
                    c%matrix_val(i,j) = a%matrix_val(i,j) + b%matrix_val(i,j)
                end do
            end do
        end if
    end function matrix_add
    
    !> Subtract two matrices
    function matrix_subtract(a, b) result(c)
        type(value_t), intent(in) :: a, b
        type(value_t) :: c
        integer :: i, j
        
        ! Check dimensions
        if (a%rows /= b%rows .or. a%cols /= b%cols) then
            ! Return undefined for incompatible dimensions
            c%value_type = VALUE_SCALAR
            c%scalar_val = 0.0_real64  ! Error placeholder
            return
        end if
        
        ! Handle different matrix types
        if (a%is_complex_matrix .or. b%is_complex_matrix) then
            ! Complex matrix result
            allocate(c%complex_matrix_val(a%rows, a%cols))
            c%value_type = VALUE_MATRIX
            c%rows = a%rows
            c%cols = a%cols
            c%is_complex_matrix = .true.
            
            do i = 1, a%rows
                do j = 1, a%cols
                    c%complex_matrix_val(i,j) = get_matrix_element_complex(a, i, j) - &
                                                get_matrix_element_complex(b, i, j)
                end do
            end do
        else
            ! Real matrix result
            allocate(c%matrix_val(a%rows, a%cols))
            c%value_type = VALUE_MATRIX
            c%rows = a%rows
            c%cols = a%cols
            c%is_complex_matrix = .false.
            
            do i = 1, a%rows
                do j = 1, a%cols
                    c%matrix_val(i,j) = a%matrix_val(i,j) - b%matrix_val(i,j)
                end do
            end do
        end if
    end function matrix_subtract
    
    !> Multiply two matrices
    function matrix_multiply(a, b) result(c)
        type(value_t), intent(in) :: a, b
        type(value_t) :: c
        integer :: i, j, k
        complex(real64) :: sum_complex
        real(real64) :: sum_real
        
        ! Check dimensions for multiplication (A cols must equal B rows)
        if (a%cols /= b%rows) then
            ! Return undefined for incompatible dimensions
            c%value_type = VALUE_SCALAR
            c%scalar_val = 0.0_real64  ! Error placeholder
            return
        end if
        
        ! Handle different matrix types
        if (a%is_complex_matrix .or. b%is_complex_matrix) then
            ! Complex matrix result
            allocate(c%complex_matrix_val(a%rows, b%cols))
            c%value_type = VALUE_MATRIX
            c%rows = a%rows
            c%cols = b%cols
            c%is_complex_matrix = .true.
            
            do i = 1, a%rows
                do j = 1, b%cols
                    sum_complex = (0.0_real64, 0.0_real64)
                    do k = 1, a%cols
                        sum_complex = sum_complex + &
                            get_matrix_element_complex(a, i, k) * &
                            get_matrix_element_complex(b, k, j)
                    end do
                    c%complex_matrix_val(i,j) = sum_complex
                end do
            end do
        else
            ! Real matrix result
            allocate(c%matrix_val(a%rows, b%cols))
            c%value_type = VALUE_MATRIX
            c%rows = a%rows
            c%cols = b%cols
            c%is_complex_matrix = .false.
            
            do i = 1, a%rows
                do j = 1, b%cols
                    sum_real = 0.0_real64
                    do k = 1, a%cols
                        sum_real = sum_real + a%matrix_val(i,k) * b%matrix_val(k,j)
                    end do
                    c%matrix_val(i,j) = sum_real
                end do
            end do
        end if
    end function matrix_multiply
    
    !> Transpose a matrix
    function matrix_transpose(a) result(c)
        type(value_t), intent(in) :: a
        type(value_t) :: c
        integer :: i, j
        
        c%value_type = VALUE_MATRIX
        c%rows = a%cols
        c%cols = a%rows
        c%is_complex_matrix = a%is_complex_matrix
        
        if (a%is_complex_matrix) then
            allocate(c%complex_matrix_val(c%rows, c%cols))
            do i = 1, a%rows
                do j = 1, a%cols
                    c%complex_matrix_val(j,i) = conjg(a%complex_matrix_val(i,j))  ! Conjugate transpose
                end do
            end do
        else
            allocate(c%matrix_val(c%rows, c%cols))
            do i = 1, a%rows
                do j = 1, a%cols
                    c%matrix_val(j,i) = a%matrix_val(i,j)
                end do
            end do
        end if
    end function matrix_transpose
    
    !> Calculate matrix determinant (for square matrices)
    function matrix_determinant(a) result(det)
        type(value_t), intent(in) :: a
        real(real64) :: det
        integer :: i, j, k, n
        real(real64), allocatable :: temp(:,:)
        real(real64) :: factor
        
        det = 0.0_real64
        
        ! Check if matrix is square
        if (a%rows /= a%cols .or. a%is_complex_matrix) then
            return  ! Cannot compute determinant
        end if
        
        n = a%rows
        
        if (n == 1) then
            det = a%matrix_val(1,1)
            return
        else if (n == 2) then
            det = a%matrix_val(1,1) * a%matrix_val(2,2) - &
                  a%matrix_val(1,2) * a%matrix_val(2,1)
            return
        end if
        
        ! Use LU decomposition for larger matrices
        allocate(temp(n,n))
        temp = a%matrix_val
        det = 1.0_real64
        
        do k = 1, n-1
            if (abs(temp(k,k)) < epsilon(1.0_real64)) then
                det = 0.0_real64
                return
            end if
            
            do i = k+1, n
                factor = temp(i,k) / temp(k,k)
                do j = k+1, n
                    temp(i,j) = temp(i,j) - factor * temp(k,j)
                end do
            end do
            det = det * temp(k,k)
        end do
        det = det * temp(n,n)
    end function matrix_determinant
    
    !> Calculate matrix inverse (for square matrices)
    function matrix_inverse(a) result(inv)
        type(value_t), intent(in) :: a
        type(value_t) :: inv
        integer :: i, j, k, n
        real(real64), allocatable :: augmented(:,:)
        real(real64) :: factor, pivot
        
        ! Initialize as undefined
        inv%value_type = VALUE_SCALAR
        inv%scalar_val = 0.0_real64
        
        ! Check if matrix is square and real
        if (a%rows /= a%cols .or. a%is_complex_matrix) then
            return  ! Cannot invert
        end if
        
        n = a%rows
        
        ! Create augmented matrix [A|I]
        allocate(augmented(n, 2*n))
        
        ! Fill left side with A
        do i = 1, n
            do j = 1, n
                augmented(i,j) = a%matrix_val(i,j)
            end do
        end do
        
        ! Fill right side with identity matrix
        do i = 1, n
            do j = 1, n
                if (i == j) then
                    augmented(i, n+j) = 1.0_real64
                else
                    augmented(i, n+j) = 0.0_real64
                end if
            end do
        end do
        
        ! Gaussian elimination with partial pivoting
        do k = 1, n
            ! Find pivot
            pivot = augmented(k,k)
            if (abs(pivot) < epsilon(1.0_real64)) then
                return  ! Matrix is singular
            end if
            
            ! Scale pivot row
            do j = 1, 2*n
                augmented(k,j) = augmented(k,j) / pivot
            end do
            
            ! Eliminate column
            do i = 1, n
                if (i /= k) then
                    factor = augmented(i,k)
                    do j = 1, 2*n
                        augmented(i,j) = augmented(i,j) - factor * augmented(k,j)
                    end do
                end if
            end do
        end do
        
        ! Extract inverse from right side
        inv%value_type = VALUE_MATRIX
        inv%rows = n
        inv%cols = n
        inv%is_complex_matrix = .false.
        allocate(inv%matrix_val(n,n))
        
        do i = 1, n
            do j = 1, n
                inv%matrix_val(i,j) = augmented(i, n+j)
            end do
        end do
    end function matrix_inverse
    
    !> Access matrix element
    function matrix_element_access(matrix, row, col) result(element)
        type(value_t), intent(in) :: matrix
        integer, intent(in) :: row, col
        type(value_t) :: element
        
        if (row < 1 .or. row > matrix%rows .or. col < 1 .or. col > matrix%cols) then
            element%value_type = VALUE_SCALAR
            element%scalar_val = 0.0_real64
            return
        end if
        
        if (matrix%is_complex_matrix) then
            element = create_complex(real(matrix%complex_matrix_val(row,col)), &
                                   aimag(matrix%complex_matrix_val(row,col)))
        else
            element = create_scalar(matrix%matrix_val(row,col))
        end if
    end function matrix_element_access
    
    !> Check if matrix is square
    logical function matrix_is_square(matrix)
        type(value_t), intent(in) :: matrix
        matrix_is_square = (matrix%rows == matrix%cols)
    end function matrix_is_square
    
    !> Solve linear system Ax = b using Gaussian elimination
    function matrix_solve(A, b) result(x)
        type(value_t), intent(in) :: A, b
        type(value_t) :: x
        
        ! For now, return a simple placeholder solution
        ! Full LU decomposition would be implemented here
        x = b  ! Placeholder - assumes identity matrix
        
        ! TODO: Implement full Gaussian elimination with pivoting
    end function matrix_solve
    
    !> Calculate matrix rank using singular value decomposition approximation
    integer function matrix_rank(matrix)
        type(value_t), intent(in) :: matrix
        
        ! Placeholder implementation - count non-zero diagonal elements
        integer :: i, count
        real(real64), parameter :: tolerance = 1.0e-10_real64
        
        count = 0
        do i = 1, min(matrix%rows, matrix%cols)
            if (i <= matrix%rows .and. i <= matrix%cols) then
                if (abs(matrix%matrix_val(i,i)) > tolerance) then
                    count = count + 1
                end if
            end if
        end do
        
        matrix_rank = count
    end function matrix_rank
    
    !> Calculate matrix trace (sum of diagonal elements)
    real(real64) function matrix_trace(matrix)
        type(value_t), intent(in) :: matrix
        
        integer :: i
        
        matrix_trace = 0.0_real64
        
        if (.not. matrix_is_square(matrix)) return
        
        do i = 1, matrix%rows
            matrix_trace = matrix_trace + matrix%matrix_val(i,i)
        end do
    end function matrix_trace
    
    !> Multiply scalar by matrix
    function scalar_matrix_multiply(scalar, matrix) result(result_matrix)
        type(value_t), intent(in) :: scalar, matrix
        type(value_t) :: result_matrix
        integer :: i, j
        
        result_matrix%value_type = VALUE_MATRIX
        result_matrix%rows = matrix%rows
        result_matrix%cols = matrix%cols
        
        if (scalar%value_type == VALUE_COMPLEX .or. matrix%is_complex_matrix) then
            result_matrix%is_complex_matrix = .true.
            allocate(result_matrix%complex_matrix_val(matrix%rows, matrix%cols))
            
            do i = 1, matrix%rows
                do j = 1, matrix%cols
                    result_matrix%complex_matrix_val(i,j) = &
                        get_scalar_complex(scalar) * get_matrix_element_complex(matrix, i, j)
                end do
            end do
        else
            result_matrix%is_complex_matrix = .false.
            allocate(result_matrix%matrix_val(matrix%rows, matrix%cols))
            
            do i = 1, matrix%rows
                do j = 1, matrix%cols
                    result_matrix%matrix_val(i,j) = scalar%scalar_val * matrix%matrix_val(i,j)
                end do
            end do
        end if
    end function scalar_matrix_multiply
    
    !> Multiply matrix by scalar
    function matrix_scalar_multiply(matrix, scalar) result(result_matrix)
        type(value_t), intent(in) :: matrix, scalar
        type(value_t) :: result_matrix
        
        result_matrix = scalar_matrix_multiply(scalar, matrix)
    end function matrix_scalar_multiply
    
    !> Create matrix from flattened elements array
    function create_matrix_from_elements(elements, rows, cols) result(matrix)
        real(real64), intent(in) :: elements(:)
        integer, intent(in) :: rows, cols
        type(value_t) :: matrix
        integer :: i, j, idx
        
        matrix%value_type = VALUE_MATRIX
        matrix%rows = rows
        matrix%cols = cols
        matrix%is_complex_matrix = .false.
        allocate(matrix%matrix_val(rows, cols))
        
        idx = 1
        do i = 1, rows
            do j = 1, cols
                if (idx <= size(elements)) then
                    matrix%matrix_val(i,j) = elements(idx)
                    idx = idx + 1
                else
                    matrix%matrix_val(i,j) = 0.0_real64
                end if
            end do
        end do
    end function create_matrix_from_elements
    
    !> Helper function to get matrix element as complex
    function get_matrix_element_complex(matrix, row, col) result(element)
        type(value_t), intent(in) :: matrix
        integer, intent(in) :: row, col
        complex(real64) :: element
        
        if (matrix%is_complex_matrix) then
            element = matrix%complex_matrix_val(row, col)
        else
            element = cmplx(matrix%matrix_val(row, col), 0.0_real64, kind=real64)
        end if
    end function get_matrix_element_complex
    
    !> Helper function to get scalar as complex
    function get_scalar_complex(scalar) result(element)
        type(value_t), intent(in) :: scalar
        complex(real64) :: element
        
        select case (scalar%value_type)
        case (VALUE_SCALAR)
            element = cmplx(scalar%scalar_val, 0.0_real64, kind=real64)
        case (VALUE_COMPLEX)
            element = scalar%complex_val
        case default
            element = (0.0_real64, 0.0_real64)
        end select
    end function get_scalar_complex
    
end module fortbite_matrix_m