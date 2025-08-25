!> Basic arithmetic operations module for FORTBITE
!>
!> Provides arithmetic operations for scalars, complex numbers, and matrices
!> with proper precision handling and type promotion.
module fortbite_arithmetic_m
    use iso_fortran_env, only: real64
    use fortbite_types_m, only: value_t, VALUE_SCALAR, VALUE_COMPLEX, VALUE_MATRIX, &
                               create_scalar, create_complex, is_real
    implicit none
    private
    
    public :: add_values, subtract_values, multiply_values, divide_values, power_values
    public :: negate_value, abs_value
    
contains

    !> Add two values
    function add_values(a, b) result(c)
        type(value_t), intent(in) :: a, b
        type(value_t) :: c
        
        ! Handle scalar + scalar
        if (a%value_type == VALUE_SCALAR .and. b%value_type == VALUE_SCALAR) then
            c = create_scalar(a%scalar_val + b%scalar_val)
            return
        end if
        
        ! Handle mixed scalar/complex operations
        if ((a%value_type == VALUE_SCALAR .and. b%value_type == VALUE_COMPLEX) .or. &
            (a%value_type == VALUE_COMPLEX .and. b%value_type == VALUE_SCALAR)) then
            
            if (a%value_type == VALUE_SCALAR) then
                c = create_complex(a%scalar_val + real(b%complex_val), aimag(b%complex_val))
            else
                c = create_complex(real(a%complex_val) + b%scalar_val, aimag(a%complex_val))
            end if
            return
        end if
        
        ! Handle complex + complex
        if (a%value_type == VALUE_COMPLEX .and. b%value_type == VALUE_COMPLEX) then
            c = create_complex(real(a%complex_val) + real(b%complex_val), &
                              aimag(a%complex_val) + aimag(b%complex_val))
            return
        end if
        
        ! TODO: Handle matrix operations in Phase 3
        ! For now, return undefined for unsupported operations
        c%value_type = 0  ! VALUE_UNDEFINED
    end function add_values
    
    !> Subtract two values
    function subtract_values(a, b) result(c)
        type(value_t), intent(in) :: a, b
        type(value_t) :: c
        
        ! Handle scalar - scalar
        if (a%value_type == VALUE_SCALAR .and. b%value_type == VALUE_SCALAR) then
            c = create_scalar(a%scalar_val - b%scalar_val)
            return
        end if
        
        ! Handle mixed scalar/complex operations
        if ((a%value_type == VALUE_SCALAR .and. b%value_type == VALUE_COMPLEX) .or. &
            (a%value_type == VALUE_COMPLEX .and. b%value_type == VALUE_SCALAR)) then
            
            if (a%value_type == VALUE_SCALAR) then
                c = create_complex(a%scalar_val - real(b%complex_val), -aimag(b%complex_val))
            else
                c = create_complex(real(a%complex_val) - b%scalar_val, aimag(a%complex_val))
            end if
            return
        end if
        
        ! Handle complex - complex
        if (a%value_type == VALUE_COMPLEX .and. b%value_type == VALUE_COMPLEX) then
            c = create_complex(real(a%complex_val) - real(b%complex_val), &
                              aimag(a%complex_val) - aimag(b%complex_val))
            return
        end if
        
        c%value_type = 0  ! VALUE_UNDEFINED
    end function subtract_values
    
    !> Multiply two values
    function multiply_values(a, b) result(c)
        type(value_t), intent(in) :: a, b
        type(value_t) :: c
        
        ! Handle scalar * scalar
        if (a%value_type == VALUE_SCALAR .and. b%value_type == VALUE_SCALAR) then
            c = create_scalar(a%scalar_val * b%scalar_val)
            return
        end if
        
        ! Handle mixed scalar/complex operations
        if ((a%value_type == VALUE_SCALAR .and. b%value_type == VALUE_COMPLEX) .or. &
            (a%value_type == VALUE_COMPLEX .and. b%value_type == VALUE_SCALAR)) then
            
            if (a%value_type == VALUE_SCALAR) then
                c = create_complex(a%scalar_val * real(b%complex_val), &
                                 a%scalar_val * aimag(b%complex_val))
            else
                c = create_complex(real(a%complex_val) * b%scalar_val, &
                                 aimag(a%complex_val) * b%scalar_val)
            end if
            return
        end if
        
        ! Handle complex * complex
        if (a%value_type == VALUE_COMPLEX .and. b%value_type == VALUE_COMPLEX) then
            c%complex_val = a%complex_val * b%complex_val
            c%value_type = VALUE_COMPLEX
            return
        end if
        
        c%value_type = 0  ! VALUE_UNDEFINED
    end function multiply_values
    
    !> Divide two values
    function divide_values(a, b) result(c)
        type(value_t), intent(in) :: a, b
        type(value_t) :: c
        
        ! Handle scalar / scalar
        if (a%value_type == VALUE_SCALAR .and. b%value_type == VALUE_SCALAR) then
            if (abs(b%scalar_val) < epsilon(b%scalar_val)) then
                write(*, '(A)') 'Error: Division by zero'
                c%value_type = 0  ! VALUE_UNDEFINED
                return
            end if
            c = create_scalar(a%scalar_val / b%scalar_val)
            return
        end if
        
        ! Handle mixed scalar/complex operations
        if ((a%value_type == VALUE_SCALAR .and. b%value_type == VALUE_COMPLEX) .or. &
            (a%value_type == VALUE_COMPLEX .and. b%value_type == VALUE_SCALAR)) then
            
            if (a%value_type == VALUE_SCALAR) then
                if (abs(b%complex_val) < epsilon(real(b%complex_val))) then
                    write(*, '(A)') 'Error: Division by zero'
                    c%value_type = 0
                    return
                end if
                c%complex_val = a%scalar_val / b%complex_val
                c%value_type = VALUE_COMPLEX
            else
                if (abs(b%scalar_val) < epsilon(b%scalar_val)) then
                    write(*, '(A)') 'Error: Division by zero'
                    c%value_type = 0
                    return
                end if
                c = create_complex(real(a%complex_val) / b%scalar_val, &
                                 aimag(a%complex_val) / b%scalar_val)
            end if
            return
        end if
        
        ! Handle complex / complex
        if (a%value_type == VALUE_COMPLEX .and. b%value_type == VALUE_COMPLEX) then
            if (abs(b%complex_val) < epsilon(real(b%complex_val))) then
                write(*, '(A)') 'Error: Division by zero'
                c%value_type = 0
                return
            end if
            c%complex_val = a%complex_val / b%complex_val
            c%value_type = VALUE_COMPLEX
            return
        end if
        
        c%value_type = 0  ! VALUE_UNDEFINED
    end function divide_values
    
    !> Raise a value to a power
    function power_values(a, b) result(c)
        type(value_t), intent(in) :: a, b
        type(value_t) :: c
        
        ! Handle scalar ** scalar
        if (a%value_type == VALUE_SCALAR .and. b%value_type == VALUE_SCALAR) then
            c = create_scalar(a%scalar_val ** b%scalar_val)
            return
        end if
        
        ! Handle complex exponentiation (more complex, implement later)
        ! For now, convert to complex and use Fortran's intrinsic
        if (a%value_type == VALUE_COMPLEX .or. b%value_type == VALUE_COMPLEX) then
            write(*, '(A)') 'Complex exponentiation not yet fully implemented'
            c%value_type = 0
            return
        end if
        
        c%value_type = 0  ! VALUE_UNDEFINED
    end function power_values
    
    !> Negate a value
    function negate_value(a) result(c)
        type(value_t), intent(in) :: a
        type(value_t) :: c
        
        select case (a%value_type)
        case (VALUE_SCALAR)
            c = create_scalar(-a%scalar_val)
        case (VALUE_COMPLEX)
            c = create_complex(-real(a%complex_val), -aimag(a%complex_val))
        case default
            c%value_type = 0  ! VALUE_UNDEFINED
        end select
    end function negate_value
    
    !> Absolute value of a value
    function abs_value(a) result(c)
        type(value_t), intent(in) :: a
        type(value_t) :: c
        
        select case (a%value_type)
        case (VALUE_SCALAR)
            c = create_scalar(abs(a%scalar_val))
        case (VALUE_COMPLEX)
            c = create_scalar(abs(a%complex_val))
        case default
            c%value_type = 0  ! VALUE_UNDEFINED
        end select
    end function abs_value
    
end module fortbite_arithmetic_m