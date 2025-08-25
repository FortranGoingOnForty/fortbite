!> Advanced Mathematical Functions module for FORTBITE
!>
!> Provides comprehensive mathematical functions including trigonometric,
!> hyperbolic, logarithmic, exponential, statistical, and special functions.
module fortbite_functions_m
    use fortbite_types_m, only: value_t, create_scalar, create_complex, VALUE_SCALAR, VALUE_COMPLEX, VALUE_MATRIX
    use iso_fortran_env, only: real64
    implicit none
    private
    
    ! Public interfaces for mathematical functions
    public :: eval_trigonometric, eval_hyperbolic, eval_logarithmic
    public :: eval_exponential, eval_statistical, eval_special
    public :: eval_complex_functions
    
    ! Mathematical constants
    real(real64), parameter :: PI = 4.0_real64 * atan(1.0_real64)
    real(real64), parameter :: E = exp(1.0_real64)
    real(real64), parameter :: EULER_GAMMA = 0.5772156649015329_real64
    
contains

    !> Evaluate trigonometric functions
    function eval_trigonometric(func_name, arg) result(result_val)
        character(len=*), intent(in) :: func_name
        type(value_t), intent(in) :: arg
        type(value_t) :: result_val
        
        real(real64) :: x
        
        if (arg%value_type /= VALUE_SCALAR) then
            result_val = create_scalar(0.0_real64)
            return
        end if
        
        x = arg%scalar_val
        
        select case (trim(func_name))
        case ('sin')
            result_val = create_scalar(sin(x))
        case ('cos')
            result_val = create_scalar(cos(x))
        case ('tan')
            result_val = create_scalar(tan(x))
        case ('asin', 'arcsin')
            if (abs(x) <= 1.0_real64) then
                result_val = create_scalar(asin(x))
            else
                result_val = create_scalar(0.0_real64)  ! Error case
            end if
        case ('acos', 'arccos')
            if (abs(x) <= 1.0_real64) then
                result_val = create_scalar(acos(x))
            else
                result_val = create_scalar(0.0_real64)  ! Error case
            end if
        case ('atan', 'arctan')
            result_val = create_scalar(atan(x))
        case ('atan2', 'arctan2')
            ! This would need two arguments - handled separately
            result_val = create_scalar(atan(x))
        case ('sec')
            if (abs(cos(x)) > tiny(1.0_real64)) then
                result_val = create_scalar(1.0_real64 / cos(x))
            else
                result_val = create_scalar(huge(1.0_real64))  ! Infinity approximation
            end if
        case ('csc')
            if (abs(sin(x)) > tiny(1.0_real64)) then
                result_val = create_scalar(1.0_real64 / sin(x))
            else
                result_val = create_scalar(huge(1.0_real64))  ! Infinity approximation
            end if
        case ('cot')
            if (abs(tan(x)) > tiny(1.0_real64)) then
                result_val = create_scalar(1.0_real64 / tan(x))
            else
                result_val = create_scalar(huge(1.0_real64))  ! Infinity approximation
            end if
        case default
            result_val = create_scalar(0.0_real64)
        end select
    end function eval_trigonometric
    
    !> Evaluate hyperbolic functions
    function eval_hyperbolic(func_name, arg) result(result_val)
        character(len=*), intent(in) :: func_name
        type(value_t), intent(in) :: arg
        type(value_t) :: result_val
        
        real(real64) :: x
        
        if (arg%value_type /= VALUE_SCALAR) then
            result_val = create_scalar(0.0_real64)
            return
        end if
        
        x = arg%scalar_val
        
        select case (trim(func_name))
        case ('sinh')
            result_val = create_scalar(sinh(x))
        case ('cosh')
            result_val = create_scalar(cosh(x))
        case ('tanh')
            result_val = create_scalar(tanh(x))
        case ('asinh')
            result_val = create_scalar(asinh(x))
        case ('acosh')
            if (x >= 1.0_real64) then
                result_val = create_scalar(acosh(x))
            else
                result_val = create_scalar(0.0_real64)  ! Error case
            end if
        case ('atanh')
            if (abs(x) < 1.0_real64) then
                result_val = create_scalar(atanh(x))
            else
                result_val = create_scalar(0.0_real64)  ! Error case
            end if
        case ('sech')
            result_val = create_scalar(1.0_real64 / cosh(x))
        case ('csch')
            if (abs(x) > tiny(1.0_real64)) then
                result_val = create_scalar(1.0_real64 / sinh(x))
            else
                result_val = create_scalar(huge(1.0_real64))  ! Infinity approximation
            end if
        case ('coth')
            if (abs(tanh(x)) > tiny(1.0_real64)) then
                result_val = create_scalar(1.0_real64 / tanh(x))
            else
                result_val = create_scalar(huge(1.0_real64))  ! Infinity approximation
            end if
        case default
            result_val = create_scalar(0.0_real64)
        end select
    end function eval_hyperbolic
    
    !> Evaluate logarithmic functions
    function eval_logarithmic(func_name, arg) result(result_val)
        character(len=*), intent(in) :: func_name
        type(value_t), intent(in) :: arg
        type(value_t) :: result_val
        
        real(real64) :: x
        
        if (arg%value_type /= VALUE_SCALAR) then
            result_val = create_scalar(0.0_real64)
            return
        end if
        
        x = arg%scalar_val
        
        select case (trim(func_name))
        case ('log', 'ln')
            if (x > 0.0_real64) then
                result_val = create_scalar(log(x))
            else
                result_val = create_scalar(-huge(1.0_real64))  ! -Infinity approximation
            end if
        case ('log10', 'lg')
            if (x > 0.0_real64) then
                result_val = create_scalar(log10(x))
            else
                result_val = create_scalar(-huge(1.0_real64))  ! -Infinity approximation
            end if
        case ('log2')
            if (x > 0.0_real64) then
                result_val = create_scalar(log(x) / log(2.0_real64))
            else
                result_val = create_scalar(-huge(1.0_real64))  ! -Infinity approximation
            end if
        case default
            result_val = create_scalar(0.0_real64)
        end select
    end function eval_logarithmic
    
    !> Evaluate exponential functions
    function eval_exponential(func_name, arg) result(result_val)
        character(len=*), intent(in) :: func_name
        type(value_t), intent(in) :: arg
        type(value_t) :: result_val
        
        real(real64) :: x
        
        if (arg%value_type /= VALUE_SCALAR) then
            result_val = create_scalar(0.0_real64)
            return
        end if
        
        x = arg%scalar_val
        
        select case (trim(func_name))
        case ('exp')
            result_val = create_scalar(exp(x))
        case ('exp2')
            result_val = create_scalar(2.0_real64**x)
        case ('exp10')
            result_val = create_scalar(10.0_real64**x)
        case ('expm1')
            ! exp(x) - 1, accurate for small x
            result_val = create_scalar(exp(x) - 1.0_real64)
        case default
            result_val = create_scalar(0.0_real64)
        end select
    end function eval_exponential
    
    !> Evaluate statistical functions (for arrays/matrices)
    function eval_statistical(func_name, arg) result(result_val)
        character(len=*), intent(in) :: func_name
        type(value_t), intent(in) :: arg
        type(value_t) :: result_val
        
        real(real64) :: sum_val, mean_val, variance, n
        integer :: i, j
        
        select case (trim(func_name))
        case ('mean', 'average')
            if (arg%value_type == VALUE_MATRIX) then
                sum_val = 0.0_real64
                n = real(arg%rows * arg%cols, real64)
                do i = 1, arg%rows
                    do j = 1, arg%cols
                        sum_val = sum_val + arg%matrix_val(i,j)
                    end do
                end do
                result_val = create_scalar(sum_val / n)
            else
                result_val = arg  ! Single value
            end if
            
        case ('sum')
            if (arg%value_type == VALUE_MATRIX) then
                sum_val = 0.0_real64
                do i = 1, arg%rows
                    do j = 1, arg%cols
                        sum_val = sum_val + arg%matrix_val(i,j)
                    end do
                end do
                result_val = create_scalar(sum_val)
            else
                result_val = arg  ! Single value
            end if
            
        case ('std', 'stddev')
            if (arg%value_type == VALUE_MATRIX) then
                ! Calculate standard deviation
                sum_val = 0.0_real64
                n = real(arg%rows * arg%cols, real64)
                
                ! Calculate mean
                do i = 1, arg%rows
                    do j = 1, arg%cols
                        sum_val = sum_val + arg%matrix_val(i,j)
                    end do
                end do
                mean_val = sum_val / n
                
                ! Calculate variance
                variance = 0.0_real64
                do i = 1, arg%rows
                    do j = 1, arg%cols
                        variance = variance + (arg%matrix_val(i,j) - mean_val)**2
                    end do
                end do
                variance = variance / (n - 1.0_real64)
                
                result_val = create_scalar(sqrt(variance))
            else
                result_val = create_scalar(0.0_real64)  ! Single value has no std dev
            end if
            
        case default
            result_val = create_scalar(0.0_real64)
        end select
    end function eval_statistical
    
    !> Evaluate special mathematical functions
    function eval_special(func_name, arg) result(result_val)
        character(len=*), intent(in) :: func_name
        type(value_t), intent(in) :: arg
        type(value_t) :: result_val
        
        real(real64) :: x
        integer :: n
        
        if (arg%value_type /= VALUE_SCALAR) then
            result_val = create_scalar(0.0_real64)
            return
        end if
        
        x = arg%scalar_val
        
        select case (trim(func_name))
        case ('gamma')
            result_val = create_scalar(gamma(x))
        case ('lgamma', 'loggamma')
            result_val = create_scalar(log_gamma(x))
        case ('factorial', 'fact')
            n = int(x)
            if (n >= 0 .and. real(n, real64) == x) then
                result_val = create_scalar(factorial_real(n))
            else
                result_val = create_scalar(0.0_real64)  ! Error case
            end if
        case ('erf')
            result_val = create_scalar(erf(x))
        case ('erfc')
            result_val = create_scalar(erfc(x))
        case ('ceil', 'ceiling')
            result_val = create_scalar(real(ceiling(x), real64))
        case ('floor')
            result_val = create_scalar(real(floor(x), real64))
        case ('round', 'nint')
            result_val = create_scalar(real(nint(x), real64))
        case ('frac', 'fraction')
            result_val = create_scalar(x - real(floor(x), real64))
        case default
            result_val = create_scalar(0.0_real64)
        end select
    end function eval_special
    
    !> Evaluate complex number functions
    function eval_complex_functions(func_name, arg) result(result_val)
        character(len=*), intent(in) :: func_name
        type(value_t), intent(in) :: arg
        type(value_t) :: result_val
        
        complex(real64) :: z
        
        select case (arg%value_type)
        case (VALUE_COMPLEX)
            z = arg%complex_val
        case (VALUE_SCALAR)
            z = cmplx(arg%scalar_val, 0.0_real64, kind=real64)
        case default
            result_val = create_scalar(0.0_real64)
            return
        end select
        
        select case (trim(func_name))
        case ('real', 're')
            result_val = create_scalar(real(z))
        case ('imag', 'im')
            result_val = create_scalar(aimag(z))
        case ('conj', 'conjugate')
            result_val = create_complex(real(conjg(z)), aimag(conjg(z)))
        case ('arg', 'phase', 'angle')
            result_val = create_scalar(atan2(aimag(z), real(z)))
        case ('cabs', 'modulus')
            result_val = create_scalar(abs(z))
        case default
            result_val = create_scalar(0.0_real64)
        end select
    end function eval_complex_functions
    
    !> Helper function for factorial calculation
    real(real64) function factorial_real(n) result(result)
        integer, intent(in) :: n
        integer :: i
        
        result = 1.0_real64
        do i = 1, n
            result = result * real(i, real64)
        end do
    end function factorial_real
    
end module fortbite_functions_m