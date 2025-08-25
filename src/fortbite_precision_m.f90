!> Precision management module for FORTBITE
!> 
!> This module provides precision control using Fortran's selected_real_kind
!> system, allowing arbitrary precision arithmetic with user control.
module fortbite_precision_m
    use iso_fortran_env, only: int32, int64, real32, real64, real128
    implicit none
    private
    
    ! Public precision parameters
    public :: wp, dp, qp, sp
    public :: default_precision, max_precision
    public :: set_default_precision, get_precision_kind
    public :: precision_info_t, get_precision_info
    
    ! Standard precision kinds
    integer, parameter :: sp = real32                    ! Single precision
    integer, parameter :: dp = real64                    ! Double precision  
    integer, parameter :: qp = real128                   ! Quad precision (if available)
    
    ! Default working precision (can be changed by user)
    integer, parameter :: default_precision = dp
    integer :: wp = default_precision                    ! Working precision
    
    ! Maximum available precision on this system
    integer, parameter :: max_precision = selected_real_kind(33, 4931)
    
    !> Precision information type
    type :: precision_info_t
        integer :: kind_param          !< Kind parameter
        integer :: decimal_digits      !< Decimal precision
        integer :: exponent_range      !< Exponent range
        character(len=20) :: name      !< Human-readable name
        logical :: available           !< Available on this system
    end type precision_info_t
    
contains

    !> Set the default working precision
    subroutine set_default_precision(precision_digits)
        integer, intent(in) :: precision_digits
        
        integer :: new_kind
        
        ! Get the appropriate kind for requested precision
        new_kind = selected_real_kind(precision_digits)
        
        if (new_kind > 0) then
            wp = new_kind
        else
            write(*, '(A,I0,A)') 'Warning: Precision with ', precision_digits, &
                ' digits not available. Using maximum available precision.'
            wp = max_precision
        end if
    end subroutine set_default_precision
    
    !> Get kind parameter for specified decimal precision
    function get_precision_kind(decimal_digits, exponent_range) result(kind_param)
        integer, intent(in) :: decimal_digits
        integer, intent(in), optional :: exponent_range
        integer :: kind_param
        
        integer :: exp_range
        
        exp_range = 37  ! Default exponent range
        if (present(exponent_range)) exp_range = exponent_range
        
        kind_param = selected_real_kind(decimal_digits, exp_range)
        
        ! Fall back to maximum precision if requested precision unavailable
        if (kind_param < 0) then
            kind_param = max_precision
        end if
    end function get_precision_kind
    
    !> Get information about a precision kind
    function get_precision_info(kind_param) result(info)
        integer, intent(in) :: kind_param
        type(precision_info_t) :: info
        
        info%kind_param = kind_param
        select case (kind_param)
        case (real32)
            info%decimal_digits = precision(1.0_real32)
            info%exponent_range = range(1.0_real32)
        case (real64) 
            info%decimal_digits = precision(1.0_real64)
            info%exponent_range = range(1.0_real64)
        case (real128)
            info%decimal_digits = precision(1.0_real128)
            info%exponent_range = range(1.0_real128)
        case default
            ! For unknown kinds, try to get info using the kind parameter
            info%decimal_digits = 15  ! reasonable default
            info%exponent_range = 307  ! reasonable default
        end select
        info%available = (kind_param > 0)
        
        ! Set human-readable name
        select case (kind_param)
        case (real32)
            info%name = 'Single Precision'
        case (real64)
            info%name = 'Double Precision'
        case (real128)
            info%name = 'Quad Precision'
        case default
            if (kind_param == max_precision) then
                info%name = 'Maximum Precision'
            else
                info%name = 'Custom Precision'
            end if
        end select
    end function get_precision_info
    
end module fortbite_precision_m