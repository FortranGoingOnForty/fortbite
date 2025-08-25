!> FORTBITE - High-Precision Calculator in Modern Fortran
!>
!> A powerful mathematical calculator leveraging Fortran's strengths in
!> scientific computing, with arbitrary precision arithmetic, complex numbers,
!> matrix operations, and extensive mathematical functions.
!>
!> Author: espadonne (mfw)
!> License: MIT
program fortbite
    use fortbite_precision_m, only: set_default_precision
    use fortbite_types_m, only: value_t
    use fortbite_io_m, only: repl_loop
    implicit none
    
    ! Initialize default precision
    call set_default_precision(15)  ! Start with ~double precision
    
    ! Start the main REPL loop
    call repl_loop()
    
end program fortbite