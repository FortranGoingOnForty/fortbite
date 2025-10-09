!> Test framework module for FORTBITE
!> Provides utilities for unit testing
module test_framework
    use iso_fortran_env, only: real64, output_unit, error_unit
    implicit none
    private

    public :: test_suite, test_case, assert_equals, assert_true, assert_false
    public :: assert_near, run_test_suite, init_test_suite, add_test_case

    !> Test case type
    type :: test_case
        character(len=100) :: name = ''
        logical :: passed = .true.
        character(len=200) :: failure_message = ''
    end type test_case

    !> Test suite type
    type :: test_suite
        character(len=100) :: name = ''
        type(test_case) :: tests(100)
        integer :: test_count = 0
        integer :: passed_count = 0
        integer :: failed_count = 0
        integer :: current_test_index = 0
    end type test_suite

contains

    !> Initialize a test suite
    function init_test_suite(name) result(suite)
        character(len=*), intent(in) :: name
        type(test_suite) :: suite

        suite%name = trim(name)
        suite%test_count = 0
        suite%passed_count = 0
        suite%failed_count = 0
        suite%current_test_index = 0
    end function init_test_suite

    !> Add a test case to the suite
    subroutine add_test_case(suite, name)
        type(test_suite), intent(inout) :: suite
        character(len=*), intent(in) :: name

        suite%test_count = suite%test_count + 1
        if (suite%test_count > size(suite%tests)) then
            write(error_unit, *) 'ERROR: Too many test cases!'
            return
        end if

        suite%current_test_index = suite%test_count
        suite%tests(suite%test_count)%name = trim(name)
        suite%tests(suite%test_count)%passed = .true.
        suite%tests(suite%test_count)%failure_message = ''
    end subroutine add_test_case

    !> Run all tests in a suite
    subroutine run_test_suite(suite)
        type(test_suite), intent(inout) :: suite
        integer :: i

        write(output_unit, '(A)') repeat('=', 60)
        write(output_unit, '(A,A)') 'Running Test Suite: ', trim(suite%name)
        write(output_unit, '(A)') repeat('=', 60)

        suite%passed_count = 0
        suite%failed_count = 0

        do i = 1, suite%test_count
            if (suite%tests(i)%passed) then
                suite%passed_count = suite%passed_count + 1
                write(output_unit, '(A,A,A)') '✓ ', trim(suite%tests(i)%name), ' ... PASSED'
            else
                suite%failed_count = suite%failed_count + 1
                write(output_unit, '(A,A,A)') '✗ ', trim(suite%tests(i)%name), ' ... FAILED'
                if (len_trim(suite%tests(i)%failure_message) > 0) then
                    write(output_unit, '(A,A)') '  ', trim(suite%tests(i)%failure_message)
                end if
            end if
        end do

        write(output_unit, '(A)') repeat('-', 60)
        write(output_unit, '(A,I0,A,I0,A,I0,A)') 'Results: ', &
            suite%passed_count, ' passed, ', &
            suite%failed_count, ' failed, ', &
            suite%test_count, ' total'
        write(output_unit, '(A)') repeat('=', 60)
    end subroutine run_test_suite

    !> Assert that two real values are equal
    subroutine assert_equals(suite, actual, expected, message)
        type(test_suite), intent(inout) :: suite
        real(real64), intent(in) :: actual, expected
        character(len=*), intent(in), optional :: message

        character(len=200) :: fail_msg
        integer :: idx

        idx = suite%current_test_index
        if (idx < 1 .or. idx > suite%test_count) return

        if (abs(actual - expected) > epsilon(1.0_real64)) then
            write(fail_msg, '(A,G0,A,G0)') 'Expected: ', expected, ', Got: ', actual
            if (present(message)) then
                fail_msg = trim(message) // ' - ' // trim(fail_msg)
            end if
            suite%tests(idx)%passed = .false.
            suite%tests(idx)%failure_message = trim(fail_msg)
        end if
    end subroutine assert_equals

    !> Assert that two real values are approximately equal
    subroutine assert_near(suite, actual, expected, tolerance, message)
        type(test_suite), intent(inout) :: suite
        real(real64), intent(in) :: actual, expected, tolerance
        character(len=*), intent(in), optional :: message

        character(len=200) :: fail_msg
        integer :: idx

        idx = suite%current_test_index
        if (idx < 1 .or. idx > suite%test_count) return

        if (abs(actual - expected) > tolerance) then
            write(fail_msg, '(A,G0,A,G0,A,G0)') 'Expected: ', expected, &
                ' (±', tolerance, '), Got: ', actual
            if (present(message)) then
                fail_msg = trim(message) // ' - ' // trim(fail_msg)
            end if
            suite%tests(idx)%passed = .false.
            suite%tests(idx)%failure_message = trim(fail_msg)
        end if
    end subroutine assert_near

    !> Assert that a condition is true
    subroutine assert_true(suite, condition, message)
        type(test_suite), intent(inout) :: suite
        logical, intent(in) :: condition
        character(len=*), intent(in), optional :: message

        integer :: idx

        idx = suite%current_test_index
        if (idx < 1 .or. idx > suite%test_count) return

        if (.not. condition) then
            suite%tests(idx)%passed = .false.
            if (present(message)) then
                suite%tests(idx)%failure_message = trim(message)
            else
                suite%tests(idx)%failure_message = 'Assertion failed: Expected true, got false'
            end if
        end if
    end subroutine assert_true

    !> Assert that a condition is false
    subroutine assert_false(suite, condition, message)
        type(test_suite), intent(inout) :: suite
        logical, intent(in) :: condition
        character(len=*), intent(in), optional :: message

        integer :: idx

        idx = suite%current_test_index
        if (idx < 1 .or. idx > suite%test_count) return

        if (condition) then
            suite%tests(idx)%passed = .false.
            if (present(message)) then
                suite%tests(idx)%failure_message = trim(message)
            else
                suite%tests(idx)%failure_message = 'Assertion failed: Expected false, got true'
            end if
        end if
    end subroutine assert_false

end module test_framework