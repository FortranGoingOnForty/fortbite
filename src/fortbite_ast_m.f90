!> Abstract Syntax Tree module for FORTBITE
!>
!> Defines the AST node types and operations for representing parsed
!> mathematical expressions in tree form.
module fortbite_ast_m
    use fortbite_types_m, only: value_t
    use iso_fortran_env, only: real64
    implicit none
    private
    
    public :: ast_node_t, ast_node_type_enum, operator_type_enum, ast_node_ptr_t
    public :: AST_LITERAL, AST_IDENTIFIER, AST_BINARY_OP, AST_UNARY_OP
    public :: AST_FUNCTION_CALL, AST_ASSIGNMENT, AST_PRECISION_SPEC, AST_MATRIX_LITERAL
    public :: OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_POW, OP_MOD
    public :: OP_UNARY_PLUS, OP_UNARY_MINUS
    public :: create_literal_node, create_identifier_node, create_binary_node
    public :: create_unary_node, create_function_node, create_assignment_node
    public :: create_precision_node, create_matrix_literal_node, free_ast, print_ast
    
    !> AST node types
    enum, bind(c)
        enumerator :: AST_LITERAL = 1
        enumerator :: AST_IDENTIFIER = 2
        enumerator :: AST_BINARY_OP = 3
        enumerator :: AST_UNARY_OP = 4
        enumerator :: AST_FUNCTION_CALL = 5
        enumerator :: AST_ASSIGNMENT = 6
        enumerator :: AST_PRECISION_SPEC = 7
        enumerator :: AST_MATRIX_LITERAL = 8
    end enum
    integer, parameter :: ast_node_type_enum = kind(AST_LITERAL)
    
    !> Operator types
    enum, bind(c)
        enumerator :: OP_ADD = 1
        enumerator :: OP_SUB = 2
        enumerator :: OP_MUL = 3
        enumerator :: OP_DIV = 4
        enumerator :: OP_POW = 5
        enumerator :: OP_MOD = 6
        enumerator :: OP_UNARY_PLUS = 7
        enumerator :: OP_UNARY_MINUS = 8
    end enum
    integer, parameter :: operator_type_enum = kind(OP_ADD)
    
    !> Pointer wrapper for AST nodes in arrays
    type :: ast_node_ptr_t
        type(ast_node_t), pointer :: ptr => null()
    end type ast_node_ptr_t
    
    !> AST node type - can represent any expression component
    type :: ast_node_t
        integer(ast_node_type_enum) :: node_type
        
        ! Node-specific data
        type(value_t) :: literal_value                    ! For AST_LITERAL
        character(len=:), allocatable :: identifier      ! For AST_IDENTIFIER
        integer(operator_type_enum) :: operator          ! For AST_BINARY_OP, AST_UNARY_OP
        character(len=:), allocatable :: function_name   ! For AST_FUNCTION_CALL
        integer :: precision_digits                       ! For AST_PRECISION_SPEC
        
        ! Child node pointers
        type(ast_node_t), pointer :: left => null()      ! Left operand
        type(ast_node_t), pointer :: right => null()     ! Right operand
        type(ast_node_t), pointer :: operand => null()   ! For unary operations
        type(ast_node_t), pointer :: expression => null() ! For precision specs
        
        ! Function arguments (array of pointer wrappers)
        type(ast_node_ptr_t), allocatable :: arguments(:)
        integer :: arg_count = 0
        
        ! Matrix literal data
        real(real64), allocatable :: matrix_elements(:,:)
        integer :: matrix_rows = 0, matrix_cols = 0
    end type ast_node_t
    
contains

    !> Create a literal value node
    function create_literal_node(value) result(node)
        type(value_t), intent(in) :: value
        type(ast_node_t), pointer :: node
        
        allocate(node)
        node%node_type = AST_LITERAL
        node%literal_value = value
    end function create_literal_node
    
    !> Create an identifier node
    function create_identifier_node(name) result(node)
        character(len=*), intent(in) :: name
        type(ast_node_t), pointer :: node
        
        allocate(node)
        node%node_type = AST_IDENTIFIER
        node%identifier = trim(name)
    end function create_identifier_node
    
    !> Create a binary operation node
    function create_binary_node(op, left_node, right_node) result(node)
        integer(operator_type_enum), intent(in) :: op
        type(ast_node_t), pointer, intent(in) :: left_node, right_node
        type(ast_node_t), pointer :: node
        
        allocate(node)
        node%node_type = AST_BINARY_OP
        node%operator = op
        node%left => left_node
        node%right => right_node
    end function create_binary_node
    
    !> Create a unary operation node
    function create_unary_node(op, operand_node) result(node)
        integer(operator_type_enum), intent(in) :: op
        type(ast_node_t), pointer, intent(in) :: operand_node
        type(ast_node_t), pointer :: node
        
        allocate(node)
        node%node_type = AST_UNARY_OP
        node%operator = op
        node%operand => operand_node
    end function create_unary_node
    
    !> Create a function call node
    function create_function_node(func_name, args) result(node)
        character(len=*), intent(in) :: func_name
        type(ast_node_t), pointer, intent(in), optional :: args(:)
        type(ast_node_t), pointer :: node
        integer :: i
        
        allocate(node)
        node%node_type = AST_FUNCTION_CALL
        node%function_name = trim(func_name)
        
        if (present(args)) then
            node%arg_count = size(args)
            allocate(node%arguments(node%arg_count))
            do i = 1, node%arg_count
                node%arguments(i)%ptr => args(i)
            end do
        else
            node%arg_count = 0
        end if
    end function create_function_node
    
    !> Create an assignment node
    function create_assignment_node(var_name, expression_node) result(node)
        character(len=*), intent(in) :: var_name
        type(ast_node_t), pointer, intent(in) :: expression_node
        type(ast_node_t), pointer :: node
        
        type(ast_node_t), pointer :: identifier_node
        
        allocate(node)
        node%node_type = AST_ASSIGNMENT
        node%operator = OP_ADD  ! Dummy value, not used for assignments
        
        ! Create identifier node for the variable
        identifier_node => create_identifier_node(var_name)
        node%left => identifier_node
        node%right => expression_node
    end function create_assignment_node
    
    !> Create a precision specification node
    function create_precision_node(expr_node, precision) result(node)
        type(ast_node_t), pointer, intent(in) :: expr_node
        integer, intent(in) :: precision
        type(ast_node_t), pointer :: node
        
        allocate(node)
        node%node_type = AST_PRECISION_SPEC
        node%precision_digits = precision
        node%expression => expr_node
    end function create_precision_node
    
    !> Create a matrix literal node
    function create_matrix_literal_node(elements, rows, cols) result(node)
        real(real64), intent(in) :: elements(:,:)
        integer, intent(in) :: rows, cols
        type(ast_node_t), pointer :: node
        
        allocate(node)
        node%node_type = AST_MATRIX_LITERAL
        node%matrix_rows = rows
        node%matrix_cols = cols
        
        allocate(node%matrix_elements(rows, cols))
        node%matrix_elements = elements
    end function create_matrix_literal_node
    
    !> Free AST and all child nodes
    recursive subroutine free_ast(node)
        type(ast_node_t), pointer, intent(inout) :: node
        integer :: i
        
        if (.not. associated(node)) return
        
        ! Free child nodes
        if (associated(node%left)) call free_ast(node%left)
        if (associated(node%right)) call free_ast(node%right)
        if (associated(node%operand)) call free_ast(node%operand)
        if (associated(node%expression)) call free_ast(node%expression)
        
        ! Free function arguments
        if (allocated(node%arguments)) then
            do i = 1, node%arg_count
                if (associated(node%arguments(i)%ptr)) then
                    call free_ast(node%arguments(i)%ptr)
                end if
            end do
            deallocate(node%arguments)
        end if
        
        ! Free matrix literal data
        if (allocated(node%matrix_elements)) then
            deallocate(node%matrix_elements)
        end if
        
        ! Free the node itself
        deallocate(node)
        nullify(node)
    end subroutine free_ast
    
    !> Print AST for debugging (recursive)
    recursive subroutine print_ast(node, indent)
        type(ast_node_t), pointer, intent(in) :: node
        integer, intent(in), optional :: indent
        
        integer :: ind, i
        character(len=50) :: spaces
        
        if (.not. associated(node)) return
        
        ind = 0
        if (present(indent)) ind = indent
        
        spaces = repeat(' ', ind)
        
        select case (node%node_type)
        case (AST_LITERAL)
            write(*, '(A,A)') trim(spaces), 'LITERAL: [value]'
            
        case (AST_IDENTIFIER)
            write(*, '(A,A,A)') trim(spaces), 'IDENTIFIER: ', node%identifier
            
        case (AST_BINARY_OP)
            write(*, '(A,A,A)') trim(spaces), 'BINARY_OP: ', get_operator_name(node%operator)
            call print_ast(node%left, ind + 2)
            call print_ast(node%right, ind + 2)
            
        case (AST_UNARY_OP)
            write(*, '(A,A,A)') trim(spaces), 'UNARY_OP: ', get_operator_name(node%operator)
            call print_ast(node%operand, ind + 2)
            
        case (AST_FUNCTION_CALL)
            write(*, '(A,A,A,A,I0,A)') trim(spaces), 'FUNCTION: ', node%function_name, &
                ' (', node%arg_count, ' args)'
            if (allocated(node%arguments)) then
                do i = 1, node%arg_count
                    call print_ast(node%arguments(i)%ptr, ind + 2)
                end do
            end if
            
        case (AST_ASSIGNMENT)
            write(*, '(A,A)') trim(spaces), 'ASSIGNMENT:'
            call print_ast(node%left, ind + 2)
            call print_ast(node%right, ind + 2)
            
        case (AST_PRECISION_SPEC)
            write(*, '(A,A,I0)') trim(spaces), 'PRECISION: ', node%precision_digits
            call print_ast(node%expression, ind + 2)
            
        case (AST_MATRIX_LITERAL)
            write(*, '(A,A,I0,A,I0,A)') trim(spaces), 'MATRIX_LITERAL: [', &
                node%matrix_rows, 'x', node%matrix_cols, ']'
            
        case default
            write(*, '(A,A)') trim(spaces), 'UNKNOWN NODE'
        end select
    end subroutine print_ast
    
    !> Get human-readable operator name
    function get_operator_name(op) result(name)
        integer(operator_type_enum), intent(in) :: op
        character(len=10) :: name
        
        select case (op)
        case (OP_ADD)
            name = '+'
        case (OP_SUB)
            name = '-'
        case (OP_MUL)
            name = '*'
        case (OP_DIV)
            name = '/'
        case (OP_POW)
            name = '**'
        case (OP_MOD)
            name = 'mod'
        case (OP_UNARY_PLUS)
            name = 'unary +'
        case (OP_UNARY_MINUS)
            name = 'unary -'
        case default
            name = 'unknown'
        end select
    end function get_operator_name
    
end module fortbite_ast_m