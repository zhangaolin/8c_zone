INCLUDE "../OperatorSrc/Module.f90"

program TestOperator
    use operator_type_module
    use operator_plus_type_module
    use operator_devide_type_module
    implicit none
    class(operator), pointer :: operator_ptr => null()
    integer :: nerror = 0, nerror_temp = 0

    Write(*,*) "Test begin"
    nullify(operator_ptr)

    Write(*,*) "Test the operator: set_info"
    allocate(operator :: operator_ptr)
    call operator_ptr%set_info(10.0,20.0)
    if ((operator_ptr%x /= 10.0) .or. (operator_ptr%y /= 20.0))then
        nerror = nerror + 1
        write(*,*) "Error: the operator: set _info have problem"
        stop 999
    endif

    write(*,*) "Test the operator: solve"
    call operator_ptr%solve("+")
    if (operator_ptr%result /= 30.0) then
        nerror = nerror + 1


end program TestOperator