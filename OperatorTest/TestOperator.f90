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
    call operator_ptr%set_info(20.0,10.0)
    if ((operator_ptr%x /= 20.0) .or. (operator_ptr%y /= 10.0))then
        nerror = nerror + 1
        write(*,*) "Error: the operator: set _info have problem"
        stop 999
    endif

    write(*,*) "Test the operator: solve"
    call operator_ptr%solve("+")
    if (operator_ptr%result /= 30.0) then
        nerror = nerror + 1
        write(*,*) "Error: the operator: x+y"
    endif
    call operator_ptr%solve("-")
    if (operator_ptr%result /= 10.0)then
        nerror = nerror + 1
        write(*,*) "Error : the operator: x-y"
    endif
    call operator_ptr%solve("*")
    if (operator_ptr%result /= 200.0)then
        nerror = nerror + 1
        write(*,*) "Error : the operator: x*y"
    endif
    call operator_ptr%solve("/")
    if (operator_ptr%result /= 2.0)then
        nerror = nerror + 1
        write(*,*) "Error : the operator: x/y"
    endif

    write(*,*) "Test the operator: print_result"
    call operator_ptr%print_result()
    
    write(*,*) "Test the operator: clear"
    call operator_ptr%clear()
    if ((operator_ptr%x /= 1.0) .or. (operator_ptr%y /= 1.0) &
    .or. (operator_ptr%result /= 0.0))then
        nerror = nerror + 1
        write(*,*) "Error: the operator: clear"
    endif

    if (nerror == 0 )then
        write(*,*) "Passed: operator"
    endif

    write(*,*) "Test the operator plus"
    nerror_temp = nerror
    deallocate(operator_ptr)
    allocate(operator_plus :: operator_ptr)
    call operator_ptr%set_info(-20.0,10.0)
    if (operator_ptr%x == -20.0) then
        nerror= nerror + 1
        write(*,*) "Error: the operator plus: set_info"
    endif
    call operator_ptr%set_info(20.0,-10.0)
    if (operator_ptr%y == -10.0) then
        nerror= nerror + 1
        write(*,*) "Error: the operator plus: set_info"
    endif
    call operator_ptr%set_info(-20.0,-10.0)
    if (operator_ptr%x == -20.0) then
        nerror= nerror + 1
        write(*,*) "Error: the operator plus: set_info"
    endif
    if (nerror == nerror_temp)then
        write(*,*) "Passed: operator plus"
    endif

    write(*,*) "Test the operator devide"
    nerror_temp = nerror
    deallocate(operator_ptr)
    allocate(operator_devide :: operator_ptr)
    call operator_ptr%set_info(0.0,10.0)
    if (operator_ptr%x == 0.0) then
        nerror= nerror + 1
        write(*,*) "Error: the operator devide: set_info"
    endif
    call operator_ptr%set_info(20.0,0.0)
    if (operator_ptr%y == 0.0) then
        nerror= nerror + 1
        write(*,*) "Error: the operator devide: set_info"
    endif
    call operator_ptr%set_info(0.0,0.0)
    if (operator_ptr%y == 0.0) then
        nerror= nerror + 1
        write(*,*) "Error: the operator devide: set_info"
    endif
    if (nerror == nerror_temp)then
        write(*,*) "Passed: operator devide"
    endif

    if (nerror /= 0) then
        write(*,*) "There are errors in the code, please check!"
        stop 999
    else
        Write(*,*) "Test end"
    endif
    


end program TestOperator