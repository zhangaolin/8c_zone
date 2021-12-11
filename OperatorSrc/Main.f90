include "Module.f90"
program Main
    use operator_type_module

    implicit none

    class(operator), pointer :: operator_ptr => null()

    write(*,*) "========== main begin =========="
    nullify(operator_ptr)
    allocate(operator :: operator_ptr) 
    call operator_ptr%set_info(10.0,2.5)
    call operator_ptr%solve("/")
    call operator_ptr%print_result()
    call operator_ptr%clear()
    deallocate(operator_ptr)
    
end program Main