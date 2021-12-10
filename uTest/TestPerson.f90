INCLUDE "../src/ModuleType.f90"

PROGRAM TestPerson
    use person_type_module
    use student_type_module
    use teacher_type_module

    IMPLICIT NONE
    class(person), pointer :: person_ptr => null()
    integer :: nerror = 0, nerror_temp = 0
    
    Write(*,*) "Test begin"
    
    allocate(person :: person_ptr)
    call person_ptr%set_info("xiao ming mother","woman",33)
    if(trim(person_ptr%name) /= "xiao ming mother" .or. trim(person_ptr%sex)/="woman" &
    .or. person_ptr%age /= 33) then
        nerror = nerror + 1
        Write(*,*) "Error set_info is not correct"
    endif

    call person_ptr%remove_info()
    if(trim(person_ptr%name) /= "" .or. trim(person_ptr%sex) /= "" &
    .or. person_ptr%age /= -1) then
        nerror = nerror + 1
        write(*,*) "Error remove_info is not correct"
    endif
    if (nerror == 0) then
        write(*,*) "Passed: person%remov_info"
    endif

    nerror_temp = nerror
    call person_ptr%set_info("xiao ming father","boy",33)
    if(trim(person_ptr%name) == "xiao ming father") then
        nerror = nerror + 1
        Write(*,*) "Error sex test error"
    endif

    call person_ptr%remove_info()
    call person_ptr%set_info("xiao ming father","man",33)
    if(trim(person_ptr%name) /= "xiao ming father") then
        nerror = nerror + 1
        write(*,*) "Error sex test error"
    endif

    call person_ptr%remove_info()
    call person_ptr%set_info("xiao ming mother","woman",33)
    if(trim(person_ptr%name) /= "xiao ming mother")then
        nerror = nerror + 1
        Write(*,*) "Error sex test error"
    endif
    
    call person_ptr%remove_info()
    call person_ptr%set_info("xiao ming father","man",-33)
    if(trim(person_ptr%name) == "xiao ming father") then
        nerror = nerror + 1
        write(*,*) "Error age<0 test error"
    endif 

    call person_ptr%remove_info()
    call person_ptr%set_info("xiao ming father","man",151)
    if(trim(person_ptr%name) == "xiao ming father") then
        nerror = nerror + 1
        write(*,*) "Error age>150 test error"
    endif
    
    if(nerror_temp == nerror) then
        write(*,*) "Passed: person%set_info(...)"
    endif

    nerror_temp = nerror
    call person_ptr%remove_info()
    call person_ptr%set_info("xiao ming","man",3)
    call person_ptr%print_my_duty()

    if (nerror == nerror_temp) then ! ???? I think it is not correct
        write(*,*) "Passed: person%print_my_duty"
    endif

    nerror_temp = nerror
    deallocate(person_ptr)
    allocate(student :: person_ptr)
    call person_ptr%set_info("xiao hua mother","woman",33)
    call person_ptr%print_my_duty()

    if (nerror == nerror_temp) then ! ???? I think it is not correct
        write(*,*) "Passed: student%print_my_duty"
    endif

    nerror_temp = nerror
    deallocate(person_ptr)
    allocate(teacher :: person_ptr)
    call person_ptr%set_info("xiao hua mother","woman",33)
    call person_ptr%print_my_duty()

    if (nerror == nerror_temp) then ! ???? I think it is not correct
        write(*,*) "Passed: teacher%print_my_duty"
    endif

    if (nerror /= 0) then
        write(*,*) "There are errors in the code, please check!"
        stop 999
    else
        Write(*,*) "Test end"
    endif
ENDPROGRAM TestPerson
