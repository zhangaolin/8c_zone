include "ModuleType.f90"
    
SUBROUTINE ModuleTest()
    use person_type_module
    use student_type_module
    use teacher_type_module

    implicit NONE

    class(person), pointer :: person_ptr => null()

    type(person), pointer :: per1 => null()
    type(student), pointer :: stu1 => null()
    type(teacher), pointer :: tea1 => null()

    Write(*,*) "========== type allocate =========="
    allocate(per1)
    call per1%set_info("xiao ming","man",3)
    call per1%print_my_duty()
    

    allocate(stu1)
    call stu1%set_info("small hua","woman",19)
    call stu1%print_my_duty()
    

    allocate(tea1)
    call tea1%set_info("lao liu","man",45)
    call tea1%print_my_duty()
    

    Write(*,*) "========== class point to type =========="
    person_ptr => per1
    call person_ptr%print_my_duty()

    person_ptr => stu1
    call person_ptr%print_my_duty()

    person_ptr => tea1
    call person_ptr%print_my_duty()

    deallocate(per1)
    deallocate(stu1)
    deallocate(tea1)

    Write(*,*) "========== class allocated to be a specified type =========="
    nullify(person_ptr)

    allocate(person :: person_ptr)
    call person_ptr%set_info("xiao ming father","man",33)
    call person_ptr%print_my_duty()
    deallocate(person_ptr)

    allocate(student :: person_ptr)
    call person_ptr%set_info("xiao hua monther","woman",49)
    call person_ptr%print_my_duty()
    deallocate(person_ptr)

    allocate(teacher :: person_ptr)
    call person_ptr%set_info("lao liu workmater","man",45)
    call person_ptr%print_my_duty()
    deallocate(person_ptr)

ENDSUBROUTINE