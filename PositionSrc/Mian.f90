include "PositionType.f90"
program Main
    use position_type_module

    implicit none

    class(position), pointer :: position_ptr => null()

    write(*,*) "========== main begin =========="
    nullify(position_ptr)

    write(*,*) "========== position =========="
    allocate(position :: position_ptr) 
    call position_ptr%initialize(-1.0)
    call position_ptr%calculate_distance()
    write(*,*) "the position x is ", position_ptr%x
    write(*,*) "the position distance is ", position_ptr%distance
    call position_ptr%clear()
    deallocate(position_ptr)
    
end program Main