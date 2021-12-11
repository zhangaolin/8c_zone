include "PositionType.f90"
include "Position_2DType.f90"

program Main
    use position_type_module
    use position_2D_type_module
    use position_3D_type_module

    implicit none

    class(position), pointer :: position_ptr => null()
    type(position_2D) :: position_2D_ptr
    type(position_3D) :: position_3D_ptr
    real              :: coordinates(3)

    write(*,*) "========== main begin =========="
    nullify(position_ptr)

    write(*,*) "========== position =========="
    allocate(position :: position_ptr) 
    coordinates(1) = 1.0
    call position_ptr%initialize(coordinates)
    call position_ptr%calculate_distance()
    write(*,*) "the position x is ", position_ptr%x
    write(*,*) "the position distance is ", position_ptr%distance
    call position_ptr%clear()
    deallocate(position_ptr)

    
    write(*,*) "========== position_2D =========="
    allocate(position_2D  :: position_ptr)
    coordinates = (/1.0, 1.0, 0.0/)
    call position_ptr%initialize(coordinates)
    call position_ptr%calculate_distance()
    write(*,*) "the position x is ", position_ptr%x
    selecttype(position_ptr)
    type is(position_2D)
        write(*,*) "the position y is ", position_ptr%y
    endselect
    write(*,*) "the position distance is ", position_ptr%distance
    call position_ptr%clear()
    deallocate(position_ptr)

    write(*,*) "========== position_3D =========="
    allocate(position_3D  :: position_ptr)
    coordinates = (/-1.0, -1.0, -1.0/)
    call position_ptr%initialize(coordinates)
    call position_ptr%calculate_distance()
    write(*,*) "the position x is ", position_ptr%x
    selecttype(position_ptr)
    type is(position_3D)
        write(*,*) "the position y is ", position_ptr%y
        write(*,*) "the position z is ", position_ptr%z
    endselect
    write(*,*) "the position distance is ", position_ptr%distance
    call position_ptr%clear()
    deallocate(position_ptr)
    
end program Main