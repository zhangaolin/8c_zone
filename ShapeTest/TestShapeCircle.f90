include "../src/ShapeType.f90"
include "../src/ShapeCircleType.f90"
include "../src/ShapeRectangularType.f90"
include "../src/ShapeCylinderType.f90"

program TestShapeCircle
    use shape_type_module
    use shape_circle_type_module
    implicit none
    class(shape), pointer :: shape_ptr => null()
    real              :: shape_num(4)
    real              :: volume
    integer :: nerror = 0, nerror_temp = 0

    Write(*,*) "Test begin"
    nullify(shape_ptr)

    write(*,*) "circle"
    allocate(shape_circle :: shape_ptr)
    shape_num(1) = 1.0
    call shape_ptr%initialize(.true.,shape_num)
    selecttype(shape_ptr)
    type is(shape_circle)
        if ((shape_ptr%is_init .eqv. .false.) .and. &
        (shape_ptr%r /= 1.0)) then
            write(*,*) "Error circle shape initialize"
            nerror = nerror + 1
        else
            write(*,*) "Passed: circle shape initialize"
        endif 
    endselect

    selecttype(shape_ptr)
    type is(shape_circle)
        call shape_ptr%get_volume_circle(volume)
        shape_num(2) = 3.14156 * shape_num(1) * shape_num(1)
        if (volume /= shape_num(2)) then
            write(*,*) "Error circle shape get_volume"
            nerror = nerror + 1
        else 
            write(*,*) "Passed: circle shape get_volume"
        endif
    endselect

    shape_num(1) = -1.0
    call shape_ptr%initialize(.true.,shape_num)
    selecttype(shape_ptr)
    type is(shape_circle)
        if ((shape_ptr%is_init .eqv. .false.) .and. &
        (shape_ptr%r == -1.0)) then
            write(*,*) "Error circle shape initialize"
            nerror = nerror + 1
        else
            write(*,*) "Passed: circle shape initialize"
        endif 
    endselect

    selecttype(shape_ptr)
    type is(shape_circle)
        call shape_ptr%clear_circle()
        if ((shape_ptr%is_init .eqv. .true.) .and. &
        (shape_ptr%r /= 0.0)) then
            write(*,*) "Error circle shape initialize"
            nerror = nerror + 1
        else
            write(*,*) "Passed: circle shape initialize"
        endif 
    endselect

    deallocate(shape_ptr)
    if (nerror == 0.0) then
        write(*,*) "Passed!"
    else
        write(*,*) "Error!"
        stop 999
    endif

    
end program 