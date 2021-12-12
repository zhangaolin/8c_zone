include "../src/ShapeType.f90"
include "../src/ShapeCircleType.f90"
include "../src/ShapeRectangularType.f90"
include "../src/ShapeCylinderType.f90"

program TestShapeCylinder
    use shape_type_module
    use shape_cylinder_type_module
    implicit none
    class(shape), pointer :: shape_ptr => null()
    real              :: shape_num(4)
    real              :: volume
    integer :: nerror = 0, nerror_temp = 0

    Write(*,*) "Test begin"
    nullify(shape_ptr)

    write(*,*) "shape_cylinder"
    allocate(shape_cylinder :: shape_ptr)
    shape_num(1) = 1.0
    shape_num(4) = 2.0
    call shape_ptr%initialize(.true.,shape_num)
    selecttype(shape_ptr)
    type is(shape_cylinder)
        if ((shape_ptr%is_init .eqv. .false.) .and. &
        (shape_ptr%r /= 1.0) .and. (shape_ptr%z /= 2.0)) then
            write(*,*) "Error shape_cylinder initialize"
            nerror = nerror + 1
        else
            write(*,*) "Passed: shape_cylinder initialize"
        endif 
    endselect

    selecttype(shape_ptr)
    type is(shape_cylinder)
        call shape_ptr%get_volume_cylinder(volume)
        shape_num(2) = 3.14156 * shape_num(1) * shape_num(1) * shape_num(4)
        if (volume /= shape_num(2)) then
            write(*,*) "Error shape_cylinder get_volume"
            nerror = nerror + 1
        else 
            write(*,*) "Passed: shape_cylinder get_volume"
        endif
    endselect

    shape_num(1) = -1.0
    shape_num(4) = -2.0
    call shape_ptr%initialize(.true.,shape_num)
    selecttype(shape_ptr)
    type is(shape_cylinder)
        if ((shape_ptr%is_init .eqv. .false.) .and. &
        (shape_ptr%r == -1.0) .and. (shape_ptr%z == -2.0)) then
            write(*,*) "Error shape_cylinder initialize"
            nerror = nerror + 1
        else
            write(*,*) "Passed: shape_cylinder initialize"
        endif 
    endselect

    selecttype(shape_ptr)
    type is(shape_cylinder)
        call shape_ptr%clear_cylinder()
        if ((shape_ptr%is_init .eqv. .true.) .and. &
        (shape_ptr%r /= 0.0) .and. (shape_ptr%z /= 0.0)) then
            write(*,*) "Error shape_cylinder shape clear"
            nerror = nerror + 1
        else
            write(*,*) "Passed: shape_cylinder shape clear"
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