include "../src/ShapeType.f90"
include "../src/ShapeCircleType.f90"
include "../src/ShapeRectangularType.f90"
include "../src/ShapeCylinderType.f90"

program TestShapeRectangular
    use shape_type_module
    use shape_rectangular_type_module
    implicit none
    class(shape), pointer :: shape_ptr => null()
    real              :: shape_num(4)
    real              :: volume
    integer :: nerror = 0, nerror_temp = 0

    Write(*,*) "Test begin"
    nullify(shape_ptr)

    write(*,*) "shape_rectangular"
    allocate(shape_rectangular :: shape_ptr)
    shape_num(2) = 1.0
    shape_num(3) = 2.0
    call shape_ptr%initialize(.true.,shape_num)
    selecttype(shape_ptr)
    type is(shape_rectangular)
        if ((shape_ptr%is_init .eqv. .false.) .and. &
        (shape_ptr%x /= 1.0) .and. (shape_ptr%y /= 2.0)) then
            write(*,*) "Error shape_rectangular initialize"
            nerror = nerror + 1
        else
            write(*,*) "Passed: shape_rectangular initialize"
        endif 
    endselect

    selecttype(shape_ptr)
    type is(shape_rectangular)
        call shape_ptr%get_volume_rectangular(volume)
        shape_num(1) = shape_num(2) * shape_num(3)
        if (volume /= shape_num(1)) then
            write(*,*) "Error shape_rectangular get_volume"
            nerror = nerror + 1
        else 
            write(*,*) "Passed: shape_rectangular get_volume"
        endif
    endselect

    shape_num(2) = -1.0
    shape_num(3) = -2.0
    call shape_ptr%initialize(.true.,shape_num)
    selecttype(shape_ptr)
    type is(shape_rectangular)
        if ((shape_ptr%is_init .eqv. .false.) .and. &
        (shape_ptr%x == -1.0) .and. (shape_ptr%y == -2.0)) then
            write(*,*) "Error shape_rectangular initialize"
            nerror = nerror + 1
        else
            write(*,*) "Passed: shape_rectangular initialize"
        endif 
    endselect

    selecttype(shape_ptr)
    type is(shape_rectangular)
        call shape_ptr%clear_rectangular()
        if ((shape_ptr%is_init .eqv. .true.) .and. &
        (shape_ptr%x /= 0.0) .and. (shape_ptr%y /= 0.0)) then
            write(*,*) "Error shape_rectangular shape clear"
            nerror = nerror + 1
        else
            write(*,*) "Passed: shape_rectangular shape clear"
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