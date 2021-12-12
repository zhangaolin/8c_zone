

program Main
    use shape_rectangular_type_module
    use shape_circle_type_module
    use shape_cylinder_type_module
    use shape_type_module
    implicit none

    class(shape), pointer :: shape_ptr => null()
    real              :: shape_num(4)
    real              :: volume1

    write(*,*) "========== main begin =========="
    nullify(shape_ptr)

    write(*,*) "========== position =========="
    allocate(shape :: shape_ptr)
    call shape_ptr%initialize(.false.)
    write(*,*) shape_ptr%is_init
    call shape_ptr%clear()
    deallocate(shape_ptr)
    
    write(*,*) "circle"
    allocate(shape_circle :: shape_ptr)
    shape_num(1) = 1.0
    call shape_ptr%initialize(.true.,shape_num)
    selecttype(shape_ptr)
    type is(shape_circle)
        call shape_ptr%get_volume_circle(volume1)
    endselect
    write(*,*) volume1
    selecttype(shape_ptr)
    type is(shape_circle)
        call shape_ptr%clear_circle()
    endselect
    deallocate(shape_ptr)

    write(*,*) "shape_rectangular_type_module"
    allocate(shape_rectangular :: shape_ptr)
    shape_num(2) = 1.0
    shape_num(3) = 2.0
    call shape_ptr%initialize(.true.,shape_num)
    selecttype(shape_ptr)
    type is(shape_rectangular)
        call shape_ptr%get_volume_rectangular(volume1)
    endselect
    write(*,*) volume1
    selecttype(shape_ptr)
    type is(shape_rectangular)
        call shape_ptr%clear_rectangular()
    endselect
    deallocate(shape_ptr)

    write(*,*) "shape_cylinder_type_module"
    allocate(shape_cylinder  :: shape_ptr)
    shape_num(4) = 2.0
    call shape_ptr%initialize(.true.,shape_num)
    selecttype(shape_ptr)
    type is(shape_cylinder )
        call shape_ptr%get_volume_cylinder(volume1)
    endselect
    write(*,*) volume1
    selecttype(shape_ptr)
    type is(shape_cylinder)
        call shape_ptr%clear_cylinder()
    endselect
    
    deallocate(shape_ptr)
    write(*,*) "main end"


end program Main