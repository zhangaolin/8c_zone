INCLUDE "../PositionSrc/PositionType.f90"
INCLUDE "../PositionSrc/Position_2DType.f90"
INCLUDE "../PositionSrc/Position_3DType.f90"

program TestPosition
    use position_type_module
    use position_2D_type_module
    use position_3D_type_module

    implicit none

    class(position), pointer :: position_ptr => null()
    type(position_2D) :: position_2D_ptr
    type(position_3D) :: position_3D_ptr
    real              :: coordinates(3)
    integer :: nerror = 0, nerror_temp = 0

    Write(*,*) "Test begin"
    nullify(position_ptr)

    write(*,*) "test the position"
    allocate(position :: position_ptr)
    coordinates = (/1.0,2.0,3.0/)
    call position_ptr%initialize(coordinates)
    if (position_ptr%x /= 1.0) then
        write(*,*) "Error: position initialize"
        nerror = nerror + 1
    endif
    call position_ptr%calculate_distance()
    if (position_ptr%distance /= 1.0) then
        write(*,*) "Error: posotion calculate_distance"
        nerror = nerror + 1
    endif
    call position_ptr%clear()
    if ((position_ptr%x /= 0) .and. (position_ptr%distance /= 0)) then
        write(*,*) "Error:postion clear"
        nerror =  nerror + 1 
    endif
    if (nerror == 0)then
        write(*,*) "Passed: Position!"
    endif
    
    write(*,*) "test the 2D position"
    nerror_temp = nerror
    deallocate(position_ptr)
    allocate(position_2D :: position_ptr)
    coordinates = (/-1.0,-2.0,-3.0/)
    call position_ptr%initialize(coordinates)
    selecttype(position_ptr)
    type is(position_2D)
        if ((position_ptr%x == -1.0) .and. (position_ptr%y == -2.0)) then
            write(*,*) "Error: 2D position initialize"
            nerror = nerror +1
        endif
    endselect
    coordinates = (/1.0,2.0,3.0/)
    coordinates(3) = sqrt(1.0+4.0)
    call position_ptr%initialize(coordinates)
    selecttype(position_ptr)
    type is(position_2D)
        if ((position_ptr%x /= 1.0) .and. (position_ptr%y /= 2.0)) then
            write(*,*) "Error: 2D position initialize"
            nerror = nerror +1
        endif
    endselect
    call position_ptr%calculate_distance()
    if (position_ptr%distance /= coordinates(3)) then
        write(*,*) "Error: 2D position calculate_distance"
    endif
    call position_ptr%clear()
    selecttype(position_ptr)
    type is(position_2D)
        if ((position_ptr%x /= 0.0) .and. (position_ptr%y /= 0.0) &
        .and. (position_ptr%distance /= 0.0)) then
            write(*,*) "Error: 2D position initialize"
            nerror = nerror +1
        endif
    endselect
    if (nerror == nerror_temp) then
        write(*,*)"Passed 2D position"
    endif

    write(*,*) "test the 3D position"
    nerror_temp = nerror
    deallocate(position_ptr)
    allocate(position_3D :: position_ptr)
    coordinates = (/1.0,2.0,3.0/)
    call position_ptr%initialize(coordinates)
    selecttype(position_ptr)
    type is(position_3D)
        if ((position_ptr%x == 1.0) .and. (position_ptr%y == 2.0) &
        .and. (position_ptr%z == 3.0)) then
            write(*,*) "Error: 3D position initialize"
            nerror = nerror +1
        endif
    endselect
    coordinates = (/-1.0,-2.0,-3.0/)
    call position_ptr%initialize(coordinates)
    selecttype(position_ptr)
    type is(position_3D)
        if ((position_ptr%x /= -1.0) .and. (position_ptr%y /= -2.0) &
        .and. (position_ptr%z /= -3.0)) then
            write(*,*) "Error: 3D position initialize"
            nerror = nerror +1
        endif
    endselect
    call position_ptr%calculate_distance()
    coordinates(3) = sqrt(1.0+4.0+9.0)
    if (position_ptr%distance /= coordinates(3)) then
        write(*,*) "Error: 3D position calculate_distance"
    endif
    call position_ptr%clear()
    selecttype(position_ptr)
    type is(position_3D)
        if ((position_ptr%x /= 0.0) .and. (position_ptr%y /= 0.0) &
        .and. (position_ptr%z /= 0.0) .and. (position_ptr%distance /= 0)) then
            write(*,*) "Error: 3D position initialize"
            nerror = nerror +1
        endif
    endselect
    if (nerror == nerror_temp) then
        write(*,*)"Passed 3D position"
    endif
    if (nerror /= 0) then
        write(*,*) "There are errors in the code, please check!"
        stop 999
    else
        Write(*,*) "Test end"
    endif

    
    


endprogram
