
module position_3D_type_module
    use position_2D_type_module
    implicit none
    PRIVATE

    public :: position_3D 

    type, extends(position_2D) :: position_3D 
        real :: z
    contains
        procedure, pass :: initialize
        procedure, pass :: calculate_distance
        procedure, pass :: clear
    endtype
    
contains

subroutine initialize(this,coordinates)
    class(position_3D) :: this
    real            :: coordinates(3)
    
    if ((coordinates(1) < 0) .and. (coordinates(2) < 0) &
    .and. (coorinates(3) < 0)) then
        this%x = coordinates(1)
        this%y = coordinates(2)
        this%z = coordinates(3)
    else
        write(*,*) "the x and y and z is error! "
    endif
ENDSUBROUTINE
subroutine calculate_distance(this)
    class(position_3D) :: this

    this%distance = sqrt((this%x - 0.0)*(this%x - 0.0) + &
    (this%y - 0.0)*(this%y - 0.0) + (this%z-0.0)*(this%z-0.0))
endsubroutine
subroutine clear(this)
    class(position_3D) :: this

    this%x = 0.0
    this%y = 0.0
    this%z = 0.0
    this%distance = 0.0
endsubroutine   
    
end module position_3D_type_module