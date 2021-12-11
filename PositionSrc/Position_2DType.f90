include "PositionType.f90"
module position_2D_type_module
    use position_type_module
    implicit none
    PRIVATE

    public :: position_2D 

    type, extends(position) :: position_2D 
        real :: y = 0.0
    contains
        procedure, pass :: initialize
        procedure, pass :: calculate_distance
        procedure, pass :: clear
    endtype
    
contains

subroutine initialize(this,coordinates)
    class(position_2D) :: this
    real            :: coordinates(3)
    
    if ((coordinates(1) > 0) .and. (coordinates(2) > 0)) then
        this%x = coordinates(1)
        this%y = coordinates(2)
    else
        write(*,*) "the x and y is error! "
    endif
ENDSUBROUTINE
subroutine calculate_distance(this)
    class(position_2D) :: this

    this%distance = sqrt((this%x - 0.0)*(this%x - 0.0) + &
    (this%y - 0.0)*(this%y - 0.0))
endsubroutine
subroutine clear(this)
    class(position_2D) :: this

    this%x = 0.0
    this%y = 0.0
    this%distance = 0.0
endsubroutine   
    
end module position_2D_type_module