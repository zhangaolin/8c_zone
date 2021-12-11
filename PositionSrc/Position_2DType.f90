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

subroutine initialize(this,x,y)
    class(position) :: this
    real            :: x
    real            :: y
    
    if ((x > 0) .and. (y > 0)) then
        this%x = x
        this%y = y
    else
        write(*,*) "the x and y is error! "
    endif
ENDSUBROUTINE
subroutine calculate_distance(this)
    class(position) :: this

    this%distance = sqrt((this%x - 0.0)*(this%x - 0.0) + &
    (this%y - 0.0)*(this%y - 0.0))
endsubroutine
subroutine clear(this)
    class(position) :: this

    this%x = 0.0
    this%y = 0.0
    this%distance = 0.0
endsubroutine   
    
end module position_2D_type_module