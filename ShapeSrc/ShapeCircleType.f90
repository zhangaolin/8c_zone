module shape_circle_type_module
    use shape_type_module
    implicit none
    public :: shape_circle
    type, extends(shape) :: shape_circle 
        real :: r = 0.0
    contains 
        procedure, pass :: initialize
        procedure, pass :: get_volume_circle 
        procedure, pass :: clear_circle
    endtype
contains
    subroutine initialize(this,is_init,shape_num)
        class(shape_circle) :: this
        logical :: is_init
        real, optional    :: shape_num(4) 
        if (shape_num(1) > 0.0) then
            this%is_init = is_init
            this%r = shape_num(1)
        else
            write(*,*) "The r is not correct"
        endif
    endsubroutine

    subroutine get_volume_circle(this,volume)
        class(shape_circle), intent(in) :: this
        real, intent(out) :: volume
        if (this%is_init .eqv. .true.)then
            write(*,*) "is_init = true"
            volume = 3.14156*this%r*this%r
        else
            write(*,*) "is_init = false"
            volume = 0
        endif
    endsubroutine

    subroutine clear_circle(this)
        class(shape_circle) :: this
        
        this%is_init = .false.
        this%r = 0

    endsubroutine

end module shape_circle_type_module