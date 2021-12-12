module shape_cylinder_type_module
    use shape_type_module
    implicit none
    public :: shape_cylinder 
    type, extends(shape) :: shape_cylinder
        real :: r = 0.0
        real :: z = 0.0
    
        contains 
        procedure, pass :: initialize
        procedure, pass :: get_volume_cylinder 
        procedure, pass :: clear_cylinder
    endtype
contains
    subroutine initialize(this,is_init,shape_num)
        class(shape_cylinder ) :: this
        logical :: is_init
        real, optional    :: shape_num(4) 
        if ((shape_num(1) > 0.0) .and. (shape_num(4) > 0.0)) then
            this%is_init = is_init
            this%r = shape_num(1)
            this%z = shape_num(4)
        else
            write(*,*) "The r is not correct"
        endif
    endsubroutine

    subroutine get_volume_cylinder(this,volume)
        class(shape_cylinder ), intent(in) :: this
        real, intent(out) :: volume
        if (this%is_init .eqv. .true.)then
            write(*,*) "is_init = true"
            volume = 3.14156*this%r*this%r*this%z
        else
            write(*,*) "is_init = false"
            volume = 0
        endif
    endsubroutine

    subroutine clear_cylinder(this)
        class(shape_cylinder) :: this
        
        this%is_init = .false.
        this%r = 0.0
        this%z = 0.0

    endsubroutine
    
end module shape_cylinder_type_module