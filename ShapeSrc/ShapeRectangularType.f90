module shape_rectangular_type_module
    use shape_type_module
    implicit none
    public :: shape_rectangular
    type, extends(shape) :: shape_rectangular
        real :: x = 0.0
        real :: y = 0.0
    contains
        procedure,pass :: initialize 
        procedure, pass :: get_volume_rectangular 
        procedure, pass :: clear_rectangular
    endtype
contains
   subroutine initialize(this,is_init,shape_num)
       class(shape_rectangular) :: this
       logical :: is_init
       real, optional    :: shape_num(4) 
       if (((shape_num(2) > 0.0) .and. (shape_num(3) > 0.0))) then
           this%is_init = is_init
           this%x = shape_num(2)
           this%y = shape_num(3)
       else
           write(*,*) "The x and y is not correct"
       endif
   endsubroutine
   
   subroutine get_volume_rectangular(this,volume)
       class(shape_rectangular), intent(in) :: this
       real, intent(out) :: volume
       if (this%is_init .eqv. .true.)then
           write(*,*) "is_init = true"
           volume = this%x*this%y
       else
           write(*,*) "is_init = false"
           volume = 0.0
       endif
   endsubroutine
   
   subroutine clear_rectangular(this)
       class(shape_rectangular) :: this
       
       this%is_init = .false.
       this%x = 0.0
       this%y = 0.0
   
   endsubroutine
    
end module shape_rectangular_type_module