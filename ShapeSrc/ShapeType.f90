module shape_type_module
    implicit none
    PRIVATE
    public :: shape
    type :: shape
        logical :: is_init = .false.
    contains
        procedure, pass :: initialize
        procedure, pass :: clear
    endtype
contains
    subroutine initialize(this,is_init,shape_num)
        class(shape) :: this
        logical :: is_init
        real, optional    :: shape_num(4) 

        this%is_init = is_init
    ENDSUBROUTINE
    subroutine clear(this)
        class(shape) :: this
        this%is_init = .false.
    endsubroutine
end module shape_type_module