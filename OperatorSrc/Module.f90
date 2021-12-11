MODULE operator_type_module
    IMPLICIT NONE 
    PRIVATE

    PUBLIC :: operator

    TYPE :: operator
      REAL        :: x = 0.0       ! init the x
      REAL        :: y = 0.0       ! init the y
      REAL        :: result = 0.0  ! init the result
    CONTAINS 
        PROCEDURE, PASS :: set_info
        PROCEDURE, PASS :: solve
        PROCEDURE, PASS :: print_result
        PROCEDURE, PASS :: clear
    ENDTYPE
    CONTAINS 
    SUBROUTINE print_result(this)
        CLASS(operator) :: this

        WRITE(*,*) "the result is ", this%result

    ENDSUBROUTINE
    subroutine set_info(this,x,y)
        CLASS(operator) :: this
        REAL        :: x      ! init the x
        REAL        :: y

        this%x = x
        this%y = y
    ENDSUBROUTINE
    subroutine solve(this,method)
        CLASS(operator) :: this
        CHARACTER :: method
        
        if (method == "+")then
            this%result = this%x + this%y
        elseif (method == "-")then
            this%result = this%x - this%y
        elseif (method == "*")then
            this%result = this%x * this%y
        elseif (method == "/")then
            this%result = this%x / this%y
        else
            write(*,*) "The method is error!"
        endif
    ENDSUBROUTINE
    subroutine clear(this)
        CLASS(operator) :: this
        this%x = 0.0
        this%y = 1.0
        this%result = 0.0
    ENDSUBROUTINE

ENDMODULE operator_type_module

module operator_plus_type_module
    use operator_type_module
    implicit none
    PRIVATE

    PUBLIC :: operator_plus

    TYPE, extends(operator) :: operator_plus
        
    CONTAINS
        PROCEDURE, PASS :: set_info
    ENDTYPE operator_plus

    CONTAINS

    subroutine set_info(this,x,y)
        CLASS(operator_plus) :: this
        REAL        :: x      ! init the x
        REAL        :: y
        if((x>0) .and. (y>0))then
            this%x = x
            this%y = y
        else
            write(*,*) "the plus calss is need the x and y > 0"
        endif    
    ENDSUBROUTINE

    
end module operator_plus_type_module

module operator_devide_type_module
    use operator_type_module
    implicit none
    PRIVATE

    PUBLIC :: operator_devide

    TYPE, extends(operator) :: operator_devide
        
    CONTAINS
        PROCEDURE, PASS :: set_info
    ENDTYPE operator_devide

    CONTAINS

    subroutine set_info(this,x,y)
        CLASS(operator_devide) :: this
        REAL        :: x      ! init the x
        REAL        :: y
        if((x /= 0) .and. (y /= 0))then
            this%x = x
            this%y = y
        else
            write(*,*) "the devide calss is need the x and y != 0"
        endif    
    ENDSUBROUTINE
    
end module operator_devide_type_module