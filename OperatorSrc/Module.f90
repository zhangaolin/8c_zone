MODULE operator_type_module
    IMPLICIT NONE 
    PRIVATE

    PUBLIC :: operator

    TYPE :: operator
      REAL        :: x = 0.0       ! init the x
      REAL        :: y = 0.0       ! init the y
      INTEGER     :: result = 0.0  ! init the result
    CONTAINS 
        PROCEDURE, PASS :: set_info
        PROCEDURE, PASS :: solve
        PROCEDURE, PASS :: print_result
        PROCEDURE, PASS :: clear
    ENDTYPE
    CONTAINS 
    SUBROUTINE print_result(this)
        CLASS(opeator) :: this

        WRITE(*,*) "the result is ", this%reslut

    ENDSUBROUTINE
    subroutine set_info(this,x,y)
        CLASS(operator) :: this
        REAL        :: x = 0.0       ! init the x
        REAL        :: y = 0.0 

        this%x = x
        this%y = y
    ENDSUBROUTINE

ENDMODULE operator_type_module