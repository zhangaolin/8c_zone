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

ENDMODULE operator_type_module