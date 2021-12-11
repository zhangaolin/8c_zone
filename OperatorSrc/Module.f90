MODULE operator_type_module
    IMPLICIT NONE 
    PRIVATE

    PUBLIC :: operator

    TYPE :: operator
      REAL        :: x = 0.0 ! init the name
      REAL        :: y = 0.0  ! init the sex
      INTEGER     :: age       ! init the age
    CONTAINS 
        PROCEDURE, PASS :: set_info
        PROCEDURE, PASS :: remove_info
        PROCEDURE, PASS :: print_my_duty
    ENDTYPE

ENDMODULE operator_type_module