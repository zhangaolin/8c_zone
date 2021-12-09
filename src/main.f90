include "1_FortranBase.f90"
include "2_DayOfYear.f90"
include "3_Type.f90"
include "4_Scope.f90"

PROGRAM Main
    CALL FortranBase()
    CALL DayOfYear()
    CALL TypeInClass()
    CALL ScpingTest()
    write(*,*) "Hello, world!"
    DO
        READ(*,*)
        EXIT
    ENDDO
ENDPROGRAM
   