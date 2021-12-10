include "1_FortranBase.f90"
include "2_DayOfYear.f90"
include "3_Type.f90"
include "4_Scope.f90"
include "6_Pointer.f90"
include "7_Moudle.f90"

PROGRAM Main
    CALL FortranBase()
    CALL DayOfYear()
    CALL TypeInClass()
    CALL ScpingTest()
    CALL ModuleTest()
    write(*,*) "Hello, world!"
    DO
        READ(*,*)
        EXIT
    ENDDO
ENDPROGRAM
   