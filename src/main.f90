include "1_FortranBase.f90"
include "2_DayOfYear.f90"
include "3_type.f90"

PROGRAM Main
    CALL FortranBase()
    CALL DayOfYear()
    CALL TypeInClass()
    write(*,*) "Hello, world!"
    DO
        READ(*,*)
        EXIT
    ENDDO
ENDPROGRAM
   