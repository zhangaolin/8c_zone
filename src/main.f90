include "1_FortranBase.f90"
include "2_DayOfYear.f90"

PROGRAM Main
    CALL FortranBase()
    CALL DayOfYear()
    write(*,*) "Hello, world!"
ENDPROGRAM
   