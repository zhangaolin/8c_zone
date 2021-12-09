include "PersonTypeModule.f90"
MODULE student_type_module
    USE person_type_moudle

    IMPLICIT NONE
    PRIVATE

    PUBLIC :: student

    TYPE, extends(person) :: student
        CHARACTER(len = 10) :: dorm = "EAST20-206"
    CONTAINS
        PROCEDURE, PASS :: print_my_duty
    ENDTYPE student

    CONTAINS
    
    SUBROUTINE print_my_duty(this)
        CLASS(student) :: this
        WRITE(*,*) "student my name is", this%name, "! My dorm room is in ", this%dorm
        WRITE(*,*) "student my duty is study! study!! study!!!"
        WRITE(*,*)
    ENDSUBROUTINE
ENDMODULE student_type_module