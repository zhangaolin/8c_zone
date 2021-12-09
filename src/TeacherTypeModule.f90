include "PersonTypeModule.f90"
MODULE teaher_type_module
    USE person_type_module
    IMPLICIT NONE
    PRIVATE

    PUBLIC :: teacher

    TYPE, EXTENDS(person) :: teacher
        CHARACTER(len = 32) :: office = "CHemical building 5th-floor"
    CONTAINS
        PROCEDURE, PASS :: print_my_duty
    ENDTYPE teacher

    CONTAINS

    SUBROUTINE print_my_duty(this)
        CLASS(teacher) :: this

        WRITE(*,*) "teacher my name is ", this%name, "! My office is in ", this%office
        WRITE(*,*) "teacher my duty is teach! teach!! teach!!!"
    ENDSUBROUTINE

ENDMODULE