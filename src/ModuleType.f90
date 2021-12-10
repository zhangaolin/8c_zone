MODULE person_type_module
    IMPLICIT NONE 
    PRIVATE

    PUBLIC :: person 

    TYPE :: person
      CHARACTER(len = 32) :: name = "" ! init the name
      CHARACTER(len = 12) :: sex = ""  ! init the sex
      INTEGER             :: age       ! init the age
    CONTAINS 
        PROCEDURE, PASS :: set_info
        PROCEDURE, PASS :: remove_info
        PROCEDURE, PASS :: print_my_duty
    ENDTYPE
    CONTAINS 
    SUBROUTINE print_my_duty(this)
        CLASS(person) :: this

        WRITE(*,*) "person I am ", this%name 
        WRITE(*,*) "person I do not know what I can do"
        WRITE(*,*)

    ENDSUBROUTINE

    SUBROUTINE set_info(this,name,sex,age)
        CLASS(person) :: this
        CHARACTER(len = *) :: name
        CHARACTER(len = *) :: sex
        INTEGER            :: age
        
        INTEGER :: nerror1 = 0
        nerror1 = 0
        IF(.not.(TRIM(sex) == "man" .or. TRIM(sex) == "woman")) THEN
            WRITE(*,*) "ERROR: sex is not man nor woman"
            nerror1 =  nerror1 + 1
        ENDIF

        IF(age<0 .or. age>150) THEN
            WRITE(*,*) "ERROR: age is less than 0 or large than 150"
            nerror1 = nerror1 + 1
        ENDIF

        IF(nerror1 == 0 ) THEN
            this%name = name
            this%sex = sex
            this%age = age
        ELSE
            this%name = ""
            this%sex = ""
            this%age = -1
            WRITE(*,*) "please check the inputs of set_info(), some wrong input is provided"
        ENDIF
    ENDSUBROUTINE

    SUBROUTINE remove_info(this)
        CLASS(person) :: this
        this%name =  ""
        this%sex = ""
        this%age = -1
    ENDSUBROUTINE

ENDMODULE person_type_module
        
MODULE student_type_module
    USE person_type_module

    IMPLICIT NONE
    PRIVATE

    PUBLIC :: student

    TYPE, extends(person) :: student
        CHARACTER(len = 10) :: dorm = "EAST20-206"
    CONTAINS
        PROCEDURE, PASS :: print_my_duty
        PROCEDURE, PASS :: remove_info
    ENDTYPE student

    CONTAINS
    
    SUBROUTINE print_my_duty(this)
        CLASS(student) :: this
        WRITE(*,*) "student my name is", this%name, "! My dorm room is in ", this%dorm
        WRITE(*,*) "student my duty is study! study!! study!!!"
        WRITE(*,*)
    ENDSUBROUTINE

    SUBROUTINE remove_info(this)
        CLASS(student) :: this
        this%name = ""
        this%sex = ""
        this%age = -1
        this%dorm = ""
    ENDSUBROUTINE


ENDMODULE student_type_module

MODULE teacher_type_module
    USE person_type_module
    IMPLICIT NONE
    PRIVATE

    PUBLIC :: teacher

    TYPE, EXTENDS(person) :: teacher
        CHARACTER(len = 32) :: office = "CHemical building 5th-floor"
    CONTAINS
        PROCEDURE, PASS :: print_my_duty
        PROCEDURE, PASS :: remove_info
    ENDTYPE teacher

    CONTAINS

    SUBROUTINE print_my_duty(this)
        CLASS(teacher) :: this

        WRITE(*,*) "teacher my name is ", this%name, "! My office is in ", this%office
        WRITE(*,*) "teacher my duty is teach! teach!! teach!!!"
    ENDSUBROUTINE

    SUBROUTINE remove_info(this)
        CLASS(teacher) :: this
        this%name = ""
        this%sex = ""
        this%age = -1
        this%office = ""
    ENDSUBROUTINE

ENDMODULE teacher_type_module
