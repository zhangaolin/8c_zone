MODULE person_type_moudle
    IMPLICIT NONE 
    PRIVATE

    PUBLIC :: person 

    TYPE :: person
      CHARACTER(len = 12) :: name = ""
      CHARACTER(len = 12) :: sex = ""
      INTEGER             :: age
    CONTAINS 
        PROCEDURE, PASS :: set_info
        PROCEDURE, PASS :: print_my_duty
    ENDTYPE
    
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
        
        INTEGER :: nerror = 0
        
        IF(.not.(TRIM(sex) == "man" .or. TRIM(sex) == "woman")) THEN
            WRITE(*,*) "ERROR: sex is not man nor woman"
            nerror =  nerror + 1
        ENDIF

        IF(age<0 .or. age>150) THEN
            WRITE(*,*) "ERROR: age is less than 0 or large than 150"
            nerror = nerror + 1
        ENDIF

        IF(nerror == 0 ) THEN
            this%name = name
            this%sex = sex
            this%age = age
        ELSE
            WRITE(*,*) "please check the inputs of set_info(), some wrong input is provided"
            STOP 999
        ENDIF
    ENDSUBROUTINE
ENDMODULE
        
        