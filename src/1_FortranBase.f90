SUBROUTINE FortranBase()
! This is a subroutine about the base use of fortran
! this part-1 is about the struct of FOrtran code
! this part-2 is about the type of dat in Fortran
! this part-3 is about the input and output in Fortran 
! this part-4 is about the IF-ENDIF in Fortran
    IMPLICIT NONE
    INTEGER :: i
    REAL    :: r
    DOUBLE PRECISION :: d
    REAL :: d2
    CHARACTER :: s
    CHARACTER(LEN=10) :: sa
    LOGICAL :: b
    ! part-1
    WRITE(*,*)"this part-1 is about the struct of FOrtran code"
    i = 1
    i = i + 1
    WRITE(*,*) i
    WRITE(*,*)

    ! part-2
    WRITE(*,*)"this part-2 is about the type of dat in Fortran"
    i = 1
    r = 1.2
    d = 1.2d0
    d2 = 1.3d0 
    s = "sa"
    s = 'sa'
    sa = "sa"
    b = .TRUE.

    ! Arithmetic operation
    i = i + 1

    !Integer arithmetic
    i = 3/4

    ! data output
    WRITE(*,*) "INTEGER:", i
    WRITE(*,*) "REAL   :", r 
    WRITE(*,*) "DOUBLE :", d 
    WRITE(*,*) "REAL(8):", d2
    WRITE(*,*) "CHARACTER:", s 
    WRITE(*,*) "CHARACTER(LEN=10):", sa 
    WRITE(*,*) "LOGICAL:", b
    WRITE(*,*)

    WRITE(*,*) "this part-3 is about the input and output in Fortran "
    WRITE(*,*) "Please input a integer:"
    READ(*,*) i
    WRITE(*,*) "The input is: ", i
    WRITE(*,*)

    WRITE(*,*) "! this part-4 is about the IF-ENDIF in Fortran"
    if (i > 1) THEN
        WRITE(*,*) "i = ", i, ", is gerater than 1!"
    ELSEIF(i < 0) THEN
        WRITE(*,*) "i = ", i, ", is less than 0!"
    ELSE 
        WRITE(*,*) "i = ", i, ", is between [0,1]"
    ENDIF 


ENDSUBROUTINE