MODULE module_example
    implicit none
    real :: x = 120
    real :: y = 200
ENDMODULE

SUBROUTINE sub1(i,j)
    implicit none
    INTEGER,intent(inout) :: i,j
    INTEGER :: array(5)
    WRITE(*,*) "This a scope function!"
    WRITE(*,*) "6nd",i, j
    WRITE(*,*)
    CALL sub2()
    WRITE(*,*) "7nd",i, j
    WRITE(*,*)
    
    array = (/(1000*i,i = 1,5)/)
    WRITE(*,*) "8nd",i,j,array
    WRITE(*,*)
    
    CONTAINS
    
      SUBROUTINE sub2
        REAL :: i
        i = 1000
        j = 2000
        WRITE(*,*) "4nd", i, j
        WRITE(*,*)
      ENDSUBROUTINE
    
ENDSUBROUTINE
    
SUBROUTINE ScpingTest()
    USE module_example
    IMPLICIT NONE
    
    INTEGER :: i = 1, j = 2
    WRITE(*,*) "1st ",i ,j ,x, y
    WRITE(*,*)
    
    CALL sub1(i,j)
    WRITE(*,*) "2nd",i, j ,x,y
    WRITE(*,*)
    
    CALL sub2()
    WRITE(*,*) "3nd", i, j, x, y
    WRITE(*,*)
    WRITE(*,*)
    
    CONTAINS
    
       SUBROUTINE sub2()
          REAL :: x
          x = 1000
          y = 2000
          WRITE(*,*) "4nd", x, y
          WRITE(*,*)
       ENDSUBROUTINE

ENDSUBROUTINE