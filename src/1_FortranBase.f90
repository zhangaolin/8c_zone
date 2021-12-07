SUBROUTINE SortSingle(n,array)
    ! This is a subroutine about the bubble sort
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    REAL(4), INTENT(INOUT), DIMENSION(n) :: array
    
    INTEGER :: i,j
    REAL(4) :: temp
    DO i = n-1, 1, -1
        DO j = 1, i
            IF(array(j) > array(j+1)) THEN
                temp = array(j)
                array(j) = array(j + 1)
                array(j+1) = temp 
            ENDIF
        ENDDO
    ENDDO
ENDSUBROUTINE


SUBROUTINE FortranBase()
    ! This is a subroutine about the base use of fortran
    ! this part-1 is about the struct of FOrtran code
    ! this part-2 is about the type of dat in Fortran
    ! this part-3 is about the input and output in Fortran 
    ! this part-4 is about the IF-ENDIF in Fortran
    ! this part-5 is about the SELECTCASE in Fortran
    ! this part-6 is about the DO-CYCLE in Fortran
    ! this part-7 is about the Array in Fortran
    ! this part-8 is about the Subriutine in Fortran
    IMPLICIT NONE
    INTEGER :: i,j,n
    INTEGER :: int_array(5) = (/2,3,5,7,10/)
    INTEGER :: int_array2d(2,3)
    INTEGER, ALLOCATABLE :: int_alloc(:,:)
    REAL    :: r
    REAL(4) :: real_array(5) = (/11.0,9.0,7.0,3.0,1.0/)
    REAL(8) :: double_array(5) = (/11.0,9.0,7.0,3.0,1.0/)
    DOUBLE PRECISION :: d
    REAL :: d2
    CHARACTER :: s
    CHARACTER(LEN=10) :: sa
    CHARACTER(len=6)  :: s_array(2)
    CHARACTER(len=6), ALLOCATABLE :: s_array2d(:,:)
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
    WRITE(*,*)

    WRITE(*,*) "this part-5 is about the SELECTCASE in Fortran"
    WRITE(*,*) "Please enter a integer between 1-7:"
    READ(*,*) i

    SELECT CASE (i)
    CASE(1)
        WRITE(*,*) "Today is Monday"
    CASE(2)
        WRITE(*,*) "Today is Tuesday"
    CASE default
        WRITE(*,*) "......"
    ENDSELECT
    WRITE(*,*)

    WRITE(*,*) "this part-6 is about the DO-CYCLE in Fortran"
    DO i = 1, 10, 2
        WRITE(*,*) "i = ", i
    ENDDO
    WRITE(*,*)

    WRITE(*,*) "this part-7 is about the Array in Fortran"
    ! array operations
    do i = 1,5
        write(*,*) i, "walie: ", int_array(i), real_array(i), double_array(i)
    ENDDO
    ! Array assignment
    DO i = 1,3
        DO j =1,2
        int_array2d(j,i) = i + j
        ENDDO
    ENDDO
    ! MUtable array operations
    n = 3
    ALLOCATE(int_alloc(3,3))
    int_alloc = 1
    WRITE(*,*) "int_alloc:", int_alloc
    DEALLOCATE(int_alloc)

    n =2
    ALLOCATE(int_alloc(n,2))
    int_alloc = 2
    WRITE(*,*) "int_alloc", int_alloc
    DEALLOCATE(int_alloc)
    WRITE(*,*)

    WRITE(*,*) "this part-8 is about the Subriutine in Fortran"
    CALL SortSingle(5,real_array)
    WRITE(*,*)



ENDSUBROUTINE