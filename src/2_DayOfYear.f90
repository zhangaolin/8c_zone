SUBROUTINE CalculateeDays(year,month,day,days_of_year)
    ! This is a subroutine calcluate the day of this year and this month
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: year,month,day
    INTEGER, INTENT(OUT) :: days_of_year
    INTEGER :: i
    INTEGER :: days_of_month(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)

    days_of_year = 0
    ! Determine if it's a leap year
    IF (MOD(year,4) == 0) THEN
        WRITE(*,*) year, " is a leap year"
        days_of_month(2) = 29
    ELSE 
        WRITE(*,*) year, "is not a leap year"
    ENDIF
    ! calculate the day of year
    IF (month == 1) THEN
        days_of_year = day
    ELSE 
        DO i = 1, month -1, 1
            days_of_year = days_of_year + days_of_month(i)
        ENDDO
        days_of_year = days_of_year + day
    ENDIF
        
ENDSUBROUTINE


SUBROUTINE DayOfYear()
    IMPLICIT NONE
    INTEGER :: year, month, day,days_of_year

    year = 1
    month = 1
    day = 1
    days_of_year = 0
    ! determine the data 
    WRITE(*,*) "please input the year:"
    READ(*,*) year
    WRITE(*,*)"please input the month:"
    READ(*,*) month
    WRITE(*,*) "please input the day"
    READ(*,*) day
    ! determine the data is correct
    IF (year < 1) THEN
        STOP
    ELSEIF (month < 1) THEN
        STOP
    ELSEIF (day < 1) THEN
        STOP
    ENDIF

    CALL CalculateeDays(year,month,day,days_of_year)
    ! the data output
    WRITE(*,*) "The day of this year is ", days_of_year
    WRITE(*,*)

ENDSUBROUTINE
