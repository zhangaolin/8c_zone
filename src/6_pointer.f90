SUBROUTINE pointer_test()
    implicit none
    
    real, pointer :: p1=> null()
    real, pointer :: p2=> null()
    real, pointer :: p3=> null()
    integer,pointer :: ptr1(:) => null()
    integer,pointer :: ptr2(:) => null()
    real, target :: t1 = 10.0,t2 = 17.0, t3
    logical :: status
    integer :: i
    
    p1 => null()
    p2 => null()
    
    status = associated(p1)
    write(*,*) "status = ",status
    
    p1 => t1
    write(*,*) "p1",p1
    
    nullify(p1)
    write(*,*) "status", associated(p1)
    
    write(*,*)"======================Pointerֵ======================"
    
    p1 => t1
    p2 => p1
    write(*,*) "p1,p2,t1,t2",p1,p2,t1,t2
    write(*,*)
    
    p1 => t2
    write(*,*)"p1,p2,t1,t2",p1,p2,t1,t2
    write(*,*)
    
    write(*,*)"=================Pointer================="
    write(*,*)"status", associated(p1)
    write(*,*)"status", associated(p2)
    write(*,*)"status", associated(p1,t1)
    write(*,*)
    
    write(*,*)"=================NULLFY================="
    nullify(p1)
    write(*,*)"status",associated(p1)
    nullify(p2)
    write(*,*)"status",associated(p2)
    
    write(*,*)"================ָPointer================"
    
    nullify(p1,p2)
    p1 => t1
    p2 => t2
    p3 => t3
    p3 = p1 + p2
    write(*,*)"p3",p3
    p1 => t2
    p3 = p1 + p2
    write(*,*)"p3",p3
    
    allocate(ptr1(1:10))
    do i = 1, 10
        ptr1(i) = i
    enddo
    
    ptr2 => ptr1
    
    deallocate(ptr1)
    
    write(*,*)"associated(ptr1,ptr2)",associated(ptr1,ptr2)
    
    
ENDSUBROUTINE