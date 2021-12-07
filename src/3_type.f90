SUBROUTINE TypeInClass()
    IMPLICIT NONE
    TYPE :: vector_type
        REAL :: x
        REAL :: y
    ENDTYPE
    
    TYPE,EXTENDS(vector_type) :: vector3D_type
        real :: z
    ENDTYPE
    
    CLASS(vector_type), ALLOCATABLE :: v
    TYPE(vector_type) :: v2D
    TYPE(vector3D_type) :: v3D
    CHARACTER :: s
    
    v2D = vector_type(1,2)
    WRITE(*,*) "v2D", v2D
    
    v3D = vector3D_type(2,3,4)
    WRITE(*,*) "v3D", v3D
    
    ALLOCATE(vector_type :: v)
    SELECTTYPE(v) !-----
    TYPE IS(vector_type)
        v = v2D
    ENDSELECT
    
    WRITE(*,*) "v", v%x, v%y
    
    DEALLOCATE(v)
    ALLOCATE(vector3D_type :: v)
    SELECTTYPE(v)
    TYPE IS(vector3D_type)
        v = v3D
    ENDSELECT
    
    WRITE(*,*) "v:", v%x,v%y
    SELECTTYPE(v)
    TYPE IS(vector3D_type)
        WRITE(*,*) "v%z:", v%z
    ENDSELECT
    
    DEALLOCATE(v)

    CONTAINS 
    TYPE(vector_type) FUNCTION vector_add(v1,v2)
      TYPE(vector_type), INTENT(IN) :: v1,v2
      vector_add%x = v1%x + v2%x 
      vector_add%y = v1%y + v2%y
    ENDFUNCTION
ENDSUBROUTINE