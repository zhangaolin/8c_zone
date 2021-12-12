SET(SOURCE_FILES
    Main.f90    
    ShapeType.f90
    ShapeCircleType.f90
    ShapeRectangularType.f90
    ShapeCylinderType.f90
   )
ADD_EXECUTABLE(shape ${SOURCE_FILES})