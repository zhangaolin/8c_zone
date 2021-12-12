SET(SOURCE_FILES
    TestShapeCircle.f90  
    # ../src/ModuleType.f90
   )
ADD_EXECUTABLE(test_Circle ${SOURCE_FILES})
ADD_TEST(NAME unitTest_circle COMMAND test_circle)

SET(SOURCE_FILES
    TestShapeCylinder.f90
    # ../src/ModuleType.f90
   )
ADD_EXECUTABLE(test_Cylinder ${SOURCE_FILES})
ADD_TEST(NAME unitTest_Cylinder COMMAND test_Cylinder)

SET(SOURCE_FILES
    TestShapeRectangular.f90  
    # ../src/ModuleType.f90
   )
ADD_EXECUTABLE(test_Rectangular ${SOURCE_FILES})
ADD_TEST(NAME unitTest_Rectangular COMMAND test_Rectangular)
