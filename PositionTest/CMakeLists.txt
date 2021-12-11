SET(SOURCE_FILES
    PositionTest.f90    
    # ../src/ModuleType.f90
   )
ADD_EXECUTABLE(test_position ${SOURCE_FILES})
ADD_TEST(NAME unitTest_position COMMAND test_position)