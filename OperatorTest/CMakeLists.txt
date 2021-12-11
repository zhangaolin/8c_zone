SET(SOURCE_FILES
    TestOperator.f90    
    # ../src/ModuleType.f90
   )
ADD_EXECUTABLE(test_operator ${SOURCE_FILES})
ADD_TEST(NAME unitTest_operator COMMAND test_operator)