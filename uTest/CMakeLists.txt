SET(SOURCE_FILES
    TestPerson.f90    
    # ../src/ModuleType.f90
   )
ADD_EXECUTABLE(test_person ${SOURCE_FILES})
ADD_TEST(NAME unitTest_person COMMAND test_person)