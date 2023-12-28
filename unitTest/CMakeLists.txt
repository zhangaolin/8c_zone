SET(SOURCE_FILES
	test_person.f90
	../src/teacher_type_module.f90
	../src/student_type_module.f90
	../src/person_type_module.f90
	)

ADD_EXECUTABLE(test_person ${SOURCE_FILES})
ADD_TEST(NAME unitTest_person COMMAND test_person)