SET(SOURCE_FILES
	program_main.f90
	teacher_type_module.f90
	student_type_module.f90
	person_type_module.f90
	)

ADD_EXECUTABLE(persons ${SOURCE_FILES})