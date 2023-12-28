PROGRAM test_person
	USE person_type_module
	USE student_type_module
	USE teacher_type_module
	IMPLICIT NONE
	
	CLASS(person),POINTER :: person_ptr => NULL()
	INTEGER :: nerror = 0, nerror_temp = 0
	
	!将class类型person_ptr，确定为person的type类
	ALLOCATE(person :: person_ptr)
	
	!-------------------- 测试person%remove_info() --------------------
	CALL person_ptr%set_info("小明妈妈","女",33）
	IF(TRIM(person_ptr%name) /= "小明妈妈" .OR. TRIM(person_ptr%sex) /= "女" .OR. person_ptr%age /= 33) THEN
		nerror = nerror + 1
		WRITE(*,*) "Error: set_info未能正确设置person的相关信息！"
	ENDIF
	
	CALL person_ptr%remove_info
	IF(TRIM(person_ptr%name) /= "" .OR. TRIM(person_ptr%sex) /= "" .OR. person_ptr%age /= -1) THEN
		nerror = nerror + 1
		WRITE(*,*) "Error: remove_info未能正确清除person的相关信息！"
	ELSEIF(nerror == 0) THEN
		WRITE(*,*) "Passed: person%remove_info()"
	ENDIF		

	!-------------------- 测试sex --------------------
	nerror_temp = nerror
	!测试：错误的sex
	CALL person_ptr%set_info("小明爸爸","男女",35）
	IF(TRIM(person_ptr%name) /= "小明爸爸") THEN
		nerror = nerror + 1
		WRITE(*,*) "Error: sex测试错误！"
	ENDIF

	!测试：正确的sex = 男
	CALL person_ptr%set_info("小明爸爸","男",35）
	IF(TRIM(person_ptr%name) /= "小明爸爸") THEN
		nerror = nerror + 1
		WRITE(*,*) "Error: sex测试错误！"
	ENDIF

	!测试：正确的sex = 女
	CALL person_ptr%set_info("小明妈妈","女",33）
	IF(TRIM(person_ptr%name) /= "小明爸爸") THEN
		nerror = nerror + 1
		WRITE(*,*) "Error: sex测试错误！"
	ENDIF

	!-------------------- 测试age --------------------
	!测试：错误的age < 0
	CALL person_ptr%remove_info()
	CALL person_ptr%set_info("小明爸爸","男",-33）
	IF(TRIM(person_ptr%name) == "小明爸爸") THEN
		nerror = nerror + 1
		WRITE(*,*) "Error: age < 0测试未检测出错误！"
	ENDIF
	
	!测试：错误的age > 150
	CALL person_ptr%remove_info()
	CALL person_ptr%set_info("小明爸爸","男",151）
	IF(TRIM(person_ptr%name) == "小明爸爸") THEN
		nerror = nerror + 1
		WRITE(*,*) "Error: age > 150测试未检测出错误！"
	ENDIF
	
	!测试：正确的age = 33
	CALL person_ptr%remove_info()
	CALL person_ptr%set_info("小明爸爸","男",33）
	IF(TRIM(person_ptr%name) /= "小明爸爸") THEN
		nerror = nerror + 1
		WRITE(*,*) "Error: age测试错误！"
	ENDIF

	IF(nerror_temp == nerror) THEN	
		WRITE(*,*) "Passed: person%set_info(...)"

	!-------------------- 测试person%set_info() --------------------
	nerror_temp = nerror
	CALL person_ptr%remove_info()
	CALL person_ptr%set_info("小明","男",3)
	CALL person_ptr%print_my_duty()












	TYPE(person),POINTER :: per1 => NULL()
	ALLOCATE(per1)
	CALL per1%set_info("小明","男",3)
	CALL per1%print_my_duty()
	DEALLOCATE(per1)

ENDPROGRAM
