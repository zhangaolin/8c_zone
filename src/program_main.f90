program main
	use person_type_module
	use student_type_module
	use teacher_type_module
	implicit none
	
	!定义一个class类型的person
	class(person),pointer :: person_ptr !=> null()
	
	!定义三个type类型的指针
	type(person),pointer :: per1 => null()
	type(student),pointer :: stu1 => null()
	type(teacher),pointer :: tea1 => null()
	
	write(*,*)"=========================== type allocate ==========================="
	!直接给type类型分配内存
	allocate(per1)
	call per1%set_info("小明","男男男女女",3)
	call per1%print_my_duty()
	
	!直接给type类型分配内存
	allocate(stu1)
	call stu1%set_info("小花","女",19)
	call stu1%print_my_duty()
	
	!直接给type类型分配内存
	allocate(tea1)
	call tea1%set_info("老刘","男",40)
	call tea1%print_my_duty()
	
	write(*,*)"=========================== class point to type ==========================="
	!利用class类型指针指向扩展类
	person_ptr => per1
	call person_ptr%print_my_duty()
	
	person_ptr => stu1
	call person_ptr%print_my_duty()
	
	person_ptr => tea1
	call person_ptr%print_my_duty()
	
	
	write(*,*)"=========================== class allocated to be a specified type ==========================="
	!给class类型分配内存，并制定为其扩展类
	nullify(person_ptr)
	
	!将class类型person_ptr确定为person的type类
	allocate(person :: person_ptr)
	call person_ptr%set_info('小明爸爸','男',33)
	call person_ptr%print_my_duty()
	
	!将class类型person_ptr确定为person的type类
	deallocate(person_ptr)
	allocate(student :: person_ptr)
	call person_ptr%set_info('小花妈妈','女',49)
	call person_ptr%print_my_duty()
	
	!将class类型person_ptr确定为person的type类
	deallocate(person_ptr)
	allocate(teacher :: person_ptr)
	call person_ptr%set_info('老刘同事','男',45)
	call person_ptr%print_my_duty()
	
endprogram