program main
	use person_type_module
	use student_type_module
	use teacher_type_module
	implicit none
	
	!����һ��class���͵�person
	class(person),pointer :: person_ptr !=> null()
	
	!��������type���͵�ָ��
	type(person),pointer :: per1 => null()
	type(student),pointer :: stu1 => null()
	type(teacher),pointer :: tea1 => null()
	
	write(*,*)"=========================== type allocate ==========================="
	!ֱ�Ӹ�type���ͷ����ڴ�
	allocate(per1)
	call per1%set_info("С��","������ŮŮ",3)
	call per1%print_my_duty()
	
	!ֱ�Ӹ�type���ͷ����ڴ�
	allocate(stu1)
	call stu1%set_info("С��","Ů",19)
	call stu1%print_my_duty()
	
	!ֱ�Ӹ�type���ͷ����ڴ�
	allocate(tea1)
	call tea1%set_info("����","��",40)
	call tea1%print_my_duty()
	
	write(*,*)"=========================== class point to type ==========================="
	!����class����ָ��ָ����չ��
	person_ptr => per1
	call person_ptr%print_my_duty()
	
	person_ptr => stu1
	call person_ptr%print_my_duty()
	
	person_ptr => tea1
	call person_ptr%print_my_duty()
	
	
	write(*,*)"=========================== class allocated to be a specified type ==========================="
	!��class���ͷ����ڴ棬���ƶ�Ϊ����չ��
	nullify(person_ptr)
	
	!��class����person_ptrȷ��Ϊperson��type��
	allocate(person :: person_ptr)
	call person_ptr%set_info('С���ְ�','��',33)
	call person_ptr%print_my_duty()
	
	!��class����person_ptrȷ��Ϊperson��type��
	deallocate(person_ptr)
	allocate(student :: person_ptr)
	call person_ptr%set_info('С������','Ů',49)
	call person_ptr%print_my_duty()
	
	!��class����person_ptrȷ��Ϊperson��type��
	deallocate(person_ptr)
	allocate(teacher :: person_ptr)
	call person_ptr%set_info('����ͬ��','��',45)
	call person_ptr%print_my_duty()
	
endprogram