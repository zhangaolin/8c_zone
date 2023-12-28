module student_type_module
	use person_type_module
	implicit none
	private
	
	public :: student
	
	!����һ��student����������
	type,extends(person) :: student
		character(len = 10) :: dorm = "��20-206"
	contains
	
		!����һ�����Ͱ󶨵Ĺ���
		procedure,pass :: print_my_duty
		procedure,pass :: remove_info
	endtype student

!-------------------------------------------------------------------

	contains

!-------------------------------------------------------------------	
	subroutine print_my_duty(this)
		class(student) :: this
		
		write(*,*) "student  ����",this%name, ",�ҵ�������", this%dorm
		write(*,*) "student  �ҵ�������ѧϰ��ѧϰ��ѧϰ��"
		write(*,*)
	endsubroutine

!-------------------------------------------------------------------
	subroutine remove_info(this)
		class(person) :: this
	
			this%name = ''
			this%sex = ''
			this%age = -1
			this%dorm = ''
	endsubroutine

endmodule student_type_module