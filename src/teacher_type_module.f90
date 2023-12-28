module teacher_type_module
	use person_type_module
	implicit none
	private
	
	public :: teacher
	
	!����һ��teacher����������
	type,extends(person) :: teacher
		character(len = 15) :: office = "����¥5¥"
	contains
	
		!����һ�����Ͱ󶨵Ĺ���
		procedure,pass :: print_my_duty
		procedure,pass :: remove_info
	endtype teacher

!-------------------------------------------------------------------

	contains

!-------------------------------------------------------------------
	subroutine print_my_duty(this)
		class(teacher) :: this
		
		write(*,*) "teacher  ����",this%name, ",�ҵİ칫����", this%office
		write(*,*) "teacher  �ҵ������ǽ�ѧ����ѧ����ѧ��"
		write(*,*)
	endsubroutine

!-------------------------------------------------------------------
	subroutine remove_info(this)
		class(person) :: this
	
			this%name = ''
			this%sex = ''
			this%age = -1
			this%office = ''
	endsubroutine

endmodule teacher_type_module