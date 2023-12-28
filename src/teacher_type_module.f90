module teacher_type_module
	use person_type_module
	implicit none
	private
	
	public :: teacher
	
	!定义一个teacher的派生类型
	type,extends(person) :: teacher
		character(len = 15) :: office = "化工楼5楼"
	contains
	
		!定义一个类型绑定的过程
		procedure,pass :: print_my_duty
		procedure,pass :: remove_info
	endtype teacher

!-------------------------------------------------------------------

	contains

!-------------------------------------------------------------------
	subroutine print_my_duty(this)
		class(teacher) :: this
		
		write(*,*) "teacher  我是",this%name, ",我的办公室在", this%office
		write(*,*) "teacher  我的任务是教学！教学！教学！"
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