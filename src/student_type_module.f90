module student_type_module
	use person_type_module
	implicit none
	private
	
	public :: student
	
	!定义一个student的派生类型
	type,extends(person) :: student
		character(len = 10) :: dorm = "东20-206"
	contains
	
		!定义一个类型绑定的过程
		procedure,pass :: print_my_duty
		procedure,pass :: remove_info
	endtype student

!-------------------------------------------------------------------

	contains

!-------------------------------------------------------------------	
	subroutine print_my_duty(this)
		class(student) :: this
		
		write(*,*) "student  我是",this%name, ",我的宿舍在", this%dorm
		write(*,*) "student  我的任务是学习！学习！学习！"
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