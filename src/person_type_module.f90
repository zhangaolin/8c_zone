module person_type_module
	implicit none
	private
	
	public :: person
	
	!定义一个person的派生类型
	type :: person
		character(len = 12) :: name = ''
		character(len = 4) :: sex
		integer :: age
	contains
	
		!定义两个类型绑定的过程
		procedure,pass :: set_info
		procedure,pass :: remove_info
		procedure,pass :: print_my_duty
	endtype
!-------------------------------------------------------------------
	
	contains

!-------------------------------------------------------------------
	subroutine print_my_duty(this)
		class(person) :: this
		
		write(*,*) "person  我是",this%name
		write(*,*) "person  我也不知道我是干什么的"
		write(*,*)
	endsubroutine

!-------------------------------------------------------------------
	
	subroutine set_info(this, name, sex, age)
		class(person) :: this
		
		character(len = *) :: name
		character(len = *) :: sex
		integer :: age
		
		integer :: nerror = 0
		
		if(.not.(trim(sex) == '男' .or. trim(sex) == '女')) then
			write(*,*) "Error: sex is not man or women"
			nerror = nerror + 1
		endif
		
		if(age < 0 .or. age > 150) then
			write(*,*) "Error: age is less than 0 or larger than 150"
			nerror = nerror + 1
		endif
		
		if(nerror == 0) then
			this%name = name
			this%sex = sex
			this%age = age
		else
			write(*,*) "Please check the inputs of set_info(), some wrong input is provided"
			stop 999
		endif
	endsubroutine

!-------------------------------------------------------------------

	subroutine remove_info(this)
		class(person) :: this
	
			this%name = ''
			this%sex = ''
			this%age = -1
	endsubroutine

endmodule