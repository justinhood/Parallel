	program pay
		real :: total, hourly, hours
		print*, "Please enter the hourly pay rate"
		read(*,*) hourly
		print*, "Please enter the number of hours worked"
		read(*,*) hours
		total = hourly* hours
		print*, "Your total pay is $", total
	end program pay
