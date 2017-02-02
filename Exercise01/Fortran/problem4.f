	program futureValue
		real :: P, i, m, n, F
		
		print*, "Please enter the intial quantity"
		read(*,*) P
		print*, "Please enter the APR"
		read(*,*) i
		print*, "Please enter the the number of times"
		print*, "interest is compounded per period"
		read(*,*) m
		print*, "Please enter the number of periods separated by spaces."
		read(*,*) n
		
		F=P*(1+i/(100*m))**(m*n)
		print*, "The future value of the account is $", F
	end program futureValue
