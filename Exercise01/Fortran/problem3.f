	program powerlevel
		real :: nrg, ratio, a, frac
		real, parameter :: first=50000000
		!Log_a(x)=ln(x)/ln(a)
		print*, "Please enter the number of MW produced."
		read(*,*) nrg
		print*, "Please enter the logarithmic base a."
		read(*,*) a
		frac=(nrg*1000000)/first
		ratio=a*log(frac)/log(a)
		print *, "The ratio of outputs is: ", ratio
	end program powerlevel
