	program relativity
		real :: nrg, mass
		!Speed of light
		real, parameter :: c=299792458

		print*, "Please enter the number of MW produced."
		read(*,*) nrg
		
		mass = (nrg*1000000*365.25*24*60*60)/(c*c)
		print *, "The total mass used for this reactor is:", mass, "kg"
	end program relativity
