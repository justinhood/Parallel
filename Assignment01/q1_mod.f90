module q1_module
	integer, parameter :: mykind=selected_real_kind(16,300)
	contains

!Create the A matrix
	subroutine makeA(A,N)
		implicit none
		real(kind=mykind), allocatable, dimension(:,:), intent(inout) :: A
		integer intent(in) :: N
		integer i, j

		allocate(A(N,N))

		do i=1, N
			do j=1, N
				A(i,j)=real(1, mykind)/real((i+j-1), mykind)
			enddo
		enddo

	end subroutine makeA
	
	!Create the B matrix
	subroutine makeB(B,N)
		implicit none
		real(kind=mykind), allocatable, dimension(:,:), intent(inout) :: B
		real(kind=mykind) :: test
		integer intent(in) :: N
		integer i,j

		allocate(B(N,N))

		do i=1, N
			do j=1, N
				test=abs(bij(i,j,N))
				if(test .LE. 10e100) then
					B(i,j)=test
				else 
					B(i,j)=complement(i,j,N)
				endif
			enddo
		enddo
	end subroutine makeB
	
	!Compute the binomial coeff
	function parens(a,b)
		implicit none
		integer, intent(in) :: a, b
		parens=real(fact(a), mykind)/(real(fact(b), mykind)*real(fact(a-b), mykind))

	end function parens
	
	!Compute the trig thing
	function curly(a,b)
		implicit none
		integer, intent(in) :: a, b
		real(kind=mykind) :: val1, val2
		
		val1=real(sin(real(a, mykind)), mykind)
		val2=real(cos(real(b, mykind)), mykind)*real(tan(real(a+b, mykind)), mykind)
		curly=val1/val2
	end function curly
	
	!Compute the B_ij expression
	function bij(a,b,N)
		implicit none
		integer, intent(in) :: a, b, N
		real(kind=mykind) :: val1, val2, val3
		
		val1=real(parens((N+a-1), (N-b)), mykind)
		val2=real(parens((N+b-1), (N-a)), mykind)
		val3=real(parens((a+b-2), (a-1)), mykind)

		bij=real((-1)**(a+b), mykind)*real((a+b-1), mykind)*val1*val2*(val3**2)
	end function bij
	
	!Compute the tilde if necessary
	function complement(a,b,N)
		implicit none
		integer, intent(in) :: a, b, N
		real(kind=mykind) :: val1, val2, val3
		
		val1=real(curly((N+a-1), (N-b)), mykind)
		val2=real(curly((N+b-1), (N-a)), mykind)
		val3=real(curly((a+b-2), (a-1)), mykind)

		bij=real((-1)**(a+b), mykind)*real((a+b-1)**2, mykind)*(val1**3)*(val2**4)*(val3**5)

	end function complement

	!Compute the factorial for the binomial
	function fact(a)
		implicit none
		integer, intent(in) :: a
		integer i, f
		f=1
		do i=1, a
			f=f*i
		enddo
		fact = f
	end function fact

end module q1_module
