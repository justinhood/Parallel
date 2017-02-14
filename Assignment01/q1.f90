!http://www.math.hawaii.edu/~hile/fortran/fort3.htm
program question1
	implicit none
	integer :: N, i, j
	double precision :: test
	double precision, dimension(:,:), allocatable :: A, B, C
	double precision :: bound
	bound=10D100
	write(*,*) "Please enter the size of the arrays, N"
	read(*,*) N
	
	!CREATE A
	allocate(A(N,N))
	do i=1, N, 1
		do j=1, N, 1
			A(i,j)=1/(real(i)+real(j)-1)
		enddo
	enddo

	!CREATE B
	allocate(B(N,N))
	do i=1, N, 1
		do j=1, N, 1
			test=bij(i,j,N)
			if(test .LE. bound) then
				B(i,j)=test
			else
				B(i,j)=complement(i,j,N)
			endif
		continue	
	continue

	!WRITE
	do i=1, N, 1
		write(12,*) (B(i,j), j=1,N)
	enddo
	deallocate(A)
	deallocate(B)

contains
	integer function fact(a)
		implicit none
		integer, intent(IN) :: alpha
		integer p, s
		p=1
		
		if(alpha .eq. 0) then
			fact=1
		else if(alpha .LE. 0) then
			print*, "This is not an allowed value for factorial"
			stop
		else
			do s=1,alpha
				p=p*s
			enddo
			fact=p
		endif

	end function fact
	
	integer function parens(alpha,b)
		implicit none
		integer, intent(IN) :: a, b
		parens = fact(a)/(fact(b)*fact(a-b))
	end function parens

	double precision function curly(a,b)
		implicit none
		integer, intent(in) :: a, b
		curly=dble(sin(real(a))/(cos(real(b))*tan(real(a+b))))
	end function curly

	double precision function bij(a,b,N)
		implicit none
		integer, intent(IN) :: a, b, N
		bij=((-1)**(a+b))*(a+b-1)*parens(N+a-1,N-b)*parens(N+b-1,N-b)*((parens(a+b-2,a-1))**2)
	end function bij

	double precision function complement(i,j,N)
		implicit none
		integer, intent(IN) :: i, j, N
		complement=((-1)**(i+j))*((i+j-1)**2)*((curly(N+i-1, N-j))**3)*((curly(N+j-1,N-i))**4)*((curly(i+j-2,i-1))**5)
	end function complement
end program question1
