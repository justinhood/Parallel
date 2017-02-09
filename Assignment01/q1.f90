program question1
	implicit none
	integer :: N, i, j
	real :: test
	integer ::whaaa
	real, dimension(:,:), allocatable :: A, B, C
	whaaa=selected_real_kind(0,100)
	real(kind=8) :: bound
	bound=10**(100)
	print*, whaaa
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
!			if(test .LE. (10**(100))) then
!				B(i,j)=test
!			else
!				B(i,j)=complement(i,j,N)
!			endif
		enddo
	enddo

	!WRITE
	do i=1, N, 1
		write(12,*) (B(i,j), j=1,N)
	enddo
	deallocate(A)
	deallocate(B)
contains
	integer function fact(n)
		implicit none
		integer, intent(IN) :: n
		integer p
		p=1
		do i=1,n
			p=p*i
		enddo
		fact=p
	end function fact
	
	real function parens(a,b)
		implicit none
		integer, intent(IN) :: a, b
		parens = fact(a)/(fact(b)*fact(a-b))
	end function parens

	real function curly(a,b)
		implicit none
		integer, intent(in) :: a, b
		curly=sin(real(a))/(cos(real(b))*tan(real(a+b)))
	end function curly

	real function bij(i,j,N)
		implicit none
		integer, intent(IN) :: i, j, N
		bij=((-1)**(i+j))*(i+j-1)*parens(N+i-1,N-j)*parens(N+j-1,N-i)*((parens(i+j-2,i-1))**2)
	end function bij

	real function complement(i,j,N)
		implicit none
		integer, intent(IN) :: i, j, N
		complement=((-1)**(i+j))*((i+j-1)**2)*((curly(N+i-1, N-j))**3)*((curly(N+j-1,N-i))**4)*((curly(i+j-2,i-1))**5)
	end function complement
end program question1
