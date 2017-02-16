program q1_main
	use q1_mod
	implicit none

	integer, parameter :: mykind=selected_real_kind(16,300)
	real(kind=mykind), allocatable, dimension(:,:) :: A, B, C
	integer N
	integer i, j
	
	
	!READ N
	open(unit=1, file="q1_input.txt")
	read(1,*) N
	close(1)
	
	call makeA(A, N)
	call makeB(B, N)
	
	!Print to check for now
	write(*,*) "MATRIX A"
	do i=1, N
		write(12,*) (A(i,j), j=1,N)
	enddo

	write(*,*) ''
	write(*,*) ''

	do i=1, N
		write(12, *) (B(i,j), j=1, N)
	enddo


end program q1_main