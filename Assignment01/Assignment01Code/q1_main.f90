program q1_main
	use q1_mod
	implicit none
	real(kind=mykind) :: time1, time2

!	integer, parameter :: mykind=selected_real_kind(16,300)
	real(kind=mykind), allocatable, dimension(:,:) :: A, B, C
	integer N
	integer :: i, j
	integer countN
	
	call CPU_TIME(time1)
	!READ N
	open(unit=1, file="q1_input.txt")
	read(1,*) N
	close(1)
	
	call makeA(A, N)
	call makeB(B, N, countN)
	call makeC(A,B,C,N)

	call CPU_TIME(time2)

	!Print to check for now
!	write(12,*) "MATRIX A"
!	do i=1, N
!		write(12,*) (A(i,j), j=1,N)
!	enddo
!
!	write(12,*) ''
!	write(12,*) ''
!	write(12,*) "Matrix B"
!	do i=1, N
!		write(12, *) (B(i,j), j=1, N)
!	enddo
!	write(12,*) ''
!	write(12,*) ''
!	write(12,*) "Matrix C"
!	do i=1, N
!		write(12, *) (C(i,j), j=1, N)
!	enddo
	write(12,*) "N=", N
	write(12,*) ''

	write(12,*) "CN(N/2, N/2)=", C(n/2,n/2)
	write(12,*) ''

	write(12,*) "CN(N,N)=", C(N,N)
	write(12,*) ''

	write(12,*) "count(N)=", countN
	write(12,*) ''

	write(12,*) "T_(1,Q1)(N)=", time2-time1
	write(12,*) ''

	deallocate(A,B,C)
end program q1_main
