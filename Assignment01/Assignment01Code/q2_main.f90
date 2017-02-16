program q2_main
	use q1_mod
	implicit none
	real(kind=mykind) :: time1, time2

!	integer, parameter :: mykind=selected_real_kind(16,300)
	real(kind=mykind), allocatable, dimension(:,:) :: A, B, C
	integer :: sizer
	integer, allocatable, dimension(:) :: N, k, countN, half, full, times
	integer :: i, j
	
	character :: format1, format2, col1, col2, col3, col4, col5

	format1="(T2,A,TR7,A,TR5,A,TR5,A,TR5,A)"
	format2="(I4,TR6,E14.8,TR5,E14.8,TR5,I4,TR5,F1.6)"

	col1="N"
	col2="C(N/2,N/2)"
	col3="C(N,N)"
	col4="count(N)"
	col5="Time(N)"

	!READ N
	open(unit=1, file="q2_input.txt")
	
	read(1,*) sizer

	allocate(k(sizer))
	allocate(N(sizer))
	allocate(countN(sizer))
	allocate(half(sizer))
	allocate(full(sizer))
	allocate(times(sizer))

	
	do i=1, sizer
		read(1,*) k(i)
	enddo

	close(1)

	do i=1, sizer
		N(i)=100*(2**k(i))
	enddo
	
	do i=1, sizer
		call CPU_TIME(time1)
		
		call makeA(A, N(i))
		call makeB(B, N(i), countN(i))
		call makeC(A,B,C,N(i))

		call CPU_TIME(time2)

		half(i)=C(N(i)/2, N(i)/2)
		full(i)=C(N(i),N(i))
		times(i)=time2-time1
		deallocate(A,B,C)
	enddo

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
	
	open(unit=1, file="q2_output.txt")
	
	write(1,format1) col1, col2, col3, col4, col5

	close(1)

	deallocate(k,N)
end program q2_main
