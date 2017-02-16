program main
	use matrix_module
	implicit none

	integer, parameter :: mykind=selected_real_kind(16,300)
	real(kind=mykind), Allocatable, dimension(:,:) :: B
	real(kind=mykind), allocatable, dimension(:) :: A, C
	integer :: N, M, i
	real :: time1, time2
	
	open(unit=1, file="input.txt")
	read(1,*) M
	read(1,*) N
	close(1)

	call A_vec(N,A)
	B=B_mat(M,N)
	
	if(N .LT. 5)then
		print *, ''
		print *, 'Matrix B :'
		print *, ''
		do i=1,M
			print *, B(i,:)
		enddo
		print *, ''
		print *, 'Matrix A :'
		print *, ''
		do i=1,N
			print *, A(i)
		enddo
		print *, ''
	endif

	call CPU_TIME(time1)
	call serial(A,B,C)
	call CPU_time(time2)

	if(N .GT. 4)then
		print *, ''
	endif

	print *, 'Elapsed time for serial version : ', time2-time1
	print *, ''

	if(N .LT. 5) then
		print *, 'Vector C from serial version: '
		print *, ''
		do i=1,N
			print *, C(i)
		enddo
		print *, ''
	endif

	deallocate(C)

	call cpu_time(time1)
	call vec_mult(A,B,C)
	call cpu_time(time2)

	print*, 'Elapsed time for vectorized version :', time2-time1
	print *, ''
	if (N .LT. 5)then
		print *, 'Vector C from Vector version :'
		print *,''
		do i=1,N
			print *, C(i)
		enddo
		print *, ''
	endif

	deallocate(A,B,C)


end program main
