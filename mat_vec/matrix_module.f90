	module matrix_module
		contains
		
		subroutine A_vec(N,A)
			implicit none
			integer, parameter :: mykind=selected_real_kind(16,300)
			real(kind=mykind), allocatable, dimension(:), intent(inout) :: A
			integer, intent(in) :: N
			integer :: i

			Allocate(A(N))

			do i=1,N
				A(i) = real(.5, mykind)*real(i, mykind)
			enddo
		end subroutine A_vec
		
		function B_mat(M,N)
			implicit none
			integer, parameter :: mykind = selected_real_kind(16,300)
			real(kind=mykind), allocatable, dimension(:,:) :: B_mat
			integer, intent(in) :: M, N
			integer :: i, j

			allocate(B_mat(M,N))

			do i=1,M
				do j=1, N
					B_mat(i,j)=real(1.5, mykind)*real(i, mykind)*real(j, mykind)
				enddo
			enddo
		end function B_mat

		subroutine serial(A,B,C)
			implicit none
			integer, parameter :: mykind = selected_real_kind(16,300)
			real(kind=mykind), Allocatable, dimension(:,:), intent(inout) :: B
			real(kind=mykind), allocatable, dimension(:), intent(inout) :: A,C
			real(kind=mykind) :: vec_sum
			integer :: i,j,k,A_row, B_row, B_col

			A_row=size(A)

			B_row=size(B(:,1))
			B_col=size(B(1,:))

			Allocate(C(A_row))

			do i=1, B_row
				vec_sum=0
				do j=1, B_col
					vec_sum = vec_sum + B(i,j)*A(j)
				enddo
				C(i)=vec_sum
			enddo
		end subroutine serial

		subroutine vec_mult(A,B,C)
			implicit none
			integer, parameter :: mykind = selected_real_kind(16,300)
			real(kind=mykind), Allocatable, dimension(:,:), intent(inout) :: B
			real(kind=mykind), allocatable, dimension(:), intent(inout) :: A,C
			real(kind=mykind) :: vec_sum
			integer :: i,j,k,A_row, B_row, B_col

			A_row=size(A)

			B_row=size(B(:,1))
			B_col=size(B(1,:))

			Allocate(C(A_row))

			do i=1, B_row
				C(i)=sum(B(i,:)*A)
			enddo
		end subroutine vec_mult
	end module matrix_module

