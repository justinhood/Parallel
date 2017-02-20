program test
	implicit none
	character :: format1, format2

	format1="(T2,A,TR7,A,TR5,A,TR5,A,TR5,A)"
	write(1,format1) "N", "C_N/2", "C_N", "count(N)", "time(N)"
end program test
