FC90 = gfortran

##############   Q1   ###############
vector_files = q1_main.f90 q1_mod.f90

##############  Q2  #################
#vector_files = q2_main.f90 q1_mod.f90


all: main_exe

main_exe:$(vector_files)
	 $(FC90) $(vector_files) -o $@

clean:
	rm *_exe *.mod
