FC90 = gfortran
vector_files = q1_main.f90 q1_mod.f90

all: main_exe

main_exe:$(vector_files)
	 $(FC90) $(vector_files) -o $@

clean:
	rm *_exe *.mod
