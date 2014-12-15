H9_exec : H9.o grow.o squeeze.o structures.o trees_structure.o light.o mortal.o write_outputs.o constants.o control.o tree.o
	gfortran H9.o grow.o squeeze.o structures.o trees_structure.o light.o mortal.o write_outputs.o constants.o control.o tree.o -o H9_exec

H9.o : H9.f90 constants.mod control.mod tree.mod
	gfortran -c H9.f90 -I/Users/adf10/netcdf/F90/include -L/Users/adf10/netcdf/F90/lib -lnetcdff
grow.o : grow.f90
	gfortran -c grow.f90
squeeze.o: squeeze.f90
	gfortran -c squeeze.f90
structures.o: structures.f90
	gfortran -c structures.f90
trees_structure.o: trees_structure.f90
	gfortran -c trees_structure.f90
light.o : light.f90
	gfortran -c light.f90
mortal.o : mortal.f90
	gfortran -c mortal.f90
write_outputs.o : write_outputs.f90
	gfortran -c write_outputs.f90
constants.mod constants.o : constants.f90
	gfortran -c constants.f90
control.mod control.o : control.f90
	gfortran -c control.f90
tree.mod tree.o : tree.f90
	gfortran -c tree.f90
