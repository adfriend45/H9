cd SOURCE
export DRIVER=/Users/Waldlaeufer/projects/H9/EXECUTE/driver.txt
export OUTPUT=/Users/Waldlaeufer/projects/H9/OUTPUT/output_ann.txt
gfortran *.f90 -o H9 -I/Users/adf10/netcdf/F90/include -L/Users/adf10/netcdf/F90/lib -lnetcdff
cd ..
mv SOURCE/H9 EXECUTE
cd EXECUTE
./H9
