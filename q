export LD_LIBRARY_PATH=/home/adf10/netcdf/F90/lib:${LD_LIBRARY_PATH}
cd SOURCE
export DRIVER=/Users/adf10/H9/EXECUTE/driver.txt
export OUTPUT=/Users/adf10/H9/OUTPUT/output_ann.txt
##export DRIVER=/store/H9/EXECUTE/driver.txt
##export OUTPUT=/store/H9/OUTPUT/output_ann.txt
##ifort *.f90 -o H9 -I/home/adf10/netcdf/F90/include -L/home/adf10/netcdf/F90/lib -lnetcdff
gfortran *.f90 -o H9 -I/Users/adf10/netcdf/F90/include -L/Users/adf10/netcdf/F90/lib -lnetcdff
cd ..
mv SOURCE/H9 EXECUTE
cd EXECUTE
./H9
