export LD_LIBRARY_PATH=/home/adf10/netcdf/F90/lib:${LD_LIBRARY_PATH}
cd SOURCE
ifort *.f90 -o H9 -I/home/adf10/netcdf/F90/include -L/home/adf10/netcdf/F90/lib -lnetcdff
cd ..
mv SOURCE/H9 EXECUTE
cd EXECUTE
./H9
