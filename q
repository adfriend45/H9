# Bash script to run Hybrid IX depending on usr and architecture
#set -x

# Set usr and determine architecture (ADF ; adf10 ; TTR = Tim's laptop)
usr="TTR"

if [ ${usr} == "TTR" ] ; then
  dir="~/projects/"
elif [ ${usr} == "ADF" ] ; then
  dir="/store/"
  export LD_LIBRARY_PATH=/home/adf10/netcdf/F90/lib:${LD_LIBRARY_PATH}
elif [ ${usr} == "adf10" ] ; then
  dir="/Users/adf10/"
  export LD_LIBRARY_PATH=/home/adf10/netcdf/F90/lib:${LD_LIBRARY_PATH}
fi

# Change to source code directory
cd SOURCE

# Export environmental variables for driver and output files
export DRIVER=${dir}H9/EXECUTE/driver.txt
export OUTPUT=${dir}H9/OUTPUT/output_ann.txt
export OUTPUT2=${dir}H9/OUTPUT/output_trees.txt

#Compile the source code including netcdf
if [ ${usr} == "TTR" ] ; then
  gfortran *.f90 -o -g -fcheck=all -Wall -I/opt/local/include -L/opt/local/lib -lnetcdff
elif [ ${usr} == "ADF" ] ; then
  ifort *.f90 -o H9 -I/home/adf10/netcdf/F90/include -L/home/adf10/netcdf/F90/lib -lnetcdff
elif [ ${usr} == "adf10" ] ; then
  ifort *.f90 -o H9 -I/Users/adf10/netcdf/F90/include -L/Users/adf10/netcdf/F90/lib -lnetcdff
fi

# Move file to EXECUTE directory and execute the file
mv H9 ../EXECUTE
cd ../EXECUTE
./H9
