# bash script to run HYBRID9 on different systems.
#set -x 

# Grab local path to see which system we are on, and use this to determine which
# options are appropriate.
CUR_PATH=$(pwd)

if [ $CUR_PATH == "/store/H9" ]; then # Looks like we are on Andrew's office Linux system.
  dir="/store/"
  export LD_LIBRARY_PATH=/home/adf10/netcdf/F90/lib:${LD_LIBRARY_PATH}
elif [ $CUR_PATH == "/Users/adf10/H9" ]; then # Looks like we are on Andrew's Mac.
  dir="/Users/adf10/"
  export LD_LIBRARY_PATH=/home/adf10/netcdf/F90/lib:${LD_LIBRARY_PATH}
elif [ $CUR_PATH == "/Users/Waldlaeufer/projects/H9" ]; then # Tim's Mac.
  dir="/Users/Waldlaeufer/projects/"
  export LD_LIBRARY_PATH=/opt/local/lib:/opt/local/include
fi

# Change to source code directory.
cd ./SOURCE

# Export environmental drivers for driver and output files.
export DRIVER=${dir}H9/EXECUTE/driver.txt
export OUTPUT=${dir}H9/OUTPUT/output_ann.txt
export OUTPUT2=${dir}H9/OUTPUT/output_trees.txt
export OUTPUT3=${dir}H9/OUTPUT/diag.txt

# Compile the source code, including netCDF.
if [ $CUR_PATH == "/store/H9" ]; then # Looks like we are on Andrew's office Linux system.
  make H9_exec
elif [ $CUR_PATH == "/Users/adf10/H9" ]; then # Looks like we are on Andrew's Mac.
  make H9_exec
elif [ $CUR_PATH == "/Users/Waldlaeufer/projects/H9" ]; then # Tim's Mac. I can't change to make file as I have to run gfortran, I don't have ifort.
  gfortran *.f90 -o H9_exec -g -fcheck=all -Wall -I/opt/local/include -L/opt/local/lib -lnetcdff
fi

# Move executable to EXECUTE directory and run.
mv H9_exec ../EXECUTE
cd ../EXECUTE
./H9_exec
