cd SOURCE
ifort *.f90 -o H9
cd ..
mv SOURCE/H9 EXECUTE
cd EXECUTE
./H9