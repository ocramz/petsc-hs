/usr/bin/env sh

cd petsc
cd petsc-3.6.2

./configure --with-cc=gcc --with-cxx=g++ --with-fc=gfortran --download-fblaslapack --download-mpich

# make all test
