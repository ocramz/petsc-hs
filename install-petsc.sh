/usr/bin/env sh
mkdir temp
cd temp

wget http://ftp.mcs.anl.gov/pub/petsc/release-snapshots/petsc-lite-3.6.2.tar.gz
tar xzvf petsc-lite-3.6.2.tar.gz
cd petsc-3.6.2

./configure --with-cc=gcc --with-cxx=g++ --with-fc=gfortran --download-fblaslapack --download-mpich

make all test
