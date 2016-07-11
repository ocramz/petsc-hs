#!/bin/bash

# environment variables
PETSC_V="$1"    # version (e.g. "3.6.2")
PETSC_DIR="$2"    # download,unpack and install directory (e.g. "$HOME/petsc")
PETSC_ARCH="$3"   # architecture id.string (e.g. "arch-linux2-c-debug")

PETSC_FTP=http://ftp.mcs.anl.gov/pub/petsc/release-snapshots
PETSC_TAR=$PETSC_FTP/petsc-lite-$PETSC_V.tar.gz

# download and untar
curl -L $PETSC_TAR | tar xz --strip-components=1 -C $PETSC_DIR/ 

# enter install dir
cd $PETSC_DIR/

# configure
if [${BUILDTYPE} == "batch"]; then 
    echo "=== Configuring in batch mode"
    ./configure --with-cc=gcc --with-cxx=g++ --with-fc=gfortran --download-fblaslapack --download-mpich --download-hdf5=yes --with-batch 
else
    echo "=== Configuring without batch mode"
    ./configure --with-cc=gcc --with-cxx=g++ --with-fc=gfortran --download-fblaslapack --download-mpich --download-hdf5=yes
fi

# compile
make PETSC_DIR=$PETSC_DIR PETSC_ARCH=$PETSC_ARCH all

# test
make all test

# exit install dir
cd ..
