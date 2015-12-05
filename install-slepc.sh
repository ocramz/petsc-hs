#!/bin/bash

# environment variables
# SC_V="$1"    # version (e.g. "3.6.2")
SC_DIR="$1"    # download,unpack and install directory (e.g. "$HOME/slepc")
# SC_ARCH="$3"   # architecture id.string (e.g. "arch-linux2-c-debug")

SC_TAR="$2"   # URL of .tar.gz


# download and untar
curl -L $SC_TAR | tar xz --strip-components=1 -C $SC_DIR/ 

# enter install dir
cd $PETSC_DIR/

# configure
./configure 

# compile
make 

# make all test
make test

# exit install dir
cd ..
