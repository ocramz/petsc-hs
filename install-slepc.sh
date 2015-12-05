#!/bin/bash

# environment variables
# SC_V="$1"    # version (e.g. "3.6.2")
SC_DIR="$1"    # download,unpack and install directory (e.g. "$HOME/slepc")
# SC_ARCH="$3"   # architecture id.string (e.g. "arch-linux2-c-debug")

SC_URL="$2"   # URL of .tar.gz

SC_TAR=slepc.tar.gz   # name of local archive copy

# download and untar
wget -O $SC_TAR $SC_URL
tar xzf $SC_TAR --strip-components=1 -C $SC_DIR/ 

# enter download dir
cd $SC_DIR/

# configure
./configure 

# compile
make 

# make all test
make test

# exit install dir
cd ..
