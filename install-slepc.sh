#!/bin/bash




# environment variables  
SC_VERSION="$1" # version (e.g. "3.6.2")
SC_DIR="$2"    # download,unpack and install directory (e.g. "$HOME/slepc")


SLEPC_PHP=http://slepc.upv.es/download/download.php?
SC_URL="$SLEPC_PHP"filename=slepc-"$SC_VERSION".tar.gz # URL of .tar.gz

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
