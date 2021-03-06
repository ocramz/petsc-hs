#!/bin/bash

SWDIR=$1

printf "\n=== Inherited environment :\n"
printenv
printf "\n=== Operating system :\n"
uname -srvmpio
lsb_release -d

# export PETSC_INCLUDE1=${PETSC_DIR}/include/
# export PETSC_INCLUDE2=${PETSC_DIR}/${PETSC_ARCH}/include/
# # export PETSC_LIB=${PETSC_DIR}/${PETSC_ARCH}/lib/
# export SLEPC_INCLUDE1=${SLEPC_DIR}/include/
# export SLEPC_INCLUDE2=${SLEPC_DIR}/${SLEPC_ARCH}/include/
# # export SLEPC_LIB=${SLEPC_DIR}/${SLEPC_ARCH}/lib/

# export PETSCHS_DIR=${SWDIR}/petsc-hs

printf "\n=== APT-Installing dependencies :\n"
apt-get update && apt-get install -y --no-install-recommends build-essential sudo

printf "\n=== Setting up FP Complete APT repository :\n"
# # get FP Complete public key
apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442
# Ubuntu 14 APT repo for FP Complete
echo 'deb http://download.fpcomplete.com/ubuntu trusty main' | sudo tee /etc/apt/sources.list.d/fpco.list

printf "\n=== APT-Installing dependencies : \n"
apt-get update -y && apt-get install -y --no-install-recommends \
			     git libgmp-dev stack &&  \
       apt-get clean && apt-get purge && \
       rm -rf /var/lib/apt/lists/*



# # Download and unpack the `stack` executable :
# mkdir -p "$HOME"/.local/bin
# export PATH=$HOME/.local/bin:$PATH

# printf "\n=== Retrieving Stack build tool\n"
# curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C $HOME/.local/bin '*/stack'


export PATH=$(stack --stack-yaml stack.yaml path --local-install-root):$PATH
export PATH=${PETSC_LIB}:${PATH}

# export LD_LIBRARY_PATH=${PETSC_LIB}:${SLEPC_LIB}:${LD_LIBRARY_PATH}

# # # check env
printf "\n=== Environment :\n"
printenv

printf "\n=== Current directory :\n"
pwd


printf "\n=== Downloading petsc-hs :\n"
cd ${SWDIR}
git clone https://github.com/ocramz/petsc-hs.git
cd ${PETSCHS_DIR}

printf "\n=== Setting up GHC ('stack setup'):\n"
stack setup

printf "\n=== Installing 'c2hs' :\n"
stack install c2hs

printf "\n=== Compiling petsc-hs dependencies"
./stack-build.sh "--dependencies-only"

# # rm source dir contents
printf "\n=== Removing petsc-hs sources and build artifacts :\n"
cd ${SWDIR}
rm -rf petsc-hs/




# # # CUT


