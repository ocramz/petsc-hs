#!/bin/bash

SWDIR=$1

printenv

export PETSC_INCLUDE1=${PETSC_DIR}/include/
export PETSC_INCLUDE2=${PETSC_DIR}/${PETSC_ARCH}/include/
export PETSC_LIB=${PETSC_DIR}/${PETSC_ARCH}/lib/
export SLEPC_INCLUDE1=${SLEPC_DIR}/include/
export SLEPC_INCLUDE2=${SLEPC_DIR}/${SLEPC_ARCH}/include/
export SLEPC_LIB=${SLEPC_DIR}/${SLEPC_ARCH}/lib/

export PETSCHS_DIR=${SWDIR}/petsc-hs


apt-get install -y --no-install recommends git libgmp-dev


mkdir -p "$HOME"/.local/bin

export PATH=$HOME/.local/bin:$PATH


# Download and unpack the `stack` executable :
curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C $HOME/.local/bin '*/stack'


export PATH=$(stack --stack-yaml stack.yaml path --local-install-root):$PATH
export PATH=${PETSC_LIB}:${PATH}

# # # check env
printenv
pwd


printf "\n=== Downloading petsc-hs"
cd ${SWDIR}
git clone https://github.com/ocramz/petsc-hs.git
cd ${PETSCHS_DIR}

printf "\n=== Compiling petsc-hs dependencies"
stack setup

printf "\n=== Compiling petsc-hs"
stack install c2hs
# # build and interpret C2Hs script (architecture-specific types)
./c2hs-build.sh ${PETSC_DIR} ${PETSC_ARCH} ${SLEPC_DIR} ${SLEPC_ARCH} ${PWD}/src/Numerical/PETSc/Internal/C2HsGen

# # build whole project
./stack-build.sh "$STACK_ARGS" "$PETSC_DIR" "$PETSC_ARCH" "$SLEPC_DIR" "$SLEPC_ARCH"


printf "\n=== Running petsc-hs example"
./stack-exec-example.sh "$STACK_ARGS" "$PETSC_DIR" "$PETSC_ARCH" "$SLEPC_DIR" "$SLEPC_ARCH"
