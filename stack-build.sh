#!/bin/bash

# # environment variables
# STACK_ARGS="$1"
# PETSC_DIR="$2"    # install directory (e.g. "$HOME/petsc")
# PETSC_ARCH="$3"   # architecture id.string (e.g. "arch-linux2-c-debug")
# SLEPC_DIR="$4"
# SLEPC_ARCH="$5"

PETSC_INCLUDE1=${PETSC_DIR}/include/
PETSC_INCLUDE2=${PETSC_DIR}/${PETSC_ARCH}/include/
PETSC_LIB=${PETSC_DIR}/${PETSC_ARCH}/lib/

SLEPC_INCLUDE1=${SLEPC_DIR}/include/
SLEPC_INCLUDE2=${SLEPC_DIR}/${SLEPC_ARCH}/include/
SLEPC_LIB=${SLEPC_DIR}/${SLEPC_ARCH}/lib/

printf "\n=== Current environment :\nPETSC_INCLUDE1 = %s\nPETSC_INCLUDE2 = %s\nPETSC_LIB = %s\nSLEPC_INCLUDE1 = %s\nSLEPC_INCLUDE2 = %s\nSLEPC_LIB = %s\n" ${PETSC_INCLUDE1} ${PETSC_INCLUDE2} ${PETSC_LIB} ${SLEPC_INCLUDE1} ${SLEPC_INCLUDE2} ${SLEPC_LIB}



printf "\n=== Building petsc-hs :\n"

printf "\n%s\n" "stack build ${STACK_ARGS} --no-terminal --extra-include-dirs=${PETSC_INCLUDE1} --extra-include-dirs=${PETSC_INCLUDE2} --extra-include-dirs=${SLEPC_INCLUDE1} --extra-include-dirs=${SLEPC_INCLUDE2} --extra-lib-dirs=${PETSC_LIB} --extra-lib-dirs=${SLEPC_LIB}" 

# stack build "$STACK_ARGS" --no-terminal --install-ghc --extra-include-dirs="$PETSC_INCLUDE1" --extra-include-dirs="$PETSC_INCLUDE2" --extra-include-dirs="$SLEPC_INCLUDE1" --extra-include-dirs="$SLEPC_INCLUDE2" --extra-lib-dirs="$PETSC_LIB" --extra-lib-dirs="$SLEPC_LIB"

stack build ${STACK_ARGS} --no-terminal --extra-include-dirs=${PETSC_INCLUDE1} --extra-include-dirs=${PETSC_INCLUDE2} --extra-include-dirs=${SLEPC_INCLUDE1} --extra-include-dirs=${SLEPC_INCLUDE2} --extra-lib-dirs=${PETSC_LIB} --extra-lib-dirs=${SLEPC_LIB}
