#!/bin/bash

# # environment variables
STACK_ARGS="$1"

PETSC_INCLUDE1=${PETSC_DIR}/include/
PETSC_INCLUDE2=${PETSC_DIR}/${PETSC_ARCH}/include/
PETSC_LIB=${PETSC_DIR}/${PETSC_ARCH}/lib/

SLEPC_INCLUDE1=${SLEPC_DIR}/include/
SLEPC_INCLUDE2=${SLEPC_DIR}/${SLEPC_ARCH}/include/
SLEPC_LIB=${SLEPC_DIR}/${SLEPC_ARCH}/lib/

printf "\n=== Current environment :\n"

printf "PETSC_DIR = %s\nPETSC_ARCH = %s\n" ${PETSC_DIR} ${PETSC_ARCH}
printf "SLEPC_DIR = %s\nSLEPC_ARCH = %s\n" ${SLEPC_DIR} ${SLEPC_ARCH}
printf "LD_LIBRARY_PATH = %s \n" ${LD_LIBRARY_PATH}


printf "PETSC_INCLUDE1 = %s\nPETSC_INCLUDE2 = %s\nPETSC_LIB = %s\nSLEPC_INCLUDE1 = %s\nSLEPC_INCLUDE2 = %s\nSLEPC_LIB = %s\n" ${PETSC_INCLUDE1} ${PETSC_INCLUDE2} ${PETSC_LIB} ${SLEPC_INCLUDE1} ${SLEPC_INCLUDE2} ${SLEPC_LIB}



printf "\n=== Building petsc-hs :\n"

printf "\n%s\n\n" "stack build ${STACK_ARGS} --no-terminal --extra-include-dirs=${PETSC_INCLUDE1} --extra-include-dirs=${PETSC_INCLUDE2} --extra-include-dirs=${SLEPC_INCLUDE1} --extra-include-dirs=${SLEPC_INCLUDE2} --extra-lib-dirs=${PETSC_LIB} --extra-lib-dirs=${SLEPC_LIB}" 

stack build ${STACK_ARGS} --no-terminal --extra-include-dirs=${PETSC_INCLUDE1} --extra-include-dirs=${PETSC_INCLUDE2} --extra-include-dirs=${SLEPC_INCLUDE1} --extra-include-dirs=${SLEPC_INCLUDE2} --extra-lib-dirs=${PETSC_LIB} --extra-lib-dirs=${SLEPC_LIB}
