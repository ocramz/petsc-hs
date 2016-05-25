#!/bin/bash

#
C2HS_DIR=${CURDIR}/src/Numerical/PETSc/Internal

# PETSc/SLEPc environment variables
PETSC_DIR="$2"    # install directory (e.g. "$HOME/petsc")
PETSC_ARCH="$3"   # architecture id.string (e.g. "arch-linux2-c-debug")
SLEPC_DIR="$4"
SLEPC_ARCH="$5"

PETSC_INCLUDE1="$PETSC_DIR"/include/
PETSC_INCLUDE2="$PETSC_DIR"/"$PETSC_ARCH"/include/
PETSC_LIB="$PETSC_DIR"/"$PETSC_ARCH"/lib/

SLEPC_INCLUDE1="$SLEPC_DIR"/include/
SLEPC_INCLUDE2="$SLEPC_DIR"/"$SLEPC_ARCH"/include/
SLEPC_LIB="$SLEPC_DIR"/"$SLEPC_ARCH"/lib/

c2hs -I${PETSC_INCLUDE1} -I${PETSC_INCLUDE2} -I${SLEPC_INCLUDE1} -I${SLEPC_INCLUDE2} ${C2HS_DIR}/Types.chs
