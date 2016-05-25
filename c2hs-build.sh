#!/bin/bash

#


# PETSc/SLEPc environment variables
PETSC_DIR="$1"    # install directory (e.g. "$HOME/petsc")
PETSC_ARCH="$2"   # architecture id.string (e.g. "arch-linux2-c-debug")
SLEPC_DIR="$3"
SLEPC_ARCH="$4"
C2HS_DIR="$5"

PETSC_INCLUDE1="$PETSC_DIR"/include/
PETSC_INCLUDE2="$PETSC_DIR"/"$PETSC_ARCH"/include/
PETSC_LIB="$PETSC_DIR"/"$PETSC_ARCH"/lib/

SLEPC_INCLUDE1="$SLEPC_DIR"/include/
SLEPC_INCLUDE2="$SLEPC_DIR"/"$SLEPC_ARCH"/include/
SLEPC_LIB="$SLEPC_DIR"/"$SLEPC_ARCH"/lib/

c2hs -C -I${PETSC_INCLUDE1} -C -I${PETSC_INCLUDE2} -C -I${SLEPC_INCLUDE1} -C -I${SLEPC_INCLUDE2} -o TypesC2Hs.hs ${C2HS_DIR}/TypesC2Hs.chs
