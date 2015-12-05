#!/bin/bash

STACK_ARGS="$1"
PETSC_DIR="$2"    # install directory (e.g. "$HOME/petsc")
PETSC_ARCH="$3"   # architecture id.string (e.g. "arch-linux2-c-debug")
SLEPC_DIR="$4"
SLEPC_ARCH="$5"

PETSC_INCLUDE1=$PETSC_DIR/include/
PETSC_INCLUDE2=$PETSC_DIR/$PETSC_ARCH/include/
PETSC_LIB=$PETSC_DIR/$PETSC_ARCH/lib/

SLEPC_INCLUDE1=$SLEPC_DIR/include/
SLEPC_INCLUDE2=$SLEPC_DIR/$SLEPC_ARCH/include/
SLEPC_LIB=$SLEPC_DIR/$SLEPC_ARCH/lib/

stack build $STACK_ARGS --no-terminal --install-ghc --extra-include-dirs=$PETSC_INCLUDE1:$PETSC_INCLUDE2:$SLEPC_INCLUDE1:$SLEPC_INCLUDE2 --extra-lib-dirs=$PETSC_LIB:$SLEPC_LIB
