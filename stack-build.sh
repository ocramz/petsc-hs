#!/bin/bash

STACK_ARGS="$1"
PETSC_DIR="$2"    # download,unpack and install directory (e.g. "$HOME/petsc")
PETSC_ARCH="$3"   # architecture id.string (e.g. "arch-linux2-c-debug")

PETSC_INCLUDE1=$PETSC_DIR/include/
PETSC_INCLUDE2=$PETSC_DIR/$PETSC_ARCH/include/
PETSC_LIB=$PETSC_DIR/$PETSC_ARCH/lib/

stack build $STACK_ARGS --no-terminal --extra-include-dirs=$PETSC_INCLUDE1:$PETSC_INCLUDE2 --extra-lib-dirs=$PETSC_LIB
