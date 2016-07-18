#!/bin/bash


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

C2HS_GEN_FILE=TypesC2HsGen 

# # generate c2hs
printf "\n=== Generate C2HS bindings (%s):\n" ${C2HS_GEN_FILE}.chs
stack exec runhaskell ${C2HS_DIR}/GenerateC2Hs.hs > ${C2HS_DIR}/${C2HS_GEN_FILE}.chs

printf "\n=== Contents of %s:\n" ${C2HS_DIR}
ls -lsA ${C2HS_DIR}

cd ${PETSCHS_DIR}

# # interpret c2hs -> render Haskell bindings
printf "\n=== Run c2hs -> render Haskell FFI bindings:\n"
printf "\n%s\n\n" "stack exec c2hs --  -C -I${PETSC_INCLUDE1} -C -I${PETSC_INCLUDE2} -C -I${SLEPC_INCLUDE1} -C -I${SLEPC_INCLUDE2} -o ${C2HS_DIR}/${C2HS_GEN_FILE}.hs ${C2HS_DIR}/${C2HS_GEN_FILE}.chs"
stack exec c2hs --  -C -I${PETSC_INCLUDE1} -C -I${PETSC_INCLUDE2} -C -I${SLEPC_INCLUDE1} -C -I${SLEPC_INCLUDE2} -o ${C2HS_DIR}/${C2HS_GEN_FILE}.hs ${C2HS_DIR}/${C2HS_GEN_FILE}.chs

# stack exec c2hs --  -C -I${PETSC_INCLUDE2} -C -I${PETSC_INCLUDE1}  -C -I${SLEPC_INCLUDE1} -o ${C2HS_DIR}/${C2HS_GEN_FILE}.hs ${C2HS_DIR}/${C2HS_GEN_FILE}.chs

# stack exec c2hs --  -C -I${PETSC_INCLUDE1} -C -iprefix "petsc" -C -iwithprefix${PETSC_INCLUDE2} -C -I${SLEPC_INCLUDE1} -C -I${SLEPC_INCLUDE2} -o ${C2HS_DIR}/${C2HS_GEN_FILE}.hs ${C2HS_DIR}/${C2HS_GEN_FILE}.chs
