#!/bin/bash


# # NB : PETSc/SLEPc environment variables must be already set at this stage
printenv
# printenv | grep PETSC ..

# STACK_ARGS="$STACK_ARGS"
# PETSC_DIR="$PETSC_DIR"
# SLEPC_DIR="$SLEPC_DIR"
# PETSC_ARCH="$PETSC_ARCH"
# SLEPC_ARCH="$SLEPC_ARCH"

cd ${PETSC_HS}
# get up to date source
git pull



# # build and interpret C2Hs script (architecture-specific types)
./c2hs-build.sh ${PETSC_DIR} ${PETSC_ARCH} ${SLEPC_DIR} ${SLEPC_ARCH} ${PWD}/src/Numerical/PETSc/Internal/C2HsGen

# # build whole project
./stack-build.sh ${STACK_ARGS} ${PETSC_DIR} ${PETSC_ARCH} ${SLEPC_DIR} ${SLEPC_ARCH}

# printf "\n=== Stack path :\n"
# stack path

printf "\n=== Running petsc-hs example\n"

stack exec petsc-example # needs LD_LIBRARY_PATH to point at PETSc and SLEPc dynlib directory

# ./stack-exec-example.sh ${STACK_ARGS} ${PETSC_DIR} ${PETSC_ARCH} ${SLEPC_DIR} ${SLEPC_ARCH}



