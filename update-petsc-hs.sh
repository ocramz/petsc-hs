#!/bin/bash


# # NB : PETSc/SLEPc environment variables must be already set at this stage

cd ${PETSCHS_DIR}

printf "\n=== Inherited environment:\n"
printenv

# printf "\n=== Current directory:\n"
# pwd

# get up to date source
printf "\n=== Update source tree ('git pull')"
git pull



# # build and interpret C2Hs script (architecture-specific types)
./c2hs-build.sh ${PETSC_DIR} ${PETSC_ARCH} ${SLEPC_DIR} ${SLEPC_ARCH} ${PETSCHS_DIR}/src/Numerical/PETSc/Internal/C2HsGen

# # build whole project
./stack-build.sh ${STACK_ARGS} ${PETSC_DIR} ${PETSC_ARCH} ${SLEPC_DIR} ${SLEPC_ARCH}

# printf "\n=== Stack path :\n"
# stack path

printf "\n=== Running petsc-hs example\n"

stack exec petsc-example # needs LD_LIBRARY_PATH to point at PETSc and SLEPc dynlib directory

# ./stack-exec-example.sh ${STACK_ARGS} ${PETSC_DIR} ${PETSC_ARCH} ${SLEPC_DIR} ${SLEPC_ARCH}



