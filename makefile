# # architecture flags
SLEPC_ARCH = arch-darwin-c-debug
ARCH_COMPLEX = arch-darwin-c-debug-complex

# # composite flags
PETSC_DIR_ARCH = ${PETSC_DIR}/${PETSC_ARCH}
# PETSC_DIR_ARCH = ${PETSC_DIR}/${ARCH_COMPLEX}
PETSC_DIR_ARCH_INCLUDE = ${PETSC_DIR_ARCH}/include

SLEPC_DIR_ARCH = ${SLEPC_DIR}/${SLEPC_ARCH}
SLEPC_DIR_ARCH_INCLUDE = ${SLEPC_DIR_ARCH}/include

# # petsc-hs related directories
SRCDIR = ${CURDIR}/src/Numerical/PETSc
SRCPARDIR = ${CURDIR}/src/Numerical
CBITS = ${CURDIR}/src/cbits
TESTDIR = ${CURDIR}/test
LIBDIR = ${CURDIR}/lib
EXAMPLEDIR = ${CURDIR}/examples

C2HS_DIR = $(shell pwd)/src/Numerical/PETSc/Internal/C2HsGen
C2HS_GEN_FILE=TypesC2HsGen 

main:
	make step1
	make step2
	make step3

step1:
	make c2hs
	stack ghc -- -optc -g ${SRCDIR}/Internal/InlineC.hs -isrc/

step2:
	cc -w -c -g ${SRCDIR}/Internal/InlineC.c -o ${LIBDIR}/InlineC_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include -I${SLEPC_DIR_ARCH}/include -I${SLEPC_DIR}/include

step3:
	stack exec ghci ${SRCDIR}/Spec.hs ${SRCDIR}/Internal/InlineC.hs  ${LIBDIR}/InlineC_c.o  -- -isrc/ -L${PETSC_DIR_ARCH}/lib -L${SLEPC_DIR_ARCH}/lib -lpetsc -lmpich -lslepc


step_test:
	stack exec ghci ${SRCDIR}/Test.hs ${SRCDIR}/Internal/InlineC.hs  ${LIBDIR}/InlineC_c.o  ${LIBDIR}/Internal.o -- -isrc/ -L${PETSC_DIR_ARCH}/lib -L${SLEPC_DIR_ARCH}/lib -lpetsc -lmpich -lslepc


tests:
	make step_test

specs:
	make step3


reload:
	make step3



# ARGS="--resolver nightly"
ARGS=""

stack_build:
	# chmod +x stack-build.sh
	./stack-build.sh ${ARGS} ${PETSC_DIR} ${PETSC_ARCH} ${SLEPC_DIR} ${SLEPC_ARCH}

stack_haddock:
	./stack-haddock.sh ${ARGS} ${PETSC_DIR} ${PETSC_ARCH} ${SLEPC_DIR} ${SLEPC_ARCH}


grind:
	valgrind --leak-check=yes --track-origins=yes stack exec petsc-valgrind 


PETSC_INCLUDE1=${PETSC_DIR}/include/
SLEPC_INCLUDE1=${SLEPC_DIR}/include/
PETSC_INCLUDE2 = ${PETSC_DIR_ARCH_INCLUDE}
SLEPC_INCLUDE2 = ${SLEPC_DIR_ARCH_INCLUDE}

c2hs:
	# ./c2hs-build.sh ${PETSC_DIR} ${PETSC_ARCH} ${SLEPC_DIR} ${SLEPC_ARCH} $(shell pwd)/src/Numerical/PETSc/Internal/C2HsGen
	stack exec runhaskell ${C2HS_DIR}/GenerateC2Hs.hs > ${C2HS_DIR}/${C2HS_GEN_FILE}.chs
	stack exec c2hs --  -C -I${PETSC_INCLUDE1} -C -iprefix "petsc" -C -iwithprefix${PETSC_INCLUDE2} -C -I${SLEPC_INCLUDE1} -C -I${SLEPC_INCLUDE2} -o ${C2HS_DIR}/${C2HS_GEN_FILE}.hs ${C2HS_DIR}/${C2HS_GEN_FILE}.chs

