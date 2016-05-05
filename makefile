SLEPC_ARCH = arch-darwin-c-debug
ARCH_COMPLEX = arch-darwin-c-debug-complex

PETSC_DIR_ARCH = ${PETSC_DIR}/${PETSC_ARCH}
# PETSC_DIR_ARCH = ${PETSC_DIR}/${ARCH_COMPLEX}
PETSC_DIR_ARCH_INCLUDE = ${PETSC_DIR_ARCH}/include

SLEPC_DIR_ARCH = ${SLEPC_DIR}/${SLEPC_ARCH}
SLEPC_DIR_ARCH_INCLUDE = ${SLEPC_DIR_ARCH}/include

SRCDIR = ${CURDIR}/src/Numerical/PETSc
SRCPARDIR = ${CURDIR}/src/Numerical
CBITS = ${CURDIR}/src/cbits
TESTDIR = ${CURDIR}/test
LIBDIR = ${CURDIR}/lib
EXAMPLEDIR = ${CURDIR}/examples

main:
	make step1
	make step2
	make step3

step1:
	ghc -v ${SRCDIR}/Internal/InlineC.hs -isrc/

step2:
	cc -w -c ${SRCDIR}/Internal/InlineC.c -o ${LIBDIR}/InlineC_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include -I${SLEPC_DIR_ARCH}/include -I${SLEPC_DIR}/include

step3:
	ghci ${SRCDIR}/Spec.hs ${SRCDIR}/Internal/InlineC.hs  ${LIBDIR}/InlineC_c.o  ${LIBDIR}/Internal.o -isrc/ -L${PETSC_DIR_ARCH}/lib -L${SLEPC_DIR_ARCH}/lib -lpetsc -lmpich -lslepc


step_test:
	ghci ${SRCDIR}/Test.hs ${SRCDIR}/Internal/InlineC.hs  ${LIBDIR}/InlineC_c.o  ${LIBDIR}/Internal.o -isrc/ -L${PETSC_DIR_ARCH}/lib -L${SLEPC_DIR_ARCH}/lib -lpetsc -lmpich -lslepc


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



