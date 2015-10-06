PETSC_DIR_ARCH = ${PETSC_DIR}/arch-darwin-c-debug
PETSC_DIR_ARCH_INCLUDE = ${PETSC_DIR_ARCH}/include
SRCDIR = ${CURDIR}/src/Numerical/PETSc
SRCPARDIR = ${CURDIR}/src/Numerical
CBITS = ${CURDIR}/src/cbits
TESTDIR = ${CURDIR}/test
LIBDIR = ${CURDIR}/lib
EXAMPLEDIR = ${CURDIR}/examples

main:
	make step1
	make step2a
	make step2b
	make step3

step1:
	ghc  ${SRCDIR}/Raw/Internal.hs ${SRCDIR}/Raw/InlineC.hs -isrc/

step2a:
	cc -c ${SRCDIR}/Raw/Internal.c -o ${LIBDIR}/Internal_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include

step2b:
	cc -c ${SRCDIR}/Raw/InlineC.c -o ${LIBDIR}/InlineC_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include

step3:
	ghci ${EXAMPLEDIR}/TestMain2.hs ${SRCDIR}/Raw/InlineC.hs  ${LIBDIR}/InlineC_c.o  ${LIBDIR}/Internal_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich

reload:
	make step3




# reload_fem:
# 	ghci ${SRCDIR}/TestFEM.hs ${SRCDIR}/Test2.hs  ${LIBDIR}/Test2_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich

# reload_bs:
# 	ghci ${SRCDIR}/TaoBlackScholes.hs ${SRCDIR}/Test2.hs  ${LIBDIR}/Test2_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich







clean:
	rm lib/*.o
	rm src/Numerical/PETSc/*.o
