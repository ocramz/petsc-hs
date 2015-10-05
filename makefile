PETSC_DIR_ARCH = ${PETSC_DIR}/arch-darwin-c-debug
PETSC_DIR_ARCH_INCLUDE = ${PETSC_DIR_ARCH}/include
SRCDIR = ${CURDIR}/src/Numerical/PETSc
SRCPARDIR = ${CURDIR}/src/Numerical
CBITS = ${CURDIR}/src/cbits
TESTDIR = ${CURDIR}/test
LIBDIR = ${CURDIR}/lib

main:
	make step1
	make step2a
	make step2b
	make step3

step1:
	ghc  ${SRCDIR}/Internal2.hs ${SRCDIR}/Raw/InlineC.hs -isrc/

step2a:
	cc -c ${SRCDIR}/Internal2.c -o ${LIBDIR}/Internal2_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include

step2b:
	cc -c ${SRCDIR}/Raw/InlineC.c -o ${LIBDIR}/InlineC_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include

step3:
	ghci ${SRCDIR}/TestMain2.hs ${SRCDIR}/Raw/InlineC.hs  ${LIBDIR}/InlineC_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich

reload:
	make step3


# main: 
# 	# c2hs ${SRCDIR}/Types.chs  -C -I${PETSC_DIR_ARCH}/include -C -I${PETSC_DIR}/include 
# 	# ghc -c -dynamic ${SRCDIR}/Internal2.hs ${SRCDIR}/Test2.hs 
# 	ghc ${SRCDIR}/Internal2.hs ${SRCDIR}/Test2.hs 
# 	# ghc -c ${SRCDIR}/Test1.hs 
# 	cc -c ${SRCDIR}/Internal2.c -o ${LIBDIR}/Internal2_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include
# 	cc -c ${SRCDIR}/Test2.c -o ${LIBDIR}/Test2_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include
# 	ghci ${SRCDIR}/TestMain.hs ${SRCDIR}/Test2.hs  ${LIBDIR}/Test2_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich
# 	# ghci ${SRCDIR}/Test1.hs ${SRCDIR}/Types.hs  ${LIBDIR}/Test1_c.o ${LIBDIR}/InternalTest_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich


# reload_TestMain:
# 	ghci ${SRCDIR}/TestMain.hs ${SRCDIR}/Test2.hs  ${LIBDIR}/Test2_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich

# reload_fem:
# 	ghci ${SRCDIR}/TestFEM.hs ${SRCDIR}/Test2.hs  ${LIBDIR}/Test2_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich

# reload_bs:
# 	ghci ${SRCDIR}/TaoBlackScholes.hs ${SRCDIR}/Test2.hs  ${LIBDIR}/Test2_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich







clean:
	rm lib/*.o
	rm src/Numerical/PETSc/*.o
