PETSC_DIR_ARCH = ${PETSC_DIR}/arch-darwin-c-debug
PETSC_DIR_ARCH_INCLUDE = ${PETSC_DIR_ARCH}/include

SRCDIR = ${CURDIR}/src/Numerical/PETSc
SRCPARDIR = ${CURDIR}/src/Numerical
CBITS = ${CURDIR}/src/cbits
TESTDIR = ${CURDIR}/test
LIBDIR = ${CURDIR}/lib

# main:
# 	ghc -c src/Main.hs 
# 	cc -c src/Main.c -o src/Main_c.o
# 	ghc src/Main.o src/Main_c.o -lm -o Main 

# main:
# 	ghc -c src/Test1.hs
# 	cc -c src/Test1.c -o src/Test1_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include
# 	# ghc src/Main.hs  src/Test1.o src/Test1_c.o -lm -o Main 
# 	ghci src/Main.hs src/Test1_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich

# main:
# 	c2hs ${SRCDIR}/Types.chs -C -I${PETSC_DIR_ARCH}/include -C -I${PETSC_DIR}/include
# 	# ghc -c ${SRCDIR}/Types.hs
# 	ghc -v ${SRCDIR}/Internal.hs 
# 	cc -c ${SRCDIR}/Internal.c -o ${LIBDIR}/Internal_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include
# 	ghci ${SRCDIR}/Main.hs  ${SRCDIR}/Types.hs ${LIBDIR}/Internal_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich


# test: 
# 	# c2hs ${SRCDIR}/Types.chs  -C -I${PETSC_DIR_ARCH}/include -C -I${PETSC_DIR}/include 
# 	ghc -c -dynamic ${SRCDIR}/InternalTest.hs ${SRCDIR}/Test1.hs 
# 	# ghc -c ${SRCDIR}/Test1.hs 
# 	cc -c ${SRCDIR}/InternalTest.c -o ${LIBDIR}/InternalTest_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include
# 	cc -c ${SRCDIR}/Test1.c -o ${LIBDIR}/Test1_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include
# 	ghci ${SRCDIR}/Test1.hs  ${LIBDIR}/Test1_c.o  ${LIBDIR}/InternalTest_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich
# 	# ghci ${SRCDIR}/Test1.hs ${SRCDIR}/Types.hs  ${LIBDIR}/Test1_c.o ${LIBDIR}/InternalTest_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich

main: 
	# c2hs ${SRCDIR}/Types.chs  -C -I${PETSC_DIR_ARCH}/include -C -I${PETSC_DIR}/include 
	# ghc -c -dynamic ${SRCDIR}/Internal2.hs ${SRCDIR}/Test2.hs 
	ghc ${SRCDIR}/Internal2.hs ${SRCDIR}/Test2.hs 
	# ghc -c ${SRCDIR}/Test1.hs 
	cc -c ${SRCDIR}/Internal2.c -o ${LIBDIR}/Internal2_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include
	cc -c ${SRCDIR}/Test2.c -o ${LIBDIR}/Test2_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include
	ghci ${SRCDIR}/TestMain.hs ${SRCDIR}/Test2.hs  ${LIBDIR}/Test2_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich
	# ghci ${SRCDIR}/Test1.hs ${SRCDIR}/Types.hs  ${LIBDIR}/Test1_c.o ${LIBDIR}/InternalTest_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich


reload_TestMain:
	ghci ${SRCDIR}/TestMain.hs ${SRCDIR}/Test2.hs  ${LIBDIR}/Test2_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich

# reload_fem:
# 	ghci ${SRCDIR}/TestFEM.hs ${SRCDIR}/Test2.hs  ${LIBDIR}/Test2_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich

# reload_bs:
# 	ghci ${SRCDIR}/TaoBlackScholes.hs ${SRCDIR}/Test2.hs  ${LIBDIR}/Test2_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich




# test:
# 	# c2hs ${SRCDIR}/Types.chs  -C -I${PETSC_DIR_ARCH}/include -C -I${PETSC_DIR}/include  
# 	# ghc ${SRCDIR}/Internal2.hs ${SRCDIR}/Types.hs ${SRCDIR}/Test2_1.hs
# 	ghc ${SRCDIR}/Internal2.hs ${SRCDIR}/Test2_1.hs
# 	cc -c ${SRCDIR}/Internal2.c -o ${LIBDIR}/Internal2_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include
# 	# ghc -c ${SRCDIR}/Test2_1.hs ${SRCDIR}/Internal2.hs
# 	cc -c ${SRCDIR}/Test2_1.c -o ${LIBDIR}/Test2_1_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include
# 	ghci ${SRCDIR}/Test2_1.hs  ${LIBDIR}/Test2_1_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich




# testMain:
# 	# c2hs ${SRCDIR}/Types.chs  -C -I${PETSC_DIR_ARCH}/include -C -I${PETSC_DIR}/include  
# 	ghc ${SRCDIR}/Internal2.hs ${SRCDIR}/Test2_1.hs # ${SRCDIR}/Types.hs
# 	cc -c ${SRCDIR}/Internal2.c -o ${LIBDIR}/Internal2_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include
# 	cc -c ${SRCDIR}/Test2_1.c -o ${LIBDIR}/Test2_1_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include
# 	ghci ${SRCDIR}/TestMain.hs ${SRCDIR}/Test2_1.hs  ${LIBDIR}/Test2_1_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich




main_new: 
	# c2hs ${SRCDIR}/Types.chs  -C -I${PETSC_DIR_ARCH}/include -C -I${PETSC_DIR}/include 
	# # # ghc -c -dynamic ${SRCDIR}/Internal2.hs ${SRCDIR}/Test2.hs 
	# ghc ${SRCDIR}/Internal2.hs ${SRCDIR}/Test2.hs 
	ghc  ${SRCDIR}/Internal2.hs ${SRCDIR}/Raw/InlineC.hs -isrc/

	cc -c ${SRCDIR}/Internal2.c -o ${LIBDIR}/Internal2_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include
	cc -c ${SRCDIR}/Raw/InlineC.c -o ${LIBDIR}/InlineC_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include

	ghci ${SRCDIR}/TestMain2.hs ${SRCDIR}/Raw/InlineC.hs  ${LIBDIR}/InlineC_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich

reload_testMain2:
	ghci ${SRCDIR}/TestMain2.hs ${SRCDIR}/Raw/InlineC.hs  ${LIBDIR}/InlineC_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich	






main_Internal3: 
	# c2hs ${SRCDIR}/Types.chs  -C -I${PETSC_DIR_ARCH}/include -C -I${PETSC_DIR}/include 
	# # # ghc -c -dynamic ${SRCDIR}/Internal2.hs ${SRCDIR}/Test2.hs 
	# ghc ${SRCDIR}/Internal2.hs ${SRCDIR}/Test2.hs 
	ghc  ${SRCDIR}/Internal3.hs ${SRCDIR}/Raw/InlineC.hs -isrc/

	cc -c ${SRCDIR}/Internal3.c -o ${LIBDIR}/Internal3_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include
	cc -c ${SRCDIR}/Raw/InlineC.c -o ${LIBDIR}/InlineC_c.o -I${PETSC_DIR_ARCH}/include -I${PETSC_DIR}/include

	ghci ${SRCDIR}/TestMain2.hs ${SRCDIR}/Raw/InlineC.hs  ${LIBDIR}/InlineC_c.o  ${LIBDIR}/Internal3_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich

# reload_testMain3:
# 	ghci ${SRCDIR}/TestMain2.hs ${SRCDIR}/Raw/InlineC.hs  ${LIBDIR}/InlineC_c.o  ${LIBDIR}/Internal2_c.o -isrc/ -L${PETSC_DIR_ARCH}/lib -lpetsc -lmpich






clean:
	rm lib/*.o
	rm src/Numerical/PETSc/*.o
