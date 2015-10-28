{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Raw.InlineC
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | Foreign signatures, + everything that requires an inline-c pass
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Raw.InlineC where

import Numerical.PETSc.Raw.Internal
import Numerical.PETSc.Raw.Types
import Numerical.PETSc.Raw.Utils

import Language.C.Inline as C
import Language.C.Inline.Context
import Control.Exception
import Foreign
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Control.Monad
-- import Control.Arrow ((***), (&&&))
-- import Control.Applicative
import Foreign.C.Types
import Foreign.C.String
import qualified Foreign.ForeignPtr.Safe         as FPS

import qualified Data.Vector.Storable as V

import System.IO.Unsafe (unsafePerformIO)

context petscCtx

C.include "<petscsnes.h>"
C.include "<petsctao.h>"
C.include "<petscdm.h>"
C.include "<petscdmda.h>"
C.include "<petscdmcomposite.h>"
C.include "<petscts.h>"
C.include "<petscviewer.h>"
C.include "<petscviewerhdf5.h>"


petscDecide = -1

-- * IS

isCreateStride' c n first step is = [C.exp|
     int{ISCreateStride(
            $(int c),
            $(PetscInt n),
            $(PetscInt first),
            $(PetscInt step),
            $(IS* is)) }|]

isCreateStride comm n first step =
  withPtr $ \is -> isCreateStride' c n first step is
   where c = unComm comm






--    ISCreateGeneral - Creates a data structure for an index set
--    containing a list of integers.  --    Collective on MPI_Comm
--    Input Parameters:
-- +  comm - the MPI communicator
-- .  n - the length of the index set
-- .  idx - the list of integers
-- -  mode - see PetscCopyMode for meaning of this flag.
--    Output Parameter:
-- .  is - the new index set

--    Notes:
--    When the communicator is not MPI_COMM_SELF, the operations on IS are NOT
--    conceptually the same as MPI_Group operations. The IS are then
--    distributed sets of indices and thus certain operations on them are
--    collective.

isCreateGeneral' c n idxp mo isp  =
  [C.exp|int{ISCreateGeneral($(int c),
                             $(PetscInt n),
                             $(PetscInt* idxp),
                             $(int mo),
                             $(IS* isp))}|]

isCreateGeneral comm n idx mode =
   withArray idx $ \idxp ->
    withPtr $ \isp -> isCreateGeneral' c n idxp mo isp 
     where mo = fromIntegral $ petscCopyModeToInt mode
           c = unComm comm

isDestroy' iisp = [C.exp|int{ISDestroy($(IS* iisp))} |]

isDestroy iis = with iis isDestroy' 


-- withIsCreateGeneral comm n idx mode = bracket (isCreateGeneral comm n idx mode) isDestroy




-- -- * IS coloring : see e.g. www.mcs.anl.gov/petsc/petsc-current/src/snes/examples/tutorials/ex5s.c.html

-- PetscErrorCode  ISColoringCreate(MPI_Comm comm,PetscInt ncolors,PetscInt n,const ISColoringValue colors[],PetscCopyMode mode,ISColoring *iscoloring)
isColoringCreate' comm ncolors n cols copymode =
   withPtr $ \iscoloring ->
    withArray cols $ \colv -> 
  [C.exp|int{ISColoringCreate($(int c),$(int ncolors),$(int n),$(int* colv),$(int mo),$(ISColoring* iscoloring))}|]
     where
       c = unComm comm
       mo = toCInt $ petscCopyModeToInt copymode


-- PetscErrorCode  ISColoringDestroy(ISColoring *iscoloring)
isColoringDestroy' isc = with isc $ \iscp -> [C.exp|int{ISColoringDestroy($(ISColoring* iscp))}|]












-- * Vec

-- PetscErrorCode  VecView(Vec vec,PetscViewer viewer)
vecView1 ve viewer =
  [C.exp|int{VecView($(Vec ve),$(PetscViewer viewer))}|]
  

-- PetscErrorCode  PetscObjectSetName(PetscObject obj,const char name[])
vecSetName1 v name = withCString name $ \n ->
  [C.exp|int{PetscObjectSetName($(Vec v),$(char* n))}|]


vecCreate0' comm p = [C.exp|int{VecCreate($(int c), $(Vec *p))} |]
  where c = unComm comm

vecCreate' :: Comm -> IO (Vec, CInt)
vecCreate' c = withPtr (vecCreate0' c) 

-- PetscErrorCode VecCreateMPI(MPI_Comm comm, int m, int M, Vec* x)
vecCreateMPI0' comm m1' m2' p = [C.exp|int{VecCreateMPI($(int c), $(int m1), $(int m2), $(Vec *p))}|] 
  where c = unComm comm
        m1 = toCInt m1'
        m2 = toCInt m2'

vecCreateMPI' :: Comm -> Int -> Int -> IO (Vec, CInt)
vecCreateMPI' c nlocal nglobal = withPtr (vecCreateMPI0' c nlocal nglobal) 

vecCreateMPILocal c m = vecCreateMPI' c m m


-- PetscErrorCode  VecSetBlockSize(Vec v,PetscInt bs)
vecSetBlockSize1 v bs =
  [C.exp|int{VecSetBlockSize($(Vec v), $(int b))}|] where b = toCInt bs
                                                          


-- PETSC_EXTERN PetscErrorCode VecSetValues(Vec,PetscInt,const PetscInt[],const PetscScalar[],InsertMode);
-- PetscErrorCode  VecSetValues(Vec x,PetscInt ni,const PetscInt ix[],const PetscScalar y[],InsertMode iora)
-- 0-based indices
   --  -- --Not Collective
--    Input Parameters:
-- +  x - vector to insert in
-- .  ni - number of elements to add
-- .  ix - indices where to add
-- .  y - array of values
-- -  iora - either INSERT_VALUES or ADD_VALUES, where
--    ADD_VALUES adds values to any existing entries, and
--    INSERT_VALUES replaces existing entries with new values

--    VecSetValues() sets x[ix[i]] = y[i], for i=0,...,ni-1.

--    Calls to VecSetValues() with the INSERT_VALUES and ADD_VALUES
--    options cannot be mixed without intervening calls to the assembly
--    routines.
--    These values may be cached, so VecAssemblyBegin() and VecAssemblyEnd()
--    MUST be called after all calls to VecSetValues() have been completed.

vecSetValues' ::
  Vec -> CInt -> Ptr CInt -> Ptr PetscScalar_ -> InsertMode_ -> IO CInt
vecSetValues' x ni ixx y imm = [C.exp|int{VecSetValues($(Vec x), $(int ni), $(int* ixx), $(PetscScalar* y), $(int im))}|] where im = fromIntegral $ insertModeToInt imm

-- vecSetValues'' x ni ixx y imm =
--   [C.exp|int{VecSetValues($(Vec x), $(int ni), $(int* ixx), $(PetscScalar* y), $(int im))}|] where im = fromIntegral $ insertModeToInt imm

vecSetValues :: Vec -> [CInt] -> [PetscScalar_] -> InsertMode_ -> IO CInt
vecSetValues x ix y im =
  withArray ix $ \ixx ->
   withArray y $ \yy -> vecSetValues' x ni ixx yy im
  where
  ni = fromIntegral (length ix)
-- neeeds :
--  consistency (ordered, max belonging to index set of Vec) check
--

-- vecSetValuesSafe v ix y
--   | c1 && c2 = vecSetValues v ix y
--   | otherwise = error "vecSetValuesSafe: "
--      where
--       c1 = length ix == length y
--       c2 = a >= 0 && b <= sv where
--         ixs = sort ix
--         (a, b) = (head ixs, last ixs)
--       sv = vecSize v



-- | Compares two vectors. Returns true if the two vectors are either pointing to the same memory buffer, or if the two vectors have the same local and global layout as well as bitwise equality of all entries. Does NOT take round-off errors into account.

-- PETSC_EXTERN PetscErrorCode VecEqual(Vec,Vec,PetscBool *);
vecEqual1 v1 v2 = withPtr ( \b ->
  [C.exp|int{VecEqual($(Vec v1), $(Vec v2), $(PetscBool* b))}|] )



vecDestroy0' p = [C.exp|int{VecDestroy($(Vec *p))}|]

vecDestroy' :: Vec -> IO CInt
vecDestroy' p = with p vecDestroy0' 


vecCopy1 vorig vcopy = [C.exp|int{VecCopy($(Vec vorig), $(Vec vcopy))}|] 

-- -- NB : VecDuplicate DOES NOT COPY CONTENTS (only structure): use VecCopy
-- PetscErrorCode  VecDuplicate(Vec v,Vec *newv)
vecDuplicate' p1 p2 = [C.exp| int{VecDuplicate($(Vec p1), $(Vec *p2))}|] 
vecDuplicate1 v = withPtr (vecDuplicate' v) 


vecAssemblyBegin' :: Vec -> IO CInt
vecAssemblyBegin' v = [C.exp|int{VecAssemblyBegin($(Vec v))}|]

vecAssemblyEnd' :: Vec -> IO CInt
vecAssemblyEnd' v = [C.exp|int{VecAssemblyEnd($(Vec v))}|] 

-- vecAssembly1 v = vecAssemblyBegin v >> vecAssemblyEnd v

-- withVecAssembly v f = do
--   vecAssemblyBegin v
--   f v
--   vecAssemblyEnd v

-- -- -- vecAssembly' = (,) <$> vecAssemblyBegin <*> vecAssemblyEnd 

-- withVec :: Comm -> (Vec -> IO a) -> IO a
-- withVec c = bracket (vecCreate c) vecDestroy

-- -- withVecPipeline :: Comm -> CInt -> (Vec -> IO a) -> (Vec -> IO c) -> IO c
-- withVecPipeline c nDim pre f = bracket (vecCreate c) vecDestroy $ \v -> do
--   vecSetSizes v (fromIntegral $ abs nDim)
--   pre v
--   vecAssembly v
--   f v

-- -- withVecMPIPipeline :: Comm -> CInt -> (Vec -> IO a) -> (Vec -> IO c) -> IO c
-- withVecMPIPipeline c nDim pre post = bracket (vecCreateMPILocal c nDim) vecDestroy $ \v -> do
--   pre v
--   vecAssembly v
--   post v


vecSet1 v n = [C.exp|int{VecSet( $(Vec v), $(PetscScalar n))}|] 

vecSetSizes1 v n = [C.exp|int{VecSetSizes( $(Vec v), PETSC_DECIDE, $(int n))}|] 

-- PETSC_EXTERN PetscErrorCode VecGetSize(Vec,PetscInt*);
vecGetSize0' v p =  [C.exp|int{VecGetSize($(Vec v), $(int *p))}|]
vecGetSize' v = withPtr $ \p -> vecGetSize0' v p
-- vecGetSizeUnsafe = unsafePerformIO . vecGetSize1

-- vecSize v = fromIntegral $ vecGetSizeUnsafe v


vecViewStdout1 v = [C.exp|int{VecView($(Vec v), PETSC_VIEWER_STDOUT_SELF)}|] 


-- PETSC_EXTERN PetscErrorCode VecGetArray(Vec,PetscScalar**);
vecGetArray0' :: Vec -> Ptr (Ptr PetscScalar_) -> IO CInt
vecGetArray0' v p =  [C.exp|int{VecGetArray($(Vec v), $(PetscScalar** p))}|]

vecGetArray' :: Vec -> Int -> IO ([PetscScalar_], CInt)
vecGetArray' v sz = do
  (p, e) <- vga v
  arr <- peekArray sz p
  return (arr, e)
    where
      vga v' = withPtr $ \p -> vecGetArray0' v' p









-- PETSC_EXTERN PetscErrorCode VecRestoreArray(Vec,PetscScalar**);
vecRestoreArray0' :: Vec -> Ptr (Ptr PetscScalar_) -> IO CInt
vecRestoreArray0' v pc = [C.exp|int{VecRestoreArray($(Vec v), $(PetscScalar** pc))}|]


vecRestoreArray' :: Vec -> [PetscScalar_] -> IO CInt
vecRestoreArray' v c = withArray c $ \cp ->
  with cp $ \cpp -> vecRestoreArray0' v cpp






-- PETSC_EXTERN PetscErrorCode VecRestoreArrayRead(Vec,const PetscScalar**);

-- withVecGetArray :: Vec -> Int -> ([PetscScalar_] -> IO c) -> IO c
-- withVecGetArray v sz
--   | sz >0 =  bracket (vecGetArray v sz) (vecRestoreArray v)
--   | otherwise = error "withVecGetArray: temp array size must be nonnegative"

-- withVecGetArraySafe v =
--   bracket (vecGetArray v sz) (vecRestoreArray v)  where
--    sz = vecSize v





-- [C.block| double {
--     int i;
--     double res;
--     for (i = 0; i < $vec-len:xs; i++) {
--       res += $vec-ptr:(double *xs)[i];
--     }
--     return res;
--   } |]

{-
PetscErrorCode  VecGetArray1d(Vec x,PetscInt m,PetscInt mstart,PetscScalar *a[])
Logically Collective
Input Parameter :
x	- the vector
m	- first dimension of two dimensional array
mstart	- first index you will use in first coordinate direction (often 0)
Output Parameter :
a -location to put pointer to the array 
-}
-- NB : type CArray = Ptr 

vecGetArray1d' x m mstart = withPtr $ \arr -> 
  [C.exp|int{VecGetArray1d($(Vec x),$(int m),$(int mstart),$(PetscScalar** arr))}|]

-- vecGetArray1d'' :: Vec -> CInt -> CInt -> Ptr PetscScalar_ -> IO CInt
-- vecGetArray1d'' x m ms a =
--   [C.exp|int{VecGetArray1d($(Vec x),$(int m),$(int ms),$vec-ptr:(PetscScalar* a[]))}|]




{-
PetscErrorCode  VecRestoreArray1d(Vec x,PetscInt m,PetscInt mstart,PetscScalar *a[])
Logically Collective
Input Parameters :
x	- the vector
m	- first dimension of two dimensional array
mstart	- first index you will use in first coordinate direction (often 0)
a	- location of pointer to array obtained from VecGetArray21()
-}

vecRestoreArray1d' x m mstart arr =
  [C.exp|int{VecRestoreArray1d($(Vec x),$(int m),$(int mstart),$(PetscScalar** arr))}|]






-- TODO row (block) indexing : these should not be interpreted as mere Ints but as indices, e.g. FEM mesh nodes -- see repa 

vecGetOwnershipRange' a =
 withPtr $ \rmin -> 
  withPtr $ \rmax ->
   [C.exp|int{VecGetOwnershipRange($(Vec a), $(PetscInt *rmin), $(PetscInt * rmax) )}|] 

vecGetOwnershipRange1 v = do
  (r1, (r2, e)) <- vecGetOwnershipRange' v
  let (r1', r2') = (fi r1, fi r2)
  return ((r1', r2'), e) 
    



-- -- -- math functions on Vec
vecDot' v1 v2 v = [C.exp|int{VecDot( $(Vec v1), $(Vec v2), $(PetscScalar * v))}|] 
vecDot1 v1 v2 = withPtr (vecDot' v1 v2) 



-- PETSC_EXTERN PetscErrorCode VecNorm(Vec,NormType,PetscReal *);
vecNorm' nt v p = [C.exp|int{VecNorm($(Vec v),$(int nti),$(PetscReal* p))}|] where
    nti = fromIntegral $ vecNormToInt nt
vecNorm1 nt v = withPtr (vecNorm' nt v)
-- vecNorm v nt = unsafePerformIO $ withPtrHandleErr2 vecNorm' nt v


-- PETSC_EXTERN PetscErrorCode VecNormalize(Vec,PetscReal *);


-- PETSC_EXTERN PetscErrorCode VecSum(Vec,PetscScalar*);
vecSum' v p = [C.exp|int{VecSum($(Vec v), $(PetscScalar* p))}|]
vecSum1 v = withPtr (vecSum' v)
-- vecSum v = unsafePerformIO $ withPtrHandleErr1 vecSum' v

-- PETSC_EXTERN PetscErrorCode VecMax(Vec,PetscInt*,PetscReal *);
vecMax' v i r = [C.exp|int{VecMax($(Vec v),$(PetscInt* i),$(PetscReal* r))}|]
-- PETSC_EXTERN PetscErrorCode VecMin(Vec,PetscInt*,PetscReal *);
vecMin' v i r = [C.exp|int{VecMin($(Vec v),$(PetscInt* i),$(PetscReal* r))}|]
-- PETSC_EXTERN PetscErrorCode VecScale(Vec,PetscScalar);
vecScale' v n = [C.exp|int{VecScale($(Vec v),$(PetscScalar n))}|]
-- PETSC_EXTERN PetscErrorCode VecPointwiseMax(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecPointwiseMaxAbs(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecPointwiseMin(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecPointwiseMult(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecPointwiseDivide(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecMaxPointwiseDivide(Vec,Vec,PetscReal*);
-- PETSC_EXTERN PetscErrorCode VecShift(Vec,PetscScalar);
vecShift' v n = [C.exp|int{VecShift($(Vec v),$(PetscScalar n))}|]
-- PETSC_EXTERN PetscErrorCode VecReciprocal(Vec);
vecReciprocal' v = [C.exp|int{VecReciprocal($(Vec v))}|]
-- PETSC_EXTERN PetscErrorCode VecPermute(Vec, IS, PetscBool );
vecPetmute' v i b = [C.exp|int{VecPermute($(Vec v),$(IS i),$(PetscBool b))}|]
-- PETSC_EXTERN PetscErrorCode VecSqrtAbs(Vec);
-- PETSC_EXTERN PetscErrorCode VecLog(Vec);
vecLog' v = [C.exp|int{VecLog($(Vec v))}|]
-- PETSC_EXTERN PetscErrorCode VecExp(Vec);
vecExp' v = [C.exp|int{VecExp($(Vec v))}|]
-- PETSC_EXTERN PetscErrorCode VecAbs(Vec);
vecAbs' v = [C.exp|int{VecAbs($(Vec v))}|]



-- PETSC_EXTERN PetscErrorCode VecAXPY(Vec,PetscScalar,Vec);
--    VecAXPY - Computes y = alpha x + y.
--    Notes: x and y MUST be different vectors
-- PetscErrorCode  VecAXPY(Vec y,PetscScalar alpha,Vec x)
vecAxpy' y a x = [C.exp|int{VecAXPY($(Vec y),$(PetscScalar a),$(Vec x))}|]
-- PETSC_EXTERN PetscErrorCode VecAXPBY(Vec,PetscScalar,PetscScalar,Vec);
-- PETSC_EXTERN PetscErrorCode VecMAXPY(Vec,PetscInt,const PetscScalar[],Vec[]);
-- PETSC_EXTERN PetscErrorCode VecAYPX(Vec,PetscScalar,Vec);
-- PETSC_EXTERN PetscErrorCode VecWAXPY(Vec,PetscScalar,Vec,Vec);
{-  VecWAXPY - Computes w = alpha x + y.   --- Logically Collective on Vec
   Input Parameters:
+  alpha - the scalar
-  x, y  - the vectors
   Output Parameter:
.  w - the result
   Level: intermediate
   Notes: w cannot be either x or y, but x and y can be the same    -}
-- PetscErrorCode  VecWAXPY(Vec w,PetscScalar alpha,Vec x,Vec y)
vecWaxpy' w a x y =
  [C.exp|int{VecWAXPY($(Vec w),$(PetscScalar a),$(Vec x),$(Vec y))}|]



-- withPtrHandleErr1 f a = withPtr (f a) >>= handleErrTup
-- withPtrHandleErr2 f a b = withPtr (f a b) >>= handleErrTup




















-- * Mat

-- data PMat = PMat { unPMat :: Mat}

-- data MatSp = MatSp { -- most information is latent within the PETSc Mat object 
--   mat :: PMat,
--   i :: V.Vector Int,
--   j :: V.Vector Int,
--   v :: V.Vector Double,
--   nrow :: Int,
--   ncol :: Int,
--   nnz :: Int}



-- PETSC_EXTERN PetscErrorCode MatSetType(Mat,MatType);
matSetType1 m mt = withCString cs $ \c -> [C.exp|int{MatSetType($(Mat m), $(char *c))}|] 
  where cs = matTypeToStr mt




-- matCreate' c p = [C.exp| int{MatCreate($(int c), $(Mat *p))} |]
matCreate0' comm = withPtr $ \p -> [C.exp| int{MatCreate($(int c), $(Mat *p))} |] 
  where c = unComm comm
matCreate' = matCreate0' 

matDestroy0' m = [C.exp|int{MatDestroy($(Mat *m))}|]
matDestroy' m = with m matDestroy0' 

-- withMat :: Comm -> (Mat -> IO a) -> IO a
-- withMat c = bracket (matCreate c) matDestroy

-- withMatPipeline :: Comm -> CInt -> CInt -> MatType_ -> (Mat -> IO a) -> (Mat -> IO b) -> IO b
-- withMatPipeline comm m n ti pre post =
--   withMat comm $ \mat -> do
--      matSetSizes mat m n
--      matSetType mat ti 
--      matSetUp mat
--      pre mat
--      matAssembly mat
--      post mat

matSetSizes' :: Mat -> Int -> Int -> IO CInt
matSetSizes' mat m n = [C.exp|int{MatSetSizes($(Mat mat), PETSC_DECIDE, PETSC_DECIDE,
                                             $(int mc), $(int nc))}|]
  where (mc, nc) = (toCInt m, toCInt n)


-- f1 i m = [C.exp|int{ ($(int i) % $(int m))  }|] -- mod
-- f2 i m = [C.exp|int{ $(int i) / $(int m)}|]     -- div

-- PetscErrorCode  MatCreateSeqAIJ(MPI_Comm comm,PetscInt m,PetscInt n,PetscInt nz,const PetscInt nnz[],Mat *A)

matCreateSeqAIJ' comm m n nz nnz =
  withPtr (\mat ->
   withArray nnz $ \nnzp ->
            [C.exp|int{MatCreateSeqAIJ($(int c),
                                       $(PetscInt m),
                                       $(PetscInt n),
                                       $(PetscInt nz),
                                       $(PetscInt* nnzp),
                                       $(Mat *mat))}|]) 
  where c = unComm comm

-- PetscErrorCode  MatCreateSeqAIJ(MPI_Comm comm,PetscInt m,PetscInt n,PetscInt nz,const PetscInt nnz[],Mat *A)  -- Collective on MPI_Comm
-- Input Parameters :
-- comm	- MPI communicator, set to PETSC_COMM_SELF
-- m	- number of rows
-- n	- number of columns
-- nz	- number of nonzeros per row (same for all rows) (if nnz is given nz is ignored)
-- nnz	- array containing the number of nonzeros in the various rows (possibly different for each row) or NULL
-- Output Parameter :
-- A -the matrix 

matCreateSeqAIJ1 comm m' n' nnz' =
  withPtr (\mat ->
   withArray nnz $ \nnzp ->
            [C.exp|int{MatCreateSeqAIJ($(int c),
                                       $(PetscInt m),
                                       $(PetscInt n),
                                       0 ,
                                       $(PetscInt* nnzp),
                                       $(Mat *mat))}|]) 
  where c = unComm comm
        (m, n, nnz) = (toCInt m', toCInt n', map toCInt nnz')

matCreateSeqAIJconstNZperRow1 comm m' n' nz' =
  withPtr (\mat ->
            [C.exp|int{MatCreateSeqAIJ($(int c),
                                       $(PetscInt m),
                                       $(PetscInt n),
                                       $(PetscInt nz),
                                       NULL ,
                                       $(Mat *mat))}|]) 
  where c = unComm comm
        (m, n, nz) = (toCInt m', toCInt n', toCInt nz')




-- PETSC_EXTERN PetscErrorCode MatCreateAIJ(MPI_Comm,PetscInt,PetscInt,PetscInt,PetscInt,PetscInt,const PetscInt[],PetscInt,const PetscInt[],Mat*);
    

--     PetscErrorCode  MatCreateMPIAIJWithArrays(MPI_Comm comm,PetscInt m,PetscInt n,PetscInt M,PetscInt N,const PetscInt i[],const PetscInt j[],const PetscScalar a[],Mat *mat)    -- Collective on MPI_Comm
-- Input Parameters :
-- comm	- MPI communicator
-- m	- number of local rows (Cannot be PETSC_DECIDE)
-- n	- This value should be the same as the local size used in creating the x vector for the matrix-vector product y = Ax. (or PETSC_DECIDE to have calculated if N is given) For square matrices n is almost always m.
-- M	- number of global rows (or PETSC_DETERMINE to have calculated if m is given)
-- N	- number of global columns (or PETSC_DETERMINE to have calculated if n is given)
-- i	- row indices
-- j	- column indices
-- a	- matrix values
--     Output Parameter :
-- mat -the matrix 

matCreateMPIAIJWithArrays' ::
  Comm -> [PetscInt_] -> [PetscInt_] -> [PetscScalar_] -> IO (Mat, CInt)
matCreateMPIAIJWithArrays' comm i j a =
  withArray i $ \ip ->
   withArray j $ \jp ->
    withArray a $ \aap -> 
  withPtr ( \mat -> [C.exp|int{MatCreateMPIAIJWithArrays($(PetscInt c),
                                    $(PetscInt m),
                                    $(PetscInt n),
                                    PETSC_DETERMINE, PETSC_DETERMINE,
                                    $(PetscInt* ip), $(PetscInt* jp),
                                    $(PetscScalar* aap), 
                                    $(Mat* mat))}|] )
  where c = unComm comm 
        m = fromIntegral $ length i -- # local rows
        n = fromIntegral $ length j -- # local rows

-- withMatMPIAIJWithArrays comm i j a =
--   bracket (matCreateMPIAIJWithArrays comm i j a) matDestroy

-- withMatMPIAIJWithArraysPipeline comm i j a body =
--   withMatMPIAIJWithArrays comm i j a $ \mat -> do
--     matAssembly mat
--     body mat


matViewStdout v = [C.exp|int{MatView($(Mat v), PETSC_VIEWER_STDOUT_SELF)}|]


-- PETSC_EXTERN PetscErrorCode MatCreateMPIAIJWithSplitArrays(MPI_Comm,PetscInt,PetscInt,PetscInt,PetscInt,PetscInt[],PetscInt[],PetscScalar[],PetscInt[],PetscInt[],PetscScalar[],Mat*);




-- -- MatComposite

-- MatCreateComposite
-- Creates a matrix as the sum of zero or more matrices
-- Synopsis :
-- #include "petscmat.h" 
-- PetscErrorCode  MatCreateComposite(MPI_Comm comm,PetscInt nmat,const Mat *mats,Mat *mat)
-- Collective on MPI_Comm
-- Input Parameters :
-- comm	- MPI communicator
-- nmat	- number of matrices to put in
-- mats	- the matrices
-- Output Parameter
-- mat -the matrix 



-- --



-- PETSC_EXTERN PetscErrorCode MatGetSize(Mat,PetscInt*,PetscInt*);
matGetSize0' v sx sy =  [C.exp|int{MatGetSize($(Mat v), $(int *sx), $(int *sy))}|]

matGetSize' :: Mat -> IO ((CInt, CInt), CInt)
matGetSize' v = withPtr ( \px ->
  withPtr $ \py -> matGetSize0' v px py ) >>= fst2M

matGetSizeUnsafeCInt' :: Mat -> ((CInt, CInt), CInt)
matGetSizeUnsafeCInt' = unsafePerformIO . matGetSize'

-- matGetSizeUnsafe' :: Mat -> (Int, Int)
-- matGetSizeUnsafe' m = (fi a', fi b') where
--   (a', b') = matGetSizeUnsafeCInt m

-- withMatSize mat f = f (matGetSizeUnsafeCInt mat)

-- f' g h = g . fst &&& h. snd
-- f'' g = f' g g

matSetFromOptions p = [C.exp| int{MatSetFromOptions($(Mat p))} |] 

-- PetscErrorCode  MatSeqAIJSetPreallocation(Mat B,PetscInt nz,const PetscInt nnz[])
-- -- Collective on MPI_Comm, CSR format
-- -- Input Parameters

-- -- B	- The matrix
-- -- nz	- number of nonzeros per row (same for all rows)
-- -- nnz	- array containing the number of nonzeros in the various rows (possibly different for each row) or NULL
-- -- -- NB : If nnz is given then nz is ignored

matSeqAIJSetPreallocation mat nz nnz =
    withArray nnz ( \nnzp ->
                     [C.exp|int{MatSeqAIJSetPreallocation( $(Mat mat),
                                                           $(PetscInt nz),
                                                           $(PetscInt *nnzp))} |]
                  ) 



-- PetscErrorCode MatSetValue(Mat m,PetscInt row,PetscInt col,PetscScalar value,InsertMode mode)
matSetValueUnsafe' m row col val im =
  [C.exp|int{MatSetValue($(Mat m),$(int rowc),$(int colc),$(PetscScalar val),$(int imm))}|] where
    imm = fromIntegral $ insertModeToInt im
    rowc = toCInt row
    colc = toCInt col


-- PetscErrorCode  MatSetValues(Mat mat,PetscInt m,const PetscInt idxm[],PetscInt n,const PetscInt idxn[],const PetscScalar v[],InsertMode addv) -- Not Collective
-- Input Parameters :
-- mat	- the matrix
-- v	- a logically two-dimensional array of values
-- m, idxm	- the number of rows and their global indices
-- n, idxn	- the number of columns and their global indices
-- addv	- either ADD_VALUES or INSERT_VALUES, where ADD_VALUES adds values to any existing entries, and INSERT_VALUES replaces existing entries with new values
-- Notes

-- If you create the matrix yourself (that is not with a call to DMCreateMatrix()) then you MUST call MatXXXXSetPreallocation() or MatSetUp() before using this routine
-- By default the values, v, are row-oriented. See MatSetOption() for other options.

-- Calls to MatSetValues() with the INSERT_VALUES and ADD_VALUES options cannot be mixed without intervening calls to the assembly routines.

-- MatSetValues() uses 0-based row and column numbers in Fortran as well as in C.

-- Negative indices may be passed in idxm and idxn, these rows and columns are simply ignored. This allows easily inserting element stiffness matrices with homogeneous Dirchlet boundary conditions that you don't want represented in the matrix


matSetValues0' mat nbx idxx_ nby idxy_ b_ im =
  [C.exp|int { MatSetValues($(Mat mat),
                      $(int nbx),
                      $(int* idxx_),
                      $(int nby),
                      $(int* idxy_),
                      $(PetscScalar* b_), $(int imm))} |] where
    imm = fromIntegral $ insertModeToInt im

matSetValues' mat idxx idxy b im
  | compatDim =
     withArray idxx $ \idxx_ ->
     withArray idxy $ \idxy_ ->
     withArray b $ \b_ ->
     matSetValues0' mat nbx idxx_ nby idxy_ b_ im 
  | otherwise = error "matSetValues: incompatible dimensions"
  where
       nbx = toCInt $ length idxx
       nby = toCInt $ length idxy
       nb = toCInt $ length b
       compatDim = (nbx*nby) == nb

matSetValuesAdd' m x y b = matSetValues' m x y b AddValues
matSetValuesInsert' m x y b = matSetValues' m x y b InsertValues




-- PetscErrorCode  MatSetValuesStencil(Mat mat,PetscInt m,const MatStencil idxm[],PetscInt n,const MatStencil idxn[],const PetscScalar v[],InsertMode addv)

{-
Not Collective
Input Parameters

mat	- the matrix
m	- number of rows being entered
idxm	- grid coordinates (and component number when dof > 1) for matrix rows being entered
n	- number of columns being entered
idxn	- grid coordinates (and component number when dof > 1) for matrix columns being entered
v	- a logically two-dimensional array of values
addv	- either ADD_VALUES or INSERT_VALUES, where ADD_VALUES adds values to any existing entries, and INSERT_VALUES replaces existing entries with new values
Notes:
----
By default the values, v, are row-oriented. See MatSetOption() for other options.
Calls to MatSetValuesStencil() with the INSERT_VALUES and ADD_VALUES options cannot be mixed without intervening calls to the assembly routines.
The grid coordinates are across the entire grid, not just the local portion
MatSetValuesStencil() uses 0-based row and column numbers in Fortran as well as in C.
-}





matAssemblyBegin' a = [C.exp|int{MatAssemblyBegin($(Mat a), MAT_FINAL_ASSEMBLY )}|]

matAssemblyEnd' a = [C.exp|int{MatAssemblyEnd($(Mat a), MAT_FINAL_ASSEMBLY )}|]

-- matAssembly =
--   matAssemblyBegin >> matAssemblyEnd

-- withMatAssembly v f = do
--   matAssemblyBegin v
--   f v
--   matAssemblyEnd v

matSetup' a = [C.exp|int{MatSetUp($(Mat a))}|] 



-- TODO row (block) indexing : these should not be interpreted as mere Ints but as indices, e.g. FEM mesh nodes

-- PETSC_EXTERN PetscErrorCode MatGetOwnershipRange(Mat,PetscInt*,PetscInt*);
matGetOwnershipRange0' a =
 withPtr $ \rmin -> 
  withPtr $ \rmax ->
   [C.exp|int{MatGetOwnershipRange($(Mat a), $(PetscInt *rmin), $(PetscInt *rmax) )}|]

matGetOwnershipRange' m = do
  (r2, (r1, e)) <- matGetOwnershipRange0' m
  return ((fi r2, fi r1), e)


  

-- -- -- Mat experiments

-- for (i=0; i<4; i++) {
--  45:     v    = 3;
--  46:     MatSetValues(A,1,&i,1,&i,&v,INSERT_VALUES);
--  47:     v    = 1;
--  48:     VecSetValues(B,1,&i,&v,INSERT_VALUES);
--  49:     VecSetValues(X,1,&i,&v,INSERT_VALUES);
--  50:   }

-- matSetDiagonal mat val =
--   [C.block|int{
--       int i;
--       for (i=0; i< $(int n); i++){
--         MatSetValues( $(Mat mat), 1, &i, 1, &i, &$(PetscScalar val), $(int imm));
--                                  };
--                                    }|] 
--    where
--     n = fst . unsafePerformIO $ matGetSize mat
--     imm = fromIntegral $ insertModeToInt InsertValues


-- matSetIdentity mat = matSetDiagonal mat 1.0





                

-- matAddBlocks mat bloc istart iend idxx idxy
--   | iend > istart = 
--   withArray idxx $ \idxx_ ->
--    withArray idxy $ \idxy_ ->
--     withArray b $ \b_ -> 
--   [C.block|int{
--       int ierr, i;
--       for (i=$(int istart_); i<=$(int iend_); i++) 
--         {
--           ierr MatSetValues(  $(Mat mat), $(int nbx), $(int* idxx_), $(int nby), 
--                               $(int* idxy_), $(PetscScalar* b_), ADD_VALUES);
--          };
--       return ierr;
--               }|] >>= handleErr
--   | otherwise = error "incompatible indices: iend istart"
--   where 
--     nbx = fromIntegral $ length $ head bloc
--     nby = fromIntegral $ length bloc
--     b = concat bloc
--     istart_ = fromIntegral istart
--     iend_ = fromIntegral iend
    



-- -- * Mat FD Coloring

-- PetscErrorCode  MatFDColoringCreate(Mat mat,ISColoring iscoloring,MatFDColoring *color)

matFDColoringCreate0' m i c =
  [C.exp| int{MatFDColoringCreate($(Mat m),$(ISColoring i),$(MatFDColoring* c)) } |]

matFDColoringCreate' m i = withPtr $ \c -> matFDColoringCreate0' m i c



-- PetscErrorCode MatFDColoringSetUp(Mat mat,ISColoring iscoloring,MatFDColoring color)
matFDColoringSetUp' mat iscoloring color =
  [C.exp|int{MatFDColoringSetUp($(Mat mat),$(ISColoring iscoloring),$(MatFDColoring color))}|]

-- PetscErrorCode  MatFDColoringDestroy(MatFDColoring *c)
matFDColoringDestroy' color = with color $ \cp -> [C.exp|int{MatFDColoringDestroy($(MatFDColoring* cp))}|]











-- * DM




-- PETSC_EXTERN PetscErrorCode DMCreate(MPI_Comm,DM*);
dmCreate' comm = withPtr ( \dm -> [C.exp|int{DMCreate($(int c), $(DM* dm))} |] ) 
  where c = unComm comm

dmDestroy' dm = with dm ( \dmp -> [C.exp|int{DMDestroy($(DM* dmp))}|] ) 

-- withDm comm = bracket (dmCreate comm) dmDestroy


-- -- DMCreate* are for setting up longer-lived data
-- -- DMGet* and DMRestore* are for temporary access (always go in pairs)

-- PETSC_EXTERN PetscErrorCode DMCreateGlobalVector(DM,Vec*);
dmCreateGlobalVector' dm = withPtr ( \v -> [C.exp|int{DMCreateGlobalVector($(DM dm), $(Vec* v))}|]) 

-- PETSC_EXTERN PetscErrorCode DMCreateLocalVector(DM,Vec*);
dmCreateLocalVector' dm = withPtr ( \v -> [C.exp|int{DMCreateLocalVector($(DM dm), $(Vec* v))}|]) 

-- PETSC_EXTERN PetscErrorCode DMGetLocalVector(DM,Vec *);
dmGetLocalVector dm = withPtr ( \v -> [C.exp|int{DMGetLocalVector($(DM dm),$(Vec* v))}|]) 

-- PETSC_EXTERN PetscErrorCode DMRestoreLocalVector(DM,Vec *);
dmRestoreLocalVector dm vv = with vv ( \v -> [C.exp|int{DMRestoreLocalVector($(DM dm),$(Vec* v))}|]) 

-- withDmLocalVector dm = bracket (dmGetLocalVector dm) (dmRestoreLocalVector dm)


-- PETSC_EXTERN PetscErrorCode DMGetGlobalVector(DM,Vec *);
dmGetGlobalVector dm = withPtr ( \v -> [C.exp|int{DMGetGlobalVector($(DM dm),$(Vec* v))}|])

-- PETSC_EXTERN PetscErrorCode DMRestoreGlobalVector(DM,Vec *);
dmRestoreGlobalVector dm vv = with vv ( \v -> [C.exp|int{DMRestoreGlobalVector($(DM dm),$(Vec* v))}|]) 


-- withDmGlobalVector dm = bracket (dmGetGlobalVector dm) (dmRestoreGlobalVector dm)


-- withDmCreateGlobalVector dm = bracket (dmCreateGlobalVector dm) vecDestroy

-- withDmCreateLocalVector dm = bracket (dmCreateLocalVector dm) vecDestroy



dmCreateMatrix' dm mat = [C.exp|int{DMCreateMatrix($(DM dm),$(Mat* mat))}|]
dmCreateMatrix dm = withPtr (dmCreateMatrix' dm) 

-- withDmCreateMatrix dm = bracket (dmCreateMatrix dm) matDestroy


-- PetscErrorCode DMGetCoordinates(DM dm, Vec *c)
-- Not Collective
-- Input Parameters :
-- dm : the DM 
-- Output Parameter
-- c : global coordinate vector 
-- Note :
-- This is a borrowed reference, so the user should NOT destroy this vector
-- Each process has only the local coordinates (does NOT have the ghost coordinates).
-- For DMDA, in two and three dimensions coordinates are interlaced (x_0,y_0,x_1,y_1,...) and (x_0,y_0,z_0,x_1,y_1,z_1...)
dmGetCoordinates dm =
 withPtr (\c-> [C.exp| int{DMGetCoordinates($(DM dm),$(Vec*c))} |] ) 







-- PetscErrorCode  DMCreateColoring(DM dm,ISColoringType ctype,ISColoring *coloring)
dmCreateColoring' d c = withPtr $ \col -> [C.exp|int{DMCreateColoring($(DM d),$(int ctype),$(ISColoring* col))}|] where
  ctype = toCInt $ isColoringTypeToInt c











-- * DMDA


-- data DMDAInterpolationType = DMDA_Q0 | DMDA_Q1 deriving (Eq, Show, Enum)
-- dmdaInterpolationTypeToInt x = fromEnum (x :: DMDAInterpolationType)

-- data DMDAElementType = DMDAElem_Q0 | DMDAElem_Q1 deriving (Eq, Show, Enum)
-- dmdaElementTypeToInt x = fromEnum (x :: DMDAElementType )

-- data DMDADirection = DMDA_X | DMDA_Y | DMDA_Z deriving (Eq, Show, Enum)
-- dmdaDirectionToInt x = fromEnum (x :: DMDADirection)


dmdaCreate' comm = withPtr ( \p -> [C.exp|int{DMDACreate($(int c), $(DM* p))}|] ) where
  c = unComm comm



-- PETSC_EXTERN PetscErrorCode DMDASetDim(DM,PetscInt);
dmdaSetDim' dm d = [C.exp|int{DMDASetDim($(DM dm), $(PetscInt d))}|] 
-- PETSC_EXTERN PetscErrorCode DMDASetSizes(DM,PetscInt,PetscInt,PetscInt);
dmdaSetSizes' dm x y z = [C.exp|int{DMDASetSizes($(DM dm), $(PetscInt x), $(PetscInt y), $(PetscInt z))}|] 

-- PetscErrorCode  DMDACreate1d(MPI_Comm comm, DMBoundaryType bx, PetscInt M, PetscInt dof, PetscInt s, const PetscInt lx[], DM *da)   -- Collective on MPI_Comm
-- Input Parameters

-- comm	- MPI communicator
-- bx	- type of ghost cells at the boundary the array should have, if any. Use DM_BOUNDARY_NONE, DM_BOUNDARY_GHOSTED, or DM_BOUNDARY_PERIODIC.
-- M	- global dimension of the array (use -M to indicate that it may be set to a different value from the command line with -da_grid_x <M>)
-- dof	- number of degrees of freedom per node
-- s	- stencil width
-- lx	- array containing number of nodes in the X direction on each processor, or NULL. If non-null, must be of length as the number of processes in the MPI_Comm.
dmdaCreate1d' comm bx m dof s lx_ =
  withArray lx_ ( \ lx ->
   withPtr ( \ dm -> [C.exp|int{DMDACreate1d($(int c),
                                              $(int bxe),
                                              $(PetscInt m),
                                              $(PetscInt dof),
                                              $(PetscInt s),
                                              $(int* lx),
                                              $(DM* dm))}|]  )) 
  where c = unComm comm
        bxe = toEnum $ dmBoundaryTypeToInt bx

-- withDmda1d comm bx m dof s lx =
--   bracket (dmdaCreate1d comm bx m dof s lx) dmDestroy



-- PetscErrorCode  DMDACreate2d(MPI_Comm comm,DMBoundaryType bx,DMBoundaryType by,DMDAStencilType stencil_type, PetscInt M,PetscInt N,PetscInt m,PetscInt n,PetscInt dof,PetscInt s,const PetscInt lx[],const PetscInt ly[],DM *da)
--    DMDACreate2d -  Creates an object that will manage the communication of  two-dimensional
--    regular array data that is distributed across some processors.
--    Collective on MPI_Comm
--    Input Parameters:
-- +  comm - MPI communicator
-- .  bx,by - type of ghost nodes the array have.
--          Use one of DM_BOUNDARY_NONE, DM_BOUNDARY_GHOSTED, DM_BOUNDARY_PERIODIC.
-- .  stencil_type - stencil type.  Use either DMDA_STENCIL_BOX or DMDA_STENCIL_STAR.
-- .  M,N - global dimension in each direction of the array (use -M and or -N to indicate that it may be set to a different value
--             from the command line with -da_grid_x <M> -da_grid_y <N>)
-- .  m,n - corresponding number of processors in each dimension
--          (or PETSC_DECIDE to have calculated)
-- .  dof - number of degrees of freedom per node
-- .  s - stencil width
-- -  lx, ly - arrays containing the number of nodes in each cell along
--            the x and y coordinates, or NULL. If non-null, these
--            must be of length as m and n, and the corresponding
--            m and n cannot be PETSC_DECIDE. The sum of the lx[] entries
--            must be M, and the sum of the ly[] entries must be N.

--    Output Parameter:
-- .  da - the resulting distributed array object


dmdaCreate2d0' comm bx by sten mm nn m n dof s lx_ ly_ =
  withArray lx_ $ \lx ->
   withArray ly_ $ \ly -> 
    withPtr ( \dm -> [C.exp|int{DMDACreate2d($(int c),
                          $(int bxe),
                          $(int bye),
                          $(int stene),
                          $(PetscInt mm),
                          $(PetscInt nn),
                          $(PetscInt m), 
                          $(PetscInt n),
                          $(PetscInt dof),
                          $(PetscInt s),
                          $(int* lx),
                          $(int* ly),
                          $(DM* dm))}|] ) 
  where c = unComm comm
        bxe = toEnum $ dmBoundaryTypeToInt bx
        bye = toEnum $ dmBoundaryTypeToInt by
        stene = toEnum $ dmdaStencilTypeToInt sten

-- | Hp : lx == ly == NULL
-- (customary in PETSc examples )
dmdaCreate2d' c bx by sten mm nn dof s =
  dmdaCreate2d0' c bx by sten mm nn petscDecide petscDecide dof s [] []

-- withDmda2d comm bx by sten mm nn dof s =
--   bracket (dmdaCreate2d' comm bx by sten mm nn dof s) dmDestroy



-- PETSC_EXTERN PetscErrorCode DMDACreateNaturalVector(DM,Vec *);
dmdaCreateNaturalVector dm = withPtr (\v ->[C.exp|int{DMDACreateNaturalVector($(DM dm), $(Vec * v))} |] )


-- PETSC_EXTERN PetscErrorCode DMDAGetCorners(DM,PetscInt*,PetscInt*,PetscInt*,PetscInt*,PetscInt*,PetscInt*);
-- Returns the global (x,y,z) indices of the lower left corner of the local region, excluding ghost points.
-- x,y,z	- the corner indices (where y and z are optional; these are used for 2D and 3D problems)
-- m,n,p	- widths in the corresponding directions (where n and p are optional; these are used for 2D and 3D problems)

-- NB 4-6 outputs

dmdaGetCorners1d' dm =
  withPtr $ \x ->
  withPtr $ \m -> [C.exp|int{DMDAGetCorners($(DM dm),
                               $(PetscInt* x),
                               NULL,
                               NULL,
                               $(PetscInt* m),
                               NULL,
                               NULL)} |]

dmdaGetCorners2d' dm =
  withPtr $ \x ->
  withPtr $ \y ->
  withPtr $ \m ->
  withPtr $ \n -> [C.exp|int{DMDAGetCorners($(DM dm),
                               $(PetscInt* x),
                               $(PetscInt* y),
                               NULL,
                               $(PetscInt* m),
                               $(PetscInt* n),
                               NULL)} |] 

dmdaGetCorners3d' dm =
     withPtr $ \x ->
     withPtr $ \y ->
     withPtr $ \z ->
     withPtr $ \m ->
     withPtr $ \n ->
     withPtr $ \p -> [C.exp|int{DMDAGetCorners($(DM dm),
                               $(PetscInt* x),
                               $(PetscInt* y),
                               $(PetscInt* z),
                               $(PetscInt* m),
                               $(PetscInt* n),
                               $(PetscInt* p))} |] 

f1d (a, (b, c)) = ((a, b), c)
f2d (a,(b,(c,(d,e)))) = (((a,b), (c, d)), e)
f3d (a, (b, (c, (d, (e, (f, g)))))) = (((a, b, c), (d, e, f)), g)

-- dmdaGetCorners1d dm = dmdaGetCorners1d' dm >>= \l -> f1d l
-- dmdaGetCorners2d dm = dmdaGetCorners2d' dm >>= \l -> f2d l
-- dmdaGetCorners3d dm = dmdaGetCorners3d' dm >>= \l -> f3d l


-- PETSC_EXTERN PetscErrorCode DMDAGetGhostCorners(DM,PetscInt*,PetscInt*,PetscInt*,PetscInt*,PetscInt*,PetscInt*);

dmdaGetGhostCorners1d' dm =
  withPtr $ \x ->
  withPtr $ \m -> [C.exp|int{DMDAGetCorners($(DM dm),
                               $(PetscInt* x),
                               NULL,
                               NULL,
                               $(PetscInt* m),
                               NULL,
                               NULL)} |]

dmdaGetGhostCorners2d' dm =
  withPtr $ \x ->
  withPtr $ \y ->
  withPtr $ \m ->
  withPtr $ \n -> [C.exp|int{DMDAGetGhostCorners($(DM dm),
                               $(PetscInt* x),
                               $(PetscInt* y),
                               NULL,
                               $(PetscInt* m),
                               $(PetscInt* n),
                               NULL)} |] 

dmdaGetGhostCorners3d' dm =
     withPtr $ \x ->
     withPtr $ \y ->
     withPtr $ \z ->
     withPtr $ \m ->
     withPtr $ \n ->
     withPtr $ \p -> [C.exp|int{DMDAGetGhostCorners($(DM dm),
                               $(PetscInt* x),
                               $(PetscInt* y),
                               $(PetscInt* z),
                               $(PetscInt* m),
                               $(PetscInt* n),
                               $(PetscInt* p))} |] 

-- dmdaGetGhostCorners1d'' dm = dmdaGetGhostCorners1d' dm >>= \l ->  f1d l
-- dmdaGetGhostCorners2d'' dm = dmdaGetGhostCorners2d' dm >>= \l ->  f2d l
-- dmdaGetGhostCorners3d'' dm = dmdaGetGhostCorners3d' dm >>= \l ->  f3d l


-- dmdaGetGhostCorners1d dm =  liftM fromIntegralTup $ dmdaGetGhostCorners1d'' dm
-- dmdaGetGhostCorners2d dm =  liftM fromIntegralTup2 $ dmdaGetGhostCorners2d'' dm
-- dmdaGetGhostCorners3d dm =  liftM fromIntegralTup3 $ dmdaGetGhostCorners3d'' dm 



-- PETSC_EXTERN PetscErrorCode DMDAGetInfo(DM,PetscInt*,PetscInt*,PetscInt*,PetscInt*,PetscInt*,PetscInt*,PetscInt*,PetscInt*,PetscInt*,DMBoundaryType*,DMBoundaryType*,DMBoundaryType*,DMDAStencilType*);
-- PETSC_EXTERN PetscErrorCode DMDAGetProcessorSubset(DM,DMDADirection,PetscInt,MPI_Comm*);
-- PETSC_EXTERN PetscErrorCode DMDAGetProcessorSubsets(DM,DMDADirection,MPI_Comm*);
-- PETSC_EXTERN PetscErrorCode DMDAGetRay(DM,DMDADirection,PetscInt,Vec*,VecScatter*)



-- PetscErrorCode  DMDASetUniformCoordinates(DM da,PetscReal xmin,PetscReal xmax,PetscReal ymin,PetscReal ymax,PetscReal zmin,PetscReal zmax)
-- Collective on DMDA
-- Input Parameters

-- da	- the distributed array object
-- xmin,xmax	- extremes in the x direction
-- ymin,ymax	- extremes in the y direction (value ignored for 1 dimensional problems)
-- zmin,zmax	- extremes in the z direction (value ignored for 1 or 2 dimensional problems)
dmdaSetUniformCoordinates' da xmin xmax ymin ymax zmin zmax =
  [C.exp|int{DMDASetUniformCoordinates($(DM da),$(PetscReal xmin),$(PetscReal xmax),$(PetscReal ymin),$(PetscReal ymax),$(PetscReal zmin),$(PetscReal zmax))}|] 



-- PetscErrorCode  DMDAVecGetArray(DM da,Vec vec,void *array)
-- dmdaVecGetArray' :: DM -> Vec -> Ptr PetscScalar_ -> IO CInt
dmdaVecGetArray' dm v vvp =   
  [C.exp|int{DMDAVecGetArray($(DM dm),
                             $(Vec v),
                             $(PetscScalar* vvp))}|]

-- dmdaVecGetArray'' dm v =   
--   withPtr ( \vvp -> [C.exp|int{DMDAVecGetArray($(DM dm),
--                              $(Vec v),
--                              $(PetscScalar** vvp))}|] ) >>= handleErrTup >>= peekArray n where n = fromIntegral $ vecGetSizeUnsafe v

-- dmdaVecGetArray = dmdaVecGetArray''

-- PetscErrorCode  DMDARestoreArray(DM da,PetscBool ghosted,void *vptr)
dmdaRestoreArray dm ghosted vptr = withArray vptr ( \vp -> 
  [C.exp|int{DMDARestoreArray($(DM dm),
                              $(PetscBool ghosted),
                              $(PetscScalar* vp))}|] ) 

-- PetscErrorCode  DMDAVecRestoreArray(DM da,Vec vec,void *array)
dmdaVecRestoreArray dm v arr = withArray arr $ \arr_ -> 
  [C.exp|int{DMDAVecRestoreArray($(DM dm), $(Vec v), $(PetscScalar* arr_))}|]

-- withDmdaArray dm v = bracket (dmdaVecGetArray dm v) (dmdaVecRestoreArray dm v)



-- Creates a vector packer, used to generate "composite" vectors made up of several subvectors.
-- Synopsis :
-- #include "petscdmcomposite.h"  
-- PetscErrorCode  DMCompositeCreate(MPI_Comm comm,DM *packer)
-- Collective on MPI_Comm
-- Input Parameter :
-- comm -the processors that will share the global vector 
-- Output Parameter :
-- packer -the packer object 
dmCompositeCreate comm =
  withPtr ( \p -> [C.exp|int{DMCompositeCreate($(int c), $(DM* p))}|] )
  where c = unComm comm






















-- * KSP






kspGetConvergedReason' ksp =
  withPtr ( \r ->
             [C.exp| int{ KSPGetConvergedReason( $(KSP ksp),
                                              $(int* r) ) } |]
          ) 
-- kspGetConvergedReason ksp = 
--   kspGetConvergedReason' ksp >>= \r -> return $ kspConvergedIntToReason (fromIntegral r)


kspCreate0' comm p = [C.exp| int{KSPCreate($(int c), $(KSP *p))}|] where
  c = unComm comm
kspCreate' c = withPtr (kspCreate0' c)

kspSetType' :: KSP -> KspType_ -> IO CInt
kspSetType' ksp kt = withCString strk $ \strp -> [C.exp|int{KSPSetType($(KSP ksp), $(char* strp))}|] where
  strk = kspTypeToStr kt

-- kspSetType :: KSP -> KspType_ -> IO ()
-- kspSetType = kspSetType'  

-- PETSC_EXTERN PetscErrorCode KSPGetType(KSP,KSPType *);
-- kspGetType ksp = alloca ( \strp -> do
--                            [C.exp|int{KSPGetType($(KSP ksp), $(char *strp))}|]
--                            peekString strp) 

kspDestroy0' p = [C.exp| int{KSPDestroy($(KSP *p))}  |]
kspDestroy' p = with p kspDestroy0' 

-- withKsp c = bracket (kspCreate c) kspDestroy

-- withKspSetupSolve c mat1 mat2 ignz kt x v post = withKsp c $ \ksp -> do
--   kspSetOperators ksp mat1 mat2
--   kspSetType ksp kt 
--   kspSetInitialGuessNonzero ksp ignz
--   kspSetUp ksp
--   kspSolve ksp x v
--   -- soln <- kspGetSolution ksp 
--   post ksp
-- -- nb this is just a reference; it becomes invalid after exiting the withKsp bracket -> we need a `withVecDuplicateCopy` outside withKspSolve to allocate space for the solution 


-- withKsp cs $ \ksp -> do
     --   kspSetOperators ksp mat mat
     --   kspSetType ksp km 
     --   kspSetInitialGuessNonzero ksp True
     --   kspSetUp ksp
     --   kspSolve ksp x v
     --   soln <- kspGetSolution ksp
     --   -- vecViewStdout soln
     --   showKspStats ksp km
     --   -- kspReasonView ksp
     --   -- return soln


kspSetOperators' ksp amat pmat =
  [C.exp|int{KSPSetOperators($(KSP ksp), $(Mat amat), $(Mat pmat))}|] 

kspSetUp' ksp = [C.exp|int{KSPSetUp($(KSP ksp))}|]

kspSolve' ksp b x = [C.exp|int{KSPSolve($(KSP ksp), $(Vec b), $(Vec x))}|] 

kspSolveTranspose' ksp b x =
  [C.exp|int{KSPSolveTranspose($(KSP ksp), $(Vec b), $(Vec x))}|] 





-- PETSC_EXTERN PetscErrorCode KSPReset(KSP);

-- PETSC_EXTERN PetscErrorCode KSPSetReusePreconditioner(KSP,PetscBool);
kspSetReusePreconditioner' ksp b = [C.exp|int{KSPSetReusePreconditioner($(KSP ksp), $(PetscBool b))}|] 

-- PETSC_EXTERN PetscErrorCode KSPRegisterAll(void);
-- PETSC_EXTERN PetscErrorCode KSPRegister(const char[],PetscErrorCode (*)(KSP));
-- PETSC_EXTERN PetscErrorCode KSPMatRegisterAll(void);

-- PETSC_EXTERN PetscErrorCode KSPSetPCSide(KSP,PCSide);
-- PETSC_EXTERN PetscErrorCode KSPGetPCSide(KSP,PCSide*);
-- PETSC_EXTERN PetscErrorCode KSPGetTolerances(KSP,PetscReal*,PetscReal*,PetscReal*,PetscInt*);

-- PETSC_EXTERN PetscErrorCode KSPSetTolerances(KSP,PetscReal,PetscReal,PetscReal,PetscInt);
-- kspSetTolerances ksp 

-- PETSC_EXTERN PetscErrorCode KSPSetInitialGuessNonzero(KSP,PetscBool );
kspSetInitialGuessNonzero' ksp b = [C.exp|int{KSPSetInitialGuessNonzero($(KSP ksp), $(PetscBool b))}|]

-- PETSC_EXTERN PetscErrorCode KSPGetInitialGuessNonzero(KSP,PetscBool  *);
-- PETSC_EXTERN PetscErrorCode KSPSetInitialGuessKnoll(KSP,PetscBool );
-- PETSC_EXTERN PetscErrorCode KSPGetInitialGuessKnoll(KSP,PetscBool *);

-- PETSC_EXTERN PetscErrorCode KSPSetErrorIfNotConverged(KSP,PetscBool );
kspSetErrorIfNotConverged ksp b = [C.exp|int{KSPSetErrorIfNotConverged($(KSP ksp), $(PetscBool b))}|] 

-- PETSC_EXTERN PetscErrorCode KSPGetErrorIfNotConverged(KSP,PetscBool  *);
-- PETSC_EXTERN PetscErrorCode KSPGetComputeEigenvalues(KSP,PetscBool *);

-- PETSC_EXTERN PetscErrorCode KSPSetComputeEigenvalues(KSP,PetscBool );
kspSetComputeEigenValues ksp b = [C.exp|int{KSPSetComputeEigenvalues($(KSP ksp), $(PetscBool b))}|]

-- PETSC_EXTERN PetscErrorCode KSPGetComputeSingularValues(KSP,PetscBool *);

-- PETSC_EXTERN PetscErrorCode KSPSetComputeSingularValues(KSP,PetscBool );
kspSetComputeSingularValues ksp b = [C.exp|int{KSPSetComputeSingularValues($(KSP ksp), $(PetscBool b))}|] 

-- PETSC_EXTERN PetscErrorCode KSPGetRhs(KSP,Vec *);
kspGetRhs' ksp = withPtr $ \v -> [C.exp|int{KSPGetRhs($(KSP ksp), $(Vec *v))}|]
-- kspGetRhs ksp = kspGetRhs' ksp 

-- PETSC_EXTERN PetscErrorCode KSPGetSolution(KSP,Vec *);
kspGetSolution' ksp = withPtr $ \v -> [C.exp|int{KSPGetSolution($(KSP ksp), $(Vec *v))}|]
-- kspGetSolution :: KSP -> IO Vec
-- kspGetSolution ksp = kspGetSolution' ksp 

-- PETSC_EXTERN PetscErrorCode KSPGetResidualNorm(KSP,PetscReal*);
kspGetResidualNorm' ksp = withPtr $ \v -> [C.exp|int{KSPGetResidualNorm($(KSP ksp), $(PetscReal *v))}|]

-- kspGetResidualNorm :: KSP -> IO PetscReal_
-- kspGetResidualNorm ksp = kspGetResidualNorm' ksp 

-- PETSC_EXTERN PetscErrorCode KSPGetIterationNumber(KSP,PetscInt*);
kspGetIterationNumber' ksp = withPtr ( \v -> [C.exp|int{KSPGetIterationNumber($(KSP ksp), $(int *v))}|] ) 

-- PETSC_EXTERN PetscErrorCode KSPSetNullSpace(KSP,MatNullSpace);
-- PETSC_EXTERN PetscErrorCode KSPGetNullSpace(KSP,MatNullSpace*);
-- PETSC_EXTERN PetscErrorCode KSPGetVecs(KSP,PetscInt,Vec**,PetscInt,Vec**);


-- PetscErrorCode  KSPReasonView(KSP ksp,PetscViewer viewer)
-- kspReasonView ksp = [C.exp|int{KSPReasonView($(KSP ksp), PETSC_VIEWER_STDOUT_SELF)}|] >>= handleErr





-- KSP experiments

-- class LinSolve a u b where
--   linsolve :: a -> b -> u

-- instance LinSolve 
















-- * PF






-- PetscErrorCode  PFCreate(MPI_Comm comm,PetscInt dimin,PetscInt dimout,PF *pf)
-- Collective on MPI_Comm
-- Input Parameters :
-- comm	- MPI communicator
-- dimin	- dimension of the space you are mapping from
-- dimout	- dimension of the space you are mapping to
pfCreate' comm dimin dimout = withPtr ( \pf ->[C.exp|int{PFCreate($(int c),$(int diminc),$(int dimoutc),$(PF*pf) )}|] ) 
  where
    c = unComm comm
    diminc = toCInt dimin
    dimoutc = toCInt dimout

-- PetscErrorCode  PFDestroy(PF *pf)
pfDestroy' pf = with pf $ \pfp -> [C.exp|int{PFDestroy($(PF* pfp))}|]

-- PETSC_EXTERN PetscErrorCode DMDACreatePF(DM,PF*);
dmdaCreatePF' dm = withPtr (\pf -> [C.exp|int{DMDACreatePF($(DM dm),$(PF*pf))}|]) 

-- PETSC_EXTERN PetscErrorCode PFSetType(PF,PFType,void*);
pfSetType' pf t o = -- not sure how to represent the pointer to void 
  withCString tstr (\tp->   [C.exp|int{PFSetType($(PF pf),$(char*tp),$(void*o))}|]
                   )  where
  tstr = pfTypeToStr t


-- PETSC_EXTERN PetscErrorCode PFSet(
-- PF,
-- PetscErrorCode(*)(void*,PetscInt,const PetscScalar*,PetscScalar*),
-- PetscErrorCode(*)(void*,Vec,Vec),PetscErrorCode(*)(void*,PetscViewer),
-- PetscErrorCode(*)(void*),void*);     -- Collective on PF
-- Input Parameters :
-- pf	- the function context
-- apply	- function to apply to an array
-- applyvec	- function to apply to a Vec
-- view	- function that prints information about the PF
-- destroy	- function to free the private function context
-- ctx	- private function context
pfSet0' pf apply applyvec view destroy ctx =
  [C.exp|int{PFSet($(PF pf),
                   $fun:(int(*apply)(void*,PetscInt,PetscScalar*,PetscScalar*)),
                   $fun:(int(*applyvec)(void*, Vec, Vec)),
                   $fun:(int(*view)(void*, int )),
                   $fun:(int(*destroy)(void*)),
                   $(void*ctx)
                  )}
        |]

pfSet0nc' pf apply applyvec view destroy ctx =
  [C.exp|int{PFSet($(PF pf),
                   $fun:(int(*apply)(void*,PetscInt,PetscScalar*,PetscScalar*)),
                   $fun:(int(*applyvec)(void*, Vec, Vec)),
                   $fun:(int(*view)(void*, int )),
                   $fun:(int(*destroy)(void*)),
                   NULL
                  )}
        |]

pfSet' pf apply applyvec viewf destroyf =
  pfSet0nc' pf f1 f2 f3 f4 where
    f1 _ = apply
    f2 _ = applyvec
    f3 _ = viewf
    f4 _ = destroyf
    -- f1' :: Storable a => CInt -> [a] -> [a] -> IO CInt
    -- f1' a arr1 arr2 = withArray arr1 $  \arrp1 ->
    --   withArray arr2 $ \arrp2 ->
    --     f1 a arrp1 arrp2


-- f :: (a -> a -> IO Int) -> (a -> a -> IO a)  -- ?

-- pfSetVec' :: PF -> (Ptr () -> Vec -> Vec -> IO CInt) -> IO ()
pfSetVec0' pf applyvec =
    [C.exp|int{PFSet($(PF pf),
                   0,
                   $fun:(int(*applyvec)( void* , Vec, Vec)),
                   0, 0, 0)}|] 

pfSetVec' pf applyvec =
  pfSetVec0' pf f where
    f _ = applyvec 

-- WARNING : `applyvec` etc. modify last argument


-- -- withPfSetVec pf f =  pfSetVec' pf ( \_ v1 v2 ->  f v1 v2)
                     

-- pfSetVec'' :: PF -> (Vec -> IO Vec) -> IO a -- how do we get it?
-- pfSetVec'' pf f = 
--   pfSetVec' pf $ \v  ->
--     with v $ \p -> f p

-- pf0 :: (Vec -> IO Vec) -> (Ptr () -> Vec -> Vec -> IO CInt)




-- PETSC_EXTERN PetscErrorCode PFApply(PF,PetscInt,const PetscScalar*,PetscScalar*);

-- PETSC_EXTERN PetscErrorCode PFApplyVec(PF,Vec,Vec);













-- * SNES





snesCreate' comm = withPtr $ \p -> [C.exp| int{SNESCreate($(int c), $(SNES *p))}|] where
  c = unComm comm 

snesSetType' :: SNES -> SnesType_ -> IO CInt
snesSetType' s t = withCString strk $ \strp -> [C.exp|int{SNESSetType($(SNES s), $(char* strp))}|] where
  strk = snesTypeToStr t


-- PetscErrorCode  SNESSetFunction(SNES snes,Vec r,PetscErrorCode (*f)(SNES,Vec,Vec,void*),void *ctx)
{-
Input Parameters :
snes	- the SNES context
r	- vector to store function value
f	- function evaluation routine; see SNESFunction for calling sequence details
ctx	- [optional] user-defined context for private data for the function evaluation routine (may be NULL)
Notes

The Newton-like methods typically solve linear systems of the form
     f'(x) x = -f(x),
where f'(x) denotes the Jacobian matrix and f(x) is the function.
-}

{-
PetscErrorCode SNESFunction(SNES snes,Vec x,Vec f,void *ctx);
Input Parameters :

snes	- the SNES context
x	- state at which to evaluate residual
ctx	- optional user-defined function context, passed in with SNESSetFunction()
Output Parameter

f -vector to put residual (function value) 
-}

snesSetFunction0' snes r f ctx =
  [C.exp|int{SNESSetFunction($(SNES snes), $(Vec r),
                             $fun:(int (*f)(SNES, Vec, void*) ),
                             $(void* ctx))}|]
snesSetFunction_' snes r f =
  [C.exp|int{SNESSetFunction($(SNES snes), $(Vec r),
                             $fun:(int (*f)(SNES, Vec, void*) ),
                             NULL )}|]
snesSetFunction' snes v f =
  snesSetFunction_' snes v f' where
    f' s a _ = f s a 


-- PETSC_EXTERN PetscErrorCode SNESDestroy(SNES*);
snesDestroy' p = with p $ \pp -> [C.exp| int{SNESDestroy($(SNES *pp))}  |]


-- PETSC_EXTERN PetscErrorCode SNESSetUp(SNES);
snesSetUp' s = [C.exp|int{SNESSetUp($(SNES s))}|] 

-- PETSC_EXTERN PetscErrorCode SNESSolve(SNES,Vec,Vec);
snesSolve' s b x = [C.exp|int{SNESSolve($(SNES s), $(Vec b), $(Vec x))}|]


-- PETSC_EXTERN PetscErrorCode SNESGetSolution(SNES,Vec*);
snesGetSolution' s = withPtr ( \v ->
  [C.exp|int{SNESGetSolution($(SNES s), $(Vec *v))}|] ) 



-- withSnes comm = bracket (snesCreate comm) snesDestroy

-- withSnesSetupSolve comm st b x pre post = 
--   withSnes comm $ \s -> do
--    snesSetType s st
--    snesSetUp s
--    -- missing : function, Jacobian
--    pre s
--    snesSolve s b x
--    post s





-- PetscErrorCode  SNESSetJacobian(SNES snes,Mat Amat,Mat Pmat,PetscErrorCode (*J)(SNES,Vec,Mat,Mat,void*),void *ctx)          -- Logically Collective on SNES and Mat
-- Input Parameters :
-- snes	- the SNES context
-- Amat	- the matrix that defines the (approximate) Jacobian
-- Pmat	- the matrix to be used in constructing the preconditioner, usually the same as Amat.
-- J	- Jacobian evaluation routine (if NULL then SNES retains any previously set value), see SNESJacobianFunction for details
-- ctx	- [optional] user-defined context for private data for the Jacobian evaluation routine (may be NULL) (if NULL then SNES retains any previously set value)

{-
Notes

If the Amat matrix and Pmat matrix are different you must call MatAssemblyBegin/End() on each matrix.
If you know the operator Amat has a null space you can use MatSetNullSpace() and MatSetTransposeNullSpace() to supply the null space to Amat and the KSP solvers will automatically use that null space as needed during the solution process.

If using SNESComputeJacobianDefaultColor() to assemble a Jacobian, the ctx argument must be a MatFDColoring.

Other defect-correction schemes can be used by computing a different matrix in place of the Jacobian. One common example is to use the "Picard linearization" which only differentiates through the highest order parts of each term.
-}

{-
PetscErrorCode SNESJacobianFunction(SNES snes,Vec x,Mat Amat,Mat Pmat,void *ctx);
x	- input vector
Amat	- the matrix that defines the (approximate) Jacobian
Pmat	- the matrix to be used in constructing the preconditioner, usually the same as Amat.
ctx	- [optional] user-defined Jacobian context
-}

snesSetJacobian0' snes amat pmat f ctx =
  [C.exp|int{SNESSetJacobian($(SNES snes),$(Mat amat),$(Mat pmat),
                             $fun:(int (*f)(SNES,Vec,Mat,Mat,void*)),$(void* ctx))}|]

snesSetJacobian' snes amat pmat f =
  snesSetJacobian0' snes amat pmat f' where
    f' s v a p _ = f s v a p 

snesSetJacobian0_' snes amat pmat f =
  [C.exp|int{SNESSetJacobian($(SNES snes),$(Mat amat),$(Mat pmat),
                             $fun:(int (*f)(SNES,Vec,Mat,Mat,void*)), NULL)}|]

snesSetJacobian_' snes amat pmat f =
  snesSetJacobian0_' snes amat pmat f' where
    f' s v a p _ = f s v a p 


-- -- monomorphic SNESSetJacobian : see e.g. www.mcs.anl.gov/petsc/petsc-current/src/snes/examples/tutorials/ex5s.c.html
-- -- usage : SNESSetJacobian(snes,J,J,SNESComputeJacobianDefaultColor,fdcoloring);
snesSetJacobian0mono' snes amat pmat f col =
  [C.exp|int{SNESSetJacobian($(SNES snes),$(Mat amat),$(Mat pmat),
                             $fun:(int (*f)(SNES,Vec,Mat,Mat,MatFDColoring*)),
                             $(MatFDColoring col))}|]



snesSetJacobianComputeDefaultColor' :: SNES -> Mat -> Mat -> MatFDColoring -> IO CInt
snesSetJacobianComputeDefaultColor' snes amat pmat =
  snesSetJacobian0mono' snes amat pmat snesComputeJacobianDefaultColor0' 



-- PetscErrorCode  SNESComputeJacobianDefaultColor(SNES snes,Vec x1,Mat J,Mat B,void *ctx)
snesComputeJacobianDefaultColor0' ::
  SNES -> Vec -> Mat -> Mat -> Ptr MatFDColoring -> IO CInt 
snesComputeJacobianDefaultColor0' snes x jj bb fdcoloring =
  [C.exp|int{SNESComputeJacobianDefaultColor($(SNES snes),$(Vec x),$(Mat jj),$(Mat bb),$(MatFDColoring* fdcoloring))}|]





-- * SNESLineSearch

-- PetscErrorCode SNESGetLineSearch(SNES snes, SNESLineSearch *linesearch)
snesGetLineSearch' snes =
  withPtr ( \ls ->
     [C.exp|int{SNESGetLineSearch($(SNES snes),
                                  $(SNESLineSearch* ls))}|]) 










-- * TS




-- PetscErrorCode  TSCreate(MPI_Comm comm, TS *ts)
tsCreate' comm =
  withPtr (\ts -> [C.exp|int{TSCreate($(int c), $(TS* ts))}|]) 
  where
   c = unComm comm

tsDestroy0' ts = [C.exp| int{TSDestroy($(TS* ts))} |] 
tsDestroy' ts = with ts tsDestroy0'

-- withTs c = bracket (tsCreate c) tsDestroy


-- PetscErrorCode  TSSetProblemType(TS ts, TSProblemType type)
tsSetProblemType' ts t =
  [C.exp|int{TSSetProblemType($(TS ts), $(int tt))}|] 
   where tt = fromIntegral $ tsProblemTypeToInt t


-- PetscErrorCode TSSetInitialTimeStep(TS ts,PetscReal initial_time,PetscReal time_step)
tsSetInitialTimeStep' ts it dt =
  [C.exp|int{TSSetInitialTimeStep($(TS ts), $(PetscReal it), $(PetscReal dt))}|] 

-- PetscErrorCode  TSSetDuration(TS ts,PetscInt maxsteps,PetscReal maxtime)
tsSetDuration' ts ms' mt =
  [C.exp|int{TSSetDuration($(TS ts),$(int ms),$(PetscReal mt))}|] 
  where
    ms = fromIntegral (ms' :: Int)



-- PetscErrorCode  TSSetRHSFunction(TS ts,Vec r,PetscErrorCode (*f)(TS,PetscReal,Vec,Vec,void*),void *ctx)        -- Logically Collective on TS
-- -- Sets the routine for evaluating the function, where U_t = G(t,u).
-- Input Parameters :
-- ts	- the TS context obtained from TSCreate()
-- r	- vector to put the computed right hand side (or NULL to have it created)
-- f	- routine for evaluating the right-hand-side function
-- ctx	- [optional] user-defined context for private data for the function evaluation routine (may be NULL)
-- Calling sequence of func :
--     func (TS ts,PetscReal t,Vec u,Vec F,void *ctx);
-- t	- current timestep
-- u	- input vector
-- F	- function vector
-- ctx	- [optional] user-defined function context
-- Notes: You must call this function or TSSetIFunction() to define your ODE. You cannot use this function when solving a DAE.



-- PetscErrorCode  TSSetRHSJacobian(TS ts,Mat Amat,Mat Pmat,TSRHSJacobian f,void *ctx)
-- --Sets the function to compute the Jacobian of G, where U_t = G(U,t), as well as the location to store the matrix.
-- Logically Collective on TS
-- Input Parameters :
-- ts	- the TS context obtained from TSCreate()
-- Amat	- (approximate) Jacobian matrix
-- Pmat	- matrix from which preconditioner is to be constructed (usually the same as Amat)
-- f	- the Jacobian evaluation routine
-- ctx	- [optional] user-defined context for private data for the Jacobian evaluation routine (may be NULL)
-- Calling sequence of f :
--     func (TS ts,PetscReal t,Vec u,Mat A,Mat B,void *ctx);
-- t	- current timestep
-- u	- input vector
-- Amat	- (approximate) Jacobian matrix
-- Pmat	- matrix from which preconditioner is to be constructed (usually the same as Amat)
-- ctx	- [optional] user-defined context for matrix evaluation routine


    


-- withTSSolve c pt t0 dt0 maxsteps maxtime v pre post = withTs c $ \ts -> do
--   -- NB: Vec v contains initial condition and will be updated with final state
--   -- -- (initial state must be given or computed internally)
--   -- NB2 : this version does not account for time-varying right-hand side
--   -- see:
--   -- http://www.mcs.anl.gov/petsc/petsc-current/src/ts/examples/tutorials/ex3.c.html
--   tsSetProblemType ts pt
--   tsSetInitialTimeStep ts t0 dt0
--   tsSetDuration ts maxsteps maxtime
--   pre ts
--   tsSolve ts v
--   post ts

  


-- TSSetFromOptions(ts);

--    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
--       Solve the problem
--       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
--    /*
--       Evaluate initial conditions
--    */

--    InitialConditions(u,&appctx);


-- PetscErrorCode  TSSetSolution(TS ts,Vec u)
tsSetSolution' ts u = [C.exp|int{TSSetSolution($(TS ts),$(Vec u))}|]


--    TSSolve(ts,u);
-- PetscErrorCode TSSolve(TS ts,Vec u)        -- Collective on TS
-- Input Parameters :
-- ts	- the TS context obtained from TSCreate()
-- u	- the solution vector (can be null if TSSetSolution() was used, otherwise must contain the initial conditions)
-- Notes :
-- The final time returned by this function may be different from the time of the internally held state accessible by TSGetSolution() and TSGetTime() because the method may have stepped over the final time.

tsSolve' ts u = [C.exp|int{TSSolve($(TS ts),$(Vec u))}|]

tsSolve_' ts = [C.exp|int{TSSolve($(TS ts), NULL)}|]
    
--    TSGetTimeStepNumber(ts,&steps);

--    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
--       View timestepping solver info
--       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

--    TSView(ts,PETSC_VIEWER_STDOUT_SELF);
tsViewStdout' ts =
  [C.exp|int{TSView($(TS ts), PETSC_VIEWER_STDOUT_SELF)}|] 


















-- * TAO


-- taoInitialize args opts help = 
--  let acc = fromIntegral $ length args in 
--   with acc $ \ac ->
--    withCStringArray args $ \aa ->
--    with aa $ \a ->
--     withCString opts $ \o ->
--     withCString help $ \h ->
--     [C.exp|int{TaoInitialize($(int *ac), $(char*** a), $(char* o), $(char *h))}|] 

-- taoFin = [C.block| int{ TaoFinalize(); }|] 

-- withTaoInit a o h f = do 
--   taoInitialize a o h
--   f
--   taoFin

-- withTaoInit0 = withTaoInit [] [] [] 

taoCreate' comm = withPtr (\p -> [C.exp| int{TaoCreate($(int c), $(Tao *p))} |] )
  where
  c = unComm comm

taoDestroy' p = with p ( \pp -> [C.exp| int{TaoDestroy($(Tao *pp))}  |] ) 

-- withTao c = bracket (taoCreate c) taoDestroy

taoSetType' tao ti = withCString ti_ ( \tip -> [C.exp|int{TaoSetType($(Tao tao), $(char* tip ))}|] )
  where
  ti_ = taoTypeToStr ti

taoViewStdout' tao = [C.exp|int{TaoView($(Tao tao), PETSC_VIEWER_STDOUT_SELF)}|]

-- taoGetConvergedReason tao = liftM taoConvergedIntToReason $
--    withPtr (\tr -> [C.exp|int{TaoGetConvergedReason($(Tao tao), $(int* tr))}|]) 


-- TaoSetInitialVector(TaoSolver tao, Vec x);
taoSetInitialVector' tao x = [C.exp|int{TaoSetInitialVector($(Tao tao),$(Vec x))}|] 

-- TaoSolve(TaoSolver tao);
taoSolve' tao = [C.exp|int{TaoSolve($(Tao tao))}|] 



-- PETSC_EXTERN PetscErrorCode TaoGetSolutionVector(Tao, Vec*);
taoGetSolutionVector' tao = withPtr (\p -> [C.exp|int{TaoGetSolutionVector($(Tao tao), $(Vec* p))}|])

-- PETSC_EXTERN PetscErrorCode TaoGetGradientVector(Tao, Vec*);
taoGetGradientVector' tao = withPtr (\p -> [C.exp|int{TaoGetGradientVector($(Tao tao), $(Vec* p))}|]) 

-- PETSC_EXTERN PetscErrorCode TaoSetObjectiveRoutine(Tao, PetscErrorCode(*)(Tao, Vec, PetscReal*,void*), void*);
taoSetObjectiveRoutine' tao f =
  [C.exp|int{TaoSetObjectiveRoutine($(Tao tao),
                                    $fun:(int (*f)(Tao, Vec, PetscReal*, void*)),
                                    NULL)}|] 
taoSetObjectiveRoutine t f = taoSetObjectiveRoutine' t f' where
  f' ta v r _ = f ta v r


-- PETSC_EXTERN PetscErrorCode TaoSetGradientRoutine(Tao, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);
taoSetGradientRoutine' tao f =
  [C.exp|int{TaoSetGradientRoutine($(Tao tao),
                                    $fun:(int (*f)(Tao, Vec, Vec, void*)),
                                    NULL)}|] 
taoSetGradientRoutine t f = taoSetGradientRoutine' t f' where
  f' ta v r _ = f ta v r
  
-- PETSC_EXTERN PetscErrorCode TaoSetObjectiveAndGradientRoutine(Tao, PetscErrorCode(*)(Tao, Vec, PetscReal*, Vec, void*), void*);
taoSetObjectiveAndGradientRoutine' tao f =
  [C.exp|int{TaoSetObjectiveAndGradientRoutine(
                $(Tao tao),
                $fun:(int (*f)(Tao, Vec, PetscReal*, Vec, void*)),
                NULL)}|] 
taoSetObjectiveAndGradientRoutine t f =
  taoSetObjectiveAndGradientRoutine' t f' where
    f' ta v r v2 _ = f ta v r v2
  
-- PETSC_EXTERN PetscErrorCode TaoSetHessianRoutine(Tao,Mat,Mat,PetscErrorCode(*)(Tao,Vec, Mat, Mat, void*), void*);
taoSetHessianRoutine' tao m1 m2 f =
  [C.exp|int{TaoSetHessianRoutine($(Tao tao), $(Mat m1), $(Mat m2),
                                    $fun:(int (*f)(Tao, Vec, Mat, Mat, void*)),
                                    NULL)}|] 
taoSetHessianRoutine t m1 m2 f = taoSetHessianRoutine' t m1 m2 f' where
  f' ta v n1 n2 _ = f ta v n1 n2


-- PETSC_EXTERN PetscErrorCode TaoSetSeparableObjectiveRoutine(Tao, Vec, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);
-- PETSC_EXTERN PetscErrorCode TaoSetConstraintsRoutine(Tao, Vec, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);
taoSetConstraintsRoutine' tao vec f =
  [C.exp|int{TaoSetConstraintsRoutine(
                $(Tao tao), $(Vec vec),
                $fun:(int (*f)(Tao, Vec, Vec, void*)),
                NULL)}|] 
taoSetConstraintsRoutine t v f = taoSetConstraintsRoutine' t v f' where
  f' ta v1 v2  _ = f ta v1 v2
  
-- PETSC_EXTERN PetscErrorCode TaoSetInequalityConstraintsRoutine(Tao, Vec, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);
taoSetInequalityConstraintsRoutine' tao vec f =
  [C.exp|int{TaoSetInequalityConstraintsRoutine(
                $(Tao tao), $(Vec vec),
                $fun:(int (*f)(Tao, Vec, Vec, void*)),
                NULL)}|] 
taoSetInequalityConstraintsRoutine t v f =
  taoSetInequalityConstraintsRoutine' t v f' where
    f' ta v1 v2  _ = f ta v1 v2
  
-- PETSC_EXTERN PetscErrorCode TaoSetEqualityConstraintsRoutine(Tao, Vec, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);
taoSetEqualityConstraintsRoutine' tao vec f =
  [C.exp|int{TaoSetEqualityConstraintsRoutine(
                $(Tao tao), $(Vec vec),
                $fun:(int (*f)(Tao, Vec, Vec, void*)),
                NULL)}|] 
taoSetEqualityConstraintsRoutine t v f =
  taoSetEqualityConstraintsRoutine' t v f' where
     f' ta v1 v2  _ = f ta v1 v2
  
-- PETSC_EXTERN PetscErrorCode TaoSetJacobianRoutine(Tao,Mat,Mat, PetscErrorCode(*)(Tao,Vec, Mat, Mat, void*), void*);
taoSetJacobianRoutine' tao m1 m2 f =
  [C.exp|int{TaoSetJacobianRoutine(
                $(Tao tao), $(Mat m1), $(Mat m2),
                $fun:(int (*f)(Tao, Vec, Mat, Mat, void*)),
                NULL)}|] 
taoSetJacobianRoutine t m1 m2 f =
  taoSetJacobianRoutine' t m1 m2 f' where
     f' ta v v1 v2  _ = f ta v v1 v2
     
-- PETSC_EXTERN PetscErrorCode TaoSetJacobianStateRoutine(Tao,Mat,Mat,Mat, PetscErrorCode(*)(Tao,Vec, Mat, Mat, Mat, void*), void*);
taoSetJacobianStateRoutine' tao m1 m2 m3 f =
  [C.exp|int{TaoSetJacobianStateRoutine(
                $(Tao tao), $(Mat m1), $(Mat m2), $(Mat m3),
                $fun:(int (*f)(Tao, Vec, Mat, Mat, Mat, void*)),
                NULL)}|] 
taoSetJacobianStateRoutine t m1 m2 m3 f =
  taoSetJacobianStateRoutine' t m1 m2 m3 f' where
     f' ta v v1 v2 v3  _ = f ta v v1 v2 v3

-- PETSC_EXTERN PetscErrorCode TaoSetJacobianDesignRoutine(Tao,Mat,PetscErrorCode(*)(Tao,Vec, Mat, void*), void*);
taoSetJacobianDesignRoutine' tao m f =
  [C.exp|int{TaoSetJacobianDesignRoutine(
                $(Tao tao), $(Mat m),
                $fun:(int (*f)(Tao, Vec, Mat, void*)),
                NULL)}|] 
taoSetJacobianDesignRoutine t m f =
  taoSetJacobianDesignRoutine' t m f' where
     f' ta v v1  _ = f ta v v1
     
-- PETSC_EXTERN PetscErrorCode TaoSetJacobianInequalityRoutine(Tao,Mat,Mat,PetscErrorCode(*)(Tao,Vec, Mat, Mat, void*), void*);
-- PETSC_EXTERN PetscErrorCode TaoSetJacobianEqualityRoutine(Tao,Mat,Mat,PetscErrorCode(*)(Tao,Vec, Mat, Mat, void*), void*);

-- PETSC_EXTERN PetscErrorCode TaoSetStateDesignIS(Tao, IS, IS);

-- PETSC_EXTERN PetscErrorCode TaoComputeObjective(Tao, Vec, PetscReal*);
taoComputeObjective' tao v =
  withPtr (\p -> [C.exp|int{TaoComputeObjective($(Tao tao),$(Vec v),$(PetscReal* p))}|] ) 
-- PETSC_EXTERN PetscErrorCode TaoComputeSeparableObjective(Tao, Vec, Vec);

-- PETSC_EXTERN PetscErrorCode TaoComputeGradient(Tao, Vec, Vec);
taoComputeGradient' tao v =
 -- -- -- -- DIRTY HACK WARNING: will it work?
  withPtr (\p -> [C.exp|int{TaoComputeGradient($(Tao tao),$(Vec v),$(Vec* p))}|] ) 
  
-- PETSC_EXTERN PetscErrorCode TaoComputeObjectiveAndGradient(Tao, Vec, PetscReal*, Vec);
-- PETSC_EXTERN PetscErrorCode TaoComputeConstraints(Tao, Vec, Vec);
-- PETSC_EXTERN PetscErrorCode TaoComputeInequalityConstraints(Tao, Vec, Vec);
-- PETSC_EXTERN PetscErrorCode TaoComputeEqualityConstraints(Tao, Vec, Vec);
-- PETSC_EXTERN PetscErrorCode TaoDefaultComputeGradient(Tao, Vec, Vec, void*);
-- PETSC_EXTERN PetscErrorCode TaoIsObjectiveDefined(Tao,PetscBool*);
taoIsObjectiveDefined' t =
 withPtr
  (\p -> [C.exp|int{TaoIsObjectiveDefined($(Tao t),
                                          $(PetscBool* p))}|]) 

-- PETSC_EXTERN PetscErrorCode TaoIsGradientDefined(Tao,PetscBool*);
taoIsGradientDefined' t =
 withPtr
  (\p -> [C.exp|int{TaoIsGradientDefined($(Tao t),
                                          $(PetscBool* p))}|]) 

-- PETSC_EXTERN PetscErrorCode TaoIsObjectiveAndGradientDefined(Tao,PetscBool*);



-- withTaoSetupSolve c ti x pre post = withTao c $ \t -> do
--   taoSetType t ti
--   taoSetInitialVector t x
--   pre t
--   taoSolve t
--   post t
--   -- taoGetConvergedReason t

-- withTaoSetup c ti x pre post = withTao c $ \t -> do
--   taoSetType t ti
--   taoSetInitialVector t x
--   pre t
--   post t
--   -- taoGetConvergedReason t



-- PETSC_EXTERN PetscErrorCode TaoComputeHessian(Tao, Vec, Mat, Mat);
-- PETSC_EXTERN PetscErrorCode TaoComputeJacobian(Tao, Vec, Mat, Mat);
-- PETSC_EXTERN PetscErrorCode TaoComputeJacobianState(Tao, Vec, Mat, Mat, Mat);
-- PETSC_EXTERN PetscErrorCode TaoComputeJacobianEquality(Tao, Vec, Mat, Mat);
-- PETSC_EXTERN PetscErrorCode TaoComputeJacobianInequality(Tao, Vec, Mat, Mat);
-- PETSC_EXTERN PetscErrorCode TaoComputeJacobianDesign(Tao, Vec, Mat);

-- PETSC_EXTERN PetscErrorCode TaoDefaultComputeHessian(Tao, Vec, Mat, Mat, void*);
-- PETSC_EXTERN PetscErrorCode TaoDefaultComputeHessianColor(Tao, Vec, Mat, Mat, void*);
-- PETSC_EXTERN PetscErrorCode TaoComputeDualVariables(Tao, Vec, Vec);
-- PETSC_EXTERN PetscErrorCode TaoComputeDualVariables(Tao, Vec, Vec);
-- PETSC_EXTERN PetscErrorCode TaoSetVariableBounds(Tao, Vec, Vec);
taoSetVariableBounds' tao x1 x2 =
  [C.exp|int{TaoSetVariableBounds($(Tao tao), $(Vec x1), $(Vec x2))}|]
  
-- PETSC_EXTERN PetscErrorCode TaoGetVariableBounds(Tao, Vec*, Vec*);
-- PETSC_EXTERN PetscErrorCode TaoGetDualVariables(Tao, Vec*, Vec*);
-- PETSC_EXTERN PetscErrorCode TaoSetInequalityBounds(Tao, Vec, Vec);
-- PETSC_EXTERN PetscErrorCode TaoGetInequalityBounds(Tao, Vec*, Vec*);

-- PETSC_EXTERN PetscErrorCode TaoSetVariableBoundsRoutine(Tao, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);
taoSetVariableBoundsRoutine' tao f =
  [C.exp|int{TaoSetVariableBoundsRoutine($(Tao tao),
                                           $fun:(int (*f)(Tao, Vec, Vec, void*)),
                                           NULL)}|]
taoSetVariableBoundsRoutine tao f =
  taoSetVariableBoundsRoutine' tao f' where
    f' t v1 v2 _ = f t v1 v2

  
-- PETSC_EXTERN PetscErrorCode TaoComputeVariableBounds(Tao);

-- PETSC_EXTERN PetscErrorCode TaoGetTolerances(Tao, PetscReal*, PetscReal*, PetscReal*, PetscReal*, PetscReal*);
-- PETSC_EXTERN PetscErrorCode TaoSetTolerances(Tao, PetscReal, PetscReal, PetscReal, PetscReal, PetscReal);
-- PETSC_EXTERN PetscErrorCode TaoGetConstraintTolerances(Tao, PetscReal*, PetscReal*);
-- PETSC_EXTERN PetscErrorCode TaoSetConstraintTolerances(Tao, PetscReal, PetscReal);
-- PETSC_EXTERN PetscErrorCode TaoSetFunctionLowerBound(Tao, PetscReal);
-- PETSC_EXTERN PetscErrorCode TaoSetInitialTrustRegionRadius(Tao, PetscReal);
-- PETSC_EXTERN PetscErrorCode TaoSetMaximumIterations(Tao, PetscInt);
-- PETSC_EXTERN PetscErrorCode TaoSetMaximumFunctionEvaluations(Tao, PetscInt);
-- PETSC_EXTERN PetscErrorCode TaoGetFunctionLowerBound(Tao, PetscReal*);
-- PETSC_EXTERN PetscErrorCode TaoGetInitialTrustRegionRadius(Tao, PetscReal*);
-- PETSC_EXTERN PetscErrorCode TaoGetCurrentTrustRegionRadius(Tao, PetscReal*);
-- PETSC_EXTERN PetscErrorCode TaoGetMaximumIterations(Tao, PetscInt*);
-- PETSC_EXTERN PetscErrorCode TaoGetMaximumFunctionEvaluations(Tao, PetscInt*);
-- PETSC_EXTERN PetscErrorCode TaoSetOptionsPrefix(Tao, const char p[]);
-- PETSC_EXTERN PetscErrorCode TaoAppendOptionsPrefix(Tao, const char p[]);
-- PETSC_EXTERN PetscErrorCode TaoGetOptionsPrefix(Tao, const char *p[]);
-- PETSC_EXTERN PetscErrorCode TaoResetStatistics(Tao);

-- PETSC_EXTERN PetscErrorCode TaoGetKSP(Tao, KSP*);











-- * Viewer

petscViewerCreate' comm = withPtr $ \h -> 
  [C.exp|int{PetscViewerCreate($(int c),$(PetscViewer* h))}|] where c = unComm comm

-- PetscErrorCode  PetscViewerSetType(PetscViewer viewer,PetscViewerType type)
petscViewerSetType' v t = withCString ts $ \tsp ->
  [C.exp|int{PetscViewerSetType($(PetscViewer v),$(char* tsp))}|] where
    -- tc = toCInt $ viewerTypeToInt t
    ts = viewerTypeToStr t

-- PetscErrorCode  PetscViewerHDF5Open(MPI_Comm comm, const char name[], PetscFileMode type, PetscViewer *hdf5v)
petscViewerHDF5Open' comm name ty =
  withPtr $ \f ->
   withCString name $ \np -> 
  [C.exp|int{PetscViewerHDF5Open($(int c),$(char* np),$(int t),$(PetscViewer* f))}|]
   where
     c = unComm comm
     t = toCInt $ viewerTypeToInt ty
    
-- -- usage
-- 339:   PetscViewerCreate(comm, hdf5v);
-- 340:   PetscViewerSetType(*hdf5v, PETSCVIEWERHDF5);
-- 341:   PetscViewerFileSetMode(*hdf5v, type);
-- 342:   PetscViewerFileSetName(*hdf5v, name);

-- PetscErrorCode PetscViewerFileSetMode(PetscViewer viewer,PetscFileMode type)
petscViewerFileSetMode' v m =
  [C.exp|int{PetscViewerFileSetMode($(PetscViewer v),$(int mp))}|] where
    mp = toCInt $ fileModeToInt m

-- PetscErrorCode  PetscViewerFileSetName(PetscViewer viewer,const char name[])
petscViewerFileSetName' v name = withCString name $ \n -> 
  [C.exp|int{PetscViewerFileSetName($(PetscViewer v),$(char* n))}|] 


-- PetscErrorCode  PetscViewerDestroy(PetscViewer *viewer)
petscViewerDestroy' v =
  with v $ \vp -> [C.exp|int{PetscViewerDestroy($(PetscViewer* vp))}|]


-- PetscErrorCode  PetscViewerHDF5PushGroup(PetscViewer viewer, const char *name)
petscViewerHDF5PushGroup1 v name = withCString name $ \n -> 
  [C.exp|int{PetscViewerHDF5PushGroup($(PetscViewer v),$(char* n))}|]

-- PetscErrorCode  PetscViewerHDF5PopGroup(PetscViewer viewer)
petscViewerHDF5PopGroup1 v =
  [C.exp|int{PetscViewerHDF5PopGroup($(PetscViewer v))}|]




-- * PETSc misc


-- PETSC_EXTERN PetscErrorCode PetscLogStageRegister(const char[],PetscLogStage*);
-- PETSC_EXTERN PetscErrorCode PetscLogStagePush(PetscLogStage);

-- petscLogStageRegister :: String -> PetscLogStage_ -> IO CInt
petscLogStageRegister' s ls =
  withCString s $ \c ->
   with ls $ \lls -> 
    [C.exp|int{PetscLogStageRegister($(char *c), $(PetscLogStage* lls ))}|] 

petscLogStagePush' ls = [C.exp|int{PetscLogStagePush($(PetscLogStage ls))}|] 

petscLogStagePop' = [C.exp|int{PetscLogStagePop()}|] 


-- -- * options

-- #include "petscsys.h"   
-- PetscErrorCode  PetscOptionsGetInt(const char pre[],const char name[],PetscInt *ivalue,PetscBool  *set)    -- Not Collective
-- Input Parameters :
-- pre	- the string to prepend to the name or NULL
-- name	- the option one is seeking
-- Output Parameters :
-- ivalue	- the integer value to return
-- set	- PETSC_TRUE if found, else PETSC_FALSE

petscOptionsGetInt' :: Ptr CChar -> Ptr CChar -> Ptr CInt -> Ptr PetscBool_ -> IO CInt
petscOptionsGetInt' pre name n s = [C.exp| int{PetscOptionsGetInt($(char *pre), $(char *name), $(int *n), $(PetscBool *s))} |]

petscOptionsGetInt'' prefix name = 
  withCString prefix ( \p ->
   withCString name $ \n ->
    withPtr $ \ptr ->
     withPtr $ \pb -> 
      petscOptionsGetInt' p n ptr pb) -- >>= \(a, (f, e)) -> handleErrTup ((a, f), e)

-- petscOptionsGetInt prefix name = do
--   (a, (f, e)) <- petscOptionsGetInt'' prefix name
--   if f then handleErrTup (Just a, e)
--        else handleErrTup (Nothing, e)

-- withPetscOptionsGetInt prefix name f = do
--   x <- petscOptionsGetInt prefix name
--   case x of (Just s) -> f s
--             Nothing  -> error "option not found"




-- PETSC_EXTERN PetscErrorCode PetscGetArgs(int*,char ***);
-- petscGetArgs                   -- 


-- PETSC_EXTERN PetscErrorCode PetscInitialized(PetscBool *);

-- petscInitialized :: IO Bool
petscInitialized = withPtr ( \b ->
     [C.exp|int{ PetscInitialized($(PetscBool * b)) } |] )   

  

-- PETSC_EXTERN PetscErrorCode PetscFinalized(PetscBool *);

-- petscFinalized :: IO Bool
petscFinalized = withPtr ( \p ->
  [C.exp|int{ PetscFinalized($(PetscBool * p))  }|] )



-- petscInit0 :: IO ()
petscInit01 = [C.exp| int{ PetscInitializeNoArguments()  }|]

-- -- PETSC_EXTERN PetscErrorCode PetscInitialize(int*,char***,const char[],const char[]);

petscInitialize1 args opts help = 
 let acc = fromIntegral $ length args in 
  with acc $ \ac ->
   withCStringArray args $ \aa ->
   with aa $ \a ->
    withCString opts $ \o ->
    withCString help $ \h ->
    [C.exp|int{PetscInitialize($(int *ac), $(char*** a), $(char* o), $(char *h))}|] 

type Argv = [String]
type OptsStr = String
type HelpStr = String

petscFin1 = [C.block| int{ PetscFinalize(); }|] 

withPetsc01 f = do -- returns IO ()
  petscInit01
  f
  petscFin1

withPetsc01', withPetsc0'' :: IO a -> IO a
withPetsc0'' f =
  petscInit01 >> (f `finally` petscFin1)
  
withPetsc01' = bracket_ petscInit01 petscFin1 -- returns IO a

-- withPetsc :: Argv -> OptsStr -> HelpStr -> IO a -> IO ()
-- withPetsc argv opts help f = do
--   petscInitialize argv opts help
--   f
--   petscFin

withPetsc' :: Argv -> OptsStr -> HelpStr -> IO a -> IO a
withPetsc' a o h = bracket_ (petscInitialize1 a o h) petscFin1

withPetsc'' a o h f = petscInitialize1 a o h >> (f `finally` petscFin1)

-- -- NB : bracket_ ignores the return type of the allocation action




-- * error codes

-- -- PETSC_EXTERN PetscErrorCode PetscErrorMessage(int,const char*[],char **);

petscErrorMessage' nn mp =
  [C.exp|int{PetscErrorMessage($(int nn), $(char** mp), NULL)} |]


-- PETSC_EXTERN PetscErrorCode PetscMemoryShowUsage(PetscViewer,const char[]);

-- pmsu v c = [C.exp|int{PetscMemoryShowUsage($())}|]

-- petscMemoryShowUsage v = do
--   p <- withPtr


-- alloca $ \stringptr -> do
--    ... call some Ptr CString function
--    peek stringptr






  

-- * MPI






-- mpiCommSize c = withPtr $ \p -> [C.exp|int{ MPI_Comm_Size($(int c), $(PetscMPIInt_ *p)) }|] 

mpiCommSize' comm = withPtr (\p -> [C.exp| int{ MPI_Comm_size($(int c), $(int *p))}|] )
  where
   c = unComm comm
mpiCommSize c =  unsafePerformIO $ mpiCommSize' c 

mpiCommRank' comm =
  withPtr
   (\p ->
     [C.exp| int{ MPI_Comm_rank($(int c), $(int *p))}|] )
  where
   c = unComm comm
   
-- mpiCommRank c =
--    MkRank $ unsafePerformIO $ mpiCommRank' c   -- FIXME surface it in PutGet


{-# NOINLINE commWorld1 #-}
commWorld1 = Comm $ unsafePerformIO [C.exp| int{ MPI_COMM_WORLD }  |] 
commSelf1 = Comm $ unsafePerformIO [C.exp| int{ MPI_COMM_SELF }  |]



    -- PetscPrintf - Prints to standard out, only from the first
    -- processor in the communicator. Calls from other processes are ignored.
petscPrintf comm s =
  withCString s
   ( \s_ -> [C.exp|int{PetscPrintf($(int c), $(char* s_))}|] ) -- >>= handleErr
  where
    c = unComm comm


petscSynchronizedPrintf' comm s = withCString s ( \s_ ->
  [C.exp|int{PetscSynchronizedPrintf($(int c), $(char* s_))}|] )
    where c = unComm comm

-- petscSynchronizedFlushStdout comm =
--   [C.exp|int{PetscSynchronizedFlush($(int c),PETSC_STDOUT )}|]
--     where c = unComm comm

petscSynchronizedFlushStdout comm =
  [C.exp|int{PetscSynchronizedFlush($(int c), 0 )}|]
    where c = unComm comm


-- syncPrintf c s =
--   petscSynchronizedPrintf c s >> petscSynchronizedFlushStdout c 


-- * misc parallelism stuff

-- localRange :: Comm -> Int -> (Int, Int)
-- localRange c m = ( istart, iend) where
--   istart = cr * (m `div` cs) + if t < cr then t else cr
--   iend = istart + (m `div` cs) + if t > cr then 1 else 0
--   cr = fromIntegral $ rankId ( mpiCommRank c )
--   cs = fromIntegral $ mpiCommSize c
--   t = m `mod` cs


  -- start = rank*(mm /size) + ((mm %size) < rank ? (mm %size) : rank);
  -- end   = start + mm /size + ((mm %size) > rank);








-- *











