{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.InlineC
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Foreign signatures, + everything that requires an inline-c pass
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.InlineC where

import           Numerical.PETSc.Internal.Internal
import           Numerical.PETSc.Internal.Types
import           Numerical.PETSc.Internal.Utils

import           Language.C.Inline                 as C
import           Language.C.Inline.Context
import           Control.Exception
import           Foreign
import           Foreign.Marshal.Array
import           Foreign.Marshal.Alloc             (alloca)
import           Foreign.Ptr                       (Ptr)
import           Control.Monad
-- import           Control.Monad.Primitive
-- import Control.Arrow ((***), (&&&))
import           Control.Applicative               ( (<$>), (<*>) )
import           Foreign.C.Types
import           Foreign.C.String
-- import qualified Foreign.ForeignPtr.Safe           as FPS

import qualified Data.ByteString as BS

import qualified Data.Vector.Storable              as V
import qualified Data.Vector.Storable.Mutable      as VM

import           System.IO.Unsafe                  (unsafePerformIO)

context petscCtx

C.include "<petscsnes.h>"
C.include "<petsctao.h>"
C.include "<petscdm.h>"
C.include "<petscdmda.h>"
C.include "<petscdmcomposite.h>"
C.include "<petscts.h>"
C.include "<petscviewer.h>"
C.include "<petscviewerhdf5.h>"
C.include "<petscsys.h>"


petscDecide = -1

-- * IS

isCreateStride_ c n first step is = [C.exp|
     int{ISCreateStride(
            $(int c),
            $(PetscInt n),
            $(PetscInt first),
            $(PetscInt step),
            $(IS* is)) }|]

isCreateStride' comm n first step =
  withPtr $ \is -> isCreateStride_ c n first step is
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

isCreateGeneral_ c n idxp mo isp  =
  [C.exp|int{ISCreateGeneral($(int c),
                             $(PetscInt n),
                             $(PetscInt* idxp),
                             $(int mo),
                             $(IS* isp))}|]

isCreateGeneral' comm n idx mode =
   withArray idx $ \idxp ->
    withPtr $ \isp -> isCreateGeneral_ c n idxp mo isp 
     where mo = fromIntegral $ petscCopyModeToInt mode
           c = unComm comm

isDestroy_ iisp = [C.exp|int{ISDestroy($(IS* iisp))} |]

isDestroy' iis = with iis isDestroy_



-- withIsCreateGeneral comm n idx mode = bracket (isCreateGeneral comm n idx mode) isDestroy




-- -- * IS coloring : see e.g. www.mcs.anl.gov/petsc/petsc-current/src/snes/examples/tutorials/ex5s.c.html

-- PetscErrorCode  ISColoringCreate(MPI_Comm comm,PetscInt ncolors,PetscInt n,const ISColoringValue colors[],PetscCopyMode mode,ISColoring *iscoloring)
isColoringCreate' :: Comm
                           -> CInt
                           -> CInt
                           -> [CInt]
                           -> PetscCopyMode_
                           -> IO (ISColoring, CInt)
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

-- vecCreateMPILocal c m = vecCreateMPI' c m m



-- -- have PETSc decide the local Vec dimension

-- PetscErrorCode VecCreateMPI(MPI_Comm comm, int m, int M, Vec* x)
vecCreateMPIdecideLoc0' comm nglob p =
  [C.exp|int{VecCreateMPI($(int c), PETSC_DECIDE, $(int m1), $(Vec *p))}|] 
    where c = unComm comm
          m1 = toCInt nglob

vecCreateMPIdecideLoc' comm nglob = withPtr (vecCreateMPIdecideLoc0' comm nglob)







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

vecSet1 v n = [C.exp|int{VecSet( $(Vec v), $(PetscScalar n))}|] 

vecSetSizes1 v n = [C.exp|int{VecSetSizes( $(Vec v), PETSC_DECIDE, $(int n))}|] 


-- | get Vec length

-- PETSC_EXTERN PetscErrorCode VecGetSize(Vec,PetscInt*);
vecGetSize0' v p =  [C.exp|int{VecGetSize($(Vec v), $(int *p))}|]

vecGetSize' :: Vec -> IO (CInt, CInt)
vecGetSize' v = withPtr $ \p -> vecGetSize0' v p

vecGetSizeUnsafe' = unsafePerformIO . vecGetSize'

vecSize' :: Vec -> Int
vecSize' = fi . fst . vecGetSizeUnsafe'



-- | view Vec values on stdout

vecViewStdout1 :: Vec -> IO CInt
vecViewStdout1 v = [C.exp|int{VecView($(Vec v), PETSC_VIEWER_STDOUT_SELF)}|] 


-- withVecGetArray' v f =
--   [C.block|
--    int{
--      PetscScalar* temp;
--      int szv = VecGetSize(v);
--      int e1 = VecGetArray($(Vec v), &temp);

--      int e2 = VecRestoreArray($(Vec v), &temp);
--      return e2;
--       }
--    |]


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

vecGetArray1' :: Vec -> IO (Ptr PetscScalar_, CInt)
vecGetArray1' v = withPtr $ \p -> vecGetArray0' v p

-- vecGetArray2' :: Vec -> IO (Ptr PetscScalar_, CInt)
-- vecGetArray2' v = withPtr $ \p -> vga v p where
--   vga v p = [C.exp|int{VecGetArray($(Vec v), $(PetscScalar** p))}|]



vectorFromC :: Storable a => Int -> Ptr a -> IO (V.Vector a)
vectorFromC len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.freeze $ VM.unsafeFromForeignPtr0 ptr' len

  


-- withMutV :: (PrimMonad m, Storable a, Storable b) =>
--  V.Vector a ->
--  ( VM.MVector (PrimState m) a -> VM.MVector (PrimState m) b ) ->
--  m (V.Vector b )
-- withMutV v f = do
--   a <- V.thaw v
--   V.freeze $ f a 



vectorToC0 :: Storable a => V.Vector a -> Int -> Ptr a -> IO ()
vectorToC0 vec len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.copy (VM.unsafeFromForeignPtr0 ptr' len) vec


vectorToC :: Storable a => V.Vector a -> Ptr a -> IO ()
vectorToC vec ptr = do
  p <- newForeignPtr_ ptr
  V.copy (VM.unsafeFromForeignPtr0 p len) vec
   where
     len = V.length vec




funIO :: (Storable a, Storable b) =>
         (V.Vector a -> V.Vector b) ->
         Int -> Ptr a -> Ptr b ->
         IO ()
funIO fun dim y f = do
--         -- Convert the pointer we get from C (y) to a vector, and then
--         -- apply the user-supplied function.
        fImm <- fun <$> vectorFromC dim y
--         -- Fill in the provided pointer with the resulting vector.
        vectorToC0 fImm dim f





-- PETSC_EXTERN PetscErrorCode VecRestoreArray(Vec,PetscScalar**);
vecRestoreArray0' :: Vec -> Ptr (Ptr PetscScalar_) -> IO CInt
vecRestoreArray0' v pc = [C.exp|int{VecRestoreArray($(Vec v), $(PetscScalar** pc))}|]

vecRestoreArrayPtr' :: Vec -> Ptr PetscScalar_ -> IO CInt
vecRestoreArrayPtr' v c = with c $ \pc -> vecRestoreArray0' v pc

vecRestoreArray' :: Vec -> [PetscScalar_] -> IO CInt
vecRestoreArray' v c = withArray c $ \cp ->
  with cp $ \cpp -> vecRestoreArray0' v cpp

-- vecRestoreArrayPtr2' :: Vec -> Ptr PetscScalar_ -> IO CInt
-- vecRestoreArrayPtr2' v c = with c $ \pc -> vra v pc
--   where
--     vra w pc = [C.exp|int{VecRestoreArray($(Vec w), $(PetscScalar** pc))}|]






-- PETSC_EXTERN PetscErrorCode VecRestoreArrayRead(Vec,const PetscScalar**);



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
    



-- | misc. math functions on Vec
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
matSetType' m mt = withCString cs $ \c -> [C.exp|int{MatSetType($(Mat m), $(char *c))}|] 
  where cs = matTypeToStr mt




-- matCreate' c p = [C.exp| int{MatCreate($(int c), $(Mat *p))} |]
matCreate0' comm = withPtr $ \p -> [C.exp| int{MatCreate($(int c), $(Mat *p))} |] 
  where c = unComm comm
matCreate' = matCreate0' 

matDestroy0' m = [C.exp|int{MatDestroy($(Mat *m))}|]
matDestroy' m = with m matDestroy0' 


matSetSizes0' :: Mat -> Int -> Int -> Int -> Int -> IO CInt
matSetSizes0' mat mlo nlo m n = [C.exp|int{MatSetSizes($(Mat mat), $(int mloc), $(int nloc),
                                             $(int mc), $(int nc))}|]
  where (mc, nc) = both (m , n) toCInt
        (mloc, nloc) = both (mlo , nlo) toCInt

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


matCreateMPIAIJWithArrays0' comm m n mm nn ip jp aap =
  withPtr ( \mat -> [C.exp|int{MatCreateMPIAIJWithArrays($(PetscInt c),
                                    $(PetscInt m),
                                    $(PetscInt n),
                                    $(PetscInt mm), $(PetscInt nn),
                                    $(PetscInt* ip), $(PetscInt* jp),
                                    $(PetscScalar* aap), 
                                    $(Mat* mat))}|] )
    where c = unComm comm 

        
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



matViewStdout' v = [C.exp|int{MatView($(Mat v), PETSC_VIEWER_STDOUT_SELF)}|]


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



-- PetscErrorCode  MatCreateSeqAIJ(MPI_Comm comm,PetscInt m,PetscInt n,PetscInt nz,const PetscInt nnz[],Mat *A)


-- PetscErrorCode  MatSeqAIJSetPreallocation(Mat B,PetscInt nz,const PetscInt nnz[])
-- -- Collective on MPI_Comm, CSR format
-- -- Input Parameters

-- -- B	- The matrix
-- -- nz	- number of nonzeros per row (same for all rows)
-- -- nnz	- array containing the number of nonzeros in the various rows (possibly different for each row) or NULL
-- -- -- NB : If nnz is given then nz is ignored

matSeqAIJSetPreallocation' mat nz nnz =
    withArray nnz ( \nnzp ->
                     [C.exp|int{MatSeqAIJSetPreallocation( $(Mat mat),
                                                           $(int nz),
                                                           $(int *nnzp))} |]
                  ) 


-- PetscErrorCode  MatMPIAIJSetPreallocation(Mat B,PetscInt d_nz,const PetscInt d_nnz[],PetscInt o_nz,const PetscInt o_nnz[])  -- Collective on MPI_Comm
-- Input Parameters :
-- B	- the matrix
-- d_nz	- number of nonzeros per row in DIAGONAL portion of local submatrix (same value is used for all local rows)
-- d_nnz	- array containing the number of nonzeros in the various rows of the DIAGONAL portion of the local submatrix (possibly different for each row) or NULL (PETSC_NULL_INTEGER in Fortran), if d_nz is used to specify the nonzero structure. The size of this array is equal to the number of local rows, i.e 'm'. For matrices that will be factored, you must leave room for (and set) the diagonal entry even if it is zero.
-- o_nz	- number of nonzeros per row in the OFF-DIAGONAL portion of local submatrix (same value is used for all local rows).
-- o_nnz	- array containing the number of nonzeros in the various rows of the OFF-DIAGONAL portion of the local submatrix (possibly different for each row) or NULL (PETSC_NULL_INTEGER in Fortran), if o_nz is used to specify the nonzero structure. The size of this array is equal to the number of local rows, i.e 'm'.
-- If the *_nnz parameter is given then the *_nz parameter is ignored
matMPIAIJSetPreallocation' b dnz dnnz onz onnz =
  withArray dnnz $ \dnnzp ->
  withArray onnz $ \onnzp ->
  [C.exp|int{MatMPIAIJSetPreallocation($(Mat b),
                                       $(int dnz),
                                       $(int* dnnzp),
                                       $(int onz),
                                       $(int* onnzp))}|]

matMPIAIJSetPreallocationConstNZPR' b dnz onz =
  [C.exp|int{MatMPIAIJSetPreallocation($(Mat b),
                                       $(int dnz),
                                       NULL,
                                       $(int onz),
                                       NULL)}|]


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

-- If you create the matrix yourself (that is not with a call to DMCreateMatrix()) then you MUST call MatXXXXSetPreallocation() or MatSetUp() _before_ using this routine
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





-- PetscErrorCode  MatSetBlockSize(Mat mat,PetscInt bs)
matSetBlockSize' mat bs = [C.exp|int{MatSetBlockSize($(Mat mat),$(int bsc))}|] where
  bsc = toCInt bs
-- Logically Collective on Mat
-- Input Parameters :
-- mat	- the matrix
-- bs	- block size
-- Notes :
-- Block row formats are MATSEQBAIJ, MATMPIBAIJ, MATSEQSBAIJ, MATMPISBAIJ. These formats ALWAYS have square block storage in the matrix.
-- This must be called before MatSetUp() or MatXXXSetPreallocation() (or will default to 1) and the block size cannot be changed later

-- PetscErrorCode  MatSetValuesBlocked(Mat mat,PetscInt m,const PetscInt idxm[],PetscInt n,const PetscInt idxn[],const PetscScalar v[],InsertMode addv)
matSetValuesBlocked0' mat m idxm n idxn v imode =
  [C.exp|int{MatSetValuesBlocked($(Mat mat),
                                 $(int m),
                                 $(int* idxm),
                                 $(int n),
                                 $(int* idxn),
                                 $(PetscScalar* v),
                                 $(int imm))}|]
  where imm = fromIntegral $ insertModeToInt imode
        
-- Not Collective
-- Input Parameters :
-- mat	- the matrix
-- v	- a logically two-dimensional array of values
-- m, idxm	- the number of block rows and their global block indices
-- n, idxn	- the number of block columns and their global block indices
-- addv	- either ADD_VALUES or INSERT_VALUES, where ADD_VALUES adds values to any existing entries, and INSERT_VALUES replaces existing entries with new values
-- Notes :
-- If you create the matrix yourself (that is not with a call to DMCreateMatrix()) then you MUST call MatXXXXSetPreallocation() or MatSetUp() before using this routine.
-- The m and n count the NUMBER of blocks in the row direction and column direction, NOT the total number of rows/columns; for example, if the block size is 2 and you are passing in values for rows 2,3,4,5 then m would be 2 (not 4). The values in idxm would be 1 2; that is the first index for each block divided by the block size.
-- Note that you must call MatSetBlockSize() when constructing this matrix (before preallocating it).
-- By default the values, v, are row-oriented, so the layout of v is the same as for MatSetValues(). See MatSetOption() for other options.
-- Calls to MatSetValuesBlocked() with the INSERT_VALUES and ADD_VALUES options cannot be mixed without intervening calls to the assembly routines.
-- MatSetValuesBlocked() uses 0-based row and column numbers in Fortran as well as in C.
-- Negative indices may be passed in idxm and idxn, these rows and columns are simply ignored. This allows easily inserting element stiffness matrices with homogeneous Dirchlet boundary conditions that you don't want represented in the matrix.
-- Each time an entry is set within a sparse matrix via MatSetValues(), internal searching must be done to determine where to place the the data in the matrix storage space. By instead inserting blocks of entries via MatSetValuesBlocked(), the overhead of matrix assembly is reduced.








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




-- PetscErrorCode  MatLUFactor(Mat mat,IS row,IS col,const MatFactorInfo *info)
matLUFactor' mat is col info =
  [C.exp|int{MatLUFactor($(Mat mat),$(IS is),$(IS col),$(MatFactorInfo* info))}|]

-- PetscErrorCode  MatFactorInfoInitialize(MatFactorInfo *info)
matFactorInfoInitialize' = withPtr $ \i -> mfii i where
  mfii info = [C.exp|int{MatFactorInfoInitialize($(MatFactorInfo* info))}|]





  

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

-- -- DMCreate* are for setting up longer-lived data
-- -- DMGet* and DMRestore* are for temporary access (always go in pairs)

-- PETSC_EXTERN PetscErrorCode DMCreateGlobalVector(DM,Vec*);
dmCreateGlobalVector' dm = withPtr ( \v -> [C.exp|int{DMCreateGlobalVector($(DM dm), $(Vec* v))}|]) 

-- PETSC_EXTERN PetscErrorCode DMCreateLocalVector(DM,Vec*);
dmCreateLocalVector' dm = withPtr ( \v -> [C.exp|int{DMCreateLocalVector($(DM dm), $(Vec* v))}|]) 


-- PETSC_EXTERN PetscErrorCode DMGetLocalVector(DM,Vec *);
-- The vector values are NOT initialized and may have garbage in them, so you may need to zero them.
-- The output parameter, g, is a regular PETSc vector that should be returned with DMRestoreLocalVector() DO NOT call VecDestroy() on it.
-- This is intended to be used for vectors you need for a short time, like within a single function call. For vectors that you intend to keep around (for example in a C struct) or pass around large parts of your code you should use DMCreateLocalVector().
-- VecStride*() operations can be useful when using DM with dof > 1

dmGetLocalVector' dm = withPtr ( \v -> [C.exp|int{DMGetLocalVector($(DM dm),$(Vec* v))}|]) 

-- PETSC_EXTERN PetscErrorCode DMRestoreLocalVector(DM,Vec *);
dmRestoreLocalVector' dm vv = with vv ( \v -> [C.exp|int{DMRestoreLocalVector($(DM dm),$(Vec* v))}|]) 


-- PETSC_EXTERN PetscErrorCode DMGetGlobalVector(DM,Vec *);
dmGetGlobalVector' dm = withPtr ( \v -> [C.exp|int{DMGetGlobalVector($(DM dm),$(Vec* v))}|])

-- PETSC_EXTERN PetscErrorCode DMRestoreGlobalVector(DM,Vec *);
dmRestoreGlobalVector' dm vv = with vv ( \v -> [C.exp|int{DMRestoreGlobalVector($(DM dm),$(Vec* v))}|]) 



dmCreateMatrix' dm mat = [C.exp|int{DMCreateMatrix($(DM dm),$(Mat* mat))}|]
dmCreateMatrix dm = withPtr (dmCreateMatrix' dm) 



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



-- | DMDASetDim removed from 3.6.2

-- -- PETSC_EXTERN PetscErrorCode DMDASetDim(DM,PetscInt);
-- dmdaSetDim' dm d = [C.exp|int{DMDASetDim($(DM dm), $(PetscInt d))}|] 




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

dmdaCreate1d0' comm bx m dof s =
   withPtr ( \ dm -> [C.exp|int{DMDACreate1d($(int c),
                                              $(int bxe),
                                              $(PetscInt m),
                                              $(PetscInt dof),
                                              $(PetscInt s),
                                              NULL,
                                              $(DM* dm))}|]  )
  where c = unComm comm
        bxe = toEnum $ dmBoundaryTypeToInt bx

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

dmdaCreate2d' comm bx by sten mm nn dof s  =
    withPtr ( \dm -> [C.exp|int{DMDACreate2d($(int c),
                          $(int bxe),
                          $(int bye),
                          $(int stene),
                          $(PetscInt mm),
                          $(PetscInt nn),
                          PETSC_DECIDE, 
                          PETSC_DECIDE,
                          $(PetscInt dof),
                          $(PetscInt s),
                          NULL,
                          NULL,
                          $(DM* dm))}|] ) 
  where c = unComm comm
        bxe = toEnum $ dmBoundaryTypeToInt bx
        bye = toEnum $ dmBoundaryTypeToInt by
        stene = toEnum $ dmdaStencilTypeToInt sten
        
-- dmdaCreate2d' c bx by sten mm nn dof s =
--   dmdaCreate2d0' c bx by sten mm nn petscDecide petscDecide dof s [] []



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
-- Logically collective on Vec
-- Input Parameters : 
-- da	- the distributed array
-- vec	- the vector, either a vector the same size as one obtained with DMCreateGlobalVector() or DMCreateLocalVector()
-- Output Parameter :
-- array -the array 
-- Notes :
-- Call DMDAVecRestoreArray() once you have finished accessing the vector entries.
-- In C, the indexing is "backwards" from what expects: array[k][j][i] NOT array[i][j][k]!

-- If vec is a local vector (obtained with DMCreateLocalVector() etc) then the ghost point locations are accessible. If it is a global vector then the ghost points are not accessible. Of course with the local vector you will have had to do the

-- appropriate DMGlobalToLocalBegin() and DMGlobalToLocalEnd() to have correct values in the ghost locations.

-- dmdaVecGetArray' :: DM -> Vec -> Ptr PetscScalar_ -> IO CInt
dmdaVecGetArray' dm v = withPtr $ \vvp -> 
  [C.exp|int{DMDAVecGetArray($(DM dm),
                             $(Vec v),
                             $(PetscScalar** vvp))}|]



-- PetscErrorCode  DMDARestoreArray(DM da,PetscBool ghosted,void *vptr)
dmdaRestoreArray' dm ghosted vptr = withArray vptr ( \vp -> 
  [C.exp|int{DMDARestoreArray($(DM dm),
                              $(PetscBool ghosted),
                              $(PetscScalar* vp))}|] ) 

-- PetscErrorCode  DMDAVecRestoreArray(DM da,Vec vec,void *array)
dmdaVecRestoreArray' dm v arr_ =
  [C.exp|int{DMDAVecRestoreArray($(DM dm), $(Vec v), $(PetscScalar* arr_))}|]




-- PetscErrorCode  DMGlobalToLocalBegin(DM dm,Vec g,InsertMode mode,Vec l)
-- Neighbor-wise Collective on DM
-- Input Parameters : 
-- dm	- the DM object
-- g	- the global vector
-- mode	- INSERT_VALUES or ADD_VALUES
-- l	- the local vector
dmGlobalToLocalBegin' dm g mode l = [C.exp|int{DMGlobalToLocalBegin($(DM dm),$(Vec g),$(int imode),$(Vec l))}|] where
  imode = toCInt $ insertModeToInt mode


-- PetscErrorCode  DMGlobalToLocalEnd(DM dm,Vec g,InsertMode mode,Vec l)
dmGlobalToLocalEnd' dm g mode l = [C.exp|int{DMGlobalToLocalEnd($(DM dm),$(Vec g),$(int imode),$(Vec l))}|] where
  imode = toCInt $ insertModeToInt mode



-- PetscErrorCode  DMLocalToGlobalBegin(DM dm,Vec l,InsertMode mode,Vec g)
dmLocalToGlobalBegin' dm locv im globv = [C.exp|int{DMLocalToGlobalBegin($(DM dm),$(Vec locv),$(int imode),$(Vec globv))}|]
  where imode = toCInt $ insertModeToInt im
-- Neighbor-wise Collective on DM
-- Input Parameters :
-- dm	- the DM object
-- l	- the local vector
-- mode	- if INSERT_VALUES then no parallel communication is used, if ADD_VALUES then all ghost points from the same base point accumulate into that base point.
-- g	- the global vector
-- Notes: In the ADD_VALUES case you normally would zero the receiving vector before beginning this operation. INSERT_VALUES is not supported for DMDA, in that case simply compute the values directly into a global vector instead of a local one.


dmLocalToGlobalEnd' dm locv im globv = [C.exp|int{DMLocalToGlobalEnd($(DM dm),$(Vec locv),$(int imode),$(Vec globv))}|]
  where imode = toCInt $ insertModeToInt im


-- PetscErrorCode DMDASNESSetFunctionLocal(DM dm,InsertMode imode,PetscErrorCode (*func)(DMDALocalInfo*,void*,void*,void*),void *ctx)  -- Logically Collective
-- Input Arguments :
-- dm	- DM to associate callback with
-- imode	- INSERT_VALUES if local function computes owned part, ADD_VALUES if it contributes to ghosted part
-- func	- local residual evaluation
-- ctx	- optional context for local residual evaluation
-- Calling sequence :
-- For PetscErrorCode (*func)(DMDALocalInfo *info,void *x, void *f, void *ctx),
-- info	- DMDALocalInfo defining the subdomain to evaluate the residual on
-- x	- dimensional pointer to state at which to evaluate residual (e.g. PetscScalar *x or **x or ***x)
-- f	- dimensional pointer to residual, write the residual here (e.g. PetscScalar *f or **f or ***f)
-- ctx	- optional context passed above





dmdaSnesSetFunctionLocal' ::
  DM ->
  InsertMode_ ->
  (DMDALocalInfo -> Ptr PetscScalar_ -> Ptr PetscScalar_ -> Ptr () -> IO CInt) ->
  Ptr () ->
  IO CInt
dmdaSnesSetFunctionLocal' dm imode f ctx =
  [C.exp|
    int{DMDASNESSetFunctionLocal(
           $(DM dm),
           $(int imo),
           $fun:(int (*f)(DMDALocalInfo,PetscScalar*,PetscScalar*,void*)),
                 $(void* ctx)) }
        |]
     where imo = toCInt $ insertModeToInt imode


-- snesSetFunction0' snes r f ctx =
--   [C.exp|int{SNESSetFunction($(SNES snes), $(Vec r),
--                              $fun:(int (*f)(SNES, Vec, void*) ),
--                              $(void* ctx))}|]




{-
PetscErrorCode  DMDAGetInfo(DM da,PetscInt *dim,PetscInt *M,PetscInt *N,PetscInt *P,PetscInt *m,PetscInt *n,PetscInt *p,PetscInt *dof,PetscInt *s,DMBoundaryType *bx,DMBoundaryType *by,DMBoundaryType *bz,DMDAStencilType *st) -- Not Collective
Input Parameter :
da -the distributed array 
Output Parameters :
dim	- dimension of the distributed array (1, 2, or 3)
M, N, P	- global dimension in each direction of the array
m, n, p	- corresponding number of procs in each dimension
dof	- number of degrees of freedom per node
s	- stencil width
bx,by,bz	- type of ghost nodes at boundary, one of DM_BOUNDARY_NONE, DM_BOUNDARY_GHOSTED, DM_BOUNDARY_MIRROR, DM_BOUNDARY_PERIODIC
st	- stencil type, either DMDA_STENCIL_STAR or DMDA_STENCIL_BOX
Note :
Use NULL (NULL_INTEGER in Fortran) in place of any output parameter that is not of interest.
-}

dmdaGetInfo_' da dim mm nn pp m n p dof s bxp byp bzp stp =
  [C.exp|
   int{DMDAGetInfo($(DM da), $(PetscInt* dim), $(PetscInt* mm), $(PetscInt* nn), $(PetscInt* pp), $(PetscInt* m), $(PetscInt* n), $(PetscInt* p), $(PetscInt* dof), $(PetscInt* s), $(int* bxp), $(int* byp), $(int* bzp), $(int* stp))} |]

dmdaGetInfo__' da = withPtr ( \dim ->
  withPtr $ \mm ->
  withPtr $ \nn ->
  withPtr $ \pp ->
  withPtr $ \m ->
  withPtr $ \n ->
  withPtr $ \p ->
  withPtr $ \dof ->
  withPtr $ \s ->
  withPtr $ \bxp ->
  withPtr $ \byp ->
  withPtr $ \bzp ->
  withPtr $ \stp ->
   dmdaGetInfo_' da dim mm nn pp m n p dof s bxp byp bzp stp ) >>=
    \(a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,(m_,err))))))))))))) ->
     return ((a,(b,c,d),(e,f,g),h,i,(j,k,l),m_), err )
   
-- dmdaGetInfo' da dim mm nn pp m n p dof s bx by bz st =
--   dmdaGetInfo_' da dim mm nn pp m n p dof s bxp byp bzp stp
--   where
--     bxp = toCInt $ dmBoundaryTypeToInt bx
--     byp = toCInt $ dmBoundaryTypeToInt by
--     bzp = toCInt $ dmBoundaryTypeToInt bz
--     stp = toCInt $ dmdaStencilTypeToInt st



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


-- | viewing DM

-- PetscErrorCode  DMView(DM dm,PetscViewer v)
dmView' dm vi = [C.exp|int{DMView($(DM dm),$(PetscViewer vi))}|]


















-- * KSP






kspGetConvergedReason' ksp =
  withPtr ( \r ->
             [C.exp| int{ KSPGetConvergedReason( $(KSP ksp),
                                              $(int* r) ) } |]
          ) 



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

-- PETSC_EXTERN PetscErrorCode KSPGetIterationNumber(KSP,PetscInt*);
kspGetIterationNumber' ksp = withPtr ( \v -> [C.exp|int{KSPGetIterationNumber($(KSP ksp), $(int *v))}|] ) 

-- PETSC_EXTERN PetscErrorCode KSPSetNullSpace(KSP,MatNullSpace);
-- PETSC_EXTERN PetscErrorCode KSPGetNullSpace(KSP,MatNullSpace*);
-- PETSC_EXTERN PetscErrorCode KSPGetVecs(KSP,PetscInt,Vec**,PetscInt,Vec**);


{-
PetscErrorCode KSPSetComputeRHS(KSP ksp,PetscErrorCode (*func)(KSP,Vec,void*),void *ctx)    --- Logically Collective :
Input Arguments :
ksp	- the KSP context
func	- function to compute the right hand side
ctx	- optional context
Calling sequence of func :
 func(KSP ksp,Vec b,void *ctx)
ksp	- the KSP context
b	- right hand side of linear system
ctx	- optional user-provided context
-}
kspSetComputeRHS_' :: KSP -> (KSP -> Vec -> Ptr () -> IO CInt) -> IO CInt
kspSetComputeRHS_' ksp f =
  [C.exp|int{KSPSetComputeRHS($(KSP ksp),$fun:(int (* f)(KSP, Vec, void*)), NULL  )}|]

kspSetComputeRHS__' :: KSP -> (KSP -> Vec -> IO CInt) -> IO CInt
kspSetComputeRHS__' ksp f = kspSetComputeRHS_' ksp g where
  g k v _ = f k v


-- PetscErrorCode  KSPReasonView(KSP ksp,PetscViewer viewer)
-- kspReasonView ksp = [C.exp|int{KSPReasonView($(KSP ksp), PETSC_VIEWER_STDOUT_SELF)}|] >>= handleErr














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

pfSet1' pf apply applyvec viewf destroyf =
  pfSet0nc' pf f1 f2 f3 f4 where
    f1 _ = apply
    f2 _ = applyvec
    f3 _ = viewf
    f4 _ = destroyf
    -- apply' :: PetscInt_ -> [PetscScalar_] -> [PetscScalar_] -> IO CInt
    -- apply' a arr1 arr2 = withArray arr1 $  \arrp1 ->
    --   withArray arr2 $ \arrp2 ->
    --     f1 a a arrp1 arrp2


pfSetArr0' ::
  PF ->
  (Ptr () -> PetscInt_ -> Ptr PetscScalar_ -> Ptr PetscScalar_ -> IO CInt ) ->
  IO CInt
pfSetArr0' pf apply =
  [C.exp|int{PFSet($(PF pf),
                   $fun:(int(*apply)(void*,PetscInt,PetscScalar*,PetscScalar*)),
                   NULL,
                   NULL,
                   NULL,
                   NULL   )}  |]

-- pfsa1 ::
--   PF ->
--   (PetscInt_ -> Ptr PetscScalar_ -> Ptr PetscScalar_ -> IO CInt) ->
--   IO CInt
pfsa1 pf f =
  pfSetArr0' pf fm where
    fm _ = f


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
                             $fun:(int (*f)(SNES, Vec, Vec, void*) ),
                             $(void* ctx))}|]
snesSetFunction_' snes r f =
  [C.exp|int{SNESSetFunction($(SNES snes), $(Vec r),
                             $fun:(int (*f)(SNES, Vec, Vec, void*) ),
                             NULL )}|]

-- snesSetFunction' :: SNES -> Vec -> (SNES -> Vec -> IO CInt) -> IO CInt
-- snesSetFunction' snes v f =
--   snesSetFunction_' snes v f' where
--     f' s a _ = f s a



    


-- PETSC_EXTERN PetscErrorCode SNESDestroy(SNES*);
snesDestroy' p = with p $ \pp -> [C.exp| int{SNESDestroy($(SNES *pp))}  |]


-- PETSC_EXTERN PetscErrorCode SNESSetUp(SNES);
snesSetUp' s = [C.exp|int{SNESSetUp($(SNES s))}|] 

-- PETSC_EXTERN PetscErrorCode SNESSolve(SNES,Vec,Vec);
snesSolve' s b x = [C.exp|int{SNESSolve($(SNES s), $(Vec b), $(Vec x))}|]

snesSolve0' s x = [C.exp|int{SNESSolve($(SNES s), NULL, $(Vec x))}|]



-- PETSC_EXTERN PetscErrorCode SNESGetSolution(SNES,Vec*);
snesGetSolution' s = withPtr ( \v ->
  [C.exp|int{SNESGetSolution($(SNES s), $(Vec *v))}|] ) 

snesGetConvergedReason' s =  withPtr ( \v ->
  [C.exp|int{SNESGetConvergedReason($(SNES s), $(int* v))}|] ) 






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

snesSetJacobian_' ::
  SNES -> Mat -> Mat -> (SNES -> Vec -> Mat -> Mat -> IO CInt) -> IO CInt
snesSetJacobian_' snes amat pmat f =
  snesSetJacobian0_' snes amat pmat f' where
    f' s v a p _ = f s v a p 


-- -- monomorphic SNESSetJacobian : see e.g. www.mcs.anl.gov/petsc/petsc-current/src/snes/examples/tutorials/ex5s.c.html
-- -- usage : SNESSetJacobian(snes,J,J,SNESComputeJacobianDefaultColor,fdcoloring);
snesSetJacobian0mono' snes amat pmat f col =
  [C.exp|int{SNESSetJacobian($(SNES snes),$(Mat amat),$(Mat pmat),
                             $fun:(int (*f)(SNES,Vec,Mat,Mat,MatFDColoring*)),
                             $(MatFDColoring col))}|]




-- | compute Jacobian by FD
-- PetscErrorCode  SNESComputeJacobianDefault(SNES snes,Vec x1,Mat J,Mat B,void *ctx)
-- Collective on SNES
-- Input Parameters :
-- x1	- compute Jacobian at this point
-- ctx	- application's function context, as set with SNESSetFunction()
-- Output Parameters :
-- J	- Jacobian matrix (not altered in this routine)
-- B	- newly computed Jacobian matrix to use with preconditioner (generally the same as J)
-- Options Database Key :
-- -snes_fd	- Activates SNESComputeJacobianDefault()
-- -snes_test_err	- Square root of function error tolerance, default square root of machine epsilon (1.e-8 in double, 3.e-4 in single)
-- -mat_fd_type	- Either wp or ds (see MATMFFD_WP or MATMFFD_DS)

snesComputeJacobianDefault0' snes x1 jj bb ctx =
  [C.exp|int{SNESComputeJacobianDefault($(SNES snes),$(Vec x1),$(Mat jj),$(Mat bb),$(void* ctx))}|]



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



-- PetscErrorCode  SNESLineSearchSetPostCheck(SNESLineSearch linesearch, PetscErrorCode (*func)(SNESLineSearch,Vec,Vec,Vec,PetscBool*,PetscBool*,void*),void *ctx)
-- Sets a user function that is called after the line search has been applied to determine the step direction and length. Allows the user a chance to change or override the decision of the line search routine  -- Logically Collective on SNESLineSearch
-- Input Parameters :
-- linesearch	- the SNESLineSearch context
-- func	- [optional] function evaluation routine, see SNESLineSearchPostCheckFunction for the calling sequence
-- ctx	- [optional] user-defined context for private data for the function evaluation routine (may be NULL)

snesLineSearchSetPostCheck' snesls f ctx =
  [C.exp|
   int{SNESLineSearchSetPostCheck(
          $(SNESLineSearch snesls),
          $fun:(int(*f)(SNESLineSearch,Vec,Vec,Vec,PetscBool*,PetscBool*,void*)),
          $(void* ctx))}|]

snesLineSearchSetPostCheck0' snesls f =
  [C.exp|
   int{SNESLineSearchSetPostCheck(
          $(SNESLineSearch snesls),
          $fun:(int(*f)(SNESLineSearch,Vec,Vec,Vec,PetscBool*,PetscBool*,void*)),
          NULL)}|]

-- syntax of callback : 
-- SNESLineSearchPostheckFunction(SNESLineSearch linesearch,Vec x,Vec y,  Vec w, *changed_y, PetscBool *changed_w);
-- Input Parameters

-- x	- old solution vector
-- y	- search direction vector
-- w	- new solution vector
-- changed_y	- indicates that the line search changed y
-- changed_w	- indicates that the line search changed w


                   -- $fun:(int(*apply)(void*,PetscInt,PetscScalar*,PetscScalar*)),














-- * TS




-- PetscErrorCode  TSCreate(MPI_Comm comm, TS *ts)
tsCreate' comm =
  withPtr (\ts -> [C.exp|int{TSCreate($(int c), $(TS* ts))}|]) 
  where
   c = unComm comm

tsDestroy0' ts = [C.exp| int{TSDestroy($(TS* ts))} |] 
tsDestroy' ts = with ts tsDestroy0'


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






-- PetscErrorCode  TSSetIFunction(TS ts,Vec res,TSIFunction f,void *ctx)

-- Set the function to compute F(t,U,U_t) where F() = 0 is the DAE to be solved.
-- Logically Collective on TS
-- Input Parameters :
-- ts	- the TS context obtained from TSCreate()
-- r	- vector to hold the residual (or NULL to have it created internally)
-- f	- the function evaluation routine
-- ctx	- user-defined context for private data for the function evaluation routine (may be NULL)
-- Calling sequence of f :
--  f(TS ts,PetscReal t,Vec u,Vec u_t,Vec F,ctx);
-- t	- time at step/stage being solved
-- u	- state vector
-- u_t	- time derivative of state vector
-- F	- function vector
-- ctx	- [optional] user-defined context for matrix evaluation routine
-- Important :
-- The user MUST call either this routine or TSSetRHSFunction() to define the ODE. When solving DAEs you must use this function.

tsSetIFunction0' ts res f =
  [C.exp|int{TSSetIFunction($(TS ts),
                            $(Vec res),
                            $fun:(int (*f)(TS,PetscReal,Vec,Vec,Vec,void*)),
                            NULL)}|]

tsSetIFunction' ts res f ctx =
  [C.exp|int{TSSetIFunction($(TS ts),
                            $(Vec res),
                            $fun:(int (*f)(TS,PetscReal,Vec,Vec,Vec,void*)),
                            $(void* ctx))}|]


  
-- PetscErrorCode  TSSetIJacobian(TS ts,Mat Amat,Mat Pmat,TSIJacobian f,void *ctx)

tsSetIJacobian0' ts amat pmat f =
  [C.exp|int{TSSetIJacobian($(TS ts),
                     $(Mat amat),
                     $(Mat pmat),
                     $fun:(int (*f)(TS,PetscReal,Vec,Vec,PetscReal,Mat,Mat,void*)), NULL)}|]

tsSetIJacobian' ts amat pmat f ctx =
  [C.exp|int{TSSetIJacobian($(TS ts),$(Mat amat),$(Mat pmat),$fun:(int (*f)(TS,PetscReal,Vec,Vec,PetscReal,Mat,Mat,void*)),$(void* ctx))}|]

  
-- Logically Collective on TS
-- Input Parameters :
-- ts	- the TS context obtained from TSCreate()
-- Amat	- (approximate) Jacobian matrix
-- Pmat	- matrix used to compute preconditioner (usually the same as Amat)
-- f	- the Jacobian evaluation routine
-- ctx	- user-defined context for private data for the Jacobian evaluation routine (may be NULL)
-- Calling sequence of f :
--  f(TS ts,PetscReal t,Vec U,Vec U_t,PetscReal a,Mat Amat,Mat Pmat,void *ctx);
-- t	- time at step/stage being solved
-- U	- state vector
-- U_t	- time derivative of state vector
-- a	- shift
-- Amat	- (approximate) Jacobian of F(t,U,W+a*U), equivalent to dF/dU + a*dF/dU_t
-- Pmat	- matrix used for constructing preconditioner, usually the same as Amat
-- ctx	- [optional] user-defined context for matrix evaluation routine
-- Notes :
-- The matrices Amat and Pmat are exactly the matrices that are used by SNES for the nonlinear solve.
-- If you know the operator Amat has a null space you can use MatSetNullSpace() and MatSetTransposeNullSpace() to supply the null space to Amat and the KSP solvers will automatically use that null space as needed during the solution process.
-- The matrix dF/dU + a*dF/dU_t you provide turns out to be the Jacobian of F(t,U,W+a*U) where F(t,U,U_t) = 0 is the DAE to be solved. The time integrator internally approximates U_t by W+a*U where the positive "shift" a and vector W depend on the integration method, step size, and past states. For example with the backward Euler method a = 1/dt and W = -a*U(previous timestep) so W + a*U = a*(U - U(previous timestep)) = (U - U(previous timestep))/dt




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
tsSetRHSFunction0' ts r f =
  [C.exp|int{TSSetRHSFunction(
                $(TS ts),
                $(Vec r),
                $fun:(int (*f)(TS, PetscReal,Vec,Vec,void*)),
                NULL)}
        |]

tsSetRHSFunction' ts r f ctx =
  [C.exp|int{TSSetRHSFunction(
                $(TS ts),
                $(Vec r),
                $fun:(int (*f)(TS, PetscReal,Vec,Vec,void*)),
                $(void* ctx))}
        |]

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

tsSetRHSJacobian0' ts amat pmat f =
  [C.exp|int{TSSetRHSJacobian($(TS ts),$(Mat amat),$(Mat pmat),$fun:(int(*f)(TS, PetscReal, Vec, Mat, Mat, void*)),NULL)}|]

tsSetRHSJacobian' ts amat pmat f ctx =
  [C.exp|int{TSSetRHSJacobian($(TS ts),$(Mat amat),$(Mat pmat),$fun:(int(*f)(TS, PetscReal, Vec, Mat, Mat, void*)),$(void* ctx))}|]


-- PetscErrorCode  TSSetDM(TS ts,DM dm)
-- Sets the DM that may be used by some preconditioners
-- Logically Collective on TS and DM
-- Input Parameters :
-- ts	- the preconditioner context
-- dm	- the dm

tsSetDm' ts dm = [C.exp|int{TSSetDM($(TS ts),$(DM dm))}|]




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



tsStep' ts = [C.exp|int{TSStep($(TS ts))}|]


-- PetscErrorCode  TSGetConvergedReason(TS ts,TSConvergedReason *reason)
tsGetConvergedReason' ts = withPtr (\p -> [C.exp|int{TSGetConvergedReason($(TS ts),$(int* p))}|])

--    TSGetTimeStepNumber(ts,&steps);

--    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
--       View timestepping solver info
--       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

--    TSView(ts,PETSC_VIEWER_STDOUT_SELF);
tsViewStdout' ts =
  [C.exp|int{TSView($(TS ts), PETSC_VIEWER_STDOUT_SELF)}|] 





-- | TS adjoint solve ()



-- PetscErrorCode  TSSetCostGradients(TS ts,PetscInt numcost,Vec *lambda,Vec *mu)
-- Sets the initial value of the gradients of the cost function w.r.t. initial conditions and w.r.t. the problem parameters for use by the TSAdjoint routines.
-- Logically Collective on TS and Vec
-- Input Parameters :
-- ts	- the TS context obtained from TSCreate()
-- lambda	- gradients with respect to the initial condition variables, the dimension and parallel layout of these vectors is the same as the ODE solution vector
-- mu	- gradients with respect to the parameters, the number of entries in these vectors is the same as the number of parameters
-- Notes: the entries in these vectors must be correctly initialized with the values
-- -- lamda_i = df/dy|finaltime ; mu_i = df/dp|finaltime

tsSetCostGradients' ts numcost lambda mu =
  [C.exp|int{TSSetCostGradients($(TS ts),$(PetscInt numcost),$(Vec* lambda),$(Vec* mu))}|]



-- PetscErrorCode  TSSetCostIntegrand(TS ts,PetscInt numcost, PetscErrorCode (*rf)(TS,PetscReal,Vec,Vec,void*),
-- PetscErrorCode (*drdyf)(TS,PetscReal,Vec,Vec*,void*),
-- PetscErrorCode (*drdpf)(TS,PetscReal,Vec,Vec*,void*),void *ctx)
tsSetCostIntegrand0' ts n rf drdyf drdpf =
  [C.exp|
   int{TSSetCostIntegrand($(TS ts),
                          $(PetscInt n),
                          $fun:(int (*rf)(TS,PetscReal,Vec,Vec,void*)),
                          $fun:(int (*drdyf)(TS,PetscReal,Vec,Vec*,void*)),
                          $fun:(int (*drdpf)(TS,PetscReal,Vec,Vec*,void*)),NULL)}|]

tsSetCostIntegrand' ts n rf drdyf drdpf ctx =
  [C.exp|
   int{TSSetCostIntegrand($(TS ts),
                          $(PetscInt n),
                          $fun:(int (*rf)(TS,PetscReal,Vec,Vec,void*)),
                          $fun:(int (*drdyf)(TS,PetscReal,Vec,Vec*,void*)),
                          $fun:(int (*drdpf)(TS,PetscReal,Vec,Vec*,void*)),$(void* ctx))}|]  


tsGetCostIntegral' ts = withPtr $ \p -> [C.exp|int{TSGetCostIntegral($(TS ts),$(Vec* p))}|]

-- Logically Collective on TS
-- Input Parameters :
-- ts	- the TS context obtained from TSCreate()
-- numcost	- number of gradients to be computed, this is the number of cost functions
-- rf	- routine for evaluating the integrand function
-- drdyf	- function that computes the gradients of the r's with respect to y,NULL if not a function y
-- drdpf	- function that computes the gradients of the r's with respect to p, NULL if not a function of p
-- ctx	- [optional] user-defined context for private data for the function evaluation routine (may be NULL)
-- Calling sequence of rf :
--     rf(TS ts,PetscReal t,Vec y,Vec f[],void *ctx);
-- t	- current timestep
-- y	- input vector
-- f	- function result; one vector entry for each cost function
-- ctx	- [optional] user-defined function context
-- Calling sequence of drdyf :
--    PetscErroCode drdyf(TS ts,PetscReal t,Vec y,Vec *drdy,void *ctx);
-- Calling sequence of drdpf :
--    PetscErroCode drdpf(TS ts,PetscReal t,Vec y,Vec *drdp,void *ctx);
-- Notes: For optimization there is generally a single cost function, numcost = 1. For sensitivities there may be multiple cost functions




-- PetscErrorCode  TSAdjointSetRHSJacobian(TS ts,Mat Amat,PetscErrorCode (*func)(TS,PetscReal,Vec,Mat,void*),void *ctx)   -- Logically Collective on TS
-- Sets the function that computes the Jacobian of G w.r.t. the parameters p where y_t = G(y,p,t), as well as the location to store the matrix.
-- Synopsis
-- Input Parameters :
-- ts	- The TS context obtained from TSCreate()
-- func	- The function
-- Calling sequence of func :
-- func (TS ts,PetscReal t,Vec y,Mat A,void *ctx);
-- t	- current timestep
-- y	- input vector (current ODE solution)
-- A	- output matrix
-- ctx	- [optional] user-defined function context
-- Notes: Amat has the same number of rows and the same row parallel layout as u, Amat has the same number of columns and parallel layout as p

tsAdjointSetRHSJacobian0' ts amat f =
  [C.exp|int{TSAdjointSetRHSJacobian($(TS ts),
                                     $(Mat amat),
                                     $fun:(int (*f)(TS, PetscReal,Vec, Mat, void*)),
                                     NULL)}|]

tsAdjointSetRHSJacobian' ts amat f ctx =
  [C.exp|int{TSAdjointSetRHSJacobian($(TS ts),
                                     $(Mat amat),
                                     $fun:(int (*f)(TS, PetscReal,Vec, Mat, void*)),
                                     $(void* ctx))}|]
  

-- PetscErrorCode TSAdjointSolve(TS ts)
tsAdjointSolve' ts = [C.exp|int{TSAdjointSolve($(TS ts))}|]




-- | TS Trajectory (state history)

-- PETSC_EXTERN PetscErrorCode TSSetSaveTrajectory(TS);
tsSetSaveTrajectory' ts = [C.exp|int{TSSetSaveTrajectory($(TS ts))}|]

-- PETSC_EXTERN PetscErrorCode TSTrajectoryCreate(MPI_Comm,TSTrajectory*);
tsTrajectoryCreate' comm = withPtr $ \tst ->
  [C.exp|int{TSTrajectoryCreate($(int c),$(TSTrajectory* tst))}|] where
    c = unComm comm
  
-- PETSC_EXTERN PetscErrorCode TSTrajectoryDestroy(TSTrajectory*);
tsTrajectoryDestroy' tst =
  with tst $ \tstp -> [C.exp|int{TSTrajectoryDestroy($(TSTrajectory* tstp))}|]
    
-- PETSC_EXTERN PetscErrorCode TSTrajectorySetType(TSTrajectory,const TSTrajectoryType);

-- PETSC_EXTERN PetscErrorCode TSTrajectorySet(TSTrajectory,TS,PetscInt,PetscReal,Vec);
-- PETSC_EXTERN PetscErrorCode TSTrajectoryGet(TSTrajectory,TS,PetscInt,PetscReal*);
-- PETSC_EXTERN PetscErrorCode TSTrajectorySetFromOptions(TSTrajectory);
-- PETSC_EXTERN PetscErrorCode TSTrajectoryRegisterAll(void);















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



-- PetscErrorCode  PetscViewerSetFormat(PetscViewer viewer,PetscViewerFormat format)
petscViewerSetFormat' v fmt =
  [C.exp|int{PetscViewerSetFormat($(PetscViewer v), $(int e))}|]
    where
      e = petscViewerFormatToCInt fmt






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




-- -- | HDF5 stuff

-- -- PetscErrorCode  PetscViewerHDF5Open(MPI_Comm comm, const char name[], PetscFileMode type, PetscViewer *hdf5v)
-- petscViewerHDF5Open' comm name ty =
--   withPtr $ \f ->
--    withCString name $ \np -> 
--   [C.exp|int{PetscViewerHDF5Open($(int c),$(const char* np),$(int t),$(PetscViewer* f))}|]
--    where
--      c = unComm comm
--      t = toCInt $ viewerTypeToInt ty
    
-- -- -- usage
-- -- 339:   PetscViewerCreate(comm, hdf5v);
-- -- 340:   PetscViewerSetType(*hdf5v, PETSCVIEWERHDF5);
-- -- 341:   PetscViewerFileSetMode(*hdf5v, type);
-- -- 342:   PetscViewerFileSetName(*hdf5v, name);


-- -- PetscErrorCode  PetscViewerHDF5PushGroup(PetscViewer viewer, const char *name)
-- petscViewerHDF5PushGroup' v name = withCString name $ \n -> 
--   [C.exp|int{PetscViewerHDF5PushGroup($(PetscViewer v),$(char* n))}|]

-- -- PetscErrorCode  PetscViewerHDF5PopGroup(PetscViewer viewer)
-- petscViewerHDF5PopGroup' v =
--   [C.exp|int{PetscViewerHDF5PopGroup($(PetscViewer v))}|]












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

withPetsc' :: Argv -> OptsStr -> HelpStr -> IO a -> IO a
withPetsc' a o h = bracket_ (petscInitialize1 a o h) petscFin1

withPetsc'' a o h f = petscInitialize1 a o h >> (f `finally` petscFin1)

-- -- NB : bracket_ ignores the return type of the allocation action







-- | version string

-- -- PetscErrorCode PetscGetVersion(char version[], size_t len)
petscGetVersion0' version szt =
  [C.exp|int{PetscGetVersion($(char* version), $(int szt))}|]







-- | error codes

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
-- mpiCommSize c =  unsafePerformIO $ mpiCommSize' c 


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










-- *











