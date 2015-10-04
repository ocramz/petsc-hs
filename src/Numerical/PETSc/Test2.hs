{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module Numerical.PETSc.Test2
       --(
       --   Vec, Mat, KSP,
       --   vecCreate, vecDestroy, vecSet, vecDot, vecAssembly, vecCreateMPI,
       --   vecSetSizes, vecCreateMPILocal, withVecPipeline, withVecMPIPipeline,
       --   withVec, withMat, withKsp,
       --   withPetsc0, withPetsc, petscInit, petscInit0, petscFin,
       --   petscInitialized, petscFinalized,
       --   commWorld,
       --   petscErrorMessage, petscErrorMessage0,
       --   maxErrorString
       --)
       where

-- import Numerical.PETSc.Types -- for some reason Types is unreachable here, hmm
-- import qualified Test.Hspec as H
import Test.QuickCheck
import Data.List
import Data.Ix
import Data.Functor ((<$>))
import Control.Exception as E
import Control.Concurrent 
import Foreign
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Control.Monad
import Control.Arrow ((***), (&&&))
import Control.Applicative
import Foreign.C.Types
import Foreign.C.String
import qualified Foreign.ForeignPtr.Safe         as FPS

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

import Data.Array.Repa as R
import Data.Array.Repa.Unsafe           as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RFP


import System.IO.Unsafe (unsafePerformIO)

import Language.C.Inline as C

import Numerical.PETSc.Internal2

context petscCtx

C.include "<petscsnes.h>"
C.include "<petsctao.h>"
C.include "<petscdmda.h>"
C.include "<petscts.h>"



petscDecide = -1 -- don't ask




data InsertMode_ = NotSetValues | InsertValues | AddValues | MaxValues | InsertAllValues | AddAllValues | InsertBCValues | AddBCValues deriving (Eq, Enum, Show)

insertModeToInt x = fromEnum (x :: InsertMode_) 


-- * AppCtx










data PetscCopyMode_ =
  PetscCopyVals | PetscOwn | PetscUsePointer deriving (Eq, Show, Enum)
petscCopyModeToInt x = fromEnum (x :: PetscCopyMode_ )

-- * IS

isCreateStride' comm n first step is =
  [C.exp|
     int{ISCreateStride(
            $(int c),
            $(PetscInt n),
            $(PetscInt first),
            $(PetscInt step),
            $(IS is)) }|]
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

isCreateGeneral' comm n idxp mo isp = 
  [C.exp|int{ISCreateGeneral($(int c),
                             $(PetscInt n),
                             $(PetscInt* idxp),
                             $(int mo),
                             $(IS* isp))}|]
  where c = unComm comm

isCreateGeneral comm n idx mode =
  withPtr (\isp ->
   withArray idx $ \idxp -> isCreateGeneral' comm n idxp mo isp) >>= handleErrTup
     where mo = fromIntegral $ petscCopyModeToInt mode


isDestroy iis =
  with iis (\iisp -> [C.exp|int{ISDestroy($(IS* iisp))} |] ) >>= handleErr

withIsCreateGeneral comm n idx mode = bracket (isCreateGeneral comm n idx mode) isDestroy







-- * Vec

newtype Vec' = Vec' (MVar Vec)

vecCreate' comm p = [C.exp|int{VecCreate($(int c), $(Vec *p))} |]
  where c = unComm comm
vecCreate c = withPtr (vecCreate' c) >>= handleErrTup

-- PetscErrorCode VecCreateMPI(MPI_Comm comm, int m, int M, Vec* x)
vecCreateMPI' comm m1' m2' p = [C.exp|int{VecCreateMPI($(int c), $(int m1), $(int m2), $(Vec *p))}|] 
  where c = unComm comm
        m1 = fromIntegral m1'
        m2 = fromIntegral m2'
vecCreateMPI c m1 m2 = withPtr (vecCreateMPI' c m1 m2) >>= handleErrTup

vecCreateMPILocal c m = vecCreateMPI c m m

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

-- vecSetValues' :: Vec -> CInt -> Ptr CInt -> Ptr PetscScalar_ -> IO CInt
vecSetValues' x ni ixx y imm = [C.exp|int{VecSetValues($(Vec x), $(int ni), $(int* ixx), $(PetscScalar* y), $(int im))}|] where im = fromIntegral $ insertModeToInt imm

-- vecSetValues'' x ni ixx y imm =
--   [C.exp|int{VecSetValues($(Vec x), $(int ni), $(int* ixx), $(PetscScalar* y), $(int im))}|] where im = fromIntegral $ insertModeToInt imm

-- vecSetValues :: Vec -> [CInt] -> [PetscScalar_] -> IO ()
vecSetValues x ix y im =
  withArray ix $ \ixx ->
   withArray y $ \yy -> vecSetValues' x ni ixx yy im >>= handleErr
  where
  ni = fromIntegral (length ix)

-- neeeds :
--  consistency (ordered, max belonging to index set of Vec) check
--

vecSetValuesSafe v ix y
  | c1 && c2 = vecSetValues v ix y
  | otherwise = error "vecSetValuesSafe: "
     where
      c1 = length ix == length y
      c2 = a >= 0 && b <= sv where
        ixs = sort ix
        (a, b) = (head ixs, last ixs)
      sv = vecSize v

-- inVecBounds v ix
--   | 

-- PETSC_EXTERN PetscErrorCode VecEqual(Vec,Vec,PetscBool *);
vecEqual v1 v2 = withPtr ( \b ->
  [C.exp|int{VecEqual($(Vec v1), $(Vec v2), $(PetscBool* b))}|] ) >>= handleErrTup

vecDestroy' p = [C.exp|int{VecDestroy($(Vec *p))}|]
vecDestroy p = with p vecDestroy' >>= handleErr


vecCopy vorig vcopy = [C.exp|int{VecCopy($(Vec vorig), $(Vec vcopy))}|] >>= handleErr

-- -- NB : VecDuplicate DOES NOT COPY CONTENTS (only structure): use VecCopy
-- PetscErrorCode  VecDuplicate(Vec v,Vec *newv)
vecDuplicate' p1 p2 = [C.exp| int{VecDuplicate($(Vec p1), $(Vec *p2))}|] 
vecDuplicate v = withPtr (vecDuplicate' v) >>= handleErrTup

withVecDuplicate v0 = bracket (vecDuplicate v0) vecDestroy -- auto-cleanup :)

withVecDuplicateCopy v f = withVecDuplicate v $ \vv -> do
  vecCopy v vv
  f vv


vecAssemblyBegin v = [C.exp|int{VecAssemblyBegin($(Vec v))}|] >>= handleErr
vecAssemblyEnd v = [C.exp|int{VecAssemblyEnd($(Vec v))}|] >>= handleErr

vecAssembly v = vecAssemblyBegin v >> vecAssemblyEnd v

withVecAssembly v f = do
  vecAssemblyBegin v
  f v
  vecAssemblyEnd v

-- vecAssembly' = (,) <$> vecAssemblyBegin <*> vecAssemblyEnd 

withVec :: Comm -> (Vec -> IO a) -> IO a
withVec c = bracket (vecCreate c) vecDestroy

-- withVecPipeline :: Comm -> CInt -> (Vec -> IO a) -> (Vec -> IO c) -> IO c
withVecPipeline c nDim pre f = bracket (vecCreate c) vecDestroy $ \v -> do
  vecSetSizes v (fromIntegral $ abs nDim)
  pre v
  vecAssembly v
  f v

-- withVecMPIPipeline :: Comm -> CInt -> (Vec -> IO a) -> (Vec -> IO c) -> IO c
withVecMPIPipeline c nDim pre post = bracket (vecCreateMPILocal c nDim) vecDestroy $ \v -> do
  pre v
  vecAssembly v
  post v


vecSet v n = [C.exp|int{VecSet( $(Vec v), $(PetscScalar n))}|] >>= handleErr

vecSetSizes v n = [C.exp|int{VecSetSizes( $(Vec v), PETSC_DECIDE, $(int n))}|] >>= handleErr

-- PETSC_EXTERN PetscErrorCode VecGetSize(Vec,PetscInt*);
vecGetSize' v p =  [C.exp|int{VecGetSize($(Vec v), $(int *p))}|]
vecGetSize v = (withPtr $ \p -> vecGetSize' v p) >>= handleErrTup
vecGetSizeUnsafe = unsafePerformIO . vecGetSize

vecSize v = fromIntegral $ vecGetSizeUnsafe v


vecViewStdout v = [C.exp|int{VecView($(Vec v), PETSC_VIEWER_STDOUT_SELF)}|] >>= handleErr


-- PETSC_EXTERN PetscErrorCode VecGetArray(Vec,PetscScalar**);
vecGetArray'' v p =  [C.exp|int{VecGetArray($(Vec v), $(PetscScalar** p))}|]
vecGetArray' v = withPtr ( \p -> [C.exp|int{VecGetArray($(Vec v), $(PetscScalar** p))}|]) >>= handleErrTup
vecGetArray v sz = do
  p <- vecGetArray' v
  peekArray sz p

vecGetArraySafe v = vecGetArray v (vecSize v) 

-- PETSC_EXTERN PetscErrorCode VecGetArrayRead(Vec,const PetscScalar**);


------ Data.Vector ----

vecGetVector v =
  vecGetArray' v >>= newForeignPtr_ >>= \l -> return $ V.unsafeFromForeignPtr0 l n
   where n = vecSize v

-- newForeignPtr_ :: Ptr a -> IO (ForeignPtr a)
-- unsafeFromForeignPtr :: Storable a => ForeignPtr a -> Int -> Int -> Vector a

vecRestoreVector' v ve = with ve (vecRestoreArray'' v)
vecRestoreVector v ve = V.unsafeWith ve (vecRestoreVector' v)

withVecGetVector v = bracket (vecGetVector v) (vecRestoreVector v)

---- end Data.Vector



vecRestoreArray'' v pc = [C.exp|int{VecRestoreArray($(Vec v), $(PetscScalar** pc))}|]

-- PETSC_EXTERN PetscErrorCode VecRestoreArray(Vec,PetscScalar**);
vecRestoreArray' v c = with c (\pc -> [C.exp|int{VecRestoreArray($(Vec v), $(PetscScalar** pc))}|]) >>= handleErr
vecRestoreArray v c = withArray c $ \cp -> vecRestoreArray' v cp

withVecRestoreArray v c f = vecRestoreArray v (f c)

withVecGetArrayUnsafe v modify =
  bracket (vecGetArray v sz) (\c -> withVecRestoreArray v c modify) return
   where sz = vecSize v

withVecGetArrayUnsafe' v f = do
  a <- vecGetArraySafe v
  vecRestoreArray v (f a)
  vecAssembly v

--   pokeArray p c
-- vecRestoreArray v c = vecRestoreArray' v >>= \p -> pokeArray p cA 
-- vecRestoreArray v pc = do
--   p <- vecRestoreArray' v pc
--   pokeArray p c
-- PETSC_EXTERN PetscErrorCode VecRestoreArrayRead(Vec,const PetscScalar**);

-- withVecGetArray :: Vec -> Int -> ([PetscScalar_] -> IO c) -> IO c
withVecGetArray v sz
  | sz >0 =  bracket (vecGetArray v sz) (vecRestoreArray v)
  | otherwise = error "withVecGetArray: temp array size must be nonnegative"

withVecGetArraySafe' v = withVecGetArray v (vecSize v)

withVecGetArraySafe v =
  bracket (vecGetArray v sz) (vecRestoreArray v)  where
   sz = vecSize v

-- withNonNeg a sz f g | sz >0 = bracket (f a sz) (g a)
--                     | otherwise = error "sz must be nonnegative"

-- withCondition a p f g | p = bracket (f a) (g a)
--                       | otherwise = error "p not satisfied"

------ REPA -----
  
-- ptr2repa p sh = do
--     fp <- FPS.newForeignPtr_ p
--     return $ RFP.fromForeignPtr sh fp

vecGetRepa :: Vec -> IO (R.Array RFP.F R.DIM1 PetscScalar_)
vecGetRepa v = do
  fp <- FPS.newForeignPtr_ =<< vecGetArray' v
  vs <- vecGetSize v
  let vsi = fromIntegral vs
  return $ RFP.fromForeignPtr (Z :. vsi) fp

vecRestoreRepa :: Vec -> R.Array RFP.F R.DIM1 PetscScalar_ -> IO ()
vecRestoreRepa v r = withForeignPtr (RFP.toForeignPtr r) (vecRestoreArray' v)

withVecGetRepa v = bracket (vecGetRepa v) (vecRestoreRepa v)

----- end repa -----


-- TODO row (block) indexing : these should not be interpreted as mere Ints but as indices, e.g. FEM mesh nodes -- see repa 

vecGetOwnershipRange' a =
 withPtr $ \rmin -> 
  withPtr $ \rmax ->
   [C.exp|int{VecGetOwnershipRange($(Vec a), $(PetscInt *rmin), $(PetscInt * rmax) )}|] 

vecGetOwnershipRange v = do
  (r1, (r2, e)) <- vecGetOwnershipRange' v
  handleErrTup ((r1, r2), e)
  -- return (r1, r2)





-- -- -- math functions on Vec
vecDot' v1 v2 v = [C.exp|int{VecDot( $(Vec v1), $(Vec v2), $(PetscScalar * v))}|] 
vecDot v1 v2 = withPtr ( \v -> vecDot' v1 v2 v ) >>= handleErrTup

-- typedef enum {NORM_1=0,NORM_2=1,NORM_FROBENIUS=2,NORM_INFINITY=3,NORM_1_AND_2=4} NormType;
data VecNorm_ = VecNorm1 | VecNorm2 | VecNormFrobenius | VecNormInfty | VecNorm1and2 deriving (Eq, Enum, Show)
vecNormToInt x = fromEnum (x :: VecNorm_ ) 

-- PETSC_EXTERN PetscErrorCode VecNorm(Vec,NormType,PetscReal *);
-- vecNorm'' v nt = unsafePerformIO $ withPtr ( \p -> [C.exp|int{VecNorm($(Vec v), $(int nti), $(PetscReal* p))}|] ) >>= handleErrTup where
--   nti = fromIntegral $ vecNormToInt nt
vecNorm' nt v p = [C.exp|int{VecNorm($(Vec v),$(int nti),$(PetscReal* p))}|] where
    nti = fromIntegral $ vecNormToInt nt
vecNorm v nt = unsafePerformIO $ withPtrHandleErr2 vecNorm' nt v
-- PETSC_EXTERN PetscErrorCode VecNormalize(Vec,PetscReal *);
-- vecNormalize v 
-- PETSC_EXTERN PetscErrorCode VecSum(Vec,PetscScalar*);
vecSum' v p = [C.exp|int{VecSum($(Vec v), $(PetscScalar* p))}|]
vecSum v = unsafePerformIO $ withPtrHandleErr1 vecSum' v
-- PETSC_EXTERN PetscErrorCode VecMax(Vec,PetscInt*,PetscReal *);
-- PETSC_EXTERN PetscErrorCode VecMin(Vec,PetscInt*,PetscReal *);
-- PETSC_EXTERN PetscErrorCode VecScale(Vec,PetscScalar);
-- PETSC_EXTERN PetscErrorCode VecPointwiseMax(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecPointwiseMaxAbs(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecPointwiseMin(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecPointwiseMult(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecPointwiseDivide(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecMaxPointwiseDivide(Vec,Vec,PetscReal*);
-- PETSC_EXTERN PetscErrorCode VecShift(Vec,PetscScalar);
-- PETSC_EXTERN PetscErrorCode VecReciprocal(Vec);
-- PETSC_EXTERN PetscErrorCode VecPermute(Vec, IS, PetscBool );
-- PETSC_EXTERN PetscErrorCode VecSqrtAbs(Vec);
-- PETSC_EXTERN PetscErrorCode VecLog(Vec);
vecLog' v = [C.exp|int{VecLog($(Vec v))}|]
-- PETSC_EXTERN PetscErrorCode VecExp(Vec);
vecExp' v = [C.exp|int{VecExp($(Vec v))}|]
-- PETSC_EXTERN PetscErrorCode VecAbs(Vec);
vecAbs' v = [C.exp|int{VecAbs($(Vec v))}|]

withPtrHandleErr1 f a = withPtr (f a) >>= handleErrTup
withPtrHandleErr2 f a b = withPtr (f a b) >>= handleErrTup




















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

data MatType_ = MatSame | MatMaij | MatSeqMaij | MatMPIMaij | MatIs | MatAij
              | MatSeqAij | MatSeqAijPThread | MatAijPthread | MatMPIAij -- etc.
                                                               deriving (Eq, Show)
matTypeToStr MatSame = "same"
matTypeToStr MatIs = "is"
matTypeToStr MatAij = "aij"
matTypeToStr MatMPIAij = "mpiaij"
matTypeToStr _ = "mpiaij" -- default

-- PETSC_EXTERN PetscErrorCode MatSetType(Mat,MatType);
matSetType m mt = withCString cs $ \c -> [C.exp|int{MatSetType($(Mat m), $(char *c))}|] >>= handleErr
  where cs = matTypeToStr mt

data MatStructure_ = DifferentNZPattern | SubsetNZPattern | SameNZPattern deriving (Eq, Enum, Show)
matStructureToInt x = fromEnum (x :: MatStructure_ )


-- matCreate' c p = [C.exp| int{MatCreate($(int c), $(Mat *p))} |]
matCreate' comm = withPtr $ \p -> [C.exp| int{MatCreate($(int c), $(Mat *p))} |] 
  where c = unComm comm
matCreate c = matCreate' c >>= handleErrTup

matDestroy' m = [C.exp|int{MatDestroy($(Mat *m))}|]
matDestroy m = with m matDestroy' >>= handleErr

withMat :: Comm -> (Mat -> IO a) -> IO a
withMat c = bracket (matCreate c) matDestroy

withMatPipeline :: Comm -> CInt -> CInt -> MatType_ -> (Mat -> IO a) -> (Mat -> IO b) -> IO b
withMatPipeline comm m n ti pre post =
  withMat comm $ \mat -> do
     matSetSizes mat m n
     matSetType mat ti 
     matSetUp mat
     pre mat
     matAssembly mat
     post mat

matSetSizes mat m n = [C.exp|int{MatSetSizes($(Mat mat), PETSC_DECIDE, PETSC_DECIDE,
                                             $(int m), $(int n))}|]


-- f1 i m = [C.exp|int{ ($(int i) % $(int m))  }|] -- mod
-- f2 i m = [C.exp|int{ $(int i) / $(int m)}|]     -- div

-- PetscErrorCode  MatCreateSeqAIJ(MPI_Comm comm,PetscInt m,PetscInt n,PetscInt nz,const PetscInt nnz[],Mat *A)

matCreateSeqAIJ comm m n nz nnz =
  withPtr (\mat ->
   withArray nnz $ \nnzp ->
            [C.exp|int{MatCreateSeqAIJ($(int c),
                                       $(PetscInt m),
                                       $(PetscInt n),
                                       $(PetscInt nz),
                                       $(PetscInt* nnzp),
                                       $(Mat *mat))}|]) >>= handleErrTup
  where c = unComm comm

withMatSeqAIJ comm m n nz nnz = bracket (matCreateSeqAIJ comm m n nz nnz) matDestroy

withMatSeqAIJPipeline comm m n nz nnz pre body =
  withMatSeqAIJ comm m n nz nnz $ \mat -> do
    pre mat
    matAssembly mat
    body mat


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

matCreateMPIAIJWithArrays comm i j a =
  withArray i $ \ip ->
   withArray j $ \jp ->
    withArray a $ \aap -> 
  withPtr ( \mat -> [C.exp|int{MatCreateMPIAIJWithArrays($(PetscInt c),
                                    $(PetscInt m),
                                    $(PetscInt n),
                                    PETSC_DETERMINE, PETSC_DETERMINE,
                                    $(PetscInt* ip), $(PetscInt* jp),
                                    $(PetscScalar* aap), 
                                    $(Mat* mat))}|] ) >>= handleErrTup
  where c = unComm comm 
        m = fromIntegral $ length i -- # local rows
        n = fromIntegral $ length j -- # local rows

withMatMPIAIJWithArrays comm i j a= bracket (matCreateMPIAIJWithArrays comm i j a) matDestroy

withMatMPIAIJWithArraysPipeline comm i j a body =
  withMatMPIAIJWithArrays comm i j a $ \mat -> do
    matAssembly mat
    body mat


matViewStdout v = [C.exp|int{MatView($(Mat v), PETSC_VIEWER_STDOUT_SELF)}|] >>= handleErr

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

data MatCompositeType_ = MatCompositeAdditive | MatCompositeMultiplicative deriving (Eq, Show, Enum)
matCompositeTypeToInt x = fromEnum (x :: MatCompositeType_ )

-- --



-- PETSC_EXTERN PetscErrorCode MatGetSize(Mat,PetscInt*,PetscInt*);
matGetSize' v sx sy =  [C.exp|int{MatGetSize($(Mat v), $(int *sx), $(int *sy))}|]

matGetSize v = withPtr ( \px ->
  withPtr $ \py -> matGetSize' v px py ) >>= fst2M >>= handleErrTup
matGetSizeUnsafeCInt = unsafePerformIO . matGetSize
matGetSizeUnsafe :: Mat -> (Int, Int)
matGetSizeUnsafe m = (fromIntegral a', fromIntegral b') where
  (a', b') = matGetSizeUnsafeCInt m

withMatSize mat f = f (matGetSizeUnsafeCInt mat)

-- f' g h = g . fst &&& h. snd
-- f'' g = f' g g

matSetFromOptions p = [C.exp| int{MatSetFromOptions($(Mat p))} |] >>= handleErr

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
                  ) >>= handleErr


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


matSetValues' mat nbx idxx_ nby idxy_ b_ im =
  [C.exp|int { MatSetValues($(Mat mat),
                      $(int nbx),
                      $(int* idxx_),
                      $(int nby),
                      $(int* idxy_),
                      $(PetscScalar* b_), $(int imm))} |] where
    imm = fromIntegral $ insertModeToInt im

matSetValues mat idxx idxy b im
  | compatDim =
     withArray idxx $ \idxx_ ->
     withArray idxy $ \idxy_ ->
     withArray b $ \b_ ->
     matSetValues' mat nbx idxx_ nby idxy_ b_ im >>= handleErr
  | otherwise = error "matSetValues: incompatible dimensions"
  where
       nbx = fromIntegral $ length idxx
       nby = fromIntegral $ length idxy
       nb = fromIntegral $ length b
       compatDim = (nbx*nby) == nb

matSetValuesAdd m x y b = matSetValues m x y b AddValues
matSetValuesInsert m x y b = matSetValues m x y b InsertValues




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





matAssemblyBegin a = [C.exp|int{MatAssemblyBegin($(Mat a), MAT_FINAL_ASSEMBLY )}|] >>= handleErr
matAssemblyEnd a = [C.exp|int{MatAssemblyEnd($(Mat a), MAT_FINAL_ASSEMBLY )}|] >>= handleErr

matAssembly =
  matAssemblyBegin >> matAssemblyEnd

withMatAssembly v f = do
  matAssemblyBegin v
  f v
  matAssemblyEnd v

matSetUp a = [C.exp|int{MatSetUp($(Mat a))}|] >>= handleErr



-- TODO row (block) indexing : these should not be interpreted as mere Ints but as indices, e.g. FEM mesh nodes

-- PETSC_EXTERN PetscErrorCode MatGetOwnershipRange(Mat,PetscInt*,PetscInt*);
matGetOwnershipRange' a =
 withPtr $ \rmin -> 
  withPtr $ \rmax ->
   [C.exp|int{MatGetOwnershipRange($(Mat a), $(PetscInt *rmin), $(PetscInt *rmax) )}|] 

matGetOwnershipRange m = do
  (r1, (r2, e)) <- matGetOwnershipRange' m
  handleErrTup ((r1, r2), e)

-- nestedTup2 (a, (b, c)) = ((a, b), c)
  

-- -- -- Mat experiments

-- for (i=0; i<4; i++) {
--  45:     v    = 3;
--  46:     MatSetValues(A,1,&i,1,&i,&v,INSERT_VALUES);
--  47:     v    = 1;
--  48:     VecSetValues(B,1,&i,&v,INSERT_VALUES);
--  49:     VecSetValues(X,1,&i,&v,INSERT_VALUES);
--  50:   }

matSetDiagonal mat val =
  [C.block|int{
      int i;
      for (i=0; i< $(int n); i++){
        MatSetValues( $(Mat mat), 1, &i, 1, &i, &$(PetscScalar val), $(int imm));
                                 };
                                   }|] >>= handleErr
   where
    n = fst . unsafePerformIO $ matGetSize mat
    imm = fromIntegral $ insertModeToInt InsertValues


matSetIdentity mat = matSetDiagonal mat 1.0





                

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
    















-- * DM


data DMBoundaryType_ = DmBNone | DmBGhosted | DmBMirror | DmBPeriodic | DmBTwist deriving (Eq, Show, Enum)
dmBoundaryTypeToInt x = fromEnum (x :: DMBoundaryType_) 

-- PETSC_EXTERN PetscErrorCode DMCreate(MPI_Comm,DM*);
dmCreate comm = withPtr ( \dm -> [C.exp|int{DMCreate($(int c), $(DM* dm))} |] ) >>= handleErrTup
  where c = unComm comm

dmDestroy dm = with dm ( \dmp -> [C.exp|int{DMDestroy($(DM* dmp))}|] ) >>= handleErr

withDm comm = bracket (dmCreate comm) dmDestroy


-- -- DMCreate* are for setting up longer-lived data
-- -- DMGet* and DMRestore* are for temporary access (always go in pairs)

-- PETSC_EXTERN PetscErrorCode DMCreateGlobalVector(DM,Vec*);
dmCreateGlobalVector dm = withPtr ( \v -> [C.exp|int{DMCreateGlobalVector($(DM dm), $(Vec* v))}|]) >>= handleErrTup

-- PETSC_EXTERN PetscErrorCode DMCreateLocalVector(DM,Vec*);
dmCreateLocalVector dm = withPtr ( \v -> [C.exp|int{DMCreateLocalVector($(DM dm), $(Vec* v))}|]) >>= handleErrTup

-- PETSC_EXTERN PetscErrorCode DMGetLocalVector(DM,Vec *);
dmGetLocalVector dm = withPtr ( \v -> [C.exp|int{DMGetLocalVector($(DM dm),$(Vec* v))}|]) >>= handleErrTup

-- PETSC_EXTERN PetscErrorCode DMRestoreLocalVector(DM,Vec *);
dmRestoreLocalVector dm vv = with vv ( \v -> [C.exp|int{DMRestoreLocalVector($(DM dm),$(Vec* v))}|]) >>= handleErr

withDmLocalVector dm = bracket (dmGetLocalVector dm) (dmRestoreLocalVector dm)


-- PETSC_EXTERN PetscErrorCode DMGetGlobalVector(DM,Vec *);
dmGetGlobalVector dm = withPtr ( \v -> [C.exp|int{DMGetGlobalVector($(DM dm),$(Vec* v))}|]) >>= handleErrTup

-- PETSC_EXTERN PetscErrorCode DMRestoreGlobalVector(DM,Vec *);
dmRestoreGlobalVector dm vv = with vv ( \v -> [C.exp|int{DMRestoreGlobalVector($(DM dm),$(Vec* v))}|]) >>= handleErr


withDmGlobalVector dm = bracket (dmGetGlobalVector dm) (dmRestoreGlobalVector dm)


withDmCreateGlobalVector dm = bracket (dmCreateGlobalVector dm) vecDestroy

withDmCreateLocalVector dm = bracket (dmCreateLocalVector dm) vecDestroy



dmCreateMatrix' dm mat = [C.exp|int{DMCreateMatrix($(DM dm),$(Mat* mat))}|]
dmCreateMatrix dm = withPtr (dmCreateMatrix' dm) >>= handleErrTup

withDmCreateMatrix dm = bracket (dmCreateMatrix dm) matDestroy


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
 withPtr (\c-> [C.exp| int{DMGetCoordinates($(DM dm),$(Vec*c))} |] ) >>= handleErrTup








-- * DMDA


data DMDAStencilType = DmSStar | DmSBox deriving (Eq, Show, Enum)
dmdaStencilTypeToInt x = fromEnum (x :: DMDAStencilType)

-- data DMDAInterpolationType = DMDA_Q0 | DMDA_Q1 deriving (Eq, Show, Enum)
-- dmdaInterpolationTypeToInt x = fromEnum (x :: DMDAInterpolationType)

-- data DMDAElementType = DMDAElem_Q0 | DMDAElem_Q1 deriving (Eq, Show, Enum)
-- dmdaElementTypeToInt x = fromEnum (x :: DMDAElementType )

-- data DMDADirection = DMDA_X | DMDA_Y | DMDA_Z deriving (Eq, Show, Enum)
-- dmdaDirectionToInt x = fromEnum (x :: DMDADirection)


dmdaCreate comm = withPtr ( \p -> [C.exp|int{DMDACreate($(int c), $(DM* p))}|] ) >>= handleErrTup where
  c = unComm comm



-- PETSC_EXTERN PetscErrorCode DMDASetDim(DM,PetscInt);
dmdaSetDim dm d = [C.exp|int{DMDASetDim($(DM dm), $(PetscInt d))}|] >>= handleErr
-- PETSC_EXTERN PetscErrorCode DMDASetSizes(DM,PetscInt,PetscInt,PetscInt);
dmdaSetSizes dm x y z = [C.exp|int{DMDASetSizes($(DM dm), $(PetscInt x), $(PetscInt y), $(PetscInt z))}|] >>= handleErr

-- PetscErrorCode  DMDACreate1d(MPI_Comm comm, DMBoundaryType bx, PetscInt M, PetscInt dof, PetscInt s, const PetscInt lx[], DM *da)   -- Collective on MPI_Comm
-- Input Parameters

-- comm	- MPI communicator
-- bx	- type of ghost cells at the boundary the array should have, if any. Use DM_BOUNDARY_NONE, DM_BOUNDARY_GHOSTED, or DM_BOUNDARY_PERIODIC.
-- M	- global dimension of the array (use -M to indicate that it may be set to a different value from the command line with -da_grid_x <M>)
-- dof	- number of degrees of freedom per node
-- s	- stencil width
-- lx	- array containing number of nodes in the X direction on each processor, or NULL. If non-null, must be of length as the number of processes in the MPI_Comm.
dmdaCreate1d comm bx m dof s lx_ =
  withArray lx_ ( \ lx ->
   withPtr ( \ dm -> [C.exp|int{DMDACreate1d($(int c),
                                              $(int bxe),
                                              $(PetscInt m),
                                              $(PetscInt dof),
                                              $(PetscInt s),
                                              $(int* lx),
                                              $(DM* dm))}|]  )) >>= handleErrTup
  where c = unComm comm
        bxe = toEnum $ dmBoundaryTypeToInt bx

withDmda1d comm bx m dof s lx =
  bracket (dmdaCreate1d comm bx m dof s lx) dmDestroy



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


dmdaCreate2d comm bx by sten mm nn m n dof s lx_ ly_ =
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
                          $(DM* dm))}|] ) >>= handleErrTup
  where c = unComm comm
        bxe = toEnum $ dmBoundaryTypeToInt bx
        bye = toEnum $ dmBoundaryTypeToInt by
        stene = toEnum $ dmdaStencilTypeToInt sten

dmdaCreate2d' c bx by sten mm nn dof s = dmdaCreate2d c bx by sten mm nn petscDecide petscDecide dof s [] []

withDmda2d comm bx by sten mm nn dof s =
  bracket (dmdaCreate2d' comm bx by sten mm nn dof s) dmDestroy



-- PETSC_EXTERN PetscErrorCode DMDACreateNaturalVector(DM,Vec *);
dmdaCreateNaturalVector dm = withPtr (\v ->[C.exp|int{DMDACreateNaturalVector($(DM dm), $(Vec * v))} |] ) >>= handleErrTup


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

dmdaGetCorners1d dm = dmdaGetCorners1d' dm >>= \l -> handleErrTup $ f1d l
dmdaGetCorners2d dm = dmdaGetCorners2d' dm >>= \l -> handleErrTup $ f2d l
dmdaGetCorners3d dm = dmdaGetCorners3d' dm >>= \l -> handleErrTup $ f3d l


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

dmdaGetGhostCorners1d'' dm = dmdaGetGhostCorners1d' dm >>= \l -> handleErrTup $ f1d l
dmdaGetGhostCorners2d'' dm = dmdaGetGhostCorners2d' dm >>= \l -> handleErrTup $ f2d l
dmdaGetGhostCorners3d'' dm = dmdaGetGhostCorners3d' dm >>= \l -> handleErrTup $ f3d l


dmdaGetGhostCorners1d dm =  liftM fromIntegralTup $ dmdaGetGhostCorners1d'' dm
dmdaGetGhostCorners2d dm =  liftM fromIntegralTup2 $ dmdaGetGhostCorners2d'' dm
dmdaGetGhostCorners3d dm =  liftM fromIntegralTup3 $ dmdaGetGhostCorners3d'' dm 

-- dmdaGetGhostCorners2d dm =  do
--   ((a,b),(c,d)) <- dmdaGetGhostCorners2d'' dm
--   return $ fromIntegralTup a

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
  [C.exp|int{DMDASetUniformCoordinates($(DM da),$(PetscReal xmin),$(PetscReal xmax),$(PetscReal ymin),$(PetscReal ymax),$(PetscReal zmin),$(PetscReal zmax))}|] >>= handleErr



-- PetscErrorCode  DMDAVecGetArray(DM da,Vec vec,void *array)
-- dmdaVecGetArray' :: DM -> Vec -> Ptr PetscScalar_ -> IO CInt
dmdaVecGetArray' dm v vvp =   
  [C.exp|int{DMDAVecGetArray($(DM dm),
                             $(Vec v),
                             $(PetscScalar* vvp))}|]

dmdaVecGetArray'' dm v =   
  withPtr ( \vvp -> [C.exp|int{DMDAVecGetArray($(DM dm),
                             $(Vec v),
                             $(PetscScalar** vvp))}|] ) >>= handleErrTup >>= peekArray n where n = fromIntegral $ vecGetSizeUnsafe v

dmdaVecGetArray = dmdaVecGetArray''

-- PetscErrorCode  DMDARestoreArray(DM da,PetscBool ghosted,void *vptr)
dmdaRestoreArray dm ghosted vptr = withArray vptr ( \vp -> 
  [C.exp|int{DMDARestoreArray($(DM dm),
                              $(PetscBool ghosted),
                              $(PetscScalar* vp))}|] ) >>= handleErr

-- PetscErrorCode  DMDAVecRestoreArray(DM da,Vec vec,void *array)
dmdaVecRestoreArray dm v arr = withArray arr $ \arr_ -> 
  [C.exp|int{DMDAVecRestoreArray($(DM dm), $(Vec v), $(PetscScalar* arr_))}|]

withDmdaArray dm v = bracket (dmdaVecGetArray dm v) (dmdaVecRestoreArray dm v)



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
  withPtr ( \p -> [C.exp|int{DMCompositeCreate($(int c), $(DM* p))}|] ) >>= handleErrTup
  where c = unComm comm






















-- * KSP




data KspType_ = KspRichardson | KspChebyshev | KspCg | KspGroppCg | KspPipeCg
              | KspCgne | KspNash | KspStcg | KspGltr | KspGmres | KspFgmres
              | KspLgmres | KspDgmres | KspPgmres | KspTcqmr | KspBcgs | KspIbcgs
              | KspFbcgs | KspFbcgsr | KspBcgsl | KspCgs | KspTfqmr | KspCr
              | KspPipecr | KspLsqr | KspPreonly | KspQcg | KspBicg | KspMinres
              | KspSymmlq | KspLcd | KspPython | KspCgr | KspSpecest
              deriving (Eq, Show)

kspTypeToStr :: KspType_ -> String
kspTypeToStr KspRichardson = "richardson"
kspTypeToStr KspChebyshev = "chebyshev"
kspTypeToStr KspCg = "cg"
kspTypeToStr KspGmres = "gmres"
kspTypeToStr KspBicg = "bicg"
kspTypeToStr KspMinres = "minres"
kspTypeToStr _ = "cg" -- default, for no particular reason

data KspConvergedReason = KspConvergedRtolNormal | KspConvergedAtolNormal
                        | KspConvergedRtol | KspConvergedAtol
                        | KspConvergedIts | KspConvergedCgNegCurv
                        | KspConvergedCgConstrained | KspConvergedStepLength
                        | KspConvergedHappyBreakdown
                        | KspDivergedNull | KspDivergedIts | KspDivergedDtol
                        | KspDivergedBreak
                        | KspDivergedBreakBicg | KspDivergedNonsymm
                        | KspDivergedIndefPC
                        | KspDivergedNaNorInf | KspDivergedIndefMat
                        | KspCConvergedIterating
                        deriving (Eq, Show, Enum)

kspConvergedIntToReason :: Int -> KspConvergedReason
kspConvergedIntToReason r =
  case r of 1    -> KspConvergedRtolNormal
            9    -> KspConvergedAtolNormal
            2    -> KspConvergedRtol 
            3    -> KspConvergedAtol
            4    -> KspConvergedIts
            5    -> KspConvergedCgNegCurv
            6    -> KspConvergedCgConstrained
            7    -> KspConvergedStepLength
            8    -> KspConvergedHappyBreakdown
            (-2) -> KspDivergedNull
            (-3) -> KspDivergedIts

kspGetConvergedReason' ksp =
  withPtr ( \r ->
             [C.exp| int{ KSPGetConvergedReason( $(KSP ksp),
                                              $(int* r) ) } |]
          ) >>= handleErrTup
kspGetConvergedReason ksp = 
  kspGetConvergedReason' ksp >>= \r -> return $ kspConvergedIntToReason (fromIntegral r)


kspCreate' comm p = [C.exp| int{KSPCreate($(int c), $(KSP *p))}|] where
  c = unComm comm
kspCreate c = withPtr (kspCreate' c) >>= handleErrTup

kspSetType' :: KSP -> KspType_ -> IO CInt
kspSetType' ksp kt = withCString strk $ \strp -> [C.exp|int{KSPSetType($(KSP ksp), $(char* strp))}|] where
  strk = kspTypeToStr kt

kspSetType :: KSP -> KspType_ -> IO ()
kspSetType ksp kt = kspSetType' ksp kt >>= handleErr

-- PETSC_EXTERN PetscErrorCode KSPGetType(KSP,KSPType *);
-- kspGetType ksp = alloca ( \strp -> do
--                            [C.exp|int{KSPGetType($(KSP ksp), $(char *strp))}|]
--                            peekString strp) 

kspDestroy' p = [C.exp| int{KSPDestroy($(KSP *p))}  |]
kspDestroy p = with p kspDestroy' >>= handleErr

withKsp c = bracket (kspCreate c) kspDestroy

withKspSetupSolve c mat1 mat2 ignz kt x v post = withKsp c $ \ksp -> do
  kspSetOperators ksp mat1 mat2
  kspSetType ksp kt 
  kspSetInitialGuessNonzero ksp ignz
  kspSetUp ksp
  kspSolve ksp x v
  -- soln <- kspGetSolution ksp 
  post ksp
-- nb this is just a reference; it becomes invalid after exiting the withKsp bracket -> we need a `withVecDuplicateCopy` outside withKspSolve to allocate space for the solution 


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


kspSetOperators ksp amat pmat =
  [C.exp|int{KSPSetOperators($(KSP ksp), $(Mat amat), $(Mat pmat))}|] >>= handleErr

kspSetUp ksp = [C.exp|int{KSPSetUp($(KSP ksp))}|] >>= handleErr

kspSolve ksp b x = [C.exp|int{KSPSolve($(KSP ksp), $(Vec b), $(Vec x))}|] >>= handleErr

kspSolveTranspose ksp b x = [C.exp|int{KSPSolveTranspose($(KSP ksp), $(Vec b), $(Vec x))}|] >>= handleErr





-- PETSC_EXTERN PetscErrorCode KSPReset(KSP);

-- PETSC_EXTERN PetscErrorCode KSPSetReusePreconditioner(KSP,PetscBool);
kspSetReusePreconditioner ksp b = [C.exp|int{KSPSetReusePreconditioner($(KSP ksp), $(PetscBool b))}|] >>= handleErr

-- PETSC_EXTERN PetscErrorCode KSPRegisterAll(void);
-- PETSC_EXTERN PetscErrorCode KSPRegister(const char[],PetscErrorCode (*)(KSP));
-- PETSC_EXTERN PetscErrorCode KSPMatRegisterAll(void);

-- PETSC_EXTERN PetscErrorCode KSPSetPCSide(KSP,PCSide);
-- PETSC_EXTERN PetscErrorCode KSPGetPCSide(KSP,PCSide*);
-- PETSC_EXTERN PetscErrorCode KSPGetTolerances(KSP,PetscReal*,PetscReal*,PetscReal*,PetscInt*);

-- PETSC_EXTERN PetscErrorCode KSPSetTolerances(KSP,PetscReal,PetscReal,PetscReal,PetscInt);
-- kspSetTolerances ksp 

-- PETSC_EXTERN PetscErrorCode KSPSetInitialGuessNonzero(KSP,PetscBool );
kspSetInitialGuessNonzero ksp b= [C.exp|int{KSPSetInitialGuessNonzero($(KSP ksp), $(PetscBool b))}|] >>= handleErr

-- PETSC_EXTERN PetscErrorCode KSPGetInitialGuessNonzero(KSP,PetscBool  *);
-- PETSC_EXTERN PetscErrorCode KSPSetInitialGuessKnoll(KSP,PetscBool );
-- PETSC_EXTERN PetscErrorCode KSPGetInitialGuessKnoll(KSP,PetscBool *);

-- PETSC_EXTERN PetscErrorCode KSPSetErrorIfNotConverged(KSP,PetscBool );
kspSetErrorIfNotConverged ksp b= [C.exp|int{KSPSetErrorIfNotConverged($(KSP ksp), $(PetscBool b))}|] >>= handleErr

-- PETSC_EXTERN PetscErrorCode KSPGetErrorIfNotConverged(KSP,PetscBool  *);
-- PETSC_EXTERN PetscErrorCode KSPGetComputeEigenvalues(KSP,PetscBool *);

-- PETSC_EXTERN PetscErrorCode KSPSetComputeEigenvalues(KSP,PetscBool );
kspSetComputeEigenValues ksp b= [C.exp|int{KSPSetComputeEigenvalues($(KSP ksp), $(PetscBool b))}|] >>= handleErr

-- PETSC_EXTERN PetscErrorCode KSPGetComputeSingularValues(KSP,PetscBool *);

-- PETSC_EXTERN PetscErrorCode KSPSetComputeSingularValues(KSP,PetscBool );
kspSetComputeSingularValues ksp b= [C.exp|int{KSPSetComputeSingularValues($(KSP ksp), $(PetscBool b))}|] >>= handleErr

-- PETSC_EXTERN PetscErrorCode KSPGetRhs(KSP,Vec *);
kspGetRhs' ksp = withPtr $ \v -> [C.exp|int{KSPGetRhs($(KSP ksp), $(Vec *v))}|]
kspGetRhs ksp = kspGetRhs' ksp >>= handleErrTup

-- PETSC_EXTERN PetscErrorCode KSPGetSolution(KSP,Vec *);
kspGetSolution' ksp = withPtr $ \v -> [C.exp|int{KSPGetSolution($(KSP ksp), $(Vec *v))}|]
kspGetSolution :: KSP -> IO Vec
kspGetSolution ksp = kspGetSolution' ksp >>= handleErrTup

-- PETSC_EXTERN PetscErrorCode KSPGetResidualNorm(KSP,PetscReal*);
kspGetResidualNorm' ksp = withPtr $ \v -> [C.exp|int{KSPGetResidualNorm($(KSP ksp), $(PetscReal *v))}|]

kspGetResidualNorm :: KSP -> IO PetscReal_
kspGetResidualNorm ksp = kspGetResidualNorm' ksp >>= handleErrTup

-- PETSC_EXTERN PetscErrorCode KSPGetIterationNumber(KSP,PetscInt*);
kspGetIterationNumber ksp = withPtr ( \v -> [C.exp|int{KSPGetIterationNumber($(KSP ksp), $(int *v))}|] ) >>= handleErrTup

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





data PFType_ = PfConstant | PfMat | PfString | PfQuick | PfIdentity | PfMatlab deriving (Eq, Show)

pfTypeToStr PfConstant = "constant"
pfTypeToStr PfMat = "mat"
pfTypeToStr PfString = "string"
pfTypeToStr PfQuick = "quick"
pfTypeToStr PfIdentity = "identity"
pfTypeToStr PfMatlab = "matlab"

-- PetscErrorCode  PFCreate(MPI_Comm comm,PetscInt dimin,PetscInt dimout,PF *pf)
-- Collective on MPI_Comm
-- Input Parameters :
-- comm	- MPI communicator
-- dimin	- dimension of the space you are mapping from
-- dimout	- dimension of the space you are mapping to
pfCreate comm dimin dimout = withPtr ( \pf ->[C.exp|int{PFCreate($(int c),$(int dimin),$(int dimout),$(PF*pf) )}|] ) >>= handleErrTup
  where
    c = unComm comm

-- PETSC_EXTERN PetscErrorCode DMDACreatePF(DM,PF*);
dmdaCreatePF dm = withPtr (\pf -> [C.exp|int{DMDACreatePF($(DM dm),$(PF*pf))}|]) >>= handleErrTup

-- PETSC_EXTERN PetscErrorCode PFSetType(PF,PFType,void*);
pfSetType pf t o = -- not sure how to represent the pointer to void 
  withCString tstr (\tp->   [C.exp|int{PFSetType($(PF pf),$(char*tp),$(void*o))}|]
                   ) >>= handleErr where
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
pfSet' pf apply applyvec view destroy ctx =
  [C.exp|int{PFSet($(PF pf),
                   $fun:(int(*apply)(void*,PetscInt,PetscScalar*,PetscScalar*)),
                   $fun:(int(*applyvec)(void*, Vec, Vec)),
                   $fun:(int(*view)(void*, int )),
                   $fun:(int(*destroy)(void*)),
                   $(void*ctx)
                  )}
        |]

-- f :: (a -> a -> IO Int) -> (a -> a -> IO a)  -- ?

-- pfSetVec' :: PF -> (Ptr () -> Vec -> Vec -> IO CInt) -> IO ()
pfSetVec' pf applyvec =
    [C.exp|int{PFSet($(PF pf),
                   0,
                   $fun:(int(*applyvec)( void* , Vec, Vec)),
                   0, 0, 0)}|] 

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


data SnesType_ = SnesNewtonLs | SnesNewtonTr | SnesPython | SnesTest
               | SnesNRichardson | SnesKspOnly | SnesViNewtonRsLs
               | SnesViNewtonSsLs | SnesNgmres | SnesQn | SnesShell | SnesNgs
               | SnesNcg | SnesFas | SnesMs | SnesNasm | SnesAnderson | SnesAspin
               | SnesComposite deriving (Eq, Show)

snesTypeToStr :: SnesType_ -> String
snesTypeToStr SnesNewtonLs = "newtonls"
snesTypeToStr SnesNewtonTr = "newtontr"
snesTypeToStr SnesNgmres = "ngmres"
snesTypeToStr _ = "newtonls" -- default


snesCreate' comm p = [C.exp| int{SNESCreate($(int c), $(SNES *p))}|] where
  c = unComm comm
snesCreate c = withPtr (snesCreate' c) >>= handleErrTup

snesSetType' :: SNES -> SnesType_ -> IO CInt
snesSetType' s t = withCString strk $ \strp -> [C.exp|int{SNESSetType($(SNES s), $(char* strp))}|] where
  strk = snesTypeToStr t

snesSetType :: SNES -> SnesType_ -> IO ()
snesSetType s t = snesSetType' s t  >>= handleErr


-- PetscErrorCode  SNESSetFunction(SNES snes,Vec r,PetscErrorCode (*f)(SNES,Vec,Vec,void*),void *ctx)
snesSetFunction' snes r f ctx =
  [C.exp|int{SNESSetFunction($(SNES snes), $(Vec r),
                             $fun:(int (*f)(SNES, Vec, Vec, void*) ),
                             $(void* ctx))}|]
snesSetFunction0 snes r f =
  [C.exp|int{SNESSetFunction($(SNES snes), $(Vec r),
                             $fun:(int (*f)(SNES, Vec, Vec, void*) ),
                             NULL )}|]
snesSetFunction snes v f =
  snesSetFunction0 snes v f' where
    f' s a b _ = f s a b


-- PETSC_EXTERN PetscErrorCode SNESDestroy(SNES*);
snesDestroy' p = [C.exp| int{SNESDestroy($(SNES *p))}  |]
snesDestroy p = with p snesDestroy' >>= handleErr

-- PETSC_EXTERN PetscErrorCode SNESSetUp(SNES);
snesSetUp s = [C.exp|int{SNESSetUp($(SNES s))}|] >>= handleErr

-- PETSC_EXTERN PetscErrorCode SNESSolve(SNES,Vec,Vec);
snesSolve s b x = [C.exp|int{SNESSolve($(SNES s), $(Vec b), $(Vec x))}|] >>= handleErr


-- PETSC_EXTERN PetscErrorCode SNESGetSolution(SNES,Vec*);
snesGetSolution s = withPtr ( \v ->
  [C.exp|int{SNESGetSolution($(SNES s), $(Vec *v))}|] ) >>= handleErrTup



withSnes comm = bracket (snesCreate comm) snesDestroy

withSnesSetupSolve comm st b x pre post = 
  withSnes comm $ \s -> do
   snesSetType s st
   snesSetUp s
   -- missing : function, Jacobian
   pre s
   snesSolve s b x
   post s
   








-- * SNESLineSearch

-- PetscErrorCode SNESGetLineSearch(SNES snes, SNESLineSearch *linesearch)
snesGetLineSearch snes =
  withPtr ( \ls ->
     [C.exp|int{SNESGetLineSearch($(SNES snes),
                                  $(SNESLineSearch* ls))}|]) >>= handleErrTup










-- * TS

data TsProblemType = TsLinear | TsNonlinear deriving (Eq, Show, Enum)
tsProblemTypeToInt x = fromEnum (x :: TsProblemType)



-- PetscErrorCode  TSCreate(MPI_Comm comm, TS *ts)
tsCreate comm =
  withPtr (\ts -> [C.exp|int{TSCreate($(int c), $(TS* ts))}|]) >>= handleErrTup
  where
   c = unComm comm

tsDestroy' ts = [C.exp| int{TSDestroy($(TS* ts))} |] 
tsDestroy ts = with ts (\tp -> tsDestroy' tp) >>= handleErr

withTs c = bracket (tsCreate c) tsDestroy


-- PetscErrorCode  TSSetProblemType(TS ts, TSProblemType type)
tsSetProblemType ts t =
  [C.exp|int{TSSetProblemType($(TS ts), $(int tt))}|] >>= handleErr
   where tt = fromIntegral $ tsProblemTypeToInt t


-- PetscErrorCode TSSetInitialTimeStep(TS ts,PetscReal initial_time,PetscReal time_step)
tsSetInitialTimeStep ts it dt =
  [C.exp|int{TSSetInitialTimeStep($(TS ts), $(PetscReal it), $(PetscReal dt))}|] >>= handleErr

-- PetscErrorCode  TSSetDuration(TS ts,PetscInt maxsteps,PetscReal maxtime)
tsSetDuration ts ms' mt =
  [C.exp|int{TSSetDuration($(TS ts),$(int ms),$(PetscReal mt))}|] >>= handleErr
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


    


withTSSolve c pt t0 dt0 maxsteps maxtime v pre post = withTs c $ \ts -> do
  -- NB: Vec v contains initial condition and will be updated with final state
  -- -- (initial state must be given or computed internally)
  -- NB2 : this version does not account for time-varying right-hand side
  -- see:
  -- http://www.mcs.anl.gov/petsc/petsc-current/src/ts/examples/tutorials/ex3.c.html
  tsSetProblemType ts pt
  tsSetInitialTimeStep ts t0 dt0
  tsSetDuration ts maxsteps maxtime
  pre ts
  tsSolve ts v
  post ts

  


-- TSSetFromOptions(ts);

--    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
--       Solve the problem
--       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
--    /*
--       Evaluate initial conditions
--    */

--    InitialConditions(u,&appctx);



--    TSSolve(ts,u);
-- PetscErrorCode TSSolve(TS ts,Vec u)        -- Collective on TS
-- Input Parameters :
-- ts	- the TS context obtained from TSCreate()
-- u	- the solution vector (can be null if TSSetSolution() was used, otherwise must contain the initial conditions)
-- Notes :
-- The final time returned by this function may be different from the time of the internally held state accessible by TSGetSolution() and TSGetTime() because the method may have stepped over the final time.

tsSolve ts u = [C.exp|int{TSSolve($(TS ts),$(Vec u))}|]

    
--    TSGetTimeStepNumber(ts,&steps);

--    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
--       View timestepping solver info
--       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

--    TSView(ts,PETSC_VIEWER_STDOUT_SELF);
tsViewStdout ts =
  [C.exp|int{TSView($(TS ts), PETSC_VIEWER_STDOUT_SELF)}|] >>= handleErr


















-- * TAO

data TaoType_ = TaoLmvm | TaoNls | TaoNtr | TaoNtl | TaoCg | TaoTron | TaoOwlqn
              | TaoBmrm | TaoBlmvm | TaoBqpip | TaoGpcg | TaoNm | TaoPounders
              | TaoLcl | TaoSsils | TaoSSfls | TaoAsils | TaoAsfls | TaoIpm | TaoTest
              deriving (Eq, Show)

taoTypeToStr TaoLmvm = "lmvm"
taoTypeToStr TaoNls = "nls"
taoTypeToStr TaoNtr = "ntr"
taoTypeToStr TaoNtl = "ntl"
taoTypeToStr TaoCg = "cg"
taoTypeToStr TaoSsils = "ssils"
taoTypeToStr TaoIpm = "ipm"
taoTypeToStr _ = "cg" -- default

data TaoConvergedReason_ = TaoConvFaTol | TaoConvFrTol | TaoConvGaTol
                         | TaoConvGrTol | TaoConvGtTol
                         | TaoConvStepTol | TaoConvMinF | TaoConvUser
                         | TaoDivMaxIts | TaoDivNan | TaoDivMaxFcn | TaoDivLsFail
                         | TaoDivTrReduct | TaoDivUser deriving (Eq,Show,Enum)

taoConvergedIntToReason x =
  case x of 1 -> TaoConvFaTol
            2 -> TaoConvFrTol
            3 -> TaoConvGaTol
            4 -> TaoConvGrTol
            5 -> TaoConvGtTol
            6 -> TaoConvStepTol
            7 -> TaoConvMinF
            8 -> TaoConvUser
            (-2) -> TaoDivMaxIts
            (-4) -> TaoDivNan
            (-5) -> TaoDivMaxFcn
            (-6) -> TaoDivLsFail
            (-7) -> TaoDivTrReduct
            (-8) -> TaoDivUser
            _ -> TaoDivUser


taoInitialize args opts help = 
 let acc = fromIntegral $ length args in 
  with acc $ \ac ->
   withCStringArray args $ \aa ->
   with aa $ \a ->
    withCString opts $ \o ->
    withCString help $ \h ->
    [C.exp|int{TaoInitialize($(int *ac), $(char*** a), $(char* o), $(char *h))}|] 

taoFin = [C.block| int{ TaoFinalize(); }|] >>= handleErr

withTaoInit a o h f = do 
  taoInitialize a o h
  f
  taoFin

withTaoInit0 = withTaoInit [] [] [] 

taoCreate comm = withPtr (\p -> [C.exp| int{TaoCreate($(int c), $(Tao *p))} |] ) >>= handleErrTup where
  c = unComm comm

taoDestroy p = with p ( \pp -> [C.exp| int{TaoDestroy($(Tao *pp))}  |] ) >>= handleErr

withTao c = bracket (taoCreate c) taoDestroy

taoSetType tao ti = withCString ti_ ( \tip -> [C.exp|int{TaoSetType($(Tao tao), $(char* tip ))}|] ) >>= handleErr where
  ti_ = taoTypeToStr ti

taoViewStdout tao = [C.exp|int{TaoView($(Tao tao), PETSC_VIEWER_STDOUT_SELF)}|]

taoGetConvergedReason tao = liftM taoConvergedIntToReason $
   withPtr (\tr -> [C.exp|int{TaoGetConvergedReason($(Tao tao), $(int* tr))}|]) >>= handleErrTup 


-- TaoSetInitialVector(TaoSolver tao, Vec x);
taoSetInitialVector tao x = [C.exp|int{TaoSetInitialVector($(Tao tao),$(Vec x))}|] >>= handleErr

-- TaoSolve(TaoSolver tao);
taoSolve tao = [C.exp|int{TaoSolve($(Tao tao))}|] >>= handleErr



-- PETSC_EXTERN PetscErrorCode TaoGetSolutionVector(Tao, Vec*);
taoGetSolutionVector tao = withPtr (\p -> [C.exp|int{TaoGetSolutionVector($(Tao tao), $(Vec* p))}|]) >>= handleErrTup
-- PETSC_EXTERN PetscErrorCode TaoGetGradientVector(Tao, Vec*);
taoGetGradientVector tao = withPtr (\p -> [C.exp|int{TaoGetGradientVector($(Tao tao), $(Vec* p))}|]) >>= handleErrTup

-- PETSC_EXTERN PetscErrorCode TaoSetObjectiveRoutine(Tao, PetscErrorCode(*)(Tao, Vec, PetscReal*,void*), void*);
taoSetObjectiveRoutine' tao f =
  [C.exp|int{TaoSetObjectiveRoutine($(Tao tao),
                                    $fun:(int (*f)(Tao, Vec, PetscReal*, void*)),
                                    NULL)}|] >>= handleErr
taoSetObjectiveRoutine t f = taoSetObjectiveRoutine' t f' where
  f' ta v r _ = f ta v r


-- PETSC_EXTERN PetscErrorCode TaoSetGradientRoutine(Tao, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);
taoSetGradientRoutine' tao f =
  [C.exp|int{TaoSetGradientRoutine($(Tao tao),
                                    $fun:(int (*f)(Tao, Vec, Vec, void*)),
                                    NULL)}|] >>= handleErr
taoSetGradientRoutine t f = taoSetGradientRoutine' t f' where
  f' ta v r _ = f ta v r
  
-- PETSC_EXTERN PetscErrorCode TaoSetObjectiveAndGradientRoutine(Tao, PetscErrorCode(*)(Tao, Vec, PetscReal*, Vec, void*), void*);
taoSetObjectiveAndGradientRoutine' tao f =
  [C.exp|int{TaoSetObjectiveAndGradientRoutine(
                $(Tao tao),
                $fun:(int (*f)(Tao, Vec, PetscReal*, Vec, void*)),
                NULL)}|] >>= handleErr
taoSetObjectiveAndGradientRoutine t f =
  taoSetObjectiveAndGradientRoutine' t f' where
    f' ta v r v2 _ = f ta v r v2
  
-- PETSC_EXTERN PetscErrorCode TaoSetHessianRoutine(Tao,Mat,Mat,PetscErrorCode(*)(Tao,Vec, Mat, Mat, void*), void*);
taoSetHessianRoutine' tao m1 m2 f =
  [C.exp|int{TaoSetHessianRoutine($(Tao tao), $(Mat m1), $(Mat m2),
                                    $fun:(int (*f)(Tao, Vec, Mat, Mat, void*)),
                                    NULL)}|] >>= handleErr
taoSetHessianRoutine t m1 m2 f = taoSetHessianRoutine' t m1 m2 f' where
  f' ta v n1 n2 _ = f ta v n1 n2


-- PETSC_EXTERN PetscErrorCode TaoSetSeparableObjectiveRoutine(Tao, Vec, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);
-- PETSC_EXTERN PetscErrorCode TaoSetConstraintsRoutine(Tao, Vec, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);
taoSetConstraintsRoutine' tao vec f =
  [C.exp|int{TaoSetConstraintsRoutine(
                $(Tao tao), $(Vec vec),
                $fun:(int (*f)(Tao, Vec, Vec, void*)),
                NULL)}|] >>= handleErr
taoSetConstraintsRoutine t v f = taoSetConstraintsRoutine' t v f' where
  f' ta v1 v2  _ = f ta v1 v2
  
-- PETSC_EXTERN PetscErrorCode TaoSetInequalityConstraintsRoutine(Tao, Vec, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);
taoSetInequalityConstraintsRoutine' tao vec f =
  [C.exp|int{TaoSetInequalityConstraintsRoutine(
                $(Tao tao), $(Vec vec),
                $fun:(int (*f)(Tao, Vec, Vec, void*)),
                NULL)}|] >>= handleErr
taoSetInequalityConstraintsRoutine t v f =
  taoSetInequalityConstraintsRoutine' t v f' where
    f' ta v1 v2  _ = f ta v1 v2
  
-- PETSC_EXTERN PetscErrorCode TaoSetEqualityConstraintsRoutine(Tao, Vec, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);
taoSetEqualityConstraintsRoutine' tao vec f =
  [C.exp|int{TaoSetEqualityConstraintsRoutine(
                $(Tao tao), $(Vec vec),
                $fun:(int (*f)(Tao, Vec, Vec, void*)),
                NULL)}|] >>= handleErr
taoSetEqualityConstraintsRoutine t v f =
  taoSetEqualityConstraintsRoutine' t v f' where
     f' ta v1 v2  _ = f ta v1 v2
  
-- PETSC_EXTERN PetscErrorCode TaoSetJacobianRoutine(Tao,Mat,Mat, PetscErrorCode(*)(Tao,Vec, Mat, Mat, void*), void*);
taoSetJacobianRoutine' tao m1 m2 f =
  [C.exp|int{TaoSetJacobianRoutine(
                $(Tao tao), $(Mat m1), $(Mat m2),
                $fun:(int (*f)(Tao, Vec, Mat, Mat, void*)),
                NULL)}|] >>= handleErr
taoSetJacobianRoutine t m1 m2 f =
  taoSetJacobianRoutine' t m1 m2 f' where
     f' ta v v1 v2  _ = f ta v v1 v2
     
-- PETSC_EXTERN PetscErrorCode TaoSetJacobianStateRoutine(Tao,Mat,Mat,Mat, PetscErrorCode(*)(Tao,Vec, Mat, Mat, Mat, void*), void*);
taoSetJacobianStateRoutine' tao m1 m2 m3 f =
  [C.exp|int{TaoSetJacobianStateRoutine(
                $(Tao tao), $(Mat m1), $(Mat m2), $(Mat m3),
                $fun:(int (*f)(Tao, Vec, Mat, Mat, Mat, void*)),
                NULL)}|] >>= handleErr
taoSetJacobianStateRoutine t m1 m2 m3 f =
  taoSetJacobianStateRoutine' t m1 m2 m3 f' where
     f' ta v v1 v2 v3  _ = f ta v v1 v2 v3

-- PETSC_EXTERN PetscErrorCode TaoSetJacobianDesignRoutine(Tao,Mat,PetscErrorCode(*)(Tao,Vec, Mat, void*), void*);
taoSetJacobianDesignRoutine' tao m f =
  [C.exp|int{TaoSetJacobianDesignRoutine(
                $(Tao tao), $(Mat m),
                $fun:(int (*f)(Tao, Vec, Mat, void*)),
                NULL)}|] >>= handleErr
taoSetJacobianDesignRoutine t m f =
  taoSetJacobianDesignRoutine' t m f' where
     f' ta v v1  _ = f ta v v1
     
-- PETSC_EXTERN PetscErrorCode TaoSetJacobianInequalityRoutine(Tao,Mat,Mat,PetscErrorCode(*)(Tao,Vec, Mat, Mat, void*), void*);
-- PETSC_EXTERN PetscErrorCode TaoSetJacobianEqualityRoutine(Tao,Mat,Mat,PetscErrorCode(*)(Tao,Vec, Mat, Mat, void*), void*);

-- PETSC_EXTERN PetscErrorCode TaoSetStateDesignIS(Tao, IS, IS);

-- PETSC_EXTERN PetscErrorCode TaoComputeObjective(Tao, Vec, PetscReal*);
taoComputeObjective tao v =
  withPtr (\p -> [C.exp|int{TaoComputeObjective($(Tao tao),$(Vec v),$(PetscReal* p))}|] ) >>= handleErrTup
-- PETSC_EXTERN PetscErrorCode TaoComputeSeparableObjective(Tao, Vec, Vec);

-- PETSC_EXTERN PetscErrorCode TaoComputeGradient(Tao, Vec, Vec);
taoComputeGradient tao v =
 -- -- -- -- DIRTY HACK WARNING: will it work?
  withPtr (\p -> [C.exp|int{TaoComputeGradient($(Tao tao),$(Vec v),$(Vec* p))}|] ) >>= handleErrTup
  
-- PETSC_EXTERN PetscErrorCode TaoComputeObjectiveAndGradient(Tao, Vec, PetscReal*, Vec);
-- PETSC_EXTERN PetscErrorCode TaoComputeConstraints(Tao, Vec, Vec);
-- PETSC_EXTERN PetscErrorCode TaoComputeInequalityConstraints(Tao, Vec, Vec);
-- PETSC_EXTERN PetscErrorCode TaoComputeEqualityConstraints(Tao, Vec, Vec);
-- PETSC_EXTERN PetscErrorCode TaoDefaultComputeGradient(Tao, Vec, Vec, void*);
-- PETSC_EXTERN PetscErrorCode TaoIsObjectiveDefined(Tao,PetscBool*);
taoIsObjectiveDefined t =
 withPtr
  (\p -> [C.exp|int{TaoIsObjectiveDefined($(Tao t),
                                          $(PetscBool* p))}|]) >>= handleErrTup

-- PETSC_EXTERN PetscErrorCode TaoIsGradientDefined(Tao,PetscBool*);
taoIsGradientDefined t =
 withPtr
  (\p -> [C.exp|int{TaoIsGradientDefined($(Tao t),
                                          $(PetscBool* p))}|]) >>= handleErrTup

-- PETSC_EXTERN PetscErrorCode TaoIsObjectiveAndGradientDefined(Tao,PetscBool*);



withTaoSetupSolve c ti x pre post = withTao c $ \t -> do
  taoSetType t ti
  taoSetInitialVector t x
  pre t
  taoSolve t
  post t
  -- taoGetConvergedReason t

withTaoSetup c ti x pre post = withTao c $ \t -> do
  taoSetType t ti
  taoSetInitialVector t x
  pre t
  post t
  -- taoGetConvergedReason t



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
taoSetVariableBounds tao x1 x2 =
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

data PetscViewerType_ = ViewerSock | ViewerAscii | ViewerBinary | ViewerString | ViewerDraw | ViewerVu | ViewerMathematica | ViewerNetcdf | ViewerHdf5 | ViewerVtk | ViewerMatlab | ViewerSaws deriving (Eq, Enum, Show)

viewerTypeToInt x = fromEnum (x :: PetscViewerType_)


viewerTypeToStr ViewerSock = "socket"
viewerTypeToStr ViewerAscii = "ascii"















-- * PETSc misc


-- PETSC_EXTERN PetscErrorCode PetscLogStageRegister(const char[],PetscLogStage*);
-- PETSC_EXTERN PetscErrorCode PetscLogStagePush(PetscLogStage);

-- petscLogStageRegister :: String -> PetscLogStage_ -> IO CInt
petscLogStageRegister s ls =
  withCString s $ \c ->
   with ls $ \lls -> 
    [C.exp|int{PetscLogStageRegister($(char *c), $(PetscLogStage* lls ))}|] >>= handleErr

petscLogStagePush ls = [C.exp|int{PetscLogStagePush($(PetscLogStage ls))}|] >>= handleErr

petscLogStagePop = [C.exp|int{PetscLogStagePop()}|] >>= handleErr


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

petscOptionsGetInt prefix name = do
  (a, (f, e)) <- petscOptionsGetInt'' prefix name
  if f then handleErrTup (Just a, e)
       else handleErrTup (Nothing, e)

withPetscOptionsGetInt prefix name f = do
  x <- petscOptionsGetInt prefix name
  case x of (Just s) -> f s
            Nothing  -> error "option not found"




-- PETSC_EXTERN PetscErrorCode PetscGetArgs(int*,char ***);
-- petscGetArgs                   -- 


-- PETSC_EXTERN PetscErrorCode PetscInitialized(PetscBool *);

petscInitialized :: IO Bool
petscInitialized = withPtr ( \b ->
     [C.exp|int{ PetscInitialized($(PetscBool * b)) } |] )  >>= handleErrTup 
-- petscInitialized = do
--   i <- petscInitialized'
--   if i then return True
--        else return False
  

-- PETSC_EXTERN PetscErrorCode PetscFinalized(PetscBool *);

petscFinalized :: IO Bool
petscFinalized = withPtr ( \p ->
  [C.exp|int{ PetscFinalized($(PetscBool * p))  }|] ) >>= handleErrTup



petscInit0 :: IO ()
petscInit0 = [C.exp| int{ PetscInitializeNoArguments()  }|] >>= handleErr

-- -- PETSC_EXTERN PetscErrorCode PetscInitialize(int*,char***,const char[],const char[]);

petscInitialize args opts help = 
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

petscFin = [C.block| int{ PetscFinalize(); }|] >>= handleErr

withPetsc0 f = do -- returns IO ()
  petscInit0
  f
  petscFin

withPetsc0' :: IO a -> IO a
withPetsc0' = bracket_ petscInit0 petscFin -- returns IO a

withPetsc :: Argv -> OptsStr -> HelpStr -> IO a -> IO ()
withPetsc argv opts help f = do
  petscInitialize argv opts help
  f
  petscFin

withPetsc' :: Argv -> OptsStr -> HelpStr -> IO a -> IO a
withPetsc' a o h = bracket_ (petscInitialize a o h) petscFin

-- -- NB : bracket_ ignores the return type of the allocation action




-- * error codes

-- -- PETSC_EXTERN PetscErrorCode PetscErrorMessage(int,const char*[],char **);

petscErrorMessage n =  do
  p <- withPtr ( pem n ) >>= handleErrTup 
  peekCString p where
    pem nn mp = [C.exp|int{PetscErrorMessage($(int nn), $(char** mp), NULL)} |]

-- PETSC_EXTERN PetscErrorCode PetscMemoryShowUsage(PetscViewer,const char[]);

-- pmsu v c = [C.exp|int{PetscMemoryShowUsage($())}|]

-- petscMemoryShowUsage v = do
--   p <- withPtr


-- alloca $ \stringptr -> do
--    ... call some Ptr CString function
--    peek stringptr






  

-- * MPI


-- -- Comm
data Comm = Comm {unComm :: CInt} deriving (Eq, Show)



-- -- Rank
newtype Rank = MkRank { rankId :: CInt -- ^ Extract numeric value of the 'Rank'
                      }
   deriving (Eq, Ord, Enum, Integral, Real, Show)

instance Num Rank where
  (MkRank x) + (MkRank y) = MkRank (x+y)
  (MkRank x) * (MkRank y) = MkRank (x*y)
  abs (MkRank x) = MkRank (abs x)
  signum (MkRank x) = MkRank (signum x)
  fromInteger x
    | x >  fromIntegral (maxBound :: CInt) = error "Rank value does not fit into 32 bits"
    | x < 0             = error "Negative Rank value"
    | otherwise         = MkRank (fromIntegral x)





-- mpiCommSize c = withPtr $ \p -> [C.exp|int{ MPI_Comm_Size($(int c), $(PetscMPIInt_ *p)) }|] 

mpiCommSize' comm = withPtr (\p -> [C.exp| int{ MPI_Comm_size($(int c), $(int *p))}|] ) >>= handleErrTup
  where
   c = unComm comm
mpiCommSize c =  unsafePerformIO $ mpiCommSize' c 

mpiCommRank' comm = withPtr (\p -> [C.exp| int{ MPI_Comm_rank($(int c), $(int *p))}|] ) >>= handleErrTup
  where
   c = unComm comm
mpiCommRank c = MkRank $ unsafePerformIO $ mpiCommRank' c 


{-# NOINLINE commWorld #-}
commWorld = Comm $ unsafePerformIO [C.exp| int{ MPI_COMM_WORLD }  |] 
commSelf = Comm $ unsafePerformIO [C.exp| int{ MPI_COMM_SELF }  |]



    -- PetscPrintf - Prints to standard out, only from the first
    -- processor in the communicator. Calls from other processes are ignored.
petscPrintf comm s =
  withCString s
   ( \s_ -> [C.exp|int{PetscPrintf($(int c), $(char* s_))}|] ) -- >>= handleErr
  where
    c = unComm comm


petscSynchronizedPrintf comm s = withCString s ( \s_ ->
  [C.exp|int{PetscSynchronizedPrintf($(int c), $(char* s_))}|] ) >>= handleErr
    where c = unComm comm

-- petscSynchronizedFlushStdout comm =
--   [C.exp|int{PetscSynchronizedFlush($(int c),PETSC_STDOUT )}|]
--     where c = unComm comm

petscSynchronizedFlushStdout comm =
  [C.exp|int{PetscSynchronizedFlush($(int c), 0 )}|]
    where c = unComm comm


syncPrintf c s =
  petscSynchronizedPrintf c s >> petscSynchronizedFlushStdout c >>= handleErr


-- * misc parallelism stuff

localRange :: Comm -> Int -> (Int, Int)
localRange c m = ( istart, iend) where
  istart = cr * (m `div` cs) + if t < cr then t else cr
  iend = istart + (m `div` cs) + if t > cr then 1 else 0
  cr = fromIntegral $ rankId ( mpiCommRank c )
  cs = fromIntegral $ mpiCommSize c
  t = m `mod` cs

  -- start = rank*(mm /size) + ((mm %size) < rank ? (mm %size) : rank);
  -- end   = start + mm /size + ((mm %size) > rank);




-- * CStrings

withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings ss f = case ss of
  [] -> f []
  (s:ss') -> withCString s $ \cs -> 
    withCStrings ss' $ \css -> f (cs:css)

withCStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray ss f = withCStrings ss $ \css -> withArray css f

withCStringArrayPtr :: [String] -> (Ptr (Ptr CString) -> IO a) -> IO a
withCStringArrayPtr ss f = withCStringArray ss $ \css -> with css f


-- *

vFromC l p = do
  ptr <- newForeignPtr_ p
  V.freeze $ VM.unsafeFromForeignPtr0 ptr l 

-- vectorFromC :: Storable a => Int -> Ptr a -> IO (V.Vector a)
-- vectorFromC len ptr = do
--   ptr' <- newForeignPtr_ ptr
--   V.freeze $ VM.unsafeFromForeignPtr0 ptr' len

-- vectorToC :: Storable a => V.Vector a -> Int -> Ptr a -> IO ()
-- vectorToC vec len ptr = do
--   ptr' <- newForeignPtr_ ptr
--   V.copy (VM.unsafeFromForeignPtr0 ptr' len) vec










-- indexing


  










-- * misc

linspace' n a b = take n [a, a + dt .. ] where
  dt = (b-a) / fromIntegral n

linspace1 n a b = mv  where
  mv1 = take n [a, a+dt ..]
  mv2 = reverse $ take n [b, b-dt ..]
  mv = Data.List.map (\(x,y) -> 1/2 * (x+y)) $ zip mv1 mv2
  dt = (b-a) / fromIntegral n

mean x = sum x / fromIntegral (length x)


-- -- tuple unpacking stuff

-- fst2 :: (a, (b, c)) -> (a,b)
fst2 = fst . snd
-- snd2 :: (a, (b, c)) -> c
snd2 =  snd . snd

both (a, b) f = (f a, f b)
all3 (a,b,c) f = (f a, f b, f c)
bothM t f = return (both t f)

-- nul = [C.exp| void*{NULL}|]

-- misteryf0 m n = [C.exp|int{ $(int m) % $(int n)   }|] -- `mod`
-- misteryf1 m n = [C.exp|int{ $(int m) / $(int n)  }|]  -- `div`

-- -- utils

-- isSorted x = all (\(a,b) -> a <= b) $ zip x (tail x)


fiInt :: CInt -> Int
fiInt = fromIntegral

fromIntegralTup t = both t fiInt
fromIntegralTup2 t = both t fromIntegralTup
fromIntegralTup3 t = both t (`all3` fiInt)




