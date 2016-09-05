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

import           Numerical.PETSc.Internal.InlineCInternal
import           Numerical.PETSc.Internal.Storable.Common (unsafeWithVS)
import           Numerical.PETSc.Internal.Types
import           Numerical.PETSc.Internal.C2HsGen.TypesC2HsGen
import           Numerical.PETSc.Internal.Utils

import           Language.C.Inline                 as C
-- import           Language.C.Inline.Context
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

import qualified Data.Vector              as V
import qualified Data.Vector.Generic              as VG
import qualified Data.Vector.Storable              as VS
import qualified Data.Vector.Storable.Mutable      as VM

import           System.IO.Unsafe                  (unsafePerformIO)


-- | Haskell-C type map

context petscCtx


-- | PETSc headers
C.include "<petscdm.h>"
C.include "<petscdmda.h>"
C.include "<petscdmcomposite.h>"
C.include "<petscts.h>"
C.include "<petscviewer.h>"
C.include "<petscviewerhdf5.h>"
C.include "<petscsys.h>"
C.include "<petsctime.h>"
C.include "<petscpctypes.h>"
C.include "<petscsf.h>"
C.include "<petscsnes.h>"
C.include "<petsctao.h>"
C.include "<petsctaolinesearch.h>"

-- | SLEPc headers
C.include "<slepceps.h>"
C.include "<slepcsvd.h>"
C.include "<slepcsys.h>"




-- petscDecide = -1

-- petscDefault = [C.exp|int{PETSC_DEFAULT}|]

-- * IS

isCreateStride_ :: CInt -> PetscInt_ -> PetscInt_ -> PetscInt_ -> Ptr IS -> IO CInt
isCreateStride_ c n first step is = [C.exp|
     int{ISCreateStride(
            $(int c),
            $(PetscInt n),
            $(PetscInt first),
            $(PetscInt step),
            $(IS* is)) }|]

isCreateStride' :: Comm -> PetscInt_ -> PetscInt_ -> PetscInt_ -> IO (IS, CInt)
isCreateStride' cc n first step =
  withPtr $ \is -> isCreateStride_ c n first step is
   where c = unComm cc






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

isCreateGeneral_ ::
  CInt -> PetscInt_ -> Ptr PetscInt_ -> CInt -> Ptr IS -> IO CInt
isCreateGeneral_ c n idxp mo isp  =
  [C.exp|int{ISCreateGeneral($(int c),
                             $(PetscInt n),
                             $(PetscInt* idxp),
                             $(int mo),
                             $(IS* isp))}|]

isCreateGeneral' ::
  Comm -> PetscInt_ -> [PetscInt_] -> PetscCopyMode_ -> IO (IS, CInt)
isCreateGeneral' com n idx mode =
   withArray idx $ \idxp ->
    withPtr $ \isp -> isCreateGeneral_ c n idxp mo isp 
     where mo = fromIntegral $ petscCopyModeToInt mode
           c = unComm com

isDestroy' :: IS -> IO CInt
isDestroy' iis = with iis isd where
  isd iisp = [C.exp|int{ISDestroy($(IS* iisp))} |]



-- withIsCreateGeneral comm n idx mode = bracket (isCreateGeneral comm n idx mode) isDestroy




-- -- * IS coloring : see e.g. www.mcs.anl.gov/petsc/petsc-current/src/snes/examples/tutorials/ex5s.c.html

-- PetscErrorCode  ISColoringCreate(MPI_Comm comm,PetscInt ncolors,PetscInt n,const ISColoringValue colors[],PetscCopyMode mode,ISColoring *iscoloring)
isColoringCreate' :: Comm
                           -> CInt
                           -> CInt
                           -> [CInt]
                           -> PetscCopyMode_
                           -> IO (ISColoring, CInt)
isColoringCreate' cc ncolors n cols copymode =
   withPtr $ \iscoloring ->
    withArray cols $ \colv -> 
  [C.exp|int{ISColoringCreate($(int c),$(int ncolors),$(int n),$(const int* colv),$(int mo),$(ISColoring* iscoloring))}|]
     where
       c = unComm cc
       mo = toCInt $ petscCopyModeToInt copymode


-- PetscErrorCode  ISColoringDestroy(ISColoring *iscoloring)
isColoringDestroy' :: ISColoring -> IO CInt
isColoringDestroy' isc = with isc $ \iscp -> [C.exp|int{ISColoringDestroy($(ISColoring* iscp))}|]









-- * PetscSF
-- "star forest" communicator pattern

-- PetscErrorCode PetscSFCreate(MPI_Comm comm,PetscSF *sf)
petscSFCreate' :: Comm -> IO (PetscSF, CInt)
petscSFCreate' cc = withPtr $ \sf ->
  [C.exp|int{PetscSFCreate($(int co),$(PetscSF* sf))}|] where co = unComm cc


petscSFDestroy' :: PetscSF -> IO CInt
petscSFDestroy' sf = with sf $ \sfp -> [C.exp|int{PetscSFDestroy($(PetscSF* sfp))}|]

-- PetscErrorCode PetscSFSetGraph(PetscSF sf,PetscInt nroots,PetscInt nleaves,const PetscInt *ilocal,PetscCopyMode localmode,const PetscSFNode *iremote,PetscCopyMode remotemode)
-- Collective
-- Input Arguments :
-- sf	- star forest
-- nroots	- number of root vertices on the current process (these are possible targets for other process to attach leaves)
-- nleaves	- number of leaf vertices on the current process, each of these references a root on any process
-- ilocal	- locations of leaves in leafdata buffers, pass NULL for contiguous storage
-- localmode	- copy mode for ilocal
-- iremote	- remote locations of root vertices for each leaf on the current process
-- remotemode	- copy mode for iremote
petscSFSetGraph' sf nroots nleaves ilocal localmode iremote remotemode =
  [C.exp|int{PetscSFSetGraph($(PetscSF sf), $(PetscInt nroots), $(PetscInt nleaves), $(const PetscInt* ilocal), $(int localmodep), $(const PetscSFNode* iremote), $(int remotemodep))}|] where
  localmodep = toCInt $ petscCopyModeToInt localmode
  remotemodep = toCInt $ petscCopyModeToInt remotemode









-- * Vec

-- PetscErrorCode  VecView(Vec vec,PetscViewer viewer)
vecView' :: Vec -> PetscViewer -> IO CInt
vecView' ve viewer =
  [C.exp|int{VecView($(Vec ve),$(PetscViewer viewer))}|]
  

-- PetscErrorCode  PetscObjectSetName(PetscObject obj,const char name[])
vecSetName1 :: Vec -> String -> IO CInt
vecSetName1 v name = withCString name $ \n ->
  [C.exp|int{PetscObjectSetName($(Vec v),$(const char* n))}|]


-- PetscErrorCode  VecLoad(Vec newvec, PetscViewer viewer)
vecLoad' :: Vec -> PetscViewer -> IO CInt
vecLoad' v viewer = [C.exp|int{VecLoad($(Vec v),$(PetscViewer viewer))}|]



vecCreate' :: Comm -> IO (Vec, CInt)
vecCreate' cc = withPtr $ \p -> [C.exp|int{VecCreate($(int c), $(Vec *p))} |]
   where c = unComm cc

         
-- PetscErrorCode VecCreateMPI(MPI_Comm comm, int m, int M, Vec* x)



vecCreateMPI' :: Comm -> Int -> Int -> IO (Vec, CInt)
vecCreateMPI' co nlocal nglobal = withPtr $ \p -> [C.exp|int{VecCreateMPI($(int c), $(int m1), $(int m2), $(Vec *p))}|] 
      where c = unComm co
            m1 = toCInt nlocal
            m2 = toCInt nglobal

-- vecCreateMPILocal c m = vecCreateMPI' c m m



-- -- have PETSc decide the local Vec dimension

-- PetscErrorCode VecCreateMPI(MPI_Comm comm, int m, int M, Vec* x)
vecCreateMPIdecideLoc0' :: Comm -> Int -> Ptr Vec -> IO CInt
vecCreateMPIdecideLoc0' cc nglob p =
  [C.exp|int{VecCreateMPI($(int c), PETSC_DECIDE, $(int m1), $(Vec *p))}|] 
    where c = unComm cc
          m1 = toCInt nglob

vecCreateMPIdecideLoc' :: Comm -> Int -> IO (Vec, CInt)
vecCreateMPIdecideLoc' cc nglob = withPtr (vecCreateMPIdecideLoc0' cc nglob)







-- PetscErrorCode  VecSetBlockSize(Vec v,PetscInt bs)
vecSetBlockSize1 :: Vec -> Int -> IO CInt
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
vecSetValues' x ni ixx y imm =
  [C.exp|int{VecSetValues($(Vec x),
                          $(int ni),
                          $(const int* ixx),
                          $(const PetscScalar* y),
                          $(int im))}|]
  where im = fromIntegral $ insertModeToInt imm



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
vecEqual' :: Vec -> Vec -> IO (PetscBool, CInt)
vecEqual' v1 v2 = withPtr ( \b ->
  [C.exp|int{VecEqual($(Vec v1), $(Vec v2), $(PetscBool* b))}|] )





vecDestroy' :: Vec -> IO CInt
vecDestroy' p = with p vd0 where
  vd0 pp = [C.exp|int{VecDestroy($(Vec *pp))}|]


vecCopy1 :: Vec -> Vec -> IO CInt
vecCopy1 vorig vcopy = [C.exp|int{VecCopy($(Vec vorig), $(Vec vcopy))}|] 

-- -- NB : VecDuplicate DOES NOT COPY CONTENTS (only structure): use VecCopy
-- PetscErrorCode  VecDuplicate(Vec v,Vec *newv)

vecDuplicate1 :: Vec -> IO (Vec, CInt)
vecDuplicate1 v = withPtr (vdup v) where
  vdup p1 p2 = [C.exp| int{VecDuplicate($(Vec p1), $(Vec *p2))}|] 


vecAssemblyBegin' :: Vec -> IO CInt
vecAssemblyBegin' v = [C.exp|int{VecAssemblyBegin($(Vec v))}|]

vecAssemblyEnd' :: Vec -> IO CInt
vecAssemblyEnd' v = [C.exp|int{VecAssemblyEnd($(Vec v))}|] 

-- vecAssembly1 v = vecAssemblyBegin v >> vecAssemblyEnd v


vecSet1 :: Vec -> PetscScalar_ -> IO CInt
vecSet1 v n = [C.exp|int{VecSet( $(Vec v), $(PetscScalar n))}|] 

vecSetSizes1 :: Vec -> CInt -> IO CInt
vecSetSizes1 v n = [C.exp|int{VecSetSizes( $(Vec v), PETSC_DECIDE, $(int n))}|] 


-- | get Vec length

-- PETSC_EXTERN PetscErrorCode VecGetSize(Vec,PetscInt*);


vecGetSize' :: Vec -> IO (CInt, CInt)
vecGetSize' v = withPtr $ \p -> vgs0 v p where
  vgs0 vv pp =  [C.exp|int{VecGetSize($(Vec vv), $(int *pp))}|]

vecGetSizeUnsafe' :: Vec -> (CInt, CInt)
vecGetSizeUnsafe' = unsafePerformIO . vecGetSize'

vecSize' :: Vec -> Int
vecSize' = fi . fst . vecGetSizeUnsafe'



-- | view Vec values on stdout

vecViewStdout1 :: Vec -> IO CInt
vecViewStdout1 v = [C.exp|int{VecView($(Vec v), PETSC_VIEWER_STDOUT_SELF)}|] 





-- PETSC_EXTERN PetscErrorCode VecGetArray(Vec,PetscScalar**);
vecGetArray' :: Vec -> IO (Ptr PetscScalar_, CInt)
vecGetArray' v = withPtr $ \p ->
  [C.exp|int{VecGetArray($(Vec v), $(PetscScalar** p))}|] 



-- PetscErrorCode VecGetArrayRead(Vec x,const PetscScalar **a)
vecGetArrayRead' :: Vec -> IO (Ptr PetscScalar_, CInt)
vecGetArrayRead' v = withPtr $ \p ->
  [C.exp|int{VecGetArrayRead($(Vec v), $(const PetscScalar** p))}|]







-- funIO :: (Storable a, Storable b) =>
--          (V.Vector a -> V.Vector b) ->
--          Int -> Ptr a -> Ptr b ->
--          IO ()
-- funIO fun dim y f = do
-- --         -- Convert the pointer we get from C (y) to a vector, and then
-- --         -- apply the user-supplied function.
--         fImm <- fun <$> vectorFromC dim y
-- --         -- Fill in the provided pointer with the resulting vector.
--         vectorToC0 fImm dim f





-- PETSC_EXTERN PetscErrorCode VecRestoreArray(Vec,PetscScalar**);
vecRestoreArray' :: Vec -> Ptr PetscScalar_ -> IO CInt
vecRestoreArray' v c = with c $ \pc ->
  [C.exp|int{VecRestoreArray($(Vec v), $(PetscScalar** pc))}|]

-- vecRestoreArray' :: Vec -> [PetscScalar_] -> IO CInt
-- vecRestoreArray' v c = withArray c $ \cp ->
--   with cp $ \cpp -> vecRestoreArray0' v cpp

-- vecRestoreArrayPtr2' :: Vec -> Ptr PetscScalar_ -> IO CInt
-- vecRestoreArrayPtr2' v c = with c $ \pc -> vra v pc
--   where
--     vra w pc = [C.exp|int{VecRestoreArray($(Vec w), $(PetscScalar** pc))}|]





-- PetscErrorCode VecRestoreArrayRead(Vec x,const PetscScalar **a)
vecRestoreArrayRead' :: Vec -> Ptr PetscScalar_ -> IO CInt
vecRestoreArrayRead' v p = with p $ \pc ->
  [C.exp|int{VecRestoreArrayRead($(Vec v), $(const PetscScalar** pc))}|]




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

vecGetArray1d' :: Vec -> CInt -> CInt -> IO (Ptr PetscScalar_, CInt)
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

vecRestoreArray1d' :: Vec -> CInt -> CInt -> Ptr (Ptr PetscScalar_) -> IO CInt
vecRestoreArray1d' x m mstart arr =
  [C.exp|int{VecRestoreArray1d($(Vec x),$(int m),$(int mstart),$(PetscScalar** arr))}|]






-- TODO row (block) indexing : these should not be interpreted as mere Ints but as indices, e.g. FEM mesh nodes -- see repa 

vecGetOwnershipRange1 :: Vec -> IO ((CInt, CInt), CInt)
vecGetOwnershipRange1 a = withPtr2 $ \rmin rmax -> 
       [C.exp|int{VecGetOwnershipRange($(Vec a), $(PetscInt *rmin), $(PetscInt * rmax) )}|] 
    



-- | misc. math functions on Vec

vecDot1 :: Vec -> Vec -> IO (PetscScalar_, CInt)
vecDot1 v1 v2 = withPtr (vdot v1 v2) where
  vdot vv1 vv2 v = [C.exp|int{VecDot( $(Vec vv1), $(Vec vv2), $(PetscScalar * v))}|] 



-- PETSC_EXTERN PetscErrorCode VecNorm(Vec,NormType,PetscReal *);

vecNorm1 :: VecNorm_ -> Vec -> IO (PetscReal_, CInt)
vecNorm1 nt v = withPtr (vnorm nt v) where
  vnorm nnt vv p = [C.exp|int{VecNorm($(Vec vv),$(int nti),$(PetscReal* p))}|] where
    nti = fromIntegral $ vecNormToInt nnt
-- vecNorm v nt = unsafePerformIO $ withPtrHandleErr2 vecNorm' nt v


-- PETSC_EXTERN PetscErrorCode VecNormalize(Vec,PetscReal *);


-- PETSC_EXTERN PetscErrorCode VecSum(Vec,PetscScalar*);

vecSum1 :: Vec -> IO (PetscScalar_, CInt)
vecSum1 v = withPtr (vs v) where
  vs vv p = [C.exp|int{VecSum($(Vec vv), $(PetscScalar* p))}|]
-- vecSum v = unsafePerformIO $ withPtrHandleErr1 vecSum' v

-- PETSC_EXTERN PetscErrorCode VecMax(Vec,PetscInt*,PetscReal *);
vecMax' :: Vec -> Ptr PetscInt_ -> Ptr PetscReal_ -> IO CInt
vecMax' v i r = [C.exp|int{VecMax($(Vec v),$(PetscInt* i),$(PetscReal* r))}|]
-- PETSC_EXTERN PetscErrorCode VecMin(Vec,PetscInt*,PetscReal *);
vecMin' :: Vec -> Ptr PetscInt_ -> Ptr PetscReal_ -> IO CInt
vecMin' v i r = [C.exp|int{VecMin($(Vec v),$(PetscInt* i),$(PetscReal* r))}|]
-- PETSC_EXTERN PetscErrorCode VecScale(Vec,PetscScalar);
vecScale' :: Vec -> PetscScalar_ -> IO CInt
vecScale' v n = [C.exp|int{VecScale($(Vec v),$(PetscScalar n))}|]
-- PETSC_EXTERN PetscErrorCode VecPointwiseMax(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecPointwiseMaxAbs(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecPointwiseMin(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecPointwiseMult(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecPointwiseDivide(Vec,Vec,Vec);
-- PETSC_EXTERN PetscErrorCode VecMaxPointwiseDivide(Vec,Vec,PetscReal*);
-- PETSC_EXTERN PetscErrorCode VecShift(Vec,PetscScalar);
vecShift' :: Vec -> PetscScalar_ -> IO CInt
vecShift' v n = [C.exp|int{VecShift($(Vec v),$(PetscScalar n))}|]
-- PETSC_EXTERN PetscErrorCode VecReciprocal(Vec);
vecReciprocal' :: Vec -> IO CInt
vecReciprocal' v = [C.exp|int{VecReciprocal($(Vec v))}|]
-- PETSC_EXTERN PetscErrorCode VecPermute(Vec, IS, PetscBool );
vecPermute' :: Vec -> IS -> PetscBool -> IO CInt
vecPermute' v i b = [C.exp|int{VecPermute($(Vec v),$(IS i),$(PetscBool b))}|]
-- PETSC_EXTERN PetscErrorCode VecSqrtAbs(Vec);
-- PETSC_EXTERN PetscErrorCode VecLog(Vec);
vecLog' :: Vec -> IO CInt
vecLog' v = [C.exp|int{VecLog($(Vec v))}|]
-- PETSC_EXTERN PetscErrorCode VecExp(Vec);
vecExp' :: Vec -> IO CInt
vecExp' v = [C.exp|int{VecExp($(Vec v))}|]
-- PETSC_EXTERN PetscErrorCode VecAbs(Vec);
vecAbs' :: Vec -> IO CInt
vecAbs' v = [C.exp|int{VecAbs($(Vec v))}|]



-- PETSC_EXTERN PetscErrorCode VecAXPY(Vec,PetscScalar,Vec);
--    VecAXPY - Computes y = alpha x + y.
--    Notes: x and y MUST be different vectors
-- PetscErrorCode  VecAXPY(Vec y,PetscScalar alpha,Vec x)
vecAxpy' :: Vec -> PetscScalar_ -> Vec -> IO CInt
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
vecWaxpy' :: Vec -> PetscScalar_ -> Vec -> Vec -> IO CInt
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
matSetType' :: Mat -> MatType_ -> IO CInt
matSetType' m mt = withCString cs $ \c -> [C.exp|int{MatSetType($(Mat m), $(char *c))}|] 
  where cs = matTypeToStr mt




-- matCreate' c p = [C.exp| int{MatCreate($(int c), $(Mat *p))} |]
matCreate' :: Comm -> IO (Mat, CInt)
matCreate' = matCreate0' where
  matCreate0' cc = withPtr $ \p -> [C.exp| int{MatCreate($(int c), $(Mat *p))} |] 
    where c = unComm cc

matDestroy' :: Mat -> IO CInt
matDestroy' m = with m matDestroy0' where
  matDestroy0' mm = [C.exp|int{MatDestroy($(Mat *mm))}|]


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
-- PetscErrorCode  MatCreateSeqAIJ(MPI_Comm comm,PetscInt m,PetscInt n,PetscInt nz,const PetscInt nnz[],Mat *A)  -- Collective on MPI_Comm
-- Input Parameters :
-- comm	- MPI communicator, set to PETSC_COMM_SELF
-- m	- number of rows
-- n	- number of columns
-- nz	- number of nonzeros per row (same for all rows) (if nnz is given nz is ignored)
-- nnz	- array containing the number of nonzeros in the various rows (possibly different for each row) or NULL
-- Output Parameter :
-- A -the matrix 
matCreateSeqAIJ0' ::
  Comm -> PetscInt_ -> PetscInt_ -> PetscInt_ -> Ptr PetscInt_ -> IO (Mat, CInt)
matCreateSeqAIJ0' cc m n nz nnzp =
  withPtr (\mat ->
            [C.exp|int{MatCreateSeqAIJ($(int c),
                                       $(PetscInt m),
                                       $(PetscInt n),
                                       $(PetscInt nz),
                                       $(const PetscInt* nnzp),
                                       $(Mat *mat))}|])  where c = unComm cc

-- ", specialized for a constant # nonzeros / row

matCreateSeqAIJconstNZperRow1 :: Comm -> Int -> Int -> Int -> IO (Mat, CInt)
matCreateSeqAIJconstNZperRow1 cc m' n' nz' =
  withPtr (\mat ->
            [C.exp|int{MatCreateSeqAIJ($(int c),
                                       $(PetscInt m),
                                       $(PetscInt n),
                                       $(PetscInt nz),
                                       NULL ,
                                       $(Mat *mat))}|]) 
  where c = unComm cc
        (m, n, nz) = (toCInt m', toCInt n', toCInt nz')


matTranspose' :: Mat -> MatReuse_ -> IO (Mat, CInt)
matTranspose' mat mr =
  withPtr $ \mp -> [C.exp|int{MatTranspose($(Mat mat),$(int mr'),$(Mat* mp))}|]
   where mr' = toCInt $ matReuseToInt mr


-- PetscErrorCode  MatCreateTranspose(Mat A,Mat *N)
-- Collective on Mat
-- Input Parameter :
-- A -the (possibly rectangular) matrix 
-- Output Parameter :
-- N -the matrix that represents A' 
-- Notes: The transpose A' is NOT actually formed! Rather the new matrix object performs the matrix-vector product by using the MatMultTranspose() on the original matrix

matCreateTranspose' :: Mat -> IO (Mat, CInt)
matCreateTranspose' mat = withPtr $ \mp -> [C.exp|int{MatCreateTranspose($(Mat mat),$(Mat* mp))}|]


-- PETSC_EXTERN PetscErrorCode MatCreateAIJ(MPI_Comm,PetscInt,PetscInt,PetscInt,PetscInt,PetscInt,const PetscInt[],PetscInt,const PetscInt[],Mat*);
-- Input Parameters :
-- comm	- MPI communicator
-- m	- number of local rows (or PETSC_DECIDE to have calculated if M is given) This value should be the same as the local size used in creating the y vector for the matrix-vector product y = Ax.
-- n	- This value should be the same as the local size used in creating the x vector for the matrix-vector product y = Ax. (or PETSC_DECIDE to have calculated if N is given) For square matrices n is almost always m.
-- M	- number of global rows (or PETSC_DETERMINE to have calculated if m is given)
-- N	- number of global columns (or PETSC_DETERMINE to have calculated if n is given)
-- d_nz	- number of nonzeros per row in DIAGONAL portion of local submatrix (same value is used for all local rows)
-- d_nnz	- array containing the number of nonzeros in the various rows of the DIAGONAL portion of the local submatrix (possibly different for each row) or NULL, if d_nz is used to specify the nonzero structure. The size of this array is equal to the number of local rows, i.e 'm'.
-- o_nz	- number of nonzeros per row in the OFF-DIAGONAL portion of local submatrix (same value is used for all local rows).
-- o_nnz	- array containing the number of nonzeros in the various rows of the OFF-DIAGONAL portion of the local submatrix (possibly different for each row) or NULL, if o_nz is used to specify the nonzero structure. The size of this array is equal to the number of local rows, i.e 'm'.
-- Output Parameter
-- A -the matrix 
-- It is recommended that one use the MatCreate(), MatSetType() and/or MatSetFromOptions(), MatXXXXSetPreallocation() paradgm instead of this routine directly


matCreateAIJ0' :: Comm -> PetscInt_ -> PetscInt_ -> PetscInt_ -> PetscInt_
                        -> PetscInt_
                        -> Ptr PetscInt_
                        -> PetscInt_
                        -> Ptr PetscInt_
                        -> IO (Mat, CInt)
matCreateAIJ0' cc m n mm nn dnz dnnzp onz onnzp = withPtr ( \mat ->
  [C.exp|int{MatCreateAIJ($(int c),
                          $(PetscInt m), $(PetscInt n),
                          $(PetscInt mm), $(PetscInt nn),
                          $(PetscInt dnz), $(const PetscInt* dnnzp),
                          $(PetscInt onz), $(const PetscInt* onnzp),
                          $(Mat* mat))}|] ) where c = unComm cc
 
-- | matCreateAIJDecide' : matCreateAIJ inferring the local sizes :

matCreateAIJ0Decide' :: Comm -> PetscInt_ -> PetscInt_
                        -> PetscInt_
                        -> Ptr PetscInt_
                        -> PetscInt_
                        -> Ptr PetscInt_
                        -> IO (Mat, CInt)
matCreateAIJ0Decide' cc mm nn dnz dnnzp onz onnzp = withPtr ( \mat ->
  [C.exp|int{MatCreateAIJ($(int c),
                          PETSC_DECIDE, PETSC_DECIDE,
                          $(PetscInt mm), $(PetscInt nn),
                          $(PetscInt dnz), $(const PetscInt* dnnzp),
                          $(PetscInt onz), $(const PetscInt* onnzp),
                          $(Mat* mat))}|] ) where c = unComm cc

-- | matCreateAIJDecideConstNZPR' : matCreateAIJ inferring the local sizes, assuming a constant # of nonzeros per row, both on- and off- diagonal :

matCreateAIJ0DecideConstNZPR' :: Comm -> PetscInt_ -> PetscInt_
                        -> PetscInt_
                        -> PetscInt_
                        -> IO (Mat, CInt)
matCreateAIJ0DecideConstNZPR' cc mm nn dnz onz = withPtr ( \mat ->
  [C.exp|int{MatCreateAIJ($(int c),
                          PETSC_DECIDE, PETSC_DECIDE,
                          $(PetscInt mm), $(PetscInt nn),
                          $(PetscInt dnz), NULL,
                          $(PetscInt onz), NULL,
                          $(Mat* mat))}|] ) where c = unComm cc

-- | matCreateAIJDecideConstNZPR' : matCreateAIJ inferring the local sizes, assuming a _variable_ # of nonzeros per row, both on- and off- diagonal :

matCreateAIJ0DecideVarNZPR' :: Comm -> PetscInt_ -> PetscInt_
                        -> VS.Vector PetscInt_
                        -> VS.Vector PetscInt_
                        -> IO (Mat, CInt)
matCreateAIJ0DecideVarNZPR' cc mm nn dnnz onnz = withPtr ( \mat ->
    VS.unsafeWith dnnz $ \dnnzp ->
    VS.unsafeWith onnz $ \onnzp ->
         [C.exp|int{MatCreateAIJ($(int c),
                          PETSC_DECIDE, PETSC_DECIDE,
                          $(PetscInt mm), $(PetscInt nn),
                          NULL, $(const PetscInt* dnnzp),
                          NULL, $(const PetscInt* onnzp),
                          $(Mat* mat))}|] ) where c = unComm cc
                                                  
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

matCreateMPIAIJWithArrays0' :: Comm
                                     -> PetscInt_
                                     -> PetscInt_
                                     -> PetscInt_
                                     -> PetscInt_
                                     -> Ptr PetscInt_
                                     -> Ptr PetscInt_
                                     -> Ptr PetscScalar_
                                     -> IO (Mat, CInt)
matCreateMPIAIJWithArrays0' cc m n mm nn ip jp aap =
  withPtr ( \mat -> [C.exp|int{MatCreateMPIAIJWithArrays($(PetscInt c),
                                    $(PetscInt m),
                                    $(PetscInt n),
                                    $(PetscInt mm), $(PetscInt nn),
                                    $(const PetscInt* ip), $(const PetscInt* jp),
                                    $(const PetscScalar* aap), 
                                    $(Mat* mat))}|] )
    where c = unComm cc 


matView' :: Mat -> PetscViewer -> IO CInt
matView' m v = [C.exp|int{MatView($(Mat m),$(PetscViewer v))}|]

-- matViewStdoutSelf' :: Mat -> IO CInt
-- matViewStdoutSelf' v = [C.exp|int{MatView($(Mat v), PETSC_VIEWER_STDOUT_SELF)}|]

-- matViewStdoutWorld' :: Mat -> IO CInt
-- matViewStdoutWorld' v = [C.exp|int{MatView($(Mat v), PETSC_VIEWER_STDOUT_WORLD)}|]


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









-- PetscErrorCode  MatCreateVecs(Mat mat,Vec *right,Vec *left)
-- Get vector(s) compatible with the matrix, i.e. with the same parallel layout
-- Collective on Mat
-- Input Parameter :
-- mat -the matrix 
-- Output Parameter :
-- right	- (optional) vector that the matrix can be multiplied against
-- left	- (optional) vector that the matrix vector product can be stored in
-- Notes :
-- The blocksize of the returned vectors is determined by the row and column block sizes set with MatSetBlockSizes() or the single blocksize (same for both) set by MatSetBlockSize().
-- Notes: These are new vectors which are not owned by the Mat, they should be destroyed in VecDestroy() when no longer needed
matCreateVecs' :: Mat -> IO ((Vec, Vec), CInt)
matCreateVecs' m = withPtr2 $ \vright vleft ->
  [C.exp|int{MatCreateVecs($(Mat m),$(Vec* vright),$(Vec* vleft))}|]

matCreateVecLeft' :: Mat -> IO (Vec, CInt)
matCreateVecLeft' m = withPtr $ \vleft ->
  [C.exp|int{MatCreateVecs($(Mat m), NULL ,$(Vec* vleft))}|]

matCreateVecRight' :: Mat -> IO (Vec, CInt)
matCreateVecRight' m = withPtr $ \vright ->
  [C.exp|int{MatCreateVecs($(Mat m), $(Vec* vright), NULL)}|]




-- PETSC_EXTERN PetscErrorCode MatGetSize(Mat,PetscInt*,PetscInt*);


matGetSize' :: Mat -> IO ((CInt, CInt), CInt)
matGetSize' m = withPtr2 $ \sx sy ->
  [C.exp|int{MatGetSize($(Mat m), $(int *sx), $(int *sy))}|]
   
matGetSizeUnsafeCInt' :: Mat -> ((CInt, CInt), CInt)
matGetSizeUnsafeCInt' = unsafePerformIO . matGetSize'

-- matGetSizeUnsafe' :: Mat -> (Int, Int)
-- matGetSizeUnsafe' m = (fi a', fi b') where
--   (a', b') = matGetSizeUnsafeCInt m

-- withMatSize mat f = f (matGetSizeUnsafeCInt mat)

-- f' g h = g . fst &&& h. snd
-- f'' g = f' g g

matSetFromOptions :: Mat -> IO CInt
matSetFromOptions p = [C.exp| int{MatSetFromOptions($(Mat p))} |] 




-- PetscErrorCode  MatSeqAIJSetPreallocation(Mat B,PetscInt nz,const PetscInt nnz[])
-- -- Collective on MPI_Comm, CSR format
-- -- Input Parameters

-- -- B	- The matrix
-- -- nz	- number of nonzeros per row (same for all rows)
-- -- nnz	- array containing the number of nonzeros in the various rows (possibly different for each row) or NULL
-- -- -- NB : If nnz is given then nz is ignored


matSeqAIJSetPreallocationConstNZPR' :: Mat -> CInt -> IO CInt
matSeqAIJSetPreallocationConstNZPR' mat nz =
                     [C.exp|int{MatSeqAIJSetPreallocation( $(Mat mat),
                                                           $(int nz),
                                                           NULL)} |]

matSeqAIJSetPreallocationVarNZPR' :: Mat -> VS.Vector CInt -> IO CInt
matSeqAIJSetPreallocationVarNZPR' mat nnz =
                     [C.exp|int{MatSeqAIJSetPreallocation( $(Mat mat),
                                                           NULL,
                                                           $vec-ptr:(int *nnz))} |]

-- PetscErrorCode  MatMPIAIJSetPreallocation(Mat B,PetscInt d_nz,const PetscInt d_nnz[],PetscInt o_nz,const PetscInt o_nnz[])  -- Collective on MPI_Comm
-- Input Parameters :
-- B	- the matrix
-- d_nz	- number of nonzeros per row in DIAGONAL portion of local submatrix (same value is used for all local rows)
-- d_nnz	- array containing the number of nonzeros in the various rows of the DIAGONAL portion of the local submatrix (possibly different for each row) or NULL (PETSC_NULL_INTEGER in Fortran), if d_nz is used to specify the nonzero structure. The size of this array is equal to the number of local rows, i.e 'm'. For matrices that will be factored, you must leave room for (and set) the diagonal entry even if it is zero.
-- o_nz	- number of nonzeros per row in the OFF-DIAGONAL portion of local submatrix (same value is used for all local rows).
-- o_nnz	- array containing the number of nonzeros in the various rows of the OFF-DIAGONAL portion of the local submatrix (possibly different for each row) or NULL (PETSC_NULL_INTEGER in Fortran), if o_nz is used to specify the nonzero structure. The size of this array is equal to the number of local rows, i.e 'm'.
-- If the *_nnz parameter is given then the *_nz parameter is ignored


-- | ", constant # zeros per row

matMPIAIJSetPreallocationConstNZPR' :: Mat -> CInt -> CInt -> IO CInt
matMPIAIJSetPreallocationConstNZPR' b dnz onz = [C.exp|int{MatMPIAIJSetPreallocation($(Mat b),
                                       $(int dnz),
                                       NULL,
                                       $(int onz),
                                       NULL)}|]

-- | ", variable # zeros per row

matMPIAIJSetPreallocationVarNZPR' ::
  Mat -> VS.Vector CInt -> VS.Vector CInt -> IO CInt
matMPIAIJSetPreallocationVarNZPR' b dnnz onnz = [C.exp|int{MatMPIAIJSetPreallocation($(Mat b),
                                       NULL,
                                       $vec-ptr:(const int* dnnz),
                                       NULL,
                                       $vec-ptr:(const int* onnz))}|]






-- PetscErrorCode MatSetValue(Mat m,PetscInt row,PetscInt col,PetscScalar value,InsertMode mode)
matSetValueUnsafe' :: Mat -> Int -> Int -> PetscScalar_ -> InsertMode_ -> IO CInt
matSetValueUnsafe' m row col val im =
  [C.exp|int{MatSetValue($(Mat m),$(int rowc),$(int colc),$(PetscScalar val),$(int imm))}|] where
    imm = fromIntegral $ insertModeToInt im
    rowc = toCInt row
    colc = toCInt col

-- PETSC_EXTERN PetscErrorCode MatZeroEntries(Mat);
matZeroEntries' :: Mat -> IO CInt
matZeroEntries' mat = [C.exp|int{MatZeroEntries($(Mat mat))}|]



-- PetscErrorCode  MatSetValues(Mat mat,PetscInt m,const PetscInt idxm[],PetscInt n,const PetscInt idxn[],const PetscScalar v[],InsertMode addv) -- Not Collective
-- Input Parameters :
-- mat	- the matrix
-- v	- a logically two-dimensional array of values
-- m, idxm	- the number of rows and their global indices
-- n, idxn	- the number of columns and their global indices
-- addv	- either ADD_VALUES or INSERT_VALUES, where ADD_VALUES adds values to any existing entries, and INSERT_VALUES replaces existing entries with new values
-- Notes :
-- If you create the matrix yourself (that is not with a call to DMCreateMatrix()) then you MUST call MatXXXXSetPreallocation() or MatSetUp() _before_ using this routine
-- By default the values, v, are row-oriented. See MatSetOption() for other options.
-- Calls to MatSetValues() with the INSERT_VALUES and ADD_VALUES options cannot be mixed without intervening calls to the assembly routines.
-- MatSetValues() uses 0-based row and column numbers in Fortran as well as in C.
-- Negative indices may be passed in idxm and idxn, these rows and columns are simply ignored. This allows easily inserting element stiffness matrices with homogeneous Dirchlet boundary conditions that you don't want represented in the matrix


matSetValues0' ::
  Mat -> CInt -> Ptr CInt -> CInt -> Ptr CInt -> Ptr PetscScalar_ -> InsertMode_ -> IO CInt
matSetValues0' mat nbx idxx_ nby idxy_ b_ im =
           [C.exp|int { MatSetValues($(Mat mat),
                                     $(int nbx),
                                     $(const int* idxx_),
                                     $(int nby),
                                     $(const int* idxy_),
                                     $(const PetscScalar* b_), $(int imm))} |] where
              imm = fromIntegral $ insertModeToInt im

-- " , using vecCtx adapters from inline-c

matSetValues0vs' ::
  Mat -> CInt -> VS.Vector CInt -> CInt -> VS.Vector CInt -> VS.Vector PetscScalar_ -> InsertMode_ -> IO CInt
matSetValues0vs' mat nbx idxx_ nby idxy_ b_ im =
           [C.exp|int { MatSetValues($(Mat mat),
                                     $(int nbx),
                                     $vec-ptr:(const int* idxx_),
                                     $(int nby),
                                     $vec-ptr:(const int* idxy_),
                                     $vec-ptr:(const PetscScalar* b_),
                                     $(int imm))} |] where
              imm = fromIntegral $ insertModeToInt im
              







-- PetscErrorCode  MatSetBlockSize(Mat mat,PetscInt bs)
matSetBlockSize' :: Mat -> Int -> IO CInt
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

matSetValuesBlocked0' ::
  Mat -> CInt -> Ptr CInt -> CInt -> Ptr CInt -> Ptr PetscScalar_ -> InsertMode_ -> IO CInt
matSetValuesBlocked0' mat m idxm n idxn v imode =
  [C.exp|int{MatSetValuesBlocked($(Mat mat),
                                 $(int m),
                                 $(const int* idxm),
                                 $(int n),
                                 $(const int* idxn),
                                 $(const PetscScalar* v),
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




matAssemblyBegin' :: Mat -> IO CInt
matAssemblyBegin' a = [C.exp|int{MatAssemblyBegin($(Mat a), MAT_FINAL_ASSEMBLY )}|]

matAssemblyEnd' :: Mat -> IO CInt
matAssemblyEnd' a = [C.exp|int{MatAssemblyEnd($(Mat a), MAT_FINAL_ASSEMBLY )}|]

-- matAssembly =
--   matAssemblyBegin >> matAssemblyEnd

matSetup' :: Mat -> IO CInt
matSetup' a = [C.exp|int{MatSetUp($(Mat a))}|] 



-- TODO row (block) indexing : these should not be interpreted as mere Ints but as indices, e.g. FEM mesh nodes

-- PETSC_EXTERN PetscErrorCode MatGetOwnershipRange(Mat,PetscInt*,PetscInt*);

matGetOwnershipRange' :: Mat -> IO ((PetscInt_, PetscInt_), CInt)
matGetOwnershipRange' m = withPtr2 ( \rmin rmax -> 
       [C.exp|int{MatGetOwnershipRange($(Mat m), $(PetscInt *rmin), $(PetscInt *rmax) )}|] ) 


-- PetscErrorCode  MatGetInfo(Mat mat,MatInfoType flag,MatInfo *info)
matGetInfo' :: Mat -> MatInfoType_ -> IO (MatInfo, CInt)
matGetInfo' mat t = withPtr $ \info -> [C.exp|int{MatGetInfo($(Mat mat),$(int inft),$(MatInfo* info))}|]  where inft = toCInt $ matInfoTypeToInt t
-- Collective on Mat if MAT_GLOBAL_MAX or MAT_GLOBAL_SUM is used as the flag
-- Input Parameters :
-- mat -the matrix 
-- Output Parameters :
-- flag	- flag indicating the type of parameters to be returned (MAT_LOCAL - local matrix, MAT_GLOBAL_MAX - maximum over all processors, MAT_GLOBAL_SUM - sum over all processors)
-- info	- matrix information context
-- Notes :
-- The MatInfo context contains a variety of matrix data, including number of nonzeros allocated and used, number of mallocs during matrix assembly, etc. Additional information for factored matrices is provided (such as the fill ratio, number of mallocs during factorization, etc.). Much of this info is printed to PETSC_STDOUT when using the runtime options
--       -info -mat_view ::ascii_info


-- PetscErrorCode MatGetRow(Mat mat,PetscInt row,PetscInt *ncols,const PetscInt *cols[],const PetscScalar *vals[])
matGetRow0' :: Mat -> CInt -> Ptr CInt -> Ptr (Ptr CInt) -> Ptr (Ptr PetscScalar_) -> IO CInt
matGetRow0' mat row ncols cols vals =
  [C.exp|int{MatGetRow($(Mat mat),$(int row),$(int* ncols),$(const int** cols),$(const PetscScalar** vals))}|]
-- Input Parameters :
-- mat	- the matrix
-- row	- the row to get
-- Output Parameters :
-- ncols	- if not NULL, the number of nonzeros in the row
-- cols	- if not NULL, the column numbers
-- vals	- if not NULL, the values
-- For better efficiency, set cols and/or vals to NULL if you do not wish to extract these quantities.
-- The user can only examine the values extracted with MatGetRow(); the values cannot be altered. To change the matrix entries, one must use MatSetValues().
-- You can only have one call to MatGetRow() outstanding for a particular matrix at a time, per processor. MatGetRow() can only obtain rows associated with the given processor, it cannot get rows from the other processors; for that we suggest using MatGetSubMatrices(), then MatGetRow() on the submatrix. The row index passed to MatGetRows() is in the global number of rows.
matGetRow' :: Mat -> CInt -> IO ((CInt, Ptr CInt, Ptr PetscScalar_), CInt)
matGetRow' mat row =
  withPtr3 ( \ncols cols vals -> matGetRow0' mat row ncols cols vals)


-- PetscErrorCode MatRestoreRow(Mat mat,PetscInt row,PetscInt *ncols,const PetscInt *cols[],const PetscScalar *vals[]) -- Not Collective
matRestoreRow0' mat row ncols cols vals = [C.exp|int{MatRestoreRow($(Mat mat),$(int row),$(int* ncols),$(const int** cols),$(const PetscScalar** vals))}|]
-- Input Parameters :
-- mat	- the matrix
-- row	- the row to get
-- ncols, cols	- the number of nonzeros and their columns
-- vals	- if nonzero the column values
-- Notes :
-- This routine should be called after you have finished examining the entries.
-- This routine zeros out ncols, cols, and vals. This is to prevent accidental us of the array after it has been restored. If you pass NULL, it will not zero the pointers. Use of cols or vals after MatRestoreRow is invalid.
matRestoreRow' mat row ncols cols vals = with ncols $ \ncolsp -> with cols $ \colsp -> with vals $ \valsp -> matRestoreRow0' mat row ncolsp colsp valsp

matRestoreRow0Safe' mat row = [C.exp|int{MatRestoreRow($(Mat mat),$(int row),NULL, NULL, NULL)}|]




-- PetscErrorCode MatGetColumnIJ(Mat mat,PetscInt shift,PetscBool symmetric,PetscBool inodecompressed,PetscInt *n,const PetscInt *ia[],const PetscInt *ja[],PetscBool  *done)
-- matGetColumnIJ' mat s symm inodec n ia ja = withPtr $ \done ->
--   [C.exp|int{MatGetColumnIJ($(Mat mat),$(int s),$(PetscBool* symm),$(PetscBool* inodec),$(int* n),$(PetscInt** ia),$(PetscInt** ja), $(PetscBool* done))}|]

-- matGetColumnIJ mat shift symm inodec n ia ja =
--   with symm $ \symmp ->
--   with inodec $ \inodecp ->
--   with n $ \np ->
--   withPtr ia $ \iap ->
--   withPtr ja $ \jap -> matGetColumnIJ' mat shift symmp inodecp np iap jap




-- PetscErrorCode  MatIsStructurallySymmetric(Mat A,PetscBool  *flg)
-- Collective on Mat
-- Input Parameter :
-- A -the matrix to test 
-- Output Parameters :
-- flg -the result
matIsStructurallySymmetric' :: Mat -> IO (PetscBool, CInt)
matIsStructurallySymmetric' mat = withPtr $ \b ->
  [C.exp|int{MatIsStructurallySymmetric($(Mat mat),$(PetscBool* b))}|]




-- PetscErrorCode  MatNorm(Mat mat,NormType type,PetscReal *nrm)
-- Collective on Mat
-- Input Parameters :
-- mat	- the matrix
-- type	- the type of norm, NORM_1, NORM_FROBENIUS, NORM_INFINITY
-- Output Parameters :
-- nrm -the resulting norm
matNorm' :: Mat -> MatNorm_ -> IO (PetscReal_, CInt)
matNorm' mat ntype = withPtr $ \nrm ->
  [C.exp|int{MatNorm($(Mat mat),$(int nt),$(PetscReal* nrm))}|] where
    nt = toCInt $ matNormToInt ntype


-- PetscErrorCode  MatGetTrace(Mat mat,PetscScalar *trace)
matGetTrace' :: Mat -> IO (PetscScalar_, CInt)
matGetTrace' mat = withPtr $ \tr -> [C.exp|int{MatGetTrace($(Mat mat),$(PetscScalar* tr))}|]


-- PetscErrorCode MatComputeBandwidth(Mat A, PetscReal fraction, PetscInt *bw)
-- Calculate the full bandwidth of the matrix, meaning the width 2k+1 where k diagonals on either side are sufficient to contain all the matrix nonzeros.
-- Collective on Mat
-- Input Parameters :
-- A	- The Mat
-- fraction	- An optional percentage of the Frobenius norm of the matrix that the bandwidth should enclose
-- Output Parameter :
-- bw -The matrix bandwidth
matComputeBandwidth' :: Mat -> PetscReal_ -> IO (PetscInt_, CInt)
matComputeBandwidth' mat f = withPtr $ \bw ->
  [C.exp|int{MatComputeBandwidth($(Mat mat),$(PetscReal f),$(PetscInt* bw))}|]



-- PetscErrorCode  MatLUFactor(Mat mat,IS row,IS col,const MatFactorInfo *info)
matLUFactor' :: Mat -> IS -> IS -> Ptr MatFactorInfo -> IO CInt
matLUFactor' mat is col info =
  [C.exp|int{MatLUFactor($(Mat mat),$(IS is),$(IS col),$(MatFactorInfo* info))}|]

-- PetscErrorCode  MatFactorInfoInitialize(MatFactorInfo *info)
matFactorInfoInitialize' :: IO (MatFactorInfo, CInt)
matFactorInfoInitialize' = withPtr $ \i -> mfii i where
  mfii info = [C.exp|int{MatFactorInfoInitialize($(MatFactorInfo* info))}|]


-- PetscErrorCode  MatMult(Mat mat,Vec x,Vec y)
-- Neighbor-wise Collective on Mat and Vec
-- Input Parameters :
-- mat	- the matrix
-- x	- the vector to be multiplied
-- Output Parameters :
-- y -the result 
-- Notes :
-- The vectors x and y cannot be the same. I.e., one cannot call MatMult(A,y,y).
matMult' :: Mat -> Vec -> Vec -> IO CInt
matMult' mat v vresult = [C.exp|int{MatMult($(Mat mat),$(Vec v),$(Vec vresult))}|]



-- PetscErrorCode  MatMultTranspose(Mat mat,Vec x,Vec y)
-- Neighbor-wise Collective on Mat and Vec
-- Input Parameters :
-- mat	- the matrix
-- x	- the vector to be multilplied
-- Output Parameters :
-- y -the result 
-- Notes :
-- The vectors x and y cannot be the same. I.e., one cannot call MatMultTranspose(A,y,y).
-- For complex numbers this does NOT compute the Hermitian (complex conjugate) transpose multiple, use MatMultHermitianTranspose()
matMultTranspose' :: Mat -> Vec -> Vec -> IO CInt
matMultTranspose' mat v vresult = [C.exp|int{MatMultTranspose($(Mat mat),$(Vec v),$(Vec vresult))}|]


-- PetscErrorCode  MatMultAdd(Mat mat,Vec v1,Vec v2,Vec v3)
-- Neighbor-wise Collective on Mat and Vec
-- Input Parameters :
-- mat	- the matrix
-- v1, v2	- the vectors
-- Output Parameters :
-- v3 -the result        v3 = v2 + A * v1
-- Notes
-- The vectors v1 and v3 cannot be the same. I.e., one cannot call MatMultAdd(A,v1,v2,v1).
matMultAdd' :: Mat -> Vec -> Vec -> Vec -> IO CInt
matMultAdd' mat v1 v2 v3 =
  [C.exp|int{MatMultAdd($(Mat mat),$(Vec v1),$(Vec v2),$(Vec v3))}|]


-- PetscErrorCode  MatMultTransposeAdd(Mat mat,Vec v1,Vec v2,Vec v3)
-- Neighbor-wise Collective on Mat and Vec
-- Input Parameters :
-- mat	- the matrix
-- v1, v2	- the vectors
-- Output Parameters :
-- v3 -the result           v3 = v2 + A' * v1
-- Notes : 
-- The vectors v1 and v3 cannot be the same. I.e., one cannot call MatMultTransposeAdd(A,v1,v2,v1).
matMultTransposeAdd' :: Mat -> Vec -> Vec -> Vec -> IO CInt
matMultTransposeAdd' mat v1 v2 v3 = [C.exp|int{MatMultTransposeAdd($(Mat mat),$(Vec v1),$(Vec v2),$(Vec v3))}|]




-- PetscErrorCode  MatMultHermitianTranspose(Mat mat,Vec x,Vec y)
-- Neighbor-wise Collective on Mat and Vec
-- Input Parameters :
-- mat	- the matrix
-- x	- the vector to be multilplied
-- Output Parameters :
-- y -the result        y = A^H x 
-- Notes :
-- The vectors x and y cannot be the same. I.e., one cannot call MatMultHermitianTranspose(A,y,y).
-- Also called the conjugate transpose, complex conjugate transpose, or adjoint.
-- For real numbers MatMultTranspose() and MatMultHermitianTranspose() are identical.
matMultHermitianTranspose' :: Mat -> Vec -> Vec -> IO CInt
matMultHermitianTranspose' m x y = [C.exp|int{MatMultHermitianTranspose($(Mat m),$(Vec x),$(Vec y))}|]




-- PetscErrorCode  MatMultHermitianTransposeAdd(Mat mat,Vec v1,Vec v2,Vec v3)
-- Neighbor-wise Collective on Mat and Vec
-- Input Parameters :
-- mat	- the matrix
-- v1, v2	- the vectors
-- Output Parameters
-- v3 -the result         v3 = v2 + A^H * v1.
-- Notes :
-- The vectors v1 and v3 cannot be the same. I.e., one cannot call MatMultHermitianTransposeAdd(A,v1,v2,v1).


matMultHermitianTransposeAdd' :: Mat -> Vec -> Vec -> Vec -> IO CInt
matMultHermitianTransposeAdd' m v1 v2 v3 = [C.exp|int{MatMultHermitianTransposeAdd($(Mat m),$(Vec v1),$(Vec v2),$(Vec v3))}|]



-- PETSC_EXTERN PetscErrorCode MatScale(Mat,PetscScalar);
matScale' :: Mat -> PetscScalar_ -> IO CInt
matScale' mat s = [C.exp|int{MatScale($(Mat mat),$(PetscScalar s))}|]

-- PETSC_EXTERN PetscErrorCode MatShift(Mat,PetscScalar);
matShift' :: Mat -> PetscScalar_ -> IO CInt
matShift' mat s = [C.exp|int{MatShift($(Mat mat),$(PetscScalar s))}|]



-- PetscErrorCode  MatMatMult(Mat A,Mat B,MatReuse scall,PetscReal fill,Mat *C)
-- Neighbor-wise Collective on Mat
-- Input Parameters :
-- A	- the left matrix
-- B	- the right matrix
-- scall	- either MAT_INITIAL_MATRIX or MAT_REUSE_MATRIX
-- fill	- expected fill as ratio of nnz(C)/(nnz(A) + nnz(B)), use PETSC_DEFAULT if you do not have a good estimate if the result is a dense matrix this is irrelevent
-- Output Parameters :
-- C -the product matrix 
-- Notes :
-- Unless scall is MAT_REUSE_MATRIX C will be created.
-- MAT_REUSE_MATRIX can only be used if the matrices A and B have the same nonzero pattern as in the previous call

-- To determine the correct fill value, run with -info and search for the string "Fill ratio" to see the value actually needed.

-- If you have many matrices with the same non-zero structure to multiply, you should either

--   1) use MAT_REUSE_MATRIX in all calls but the first or
--   2) call MatMatMultSymbolic() once and then MatMatMultNumeric() for each product needed
-- Matrix-Matrix Multiplication C=A*B.
matMatMult' matA matB scall fill =
  withPtr $ \matC ->
    [C.exp|int{MatMatMult($(Mat matA),$(Mat matB),$(int scallc),$(PetscReal fill),$(Mat* matC))}|] where scallc = toCInt $ matReuseToInt scall


-- PetscErrorCode  MatTransposeMatMult(Mat A,Mat B,MatReuse scall,PetscReal fill,Mat *C)  -- Neighbor-wise Collective on Mat
matTransposeMatMult' matA matB scall fill =
  withPtr $ \matC ->
    [C.exp|int{MatTransposeMatMult($(Mat matA),$(Mat matB),$(int scallc),$(PetscReal fill),$(Mat* matC))}|] where scallc = toCInt $ matReuseToInt scall


-- PetscErrorCode  MatMatMultSymbolic(Mat A,Mat B,PetscReal fill,Mat *C)
-- Neighbor-wise Collective on Mat
-- Input Parameters :
-- A	- the left matrix
-- B	- the right matrix
-- fill	- expected fill as ratio of nnz(C)/(nnz(A) + nnz(B)), use PETSC_DEFAULT if you do not have a good estimate, if C is a dense matrix this is irrelevent
-- Output Parameters :
-- C -the product matrix 
-- Notes :
-- Unless scall is MAT_REUSE_MATRIX C will be created.
-- To determine the correct fill value, run with -info and search for the string "Fill ratio" to see the value actually needed.
-- This routine is currently implemented for - pairs of AIJ matrices and classes which inherit from AIJ, C will be of type AIJ - pairs of AIJ (A) and Dense (B) matrix, C will be of type Dense. - pairs of Dense (A) and AIJ (B) matrix, C will be of type Dense.
matMatMultSymbolic' matA matB fill =
  withPtr $ \matC ->
     [C.exp|int{MatMatMultSymbolic($(Mat matA),$(Mat matB),$(PetscReal fill),$(Mat* matC))}|]

-- PetscErrorCode  MatMatMultNumeric(Mat A,Mat B,Mat C)
matMatMultNumeric' matA matB matC =
  [C.exp|int{MatMatMultNumeric($(Mat matA),$(Mat matB),$(Mat matC))}|]

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

-- matSetDiagonal mat val n imm =
--   [C.block|void{
--       int i;
--       for (i=0; i< $(int n); i++){
--         MatSetValues( $(Mat mat), 1, &i, 1, &i, &$(PetscScalar val), $(int imm));
--                                  };
--                                    }|] 





                

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
matFDColoringCreate0' :: Mat -> ISColoring -> Ptr MatFDColoring -> IO CInt
matFDColoringCreate0' m i c =
  [C.exp| int{MatFDColoringCreate($(Mat m),$(ISColoring i),$(MatFDColoring* c)) } |]

matFDColoringCreate' :: Mat -> ISColoring -> IO (MatFDColoring, CInt)
matFDColoringCreate' m i = withPtr $ \c -> matFDColoringCreate0' m i c



-- PetscErrorCode MatFDColoringSetUp(Mat mat,ISColoring iscoloring,MatFDColoring color)
matFDColoringSetUp' :: Mat -> ISColoring -> MatFDColoring -> IO CInt
matFDColoringSetUp' mat iscoloring color =
  [C.exp|int{MatFDColoringSetUp($(Mat mat),$(ISColoring iscoloring),$(MatFDColoring color))}|]

-- PetscErrorCode  MatFDColoringDestroy(MatFDColoring *c)
matFDColoringDestroy' :: MatFDColoring -> IO CInt
matFDColoringDestroy' color = with color $ \cp -> [C.exp|int{MatFDColoringDestroy($(MatFDColoring* cp))}|]






-- | -- MatNullSpace

-- PetscErrorCode  MatNullSpaceCreate(MPI_Comm comm,PetscBool has_cnst,PetscInt n,const Vec vecs[],MatNullSpace *SP)   -- Collective on MPI_Comm
-- Creates a data structure used to project vectors out of null spaces.
-- Input Parameters :
-- comm	- the MPI communicator associated with the object
-- has_cnst	- PETSC_TRUE if the null space contains the constant vector; otherwise PETSC_FALSE
-- n	- number of vectors (excluding constant vector) in null space
-- vecs	- the vectors that span the null space (excluding the constant vector); these vectors must be orthonormal. These vectors are NOT copied, so do not change them after this call. You should free the array that you pass in and destroy the vectors (this will reduce the reference count for them by one).
-- Notes: See MatNullSpaceSetFunction() as an alternative way of providing the null space information instead of setting vecs.
-- If has_cnst is PETSC_TRUE you do not need to pass a constant vector in as a fourth argument to this routine, nor do you need to pass in a function that eliminates the constant function into MatNullSpaceSetFunction().
-- Output Parameter:
-- SP -the null space context
matNullSpaceCreate' :: Comm -> Bool -> Int -> VS.Vector Vec -> IO (MatNullSpace, CInt)
matNullSpaceCreate' cc hasc n vecs = withPtr $ \sp ->
  VS.unsafeWith vecs $ \vecsp -> 
   [C.exp|int{MatNullSpaceCreate($(int c),$(PetscBool hascc),$(PetscInt nn),$(const Vec* vecsp),$(MatNullSpace* sp))}|]
  where c = unComm cc
        hascc = boolToPetscBoolC hasc
        nn = toCInt n

matNullSpaceDestroy' :: MatNullSpace -> IO CInt
matNullSpaceDestroy' sp = with sp $ \spp ->
  [C.exp|int{MatNullSpaceDestroy($(MatNullSpace* spp))}|]
        
-- PetscErrorCode  MatNullSpaceRemove(MatNullSpace sp,Vec vec)
-- Collective on MatNullSpace
-- Input Parameters
-- sp	- the null space context
-- vec	- the vector from which the null space is to be removed

matNullSpaceRemove' :: MatNullSpace -> Vec -> IO CInt
matNullSpaceRemove' sp v =
  [C.exp|int{MatNullSpaceRemove($(MatNullSpace sp),$(Vec v))}|]

-- MatSetNullSpace(Mat Amat,MatNullSpace nsp);
matSetNullSpace' :: Mat -> MatNullSpace -> IO CInt
matSetNullSpace' amat sp = [C.exp|int{MatSetNullSpace($(Mat amat),$(MatNullSpace sp))}|]

-- MatSetTransposeNullSpace(Mat Amat,MatNullSpace nsp);














-- * DM




-- PETSC_EXTERN PetscErrorCode DMCreate(MPI_Comm,DM*);
dmCreate' :: Comm -> IO (DM, CInt)
dmCreate' cc = withPtr ( \dm -> [C.exp|int{DMCreate($(int c), $(DM* dm))} |] ) 
  where c = unComm cc

dmDestroy' :: DM -> IO CInt
dmDestroy' dm = with dm ( \dmp -> [C.exp|int{DMDestroy($(DM* dmp))}|] ) 

-- -- DMCreate* are for setting up longer-lived data
-- -- DMGet* and DMRestore* are for temporary access (always go in pairs)

-- PETSC_EXTERN PetscErrorCode DMCreateGlobalVector(DM,Vec*);
dmCreateGlobalVector' :: DM -> IO (Vec, CInt)
dmCreateGlobalVector' dm = withPtr ( \v -> [C.exp|int{DMCreateGlobalVector($(DM dm), $(Vec* v))}|]) 

-- PETSC_EXTERN PetscErrorCode DMCreateLocalVector(DM,Vec*);
dmCreateLocalVector' :: DM -> IO (Vec, CInt)
dmCreateLocalVector' dm = withPtr ( \v -> [C.exp|int{DMCreateLocalVector($(DM dm), $(Vec* v))}|]) 


-- PETSC_EXTERN PetscErrorCode DMGetLocalVector(DM,Vec *);
-- The vector values are NOT initialized and may have garbage in them, so you may need to zero them.
-- The output parameter, g, is a regular PETSc vector that should be returned with DMRestoreLocalVector() DO NOT call VecDestroy() on it.
-- This is intended to be used for vectors you need for a short time, like within a single function call. For vectors that you intend to keep around (for example in a C struct) or pass around large parts of your code you should use DMCreateLocalVector().
-- VecStride*() operations can be useful when using DM with dof > 1

dmGetLocalVector' :: DM -> IO (Vec, CInt)
dmGetLocalVector' dm = withPtr ( \v -> [C.exp|int{DMGetLocalVector($(DM dm),$(Vec* v))}|]) 

-- PETSC_EXTERN PetscErrorCode DMRestoreLocalVector(DM,Vec *);
dmRestoreLocalVector' :: DM -> Vec -> IO CInt
dmRestoreLocalVector' dm vv = with vv ( \v -> [C.exp|int{DMRestoreLocalVector($(DM dm),$(Vec* v))}|]) 


-- PETSC_EXTERN PetscErrorCode DMGetGlobalVector(DM,Vec *);
dmGetGlobalVector' :: DM -> IO (Vec, CInt)
dmGetGlobalVector' dm = withPtr ( \v -> [C.exp|int{DMGetGlobalVector($(DM dm),$(Vec* v))}|])

-- PETSC_EXTERN PetscErrorCode DMRestoreGlobalVector(DM,Vec *);
dmRestoreGlobalVector' :: DM -> Vec -> IO CInt
dmRestoreGlobalVector' dm vv = with vv ( \v -> [C.exp|int{DMRestoreGlobalVector($(DM dm),$(Vec* v))}|]) 


dmCreateMatrix' :: DM -> IO (Mat, CInt)
dmCreateMatrix' dm = withPtr $ \mat ->
  [C.exp|int{DMCreateMatrix($(DM dm),$(Mat* mat))}|]



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
dmGetCoordinates :: DM -> IO (Vec, CInt)
dmGetCoordinates dm =
 withPtr (\c-> [C.exp| int{DMGetCoordinates($(DM dm),$(Vec*c))} |] ) 







-- PetscErrorCode  DMCreateColoring(DM dm,ISColoringType ctype,ISColoring *coloring)
dmCreateColoring' :: DM -> ISColoringType_ -> IO (ISColoring, CInt)
dmCreateColoring' d c = withPtr $ \col -> [C.exp|int{DMCreateColoring($(DM d),$(int ctype),$(ISColoring* col))}|] where
  ctype = toCInt $ isColoringTypeToInt c











-- * DMDA


-- data DMDAInterpolationType = DMDA_Q0 | DMDA_Q1 deriving (Eq, Show, Enum)
-- dmdaInterpolationTypeToInt x = fromEnum (x :: DMDAInterpolationType)

-- data DMDAElementType = DMDAElem_Q0 | DMDAElem_Q1 deriving (Eq, Show, Enum)
-- dmdaElementTypeToInt x = fromEnum (x :: DMDAElementType )

-- data DMDADirection = DMDA_X | DMDA_Y | DMDA_Z deriving (Eq, Show, Enum)
-- dmdaDirectionToInt x = fromEnum (x :: DMDADirection)

dmdaCreate' :: Comm -> IO (DM, CInt)
dmdaCreate' cc = withPtr ( \p -> [C.exp|int{DMDACreate($(int c), $(DM* p))}|] ) where
  c = unComm cc



-- | DMDASetDim removed from 3.6.2

-- -- PETSC_EXTERN PetscErrorCode DMDASetDim(DM,PetscInt);
-- dmdaSetDim' dm d = [C.exp|int{DMDASetDim($(DM dm), $(PetscInt d))}|] 




-- PETSC_EXTERN PetscErrorCode DMDASetSizes(DM,PetscInt,PetscInt,PetscInt);
dmdaSetSizes' :: DM -> PetscInt_ -> PetscInt_ -> PetscInt_ -> IO CInt
dmdaSetSizes' dm x y z = [C.exp|int{DMDASetSizes($(DM dm), $(PetscInt x), $(PetscInt y), $(PetscInt z))}|] 

-- PetscErrorCode  DMDACreate1d(MPI_Comm comm, DMBoundaryType bx, PetscInt M, PetscInt dof, PetscInt s, const PetscInt lx[], DM *da)   -- Collective on MPI_Comm
-- Input Parameters

-- comm	- MPI communicator
-- bx	- type of ghost cells at the boundary the array should have, if any. Use DM_BOUNDARY_NONE, DM_BOUNDARY_GHOSTED, or DM_BOUNDARY_PERIODIC.
-- M	- global dimension of the array (use -M to indicate that it may be set to a different value from the command line with -da_grid_x <M>)
-- dof	- number of degrees of freedom per node
-- s	- stencil width
-- lx	- array containing number of nodes in the X direction on each processor, or NULL. If non-null, must be of length as the number of processes in the MPI_Comm.

dmdaCreate1d0' ::
  Comm -> DMBoundaryType_ -> PetscInt_ -> PetscInt_ -> PetscInt_ -> IO (DM, CInt)
dmdaCreate1d0' cc bx m dof s =
   withPtr ( \ dm -> [C.exp|int{DMDACreate1d($(int c),
                                              $(DMBoundaryType bxe),
                                              $(PetscInt m),
                                              $(PetscInt dof),
                                              $(PetscInt s),
                                              NULL,
                                              $(DM* dm))}|]  )
  where c = unComm cc
        bxe = dmBoundaryTypeToC bx

dmdaCreate1d' ::
  Comm -> DMBoundaryType_ -> PetscInt_ -> PetscInt_ -> PetscInt_ -> [CInt] -> IO (DM, CInt)
dmdaCreate1d' cc bx m dof s lx_ =
  withArray lx_ ( \ lx ->
   withPtr ( \ dm -> [C.exp|int{DMDACreate1d($(int c),
                                              $(DMBoundaryType bxe),
                                              $(PetscInt m),
                                              $(PetscInt dof),
                                              $(PetscInt s),
                                              $(int* lx),
                                              $(DM* dm))}|]  )) 
  where c = unComm cc
        bxe = dmBoundaryTypeToC bx



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

-- dmdaCreate2d0' :: Comm
--                         -> DMBoundaryType_
--                         -> DMBoundaryType_
--                         -> DMDAStencilType
--                         -> PetscInt_
--                         -> PetscInt_
--                         -> PetscInt_
--                         -> PetscInt_
--                         -> PetscInt_
--                         -> PetscInt_
--                         -> [CInt]
--                         -> [CInt]
--                         -> IO (DM, CInt)
dmdaCreate2d0' cc bx by sten mm nn m n dof s lx_ ly_ =
  withArray lx_ $ \lx ->
   withArray ly_ $ \ly -> 
    withPtr ( \dm -> [C.exp|int{DMDACreate2d($(int c),
                          $(DMBoundaryType bxe),
                          $(DMBoundaryType bye),
                          $(DMDAStencilType stene),
                          $(PetscInt mm),
                          $(PetscInt nn),
                          $(PetscInt m), 
                          $(PetscInt n),
                          $(PetscInt dof),
                          $(PetscInt s),
                          $(int* lx),
                          $(int* ly),
                          $(DM* dm))}|] ) 
  where c = unComm cc
        bxe = dmBoundaryTypeToC bx
        bye = dmBoundaryTypeToC by
        stene = dmdaStencilTypeToC sten

-- | Hp : lx == ly == NULL
-- (customary in PETSc examples )

dmdaCreate2d' :: Comm
                       -> DMBoundaryType_
                       -> DMBoundaryType_
                       -> DMDAStencilType_
                       -> PetscInt_
                       -> PetscInt_
                       -> PetscInt_
                       -> PetscInt_
                       -> IO (DM, CInt)
dmdaCreate2d' cc bx by sten mm nn dof s  =
    withPtr ( \dm -> [C.exp|int{DMDACreate2d($(int c),
                          $(DMBoundaryType bxe),
                          $(DMBoundaryType bye),
                          $(DMDAStencilType stene),
                          $(PetscInt mm),
                          $(PetscInt nn),
                          PETSC_DECIDE, 
                          PETSC_DECIDE,
                          $(PetscInt dof),
                          $(PetscInt s),
                          NULL,
                          NULL,
                          $(DM* dm))}|] ) 
  where c = unComm cc
        bxe = dmBoundaryTypeToC bx
        bye = dmBoundaryTypeToC by
        stene = dmdaStencilTypeToC sten
        
-- dmdaCreate2d' c bx by sten mm nn dof s =
--   dmdaCreate2d0' c bx by sten mm nn petscDecide petscDecide dof s [] []



-- PETSC_EXTERN PetscErrorCode DMDACreateNaturalVector(DM,Vec *);
dmdaCreateNaturalVector :: DM -> IO (Vec, CInt)
dmdaCreateNaturalVector dm = withPtr (\v ->[C.exp|int{DMDACreateNaturalVector($(DM dm), $(Vec * v))} |] )


-- PETSC_EXTERN PetscErrorCode DMDAGetCorners(DM,PetscInt*,PetscInt*,PetscInt*,PetscInt*,PetscInt*,PetscInt*);
-- Returns the global (x,y,z) indices of the lower left corner of the local region, excluding ghost points.
-- x,y,z	- the corner indices (where y and z are optional; these are used for 2D and 3D problems)
-- m,n,p	- widths in the corresponding directions (where n and p are optional; these are used for 2D and 3D problems)

-- NB 4-6 outputs

dmdaGetCorners1d' :: DM -> IO (PetscInt_, (PetscInt_, CInt))
dmdaGetCorners1d' dm =
  withPtr $ \x ->
  withPtr $ \m -> [C.exp|int{DMDAGetCorners($(DM dm),
                               $(PetscInt* x),
                               NULL,
                               NULL,
                               $(PetscInt* m),
                               NULL,
                               NULL)} |]

dmdaGetCorners2d' ::
  DM -> IO (PetscInt_, (PetscInt_, (PetscInt_, (PetscInt_, CInt))))
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

dmdaGetCorners3d' ::
  DM -> IO (PetscInt_, (PetscInt_, (PetscInt_, (PetscInt_, (PetscInt_, (PetscInt_, CInt))))))
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




-- PETSC_EXTERN PetscErrorCode DMDAGetGhostCorners(DM,PetscInt*,PetscInt*,PetscInt*,PetscInt*,PetscInt*,PetscInt*);

dmdaGetGhostCorners1d' :: DM -> IO (PetscInt_, (PetscInt_, CInt))
dmdaGetGhostCorners1d' dm =
  withPtr $ \x ->
  withPtr $ \m -> [C.exp|int{DMDAGetCorners($(DM dm),
                               $(PetscInt* x),
                               NULL,
                               NULL,
                               $(PetscInt* m),
                               NULL,
                               NULL)} |]

dmdaGetGhostCorners2d' :: DM -> IO (PetscInt_, (PetscInt_, (PetscInt_, (PetscInt_, CInt))))
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

dmdaGetGhostCorners3d' :: DM -> IO (PetscInt_, (PetscInt_, (PetscInt_, (PetscInt_, (PetscInt_, (PetscInt_, CInt))))))
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
dmdaSetUniformCoordinates' :: DM
                                    -> PetscReal_
                                    -> PetscReal_
                                    -> PetscReal_
                                    -> PetscReal_
                                    -> PetscReal_
                                    -> PetscReal_
                                    -> IO CInt
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

dmdaVecGetArray' :: DM -> Vec -> IO (Ptr PetscScalar_, CInt)
dmdaVecGetArray' dm v = withPtr $ \vvp -> 
  [C.exp|int{DMDAVecGetArray($(DM dm),
                             $(Vec v),
                             $(PetscScalar** vvp))}|]



-- -- PetscErrorCode  DMDARestoreArray(DM da,PetscBool ghosted,void *vptr)
-- dmdaRestoreArray' dm ghosted vptr = withArray vptr ( \vp -> 
--   [C.exp|int{DMDARestoreArray($(DM dm),
--                               $(PetscBool ghosted),
--                               $(PetscScalar* vp))}|] ) 

-- PetscErrorCode  DMDAVecRestoreArray(DM da,Vec vec,void *array)
dmdaVecRestoreArray' :: DM -> Vec -> Ptr PetscScalar_ -> IO CInt
dmdaVecRestoreArray' dm v arr_ =
  [C.exp|int{DMDAVecRestoreArray($(DM dm), $(Vec v), $(PetscScalar* arr_))}|]




-- PetscErrorCode  DMGlobalToLocalBegin(DM dm,Vec g,InsertMode mode,Vec l)
-- Neighbor-wise Collective on DM
-- Input Parameters : 
-- dm	- the DM object
-- g	- the global vector
-- mode	- INSERT_VALUES or ADD_VALUES
-- l	- the local vector
dmGlobalToLocalBegin' :: DM -> Vec -> InsertMode_ -> Vec -> IO CInt
dmGlobalToLocalBegin' dm g mode l = [C.exp|int{DMGlobalToLocalBegin($(DM dm),$(Vec g),$(int imode),$(Vec l))}|] where
  imode = toCInt $ insertModeToInt mode


-- PetscErrorCode  DMGlobalToLocalEnd(DM dm,Vec g,InsertMode mode,Vec l)
dmGlobalToLocalEnd' :: DM -> Vec -> InsertMode_ -> Vec -> IO CInt
dmGlobalToLocalEnd' dm g mode l = [C.exp|int{DMGlobalToLocalEnd($(DM dm),$(Vec g),$(int imode),$(Vec l))}|] where
  imode = toCInt $ insertModeToInt mode



-- PetscErrorCode  DMLocalToGlobalBegin(DM dm,Vec l,InsertMode mode,Vec g)
dmLocalToGlobalBegin' :: DM -> Vec -> InsertMode_ -> Vec -> IO CInt
dmLocalToGlobalBegin' dm locv im globv = [C.exp|int{DMLocalToGlobalBegin($(DM dm),$(Vec locv),$(int imode),$(Vec globv))}|]
  where imode = toCInt $ insertModeToInt im
-- Neighbor-wise Collective on DM
-- Input Parameters :
-- dm	- the DM object
-- l	- the local vector
-- mode	- if INSERT_VALUES then no parallel communication is used, if ADD_VALUES then all ghost points from the same base point accumulate into that base point.
-- g	- the global vector
-- Notes: In the ADD_VALUES case you normally would zero the receiving vector before beginning this operation. INSERT_VALUES is not supported for DMDA, in that case simply compute the values directly into a global vector instead of a local one.

dmLocalToGlobalEnd' :: DM -> Vec -> InsertMode_ -> Vec -> IO CInt
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
   int{DMDAGetInfo($(DM da), $(PetscInt* dim), $(PetscInt* mm), $(PetscInt* nn), $(PetscInt* pp), $(PetscInt* m), $(PetscInt* n), $(PetscInt* p), $(PetscInt* dof), $(PetscInt* s), $(DMBoundaryType* bxp), $(DMBoundaryType* byp), $(DMBoundaryType* bzp), $(int* stp))} |]

dmdaGetInfo__' :: DM -> IO ((PetscInt_,
                             (PetscInt_, PetscInt_, PetscInt_),
                             (PetscInt_, PetscInt_, PetscInt_),
                             PetscInt_,
                             PetscInt_,
                             (DMBoundaryType, DMBoundaryType, DMBoundaryType),
                             CInt),
                            CInt)
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
   




-- Creates a vector packer, used to generate "composite" vectors made up of several subvectors.
-- Synopsis :
-- #include "petscdmcomposite.h"  
-- PetscErrorCode  DMCompositeCreate(MPI_Comm comm,DM *packer)
-- Collective on MPI_Comm
-- Input Parameter :
-- comm -the processors that will share the global vector 
-- Output Parameter :
-- packer -the packer object
dmCompositeCreate :: Comm -> IO (DM, CInt)
dmCompositeCreate cc =
  withPtr ( \p -> [C.exp|int{DMCompositeCreate($(int c), $(DM* p))}|] )
  where c = unComm cc


-- | viewing DM

-- PetscErrorCode  DMView(DM dm,PetscViewer v)
dmView' :: DM -> PetscViewer -> IO CInt
dmView' dm vi = [C.exp|int{DMView($(DM dm),$(PetscViewer vi))}|]


















-- * KSP





kspGetConvergedReason' :: KSP -> IO (CInt, CInt)
kspGetConvergedReason' ksp =
  withPtr ( \r -> [C.exp| int{ KSPGetConvergedReason( $(KSP ksp), $(int* r) ) } |]) 


kspCreate' :: Comm -> IO (KSP, CInt)
kspCreate' cc = withPtr $ \p -> [C.exp| int{KSPCreate($(int c), $(KSP *p))}|] where
    c = unComm cc

kspSetType' :: KSP -> KspType_ -> IO CInt
kspSetType' ksp kt = withCString strk $ \strp -> [C.exp|int{KSPSetType($(KSP ksp), $(char* strp))}|] where
  strk = kspTypeToStr kt



-- PETSC_EXTERN PetscErrorCode KSPGetType(KSP,KSPType *);
-- kspGetType ksp = alloca ( \strp -> do
--                            [C.exp|int{KSPGetType($(KSP ksp), $(char *strp))}|]
--                            peekString strp) 

kspDestroy' :: KSP -> IO CInt
kspDestroy' k = with k $ \pp -> [C.exp| int{KSPDestroy($(KSP *pp))}  |]

-- withKspSetupSolve c mat1 mat2 ignz kt x v post = withKsp c $ \ksp -> do
--   kspSetOperators ksp mat1 mat2
--   kspSetType ksp kt 
--   kspSetInitialGuessNonzero ksp ignz
--   kspSetUp ksp
--   kspSolve ksp x v
--   -- soln <- kspGetSolution ksp 
--   post ksp
-- -- nb this is just a reference; it becomes invalid after exiting the withKsp bracket -> we need a `withVecDuplicateCopy` outside withKspSolve to allocate space for the solution 




kspSetOperators' :: KSP -> Mat -> Mat -> IO CInt
kspSetOperators' ksp amat pmat =
  [C.exp|int{KSPSetOperators($(KSP ksp), $(Mat amat), $(Mat pmat))}|] 

kspSetUp' :: KSP -> IO CInt
kspSetUp' ksp = [C.exp|int{KSPSetUp($(KSP ksp))}|]

kspSolve' :: KSP -> Vec -> Vec -> IO CInt
kspSolve' ksp b x = [C.exp|int{KSPSolve($(KSP ksp), $(Vec b), $(Vec x))}|] 

kspSolveTranspose' :: KSP -> Vec -> Vec -> IO CInt
kspSolveTranspose' ksp b x =
  [C.exp|int{KSPSolveTranspose($(KSP ksp), $(Vec b), $(Vec x))}|] 





-- PETSC_EXTERN PetscErrorCode KSPReset(KSP);

-- PETSC_EXTERN PetscErrorCode KSPSetReusePreconditioner(KSP,PetscBool);
kspSetReusePreconditioner' :: KSP -> PetscBool -> IO CInt
kspSetReusePreconditioner' ksp b = [C.exp|int{KSPSetReusePreconditioner($(KSP ksp), $(PetscBool b))}|] 

-- PETSC_EXTERN PetscErrorCode KSPRegisterAll(void);
-- PETSC_EXTERN PetscErrorCode KSPRegister(const char[],PetscErrorCode (*)(KSP));
-- PETSC_EXTERN PetscErrorCode KSPMatRegisterAll(void);

-- PETSC_EXTERN PetscErrorCode KSPSetPCSide(KSP,PCSide);
-- PETSC_EXTERN PetscErrorCode KSPGetPCSide(KSP,PCSide*);
-- PETSC_EXTERN PetscErrorCode KSPGetTolerances(KSP,PetscReal*,PetscReal*,PetscReal*,PetscInt*); -- Not Collective
-- Input Parameter :
-- ksp -the Krylov subspace context 
-- Output Parameters :
-- rtol	- the relative convergence tolerance
-- abstol	- the absolute convergence tolerance
-- dtol	- the divergence tolerance
-- maxits	- maximum number of iterations
kspGetTolerances0' ksp =
  withPtr4 $ \ rtolc abstolc dtolc maxitsc ->
  [C.exp|int{KSPGetTolerances($(KSP ksp),$(PetscReal* rtolc),$(PetscReal* abstolc),$(PetscReal* dtolc),$(PetscInt* maxitsc))}|]



-- PETSC_EXTERN PetscErrorCode KSPSetTolerances(KSP,PetscReal,PetscReal,PetscReal,PetscInt);
kspSetTolerances' ::
  KSP -> Double -> Double -> Double -> Int -> IO CInt
kspSetTolerances' ksp rtol abstol dtol maxits = [C.exp|int{KSPSetTolerances($(KSP ksp),$(PetscReal rtolc),$(PetscReal abstolc),$(PetscReal dtolc),$(PetscInt maxitsc))}|] where
  rtolc = CDouble rtol
  abstolc = CDouble abstol
  dtolc = CDouble dtol
  maxitsc = toCInt maxits

-- PETSC_EXTERN PetscErrorCode KSPSetInitialGuessNonzero(KSP,PetscBool );
kspSetInitialGuessNonzero' :: KSP -> Bool -> IO CInt
kspSetInitialGuessNonzero' ksp b
  | b = [C.exp|int{KSPSetInitialGuessNonzero($(KSP ksp), PETSC_TRUE)}|]
  | otherwise = [C.exp|int{KSPSetInitialGuessNonzero($(KSP ksp), PETSC_FALSE)}|]

-- PETSC_EXTERN PetscErrorCode KSPGetInitialGuessNonzero(KSP,PetscBool  *);
-- PETSC_EXTERN PetscErrorCode KSPSetInitialGuessKnoll(KSP,PetscBool );
-- PETSC_EXTERN PetscErrorCode KSPGetInitialGuessKnoll(KSP,PetscBool *);

-- PETSC_EXTERN PetscErrorCode KSPSetErrorIfNotConverged(KSP,PetscBool );
kspSetErrorIfNotConverged :: KSP -> PetscBool -> IO CInt
kspSetErrorIfNotConverged ksp b = [C.exp|int{KSPSetErrorIfNotConverged($(KSP ksp), $(PetscBool b))}|] 

-- PETSC_EXTERN PetscErrorCode KSPGetErrorIfNotConverged(KSP,PetscBool  *);
-- PETSC_EXTERN PetscErrorCode KSPGetComputeEigenvalues(KSP,PetscBool *);

-- PETSC_EXTERN PetscErrorCode KSPSetComputeEigenvalues(KSP,PetscBool );
kspSetComputeEigenValues :: KSP -> PetscBool -> IO CInt
kspSetComputeEigenValues ksp b = [C.exp|int{KSPSetComputeEigenvalues($(KSP ksp), $(PetscBool b))}|]

-- PETSC_EXTERN PetscErrorCode KSPGetComputeSingularValues(KSP,PetscBool *);

-- PETSC_EXTERN PetscErrorCode KSPSetComputeSingularValues(KSP,PetscBool );
kspSetComputeSingularValues :: KSP -> PetscBool -> IO CInt
kspSetComputeSingularValues ksp b = [C.exp|int{KSPSetComputeSingularValues($(KSP ksp), $(PetscBool b))}|] 

-- PETSC_EXTERN PetscErrorCode KSPGetRhs(KSP,Vec *);
kspGetRhs' :: KSP -> IO (Vec, CInt)
kspGetRhs' ksp = withPtr $ \v -> [C.exp|int{KSPGetRhs($(KSP ksp), $(Vec *v))}|]
-- kspGetRhs ksp = kspGetRhs' ksp 

-- PETSC_EXTERN PetscErrorCode KSPGetSolution(KSP,Vec *);
kspGetSolution' :: KSP -> IO (Vec, CInt)
kspGetSolution' ksp = withPtr $ \v -> [C.exp|int{KSPGetSolution($(KSP ksp), $(Vec *v))}|]
-- kspGetSolution :: KSP -> IO Vec
-- kspGetSolution ksp = kspGetSolution' ksp 

-- PETSC_EXTERN PetscErrorCode KSPGetResidualNorm(KSP,PetscReal*);
kspGetResidualNorm' :: KSP -> IO (PetscReal_, CInt)
kspGetResidualNorm' ksp = withPtr $ \v -> [C.exp|int{KSPGetResidualNorm($(KSP ksp), $(PetscReal *v))}|]

-- PETSC_EXTERN PetscErrorCode KSPGetIterationNumber(KSP,PetscInt*);
kspGetIterationNumber' :: KSP -> IO (CInt, CInt)
kspGetIterationNumber' ksp = withPtr ( \v -> [C.exp|int{KSPGetIterationNumber($(KSP ksp), $(int *v))}|] ) 

-- PETSC_EXTERN PetscErrorCode KSPSetNullSpace(KSP,MatNullSpace);
-- PETSC_EXTERN PetscErrorCode KSPGetNullSpace(KSP,MatNullSpace*);
-- PETSC_EXTERN PetscErrorCode KSPGetVecs(KSP,PetscInt,Vec**,PetscInt,Vec**);

-- PetscErrorCode  KSPSetConvergenceTest(KSP ksp,PetscErrorCode (*converge)(KSP,PetscInt,PetscReal,KSPConvergedReason*,void*),void *cctx,PetscErrorCode (*destroy)(void*))
-- Logically Collective on KSP
-- Input Parameters :
-- ksp	- iterative context obtained from KSPCreate()
-- converge	- pointer to int function
-- cctx	- context for private data for the convergence routine (may be null)
-- destroy	- a routine for destroying the context (may be null)
-- Calling sequence of converge :
--     converge (KSP ksp, int it, PetscReal rnorm, KSPConvergedReason *reason,void *mctx)
-- ksp	- iterative context obtained from KSPCreate()
-- it	- iteration number
-- rnorm	- (estimated) 2-norm of (preconditioned) residual
-- reason	- the reason why it has converged or diverged
-- cctx	- optional convergence context, as set by KSPSetConvergenceTest()
kspSetConvergenceTest' ::
  KSP -> (KSP -> PetscInt_ -> PetscReal_ -> Ptr CInt -> IO a) -> IO CInt
kspSetConvergenceTest' ksp convergef =
  [C.exp|int{
      KSPSetConvergenceTest(
          $(KSP ksp),
          $fun:(int (* cf) (KSP, PetscInt, PetscReal, int*, void*)),
          NULL, NULL )} |] where
  cf ks nit rnorm nreasonp _p = do
    convergef ks nit rnorm nreasonp
    return (0 :: CInt)
    




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



kspSetComputeRHS__' :: KSP -> (KSP -> Vec -> IO CInt) -> IO CInt
kspSetComputeRHS__' ksp f = kspSetComputeRHS_' ksp g where
  g k v _ = f k v
  kspSetComputeRHS_' ks ff =
    [C.exp|int{KSPSetComputeRHS($(KSP ks),$fun:(int (* ff)(KSP, Vec, void*)), NULL  )}|]


-- PetscErrorCode  KSPReasonView(KSP ksp,PetscViewer viewer)
-- kspReasonView ksp = [C.exp|int{KSPReasonView($(KSP ksp), PETSC_VIEWER_STDOUT_SELF)}|] >>= handleErr



kspGetPc' :: KSP -> IO (PC, CInt)
kspGetPc' k = withPtr $ \pc -> [C.exp|int{KSPGetPC($(KSP k),$(PC* pc))}|]






-- * PC
pcSetType' :: PC -> PCType_ -> IO CInt
pcSetType' pc pct = withCString t $ \tp -> [C.exp|int{PCSetType($(PC pc),$(char* tp))}|] where
  t = pcTypeToString pct
  









-- * PF

-- -- PetscErrorCode  PFCreate(MPI_Comm comm,PetscInt dimin,PetscInt dimout,PF *pf)
-- -- Collective on MPI_Comm
-- -- Input Parameters :
-- -- comm	- MPI communicator
-- -- dimin	- dimension of the space you are mapping from
-- -- dimout	- dimension of the space you are mapping to
-- pfCreate' :: Comm -> Int -> Int -> IO (PF, CInt)
-- pfCreate' cc dimin dimout = withPtr ( \pf ->[C.exp|int{PFCreate($(int c),$(int diminc),$(int dimoutc),$(PF*pf) )}|] ) 
--   where
--     c = unComm cc
--     diminc = toCInt dimin
--     dimoutc = toCInt dimout

-- -- PetscErrorCode  PFDestroy(PF *pf)
-- pfDestroy' :: PF -> IO CInt
-- pfDestroy' pf = with pf $ \pfp -> [C.exp|int{PFDestroy($(PF* pfp))}|]

-- -- PETSC_EXTERN PetscErrorCode DMDACreatePF(DM,PF*);
-- dmdaCreatePF' :: DM -> IO (PF, CInt)
-- dmdaCreatePF' dm = withPtr (\pf -> [C.exp|int{DMDACreatePF($(DM dm),$(PF*pf))}|]) 

-- -- PETSC_EXTERN PetscErrorCode PFSetType(PF,PFType,void*);
-- -- pfSetType' :: PF -> PFType_ -> Ptr () -> IO CInt
-- pfSetType' pf t o = -- not sure how to represent the pointer to void 
--   withCString tstr (\tp->   [C.exp|int{PFSetType($(PF pf),$(char*tp),$(void*o))}|]
--                    )  where
--   tstr = pfTypeToStr t


-- -- PETSC_EXTERN PetscErrorCode PFSet(
-- -- PF,
-- -- PetscErrorCode(*)(void*,PetscInt,const PetscScalar*,PetscScalar*),
-- -- PetscErrorCode(*)(void*,Vec,Vec),PetscErrorCode(*)(void*,PetscViewer),
-- -- PetscErrorCode(*)(void*),void*);     -- Collective on PF
-- -- Input Parameters :
-- -- pf	- the function context
-- -- apply	- function to apply to an array
-- -- applyvec	- function to apply to a Vec
-- -- view	- function that prints information about the PF
-- -- destroy	- function to free the private function context
-- -- ctx	- private function context
-- pfSet0' pf apply applyvec view destroy ctx =
--   [C.exp|int{PFSet($(PF pf),
--                    $fun:(int(*apply)(void*,PetscInt,PetscScalar*,PetscScalar*)),
--                    $fun:(int(*applyvec)(void*, Vec, Vec)),
--                    $fun:(int(*view)(void*, int )),
--                    $fun:(int(*destroy)(void*)),
--                    $(void*ctx)
--                   )}
--         |]

-- pfSet0nc' pf apply applyvec view destroy ctx =
--   [C.exp|int{PFSet($(PF pf),
--                    $fun:(int(*apply)(void*,PetscInt,PetscScalar*,PetscScalar*)),
--                    $fun:(int(*applyvec)(void*, Vec, Vec)),
--                    $fun:(int(*view)(void*, int )),
--                    $fun:(int(*destroy)(void*)),
--                    NULL
--                   )}
--         |]

-- pfSet1' pf apply applyvec viewf destroyf =
--   pfSet0nc' pf f1 f2 f3 f4 where
--     f1 _ = apply
--     f2 _ = applyvec
--     f3 _ = viewf
--     f4 _ = destroyf
--     -- apply' :: PetscInt_ -> [PetscScalar_] -> [PetscScalar_] -> IO CInt
--     -- apply' a arr1 arr2 = withArray arr1 $  \arrp1 ->
--     --   withArray arr2 $ \arrp2 ->
--     --     f1 a a arrp1 arrp2


-- pfSetArr0' ::
--   PF ->
--   (Ptr () -> PetscInt_ -> Ptr PetscScalar_ -> Ptr PetscScalar_ -> IO CInt ) ->
--   IO CInt
-- pfSetArr0' pf apply =
--   [C.exp|int{PFSet($(PF pf),
--                    $fun:(int(*apply)(void*,PetscInt,PetscScalar*,PetscScalar*)),
--                    NULL,
--                    NULL,
--                    NULL,
--                    NULL   )}  |]

-- -- pfsa1 ::
-- --   PF ->
-- --   (PetscInt_ -> Ptr PetscScalar_ -> Ptr PetscScalar_ -> IO CInt) ->
-- --   IO CInt
-- pfsa1 pf f =
--   pfSetArr0' pf fm where
--     fm _ = f


-- -- pfSetVec' :: PF -> (Ptr () -> Vec -> Vec -> IO CInt) -> IO ()
-- pfSetVec0' pf applyvec =
--     [C.exp|int{PFSet($(PF pf),
--                    0,
--                    $fun:(int(*applyvec)( void* , Vec, Vec)),
--                    0, 0, 0)}|] 

-- pfSetVec' pf applyvec =
--   pfSetVec0' pf f where
--     f _ = applyvec 

-- -- WARNING : `applyvec` etc. modify last argument




-- -- PETSC_EXTERN PetscErrorCode PFApply(PF,PetscInt,const PetscScalar*,PetscScalar*);

-- -- PETSC_EXTERN PetscErrorCode PFApplyVec(PF,Vec,Vec);



















-- * SNES


snesCreate' :: Comm -> IO (SNES, CInt)
snesCreate' cc = withPtr $ \p -> [C.exp| int{SNESCreate($(int c), $(SNES *p))}|] where
  c = unComm cc

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
PetscErrorCode SNESFunction(SNES snes,Vec x,Vec y,void *ctx);
Input Parameters :

snes	- the SNES context
x	- point at which to evaluate residual
ctx	- optional user-defined function context, passed in with SNESSetFunction()
Output Parameter

y       - vector for residual (function value) 
-}

-- snesSetFunction0' :: SNES
--                      -> Vec
--                      -> (SNES -> Vec -> Vec -> Ptr () -> IO CInt)
--                      -> Ptr ()
--                      -> IO CInt
-- snesSetFunction0' snes r f ctx =
--   [C.exp|int{SNESSetFunction($(SNES snes), $(Vec r),
--                              $fun:(int (*f)(SNES, Vec, Vec, void*) ),
--                              $(void* ctx))}|]
  
snesSetFunction_' ::
  SNES -> Vec -> (SNES -> Vec -> Vec -> Ptr () -> IO CInt) -> IO CInt
snesSetFunction_' snes r f =
  [C.exp|int{SNESSetFunction($(SNES snes), $(Vec r),
                             $fun:(int (*f)(SNES, Vec, Vec, void*) ),
                             NULL )}|]




-- PetscErrorCode  SNESComputeFunction(SNES snes,Vec x,Vec y)
-- Collective on SNES
-- Input Parameters :
-- snes	- the SNES context
-- x	- input vector
-- Output Parameter :
-- y -function vector, as set by SNESSetFunction()
snesComputeFunction' :: SNES -> Vec -> Vec -> IO CInt
snesComputeFunction' snes x y = [C.exp|int{SNESComputeFunction($(SNES snes),$(Vec x),$(Vec y))}|]

    


-- PETSC_EXTERN PetscErrorCode SNESDestroy(SNES*);
snesDestroy' :: SNES -> IO CInt
snesDestroy' p = with p $ \pp -> [C.exp| int{SNESDestroy($(SNES *pp))}  |]


-- PETSC_EXTERN PetscErrorCode SNESSetUp(SNES);
snesSetUp' :: SNES -> IO CInt
snesSetUp' s = [C.exp|int{SNESSetUp($(SNES s))}|]

-- PetscErrorCode  SNESSetFromOptions(SNES snes)
snesSetFromOptions' :: SNES -> IO CInt
snesSetFromOptions' s = [C.exp|int{SNESSetFromOptions($(SNES s))}|]

-- PETSC_EXTERN PetscErrorCode SNESSolve(SNES,Vec,Vec);
snesSolve' :: SNES -> Vec -> Vec -> IO CInt
snesSolve' s b x = [C.exp|int{SNESSolve($(SNES s), $(Vec b), $(Vec x))}|]

snesSolve0' :: SNES -> Vec -> IO CInt
snesSolve0' s x = [C.exp|int{SNESSolve($(SNES s), NULL, $(Vec x))}|]



-- PETSC_EXTERN PetscErrorCode SNESGetSolution(SNES,Vec*);
snesGetSolution' :: SNES -> IO (Vec, CInt)
snesGetSolution' s = withPtr ( \v ->
  [C.exp|int{SNESGetSolution($(SNES s), $(Vec *v))}|] ) 

snesGetConvergedReason' :: SNES -> IO (CInt, CInt)
snesGetConvergedReason' s =  withPtr ( \v ->
  [C.exp|int{SNESGetConvergedReason($(SNES s), $(int* v))}|] ) 


-- PetscErrorCode  SNESReasonView(SNES snes,PetscViewer viewer)
snesReasonView' :: SNES -> PetscViewer -> IO CInt
snesReasonView' s vi = [C.exp|int{SNESReasonView($(SNES s),$(PetscViewer vi))}|]



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

snesSetJacobian' :: SNES
                    -> Mat
                    -> Mat
                    -> (SNES -> Vec -> Mat -> Mat -> IO CInt)
                    -> Ptr ()
                    -> IO CInt
snesSetJacobian' snes amat pmat f =
  snesSetJacobian0' snes amat pmat f' where
    f' s v a p _ = f s v a p
    snesSetJacobian0' snes_ amat_ pmat_ fc ctx =
      [C.exp|int{SNESSetJacobian($(SNES snes_),$(Mat amat_),$(Mat pmat_),
                         $fun:(int (*fc)(SNES,Vec,Mat,Mat,void*)),$(void* ctx))}|]



snesSetJacobian_' ::
  SNES -> Mat -> Mat -> (SNES -> Vec -> Mat -> Mat -> IO CInt) -> IO CInt
snesSetJacobian_' snes amat pmat f =
  snesSetJacobian0_' snes amat pmat f' where
    f' s v a p _ = f s v a p
    snesSetJacobian0_' snes_ amat_ pmat_ fc =
      [C.exp|int{SNESSetJacobian($(SNES snes_),$(Mat amat_),$(Mat pmat_),
                             $fun:(int (*fc)(SNES,Vec,Mat,Mat,void*)), NULL)}|]


-- -- monomorphic SNESSetJacobian : see e.g. www.mcs.anl.gov/petsc/petsc-current/src/snes/examples/tutorials/ex5s.c.html
-- -- usage : SNESSetJacobian(snes,J,J,SNESComputeJacobianDefaultColor,fdcoloring);
snesSetJacobian0mono' ::
  SNES
  -> Mat
  -> Mat
  -> (SNES -> Vec -> Mat -> Mat -> Ptr MatFDColoring -> IO CInt)
  -> MatFDColoring
  -> IO CInt
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

snesComputeJacobianDefault0' :: SNES -> Vec -> Mat -> Mat -> Ptr () -> IO CInt
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
snesGetLineSearch' :: SNES -> IO (SNESLineSearch, CInt)
snesGetLineSearch' snes =
  withPtr ( \ls ->
     [C.exp|int{SNESGetLineSearch($(SNES snes),
                                  $(SNESLineSearch* ls))}|]) 



-- PetscErrorCode SNESSetLineSearch(SNES snes, SNESLineSearch linesearch)
snesSetLineSearch' :: SNES -> SNESLineSearch -> IO CInt
snesSetLineSearch' snes ls = [C.exp|int{SNESSetLineSearch($(SNES snes),$(SNESLineSearch ls))}|]



-- PetscErrorCode  SNESLineSearchSetPostCheck(SNESLineSearch linesearch, PetscErrorCode (*func)(SNESLineSearch,Vec,Vec,Vec,PetscBool*,PetscBool*,void*),void *ctx)
-- Sets a user function that is called after the line search has been applied to determine the step direction and length. Allows the user a chance to change or override the decision of the line search routine  -- Logically Collective on SNESLineSearch
-- Input Parameters :
-- linesearch	- the SNESLineSearch context
-- func	- [optional] function evaluation routine, see SNESLineSearchPostCheckFunction for the calling sequence
-- ctx	- [optional] user-defined context for private data for the function evaluation routine (may be NULL)

snesLineSearchSetPostCheck' ::
  SNESLineSearch
  -> (SNESLineSearch
      -> Vec
      -> Vec
      -> Vec
      -> Ptr PetscBool
      -> Ptr PetscBool
      -> Ptr ()
      -> IO CInt)
  -> Ptr ()
  -> IO CInt
snesLineSearchSetPostCheck' snesls f ctx =
  [C.exp|
   int{SNESLineSearchSetPostCheck(
          $(SNESLineSearch snesls),
          $fun:(int(*f)(SNESLineSearch,Vec,Vec,Vec,PetscBool*,PetscBool*,void*)),
          $(void* ctx))}|]

snesLineSearchSetPostCheck0' ::
  SNESLineSearch
  -> (SNESLineSearch
      -> Vec
      -> Vec
      -> Vec
      -> Ptr PetscBool
      -> Ptr PetscBool
      -> Ptr ()
      -> IO CInt)
  -> IO CInt
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


-- PetscErrorCode  SNESLineSearchSetTolerances(SNESLineSearch linesearch,PetscReal steptol,PetscReal maxstep, PetscReal rtol, PetscReal atol, PetscReal ltol, PetscInt max_its)
snesLineSearchSetTolerances' ::
  SNESLineSearch
  -> PetscReal_
  -> PetscReal_
  -> PetscReal_
  -> PetscReal_
  -> PetscReal_
  -> Int
  -> IO CInt
snesLineSearchSetTolerances' ls steptol maxstep rtol atol ltol maxits =
  [C.exp|int{SNESLineSearchSetTolerances($(SNESLineSearch ls),$(PetscReal steptol),$(PetscReal maxstep),$(PetscReal rtol),$(PetscReal atol),$(PetscReal ltol),$(int mi))}|] where
  mi = toCInt maxits
-- Input Parameters :
-- linesearch	- linesearch context
-- steptol	- The minimum steplength
-- maxstep	- The maximum steplength
-- rtol	- The relative tolerance for iterative line searches
-- atol	- The absolute tolerance for iterative line searches
-- ltol	- The change in lambda tolerance for iterative line searches
-- max_it	- The maximum number of iterations of the line search
-- Notes :
-- The user may choose to not set any of the tolerances using PETSC_DEFAULT in place of an argument.



-- PetscErrorCode  SNESLineSearchGetLambda(SNESLineSearch linesearch,PetscReal *lambda)
-- get linesearch step length
snesLineSearchGetLambda' :: SNESLineSearch -> IO (PetscReal_, CInt)
snesLineSearchGetLambda' ls = withPtr $ \lam -> [C.exp|int{SNESLineSearchGetLambda($(SNESLineSearch ls), $(PetscReal* lam))}|]
-- Input Parameters :
-- linesearch -linesearch context 
-- Output Parameters :
-- lambda -The last steplength computed during SNESLineSearchApply() 
-- Notes :
-- This is useful in methods where the solver is ill-scaled and requires some adaptive notion of the difference in scale between the solution and the function. For instance, SNESQN may be scaled by the line search lambda using the argument -snes_qn_scaling ls.

-- PetscErrorCode  SNESLineSearchSetLambda(SNESLineSearch linesearch, PetscReal lambda)
snesLineSearchSetLambda' :: SNESLineSearch -> PetscReal_ -> IO CInt
snesLineSearchSetLambda' ls lam = [C.exp|int{SNESLineSearchSetLambda($(SNESLineSearch ls), $(PetscReal lam))}|]
-- Input Parameters :
-- linesearch	- linesearch context
-- lambda	- The last steplength.
-- Notes :
-- This routine is typically used within implementations of SNESLineSearchApply() to set the final steplength. This routine (and SNESLineSearchGetLambda()) were added in order to facilitate Quasi-Newton methods that use the previous steplength as an inner scaling parameter.





-- PetscErrorCode SNESVISetVariableBounds(SNES snes, Vec xl, Vec xu)
-- Input Parameters :
-- snes -the SNES context. 
-- xl -lower bound. 
-- xu -upper bound. 
-- Notes :
-- If this routine is not called then the lower and upper bounds are set to PETSC_NINFINITY and PETSC_INFINITY respectively during SNESSetUp().

snesVISetVariableBounds' :: SNES -> Vec -> Vec -> IO CInt
snesVISetVariableBounds' snes xl xu =
  [C.exp|int{SNESVISetVariableBounds($(SNES snes),$(Vec xl),$(Vec xu))}|]




-- | SNES blocks


snesSetFunctionBlock1 snes r f =
  [C.block|
   int{
      SNESSetFunction($(SNES snes),$(Vec r),$fun:(int (* funIO) (SNES s, Vec x, Vec y, void* p)), NULL);
      return 0;
      }
          |] where
        funIO _s xin yout _p = do
          f xin yout
          return (0 :: CInt)

-- PetscErrorCode Wrapper(SNES snes,Vec x,Vec y,void *dummy)

-- snesFunctionWrap _snes x y f =
--   [C.block|int{
--       const PetscScalar *xx;
--       PetscScalar *yy;
--       VecGetArrayRead($(Vec x), &xx);
--       VecGetArray($(Vec y), &yy);

--       VecRestoreArrayRead($(Vec x), &xx);
--       VecRestoreArray($(Vec y), &yy);
--       return 0;
--               }
--    |]

















snesView' :: SNES -> PetscViewer -> IO CInt
snesView' m v = [C.exp|int{SNESView($(SNES m),$(PetscViewer v))}|]





snesGetKsp' :: SNES -> IO (KSP, CInt)
snesGetKsp' s = withPtr $ \k -> [C.exp|int{SNESGetKSP($(SNES s),$(KSP* k))}|] 


















-- * TS




-- PetscErrorCode  TSCreate(MPI_Comm comm, TS *ts)
tsCreate' :: Comm -> IO (TS, CInt)
tsCreate' cc =
  withPtr (\ts -> [C.exp|int{TSCreate($(int c), $(TS* ts))}|]) 
  where
   c = unComm cc

tsDestroy' :: TS -> IO CInt
tsDestroy' ts = with ts $ \tsp -> [C.exp| int{TSDestroy($(TS* tsp))} |] 


-- PetscErrorCode  TSSetProblemType(TS ts, TSProblemType type)
tsSetProblemType' :: TS -> TsProblemType -> IO CInt
tsSetProblemType' ts t =
  [C.exp|int{TSSetProblemType($(TS ts), $(int tt))}|] 
   where tt = fromIntegral $ tsProblemTypeToInt t


-- PetscErrorCode TSSetInitialTimeStep(TS ts,PetscReal initial_time,PetscReal time_step)
tsSetInitialTimeStep' :: TS -> PetscReal_ -> PetscReal_ -> IO CInt
tsSetInitialTimeStep' ts it dt =
  [C.exp|int{TSSetInitialTimeStep($(TS ts), $(PetscReal it), $(PetscReal dt))}|] 

-- PetscErrorCode  TSSetDuration(TS ts,PetscInt maxsteps,PetscReal maxtime)
tsSetDuration' :: TS -> Int -> PetscReal_ -> IO CInt
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

tsSetIFunction0' :: TS
                    -> Vec
                    -> (TS -> PetscReal_ -> Vec -> Vec -> Vec -> Ptr () -> IO CInt)
                    -> IO CInt
tsSetIFunction0' ts res f =
  [C.exp|int{TSSetIFunction($(TS ts),
                            $(Vec res),
                            $fun:(int (*f)(TS,PetscReal,Vec,Vec,Vec,void*)),
                            NULL)}|]

tsSetIFunction' :: TS
                   -> Vec
                   -> (TS -> PetscReal_ -> Vec -> Vec -> Vec -> Ptr () -> IO CInt)
                   -> Ptr ()
                   -> IO CInt
tsSetIFunction' ts res f ctx =
  [C.exp|int{TSSetIFunction($(TS ts),
                            $(Vec res),
                            $fun:(int (*f)(TS,PetscReal,Vec,Vec,Vec,void*)),
                            $(void* ctx))}|]


  
-- PetscErrorCode  TSSetIJacobian(TS ts,Mat Amat,Mat Pmat,TSIJacobian f,void *ctx)
tsSetIJacobian0' :: TS
                    -> Mat
                    -> Mat
                    -> (TS
                        -> PetscReal_
                        -> Vec
                        -> Vec
                        -> PetscReal_
                        -> Mat
                        -> Mat
                        -> Ptr ()
                        -> IO CInt)
                    -> IO CInt
tsSetIJacobian0' ts amat pmat f =
  [C.exp|int{TSSetIJacobian($(TS ts),
                     $(Mat amat),
                     $(Mat pmat),
                     $fun:(int (*f)(TS,PetscReal,Vec,Vec,PetscReal,Mat,Mat,void*)), NULL)}|]

tsSetIJacobian' :: TS
                   -> Mat
                   -> Mat
                   -> (TS
                       -> PetscReal_
                       -> Vec
                       -> Vec
                       -> PetscReal_
                       -> Mat
                       -> Mat
                       -> Ptr ()
                       -> IO CInt)
                   -> Ptr ()
                   -> IO CInt
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
tsSetRHSFunction0' :: TS
                      -> Vec
                      -> (TS -> PetscReal_ -> Vec -> Vec -> Ptr () -> IO CInt)
                      -> IO CInt
tsSetRHSFunction0' ts r f =
  [C.exp|int{TSSetRHSFunction(
                $(TS ts),
                $(Vec r),
                $fun:(int (*f)(TS, PetscReal,Vec,Vec,void*)),
                NULL)}
        |]
  
tsSetRHSFunction' :: TS
                     -> Vec
                     -> (TS -> PetscReal_ -> Vec -> Vec -> Ptr () -> IO CInt)
                     -> Ptr ()
                     -> IO CInt
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

tsSetRHSJacobian0' :: TS
                      -> Mat
                      -> Mat
                      -> (TS -> PetscReal_ -> Vec -> Mat -> Mat -> Ptr () -> IO CInt)
                      -> IO CInt
tsSetRHSJacobian0' ts amat pmat f =
  [C.exp|int{TSSetRHSJacobian($(TS ts),$(Mat amat),$(Mat pmat),$fun:(int(*f)(TS, PetscReal, Vec, Mat, Mat, void*)),NULL)}|]

tsSetRHSJacobian' :: TS
                     -> Mat
                     -> Mat
                     -> (TS -> PetscReal_ -> Vec -> Mat -> Mat -> Ptr () -> IO CInt)
                     -> Ptr ()
                     -> IO CInt
tsSetRHSJacobian' ts amat pmat f ctx =
  [C.exp|int{TSSetRHSJacobian($(TS ts),$(Mat amat),$(Mat pmat),$fun:(int(*f)(TS, PetscReal, Vec, Mat, Mat, void*)),$(void* ctx))}|]


-- PetscErrorCode  TSSetDM(TS ts,DM dm)
-- Sets the DM that may be used by some preconditioners
-- Logically Collective on TS and DM
-- Input Parameters :
-- ts	- the preconditioner context
-- dm	- the dm
tsSetDm' :: TS -> DM -> IO CInt
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
tsSetSolution' :: TS -> Vec -> IO CInt
tsSetSolution' ts u = [C.exp|int{TSSetSolution($(TS ts),$(Vec u))}|]


--    TSSolve(ts,u);
-- PetscErrorCode TSSolve(TS ts,Vec u)        -- Collective on TS
-- Input Parameters :
-- ts	- the TS context obtained from TSCreate()
-- u	- the solution vector (can be null if TSSetSolution() was used, otherwise must contain the initial conditions)
-- Notes :
-- The final time returned by this function may be different from the time of the internally held state accessible by TSGetSolution() and TSGetTime() because the method may have stepped over the final time.
tsSolve' :: TS -> Vec -> IO CInt
tsSolve' ts u = [C.exp|int{TSSolve($(TS ts),$(Vec u))}|]

tsSolve_' :: TS -> IO CInt
tsSolve_' ts = [C.exp|int{TSSolve($(TS ts), NULL)}|]


tsStep' :: TS -> IO CInt
tsStep' ts = [C.exp|int{TSStep($(TS ts))}|]


-- PetscErrorCode  TSGetConvergedReason(TS ts,TSConvergedReason *reason)
tsGetConvergedReason' :: TS -> IO (CInt, CInt)
tsGetConvergedReason' ts = withPtr (\p -> [C.exp|int{TSGetConvergedReason($(TS ts),$(int* p))}|])


-- PetscErrorCode  TSSetExactFinalTime(TS ts,TSExactFinalTimeOption eftopt)
-- Logically Collective on TS
-- Input Parameter :
-- ts	- the time-step context
-- eftopt	- exact final time option
tsSetExactFinalTime' :: TS -> TsExactFinalTimeOption_ -> IO CInt
tsSetExactFinalTime' ts ft  = [C.exp|int{TSSetExactFinalTime($(TS ts),$(int o))}|] where o = tsExactFinalTimeOptionToCInt ft


--    TSGetTimeStepNumber(ts,&steps);

--    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
--       View timestepping solver info
--       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

--    TSView(ts,PETSC_VIEWER_STDOUT_SELF);
tsViewStdout' :: TS -> IO CInt
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
tsSetCostGradients' :: TS -> PetscInt_ -> Ptr Vec -> Ptr Vec -> IO CInt
tsSetCostGradients' ts numcost lambda mu =
  [C.exp|int{TSSetCostGradients($(TS ts),$(PetscInt numcost),$(Vec* lambda),$(Vec* mu))}|]



-- PetscErrorCode  TSSetCostIntegrand(TS ts,PetscInt numcost, PetscErrorCode (*rf)(TS,PetscReal,Vec,Vec,void*),
-- PetscErrorCode (*drdyf)(TS,PetscReal,Vec,Vec*,void*),
-- PetscErrorCode (*drdpf)(TS,PetscReal,Vec,Vec*,void*),PetscBool fwd, void *ctx)
tsSetCostIntegrand0' :: TS
                        -> PetscInt_
                        -> (TS -> PetscReal_ -> Vec -> Vec -> Ptr () -> IO CInt)
                        -> (TS -> PetscReal_ -> Vec -> Ptr Vec -> Ptr () -> IO CInt)
                        -> (TS -> PetscReal_ -> Vec -> Ptr Vec -> Ptr () -> IO CInt)
                        -> PetscBool
                        -> IO CInt
tsSetCostIntegrand0' ts n rf drdyf drdpf fwdf =
  [C.exp|
   int{TSSetCostIntegrand($(TS ts),
                          $(PetscInt n),
                          $fun:(int (*rf)(TS,PetscReal,Vec,Vec,void*)),
                          $fun:(int (*drdyf)(TS,PetscReal,Vec,Vec*,void*)),
                          $fun:(int (*drdpf)(TS,PetscReal,Vec,Vec*,void*)),$(PetscBool fwdf), NULL)}|]

tsSetCostIntegrand' :: TS
                       -> PetscInt_
                       -> (TS -> PetscReal_ -> Vec -> Vec -> Ptr () -> IO CInt)
                       -> (TS -> PetscReal_ -> Vec -> Ptr Vec -> Ptr () -> IO CInt)
                       -> (TS -> PetscReal_ -> Vec -> Ptr Vec -> Ptr () -> IO CInt)
                       -> PetscBool 
                       -> Ptr ()
                       -> IO CInt
tsSetCostIntegrand' ts n rf drdyf drdpf fwdf ctx =
  [C.exp|
   int{TSSetCostIntegrand($(TS ts),
                          $(PetscInt n),
                          $fun:(int (*rf)(TS,PetscReal,Vec,Vec,void*)),
                          $fun:(int (*drdyf)(TS,PetscReal,Vec,Vec*,void*)),
                          $fun:(int (*drdpf)(TS,PetscReal,Vec,Vec*,void*)),$(PetscBool fwdf), $(void* ctx))}|]  

tsGetCostIntegral' :: TS -> IO (Vec, CInt)
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

tsAdjointSetRHSJacobian0' :: TS
                             -> Mat
                             -> (TS -> PetscReal_ -> Vec -> Mat -> Ptr () -> IO CInt)
                             -> IO CInt
tsAdjointSetRHSJacobian0' ts amat f =
  [C.exp|int{TSAdjointSetRHSJacobian($(TS ts),
                                     $(Mat amat),
                                     $fun:(int (*f)(TS, PetscReal,Vec, Mat, void*)),
                                     NULL)}|]

tsAdjointSetRHSJacobian' :: TS
                            -> Mat
                            -> (TS -> PetscReal_ -> Vec -> Mat -> Ptr () -> IO CInt)
                            -> Ptr ()
                            -> IO CInt
tsAdjointSetRHSJacobian' ts amat f ctx =
  [C.exp|int{TSAdjointSetRHSJacobian($(TS ts),
                                     $(Mat amat),
                                     $fun:(int (*f)(TS, PetscReal,Vec, Mat, void*)),
                                     $(void* ctx))}|]
  

-- PetscErrorCode TSAdjointSolve(TS ts)
tsAdjointSolve' :: TS -> IO CInt
tsAdjointSolve' ts = [C.exp|int{TSAdjointSolve($(TS ts))}|]




-- | TS Trajectory (state history)

-- PETSC_EXTERN PetscErrorCode TSSetSaveTrajectory(TS);
tsSetSaveTrajectory' :: TS -> IO CInt
tsSetSaveTrajectory' ts = [C.exp|int{TSSetSaveTrajectory($(TS ts))}|]

-- PETSC_EXTERN PetscErrorCode TSTrajectoryCreate(MPI_Comm,TSTrajectory*);
tsTrajectoryCreate' :: Comm -> IO (TSTrajectory, CInt)
tsTrajectoryCreate' cc = withPtr $ \tst ->
  [C.exp|int{TSTrajectoryCreate($(int c),$(TSTrajectory* tst))}|] where
    c = unComm cc
  
-- PETSC_EXTERN PetscErrorCode TSTrajectoryDestroy(TSTrajectory*);
tsTrajectoryDestroy' :: TSTrajectory -> IO CInt
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
taoCreate' :: Comm -> IO (Tao, CInt)
taoCreate' cc = withPtr (\p -> [C.exp| int{TaoCreate($(int c), $(Tao *p))} |] )
  where
  c = unComm cc

taoDestroy' :: Tao -> IO CInt
taoDestroy' p = with p ( \pp -> [C.exp| int{TaoDestroy($(Tao *pp))}  |] ) 

taoSetType' :: Tao -> TaoType_ -> IO CInt
taoSetType' tao ti = withCString ti_ ( \tip -> [C.exp|int{TaoSetType($(Tao tao), $(char* tip ))}|] )
  where
  ti_ = taoTypeToStr ti

taoViewStdout' :: Tao -> IO CInt
taoViewStdout' tao = [C.exp|int{TaoView($(Tao tao), PETSC_VIEWER_STDOUT_SELF)}|]

-- taoGetConvergedReason tao = liftM taoConvergedIntToReason $
--    withPtr (\tr -> [C.exp|int{TaoGetConvergedReason($(Tao tao), $(int* tr))}|]) 


-- TaoSetInitialVector(TaoSolver tao, Vec x);
taoSetInitialVector' :: Tao -> Vec -> IO CInt
taoSetInitialVector' tao x = [C.exp|int{TaoSetInitialVector($(Tao tao),$(Vec x))}|] 

-- TaoSolve(TaoSolver tao);
taoSolve' :: Tao -> IO CInt
taoSolve' tao = [C.exp|int{TaoSolve($(Tao tao))}|] 



-- PETSC_EXTERN PetscErrorCode TaoGetSolutionVector(Tao, Vec*);
taoGetSolutionVector' :: Tao -> IO (Vec, CInt)
taoGetSolutionVector' tao = withPtr (\p -> [C.exp|int{TaoGetSolutionVector($(Tao tao), $(Vec* p))}|])

-- PETSC_EXTERN PetscErrorCode TaoGetGradientVector(Tao, Vec*);
taoGetGradientVector' :: Tao -> IO (Vec, CInt)
taoGetGradientVector' tao = withPtr (\p -> [C.exp|int{TaoGetGradientVector($(Tao tao), $(Vec* p))}|]) 

-- PETSC_EXTERN PetscErrorCode TaoSetObjectiveRoutine(Tao, PetscErrorCode(*)(Tao, Vec, PetscReal*,void*), void*);
taoSetObjectiveRoutine ::
  Tao ->
  (Tao -> Vec -> Ptr PetscReal_ -> IO CInt) ->
  IO CInt  
taoSetObjectiveRoutine t f = taoSetObjectiveRoutine' t f' where
  f' ta v r _ = f ta v r
  taoSetObjectiveRoutine' tao ff =
    [C.exp|int{TaoSetObjectiveRoutine($(Tao tao),
                                      $fun:(int (*ff)(Tao, Vec, PetscReal*, void*)),
                                      NULL)}|]


-- PETSC_EXTERN PetscErrorCode TaoSetGradientRoutine(Tao, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);

taoSetGradientRoutine :: Tao -> (Tao -> Vec -> Vec -> IO CInt) -> IO CInt
taoSetGradientRoutine t f = taoSetGradientRoutine' t f' where
  f' ta v r _ = f ta v r
  taoSetGradientRoutine' tao fc =
    [C.exp|int{TaoSetGradientRoutine($(Tao tao),
                                     $fun:(int (*fc)(Tao, Vec, Vec, void*)),
                                     NULL)}|]
  
-- PETSC_EXTERN PetscErrorCode TaoSetObjectiveAndGradientRoutine(Tao, PetscErrorCode(*)(Tao, Vec, PetscReal*, Vec, void*), void*);

taoSetObjectiveAndGradientRoutine ::
  Tao ->
  (Tao -> Vec -> Ptr PetscReal_ -> Vec -> IO CInt) ->
  IO CInt
taoSetObjectiveAndGradientRoutine t f =
  taoSetObjectiveAndGradientRoutine' t f' where
    f' ta v r v2 _ = f ta v r v2
    taoSetObjectiveAndGradientRoutine' tao fc =
      [C.exp|int{TaoSetObjectiveAndGradientRoutine(
                    $(Tao tao),
                    $fun:(int (*fc)(Tao, Vec, PetscReal*, Vec, void*)),
                    NULL)}|] 
  
-- PETSC_EXTERN PetscErrorCode TaoSetHessianRoutine(Tao,Mat,Mat,PetscErrorCode(*)(Tao,Vec, Mat, Mat, void*), void*);

taoSetHessianRoutine ::
  Tao -> Mat -> Mat ->
  (Tao -> Vec -> Mat -> Mat -> IO CInt) ->
  IO CInt
taoSetHessianRoutine t m1 m2 f = taoSetHessianRoutine' t m1 m2 f' where
  f' ta v n1 n2 _ = f ta v n1 n2
  taoSetHessianRoutine' tao m1_ m2_ fc =
    [C.exp|int{TaoSetHessianRoutine($(Tao tao), $(Mat m1_), $(Mat m2_),
                                    $fun:(int (*fc)(Tao, Vec, Mat, Mat, void*)),
                                    NULL)}|]


-- PETSC_EXTERN PetscErrorCode TaoSetSeparableObjectiveRoutine(Tao, Vec, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);
-- PETSC_EXTERN PetscErrorCode TaoSetConstraintsRoutine(Tao, Vec, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);

taoSetConstraintsRoutine' ::
  Tao -> Vec ->
  (Tao -> Vec -> Vec -> IO CInt) -> IO CInt
taoSetConstraintsRoutine' t v f = taoSetConstraintsRoutine0' t v f' where
  f' ta v1 v2  _ = f ta v1 v2
  taoSetConstraintsRoutine0' tao vec fc =
    [C.exp|int{TaoSetConstraintsRoutine(
                  $(Tao tao), $(Vec vec),
                  $fun:(int (*fc)(Tao, Vec, Vec, void*)),
                  NULL)}|]
  
-- PETSC_EXTERN PetscErrorCode TaoSetInequalityConstraintsRoutine(Tao, Vec, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);

taoSetInequalityConstraintsRoutine' ::
  Tao -> Vec ->
  (Tao -> Vec -> Vec -> IO CInt) -> IO CInt
taoSetInequalityConstraintsRoutine' t v f =
  taoSetInequalityConstraintsRoutine0' t v f' where
    f' ta v1 v2  _ = f ta v1 v2
    taoSetInequalityConstraintsRoutine0' tao vec fc =
      [C.exp|int{TaoSetInequalityConstraintsRoutine(
                    $(Tao tao), $(Vec vec),
                    $fun:(int (*fc)(Tao, Vec, Vec, void*)),
                    NULL)}|] 
  
-- PETSC_EXTERN PetscErrorCode TaoSetEqualityConstraintsRoutine(Tao, Vec, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);

taoSetEqualityConstraintsRoutine' ::
  Tao -> Vec ->
  (Tao -> Vec -> Vec -> IO CInt) -> IO CInt
taoSetEqualityConstraintsRoutine' t v f =
  taoSetEqualityConstraintsRoutine0' t v f' where
     f' ta v1 v2  _ = f ta v1 v2
     taoSetEqualityConstraintsRoutine0' tao vec fc =
       [C.exp|int{TaoSetEqualityConstraintsRoutine(
                     $(Tao tao), $(Vec vec),
                     $fun:(int (*fc)(Tao, Vec, Vec, void*)),
                     NULL)}|]
  
-- PETSC_EXTERN PetscErrorCode TaoSetJacobianRoutine(Tao,Mat,Mat, PetscErrorCode(*)(Tao,Vec, Mat, Mat, void*), void*);


taoSetJacobianRoutine' ::
  Tao -> Mat -> Mat ->
  (Tao -> Vec -> Mat -> Mat -> IO CInt) -> IO CInt
taoSetJacobianRoutine' t m1 m2 f =
  taoSetJacobianRoutine0' t m1 m2 f' where
     f' ta v v1 v2  _ = f ta v v1 v2
     taoSetJacobianRoutine0' tao m1_ m2_ fc =
       [C.exp|int{TaoSetJacobianRoutine(
                     $(Tao tao), $(Mat m1_), $(Mat m2_),
                     $fun:(int (*fc)(Tao, Vec, Mat, Mat, void*)),
                     NULL)}|]
     
-- PETSC_EXTERN PetscErrorCode TaoSetJacobianStateRoutine(Tao,Mat,Mat,Mat, PetscErrorCode(*)(Tao,Vec, Mat, Mat, Mat, void*), void*);

taoSetJacobianStateRoutine' ::
  Tao
  -> Mat
  -> Mat
  -> Mat
  -> (Tao -> Vec -> Mat -> Mat -> Mat -> IO CInt)
  -> IO CInt
taoSetJacobianStateRoutine' t m1 m2 m3 f =
  taoSetJacobianStateRoutine0' t m1 m2 m3 f' where
     f' ta v v1 v2 v3  _ = f ta v v1 v2 v3
     taoSetJacobianStateRoutine0' tao m1_ m2_ m3_ fc =
       [C.exp|int{TaoSetJacobianStateRoutine(
                     $(Tao tao), $(Mat m1_), $(Mat m2_), $(Mat m3_),
                     $fun:(int (*fc)(Tao, Vec, Mat, Mat, Mat, void*)),
                     NULL)}|] 

-- PETSC_EXTERN PetscErrorCode TaoSetJacobianDesignRoutine(Tao,Mat,PetscErrorCode(*)(Tao,Vec, Mat, void*), void*);

taoSetJacobianDesignRoutine' ::
  Tao ->
  Mat ->
  (Tao -> Vec -> Mat -> IO CInt) ->
  IO CInt
taoSetJacobianDesignRoutine' t m f =
  taoSetJacobianDesignRoutine0' t m f' where
     f' ta v v1  _ = f ta v v1
     taoSetJacobianDesignRoutine0' tao m_ fc =
       [C.exp|int{TaoSetJacobianDesignRoutine(
                     $(Tao tao), $(Mat m_),
                     $fun:(int (*fc)(Tao, Vec, Mat, void*)),
                     NULL)}|] 
     
-- PETSC_EXTERN PetscErrorCode TaoSetJacobianInequalityRoutine(Tao,Mat,Mat,PetscErrorCode(*)(Tao,Vec, Mat, Mat, void*), void*);
-- PETSC_EXTERN PetscErrorCode TaoSetJacobianEqualityRoutine(Tao,Mat,Mat,PetscErrorCode(*)(Tao,Vec, Mat, Mat, void*), void*);

-- PETSC_EXTERN PetscErrorCode TaoSetStateDesignIS(Tao, IS, IS);

-- PETSC_EXTERN PetscErrorCode TaoComputeObjective(Tao, Vec, PetscReal*);
taoComputeObjective' :: Tao -> Vec -> IO (PetscReal_, CInt)
taoComputeObjective' tao v =
  withPtr (\p -> [C.exp|int{TaoComputeObjective($(Tao tao),$(Vec v),$(PetscReal* p))}|] ) 
-- PETSC_EXTERN PetscErrorCode TaoComputeSeparableObjective(Tao, Vec, Vec);

-- PETSC_EXTERN PetscErrorCode TaoComputeGradient(Tao, Vec, Vec);
taoComputeGradient0' :: Tao -> Vec -> Vec -> IO CInt
taoComputeGradient0' tao v g=
  [C.exp|int{TaoComputeGradient($(Tao tao),$(Vec v),$(Vec g))}|] 
  
-- PETSC_EXTERN PetscErrorCode TaoComputeObjectiveAndGradient(Tao, Vec, PetscReal*, Vec);
-- PETSC_EXTERN PetscErrorCode TaoComputeConstraints(Tao, Vec, Vec);
-- PETSC_EXTERN PetscErrorCode TaoComputeInequalityConstraints(Tao, Vec, Vec);
-- PETSC_EXTERN PetscErrorCode TaoComputeEqualityConstraints(Tao, Vec, Vec);
-- PETSC_EXTERN PetscErrorCode TaoDefaultComputeGradient(Tao, Vec, Vec, void*);
-- PETSC_EXTERN PetscErrorCode TaoIsObjectiveDefined(Tao,PetscBool*);
taoIsObjectiveDefined' :: Tao -> IO (PetscBool, CInt)
taoIsObjectiveDefined' t =
 withPtr
  (\p -> [C.exp|int{TaoIsObjectiveDefined($(Tao t),
                                          $(PetscBool* p))}|]) 

-- PETSC_EXTERN PetscErrorCode TaoIsGradientDefined(Tao,PetscBool*);
taoIsGradientDefined' :: Tao -> IO (PetscBool, CInt)
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
taoSetVariableBounds' :: Tao -> Vec -> Vec -> IO CInt
taoSetVariableBounds' tao x1 x2 =
  [C.exp|int{TaoSetVariableBounds($(Tao tao), $(Vec x1), $(Vec x2))}|]
  
-- PETSC_EXTERN PetscErrorCode TaoGetVariableBounds(Tao, Vec*, Vec*);
-- PETSC_EXTERN PetscErrorCode TaoGetDualVariables(Tao, Vec*, Vec*);
-- PETSC_EXTERN PetscErrorCode TaoSetInequalityBounds(Tao, Vec, Vec);
-- PETSC_EXTERN PetscErrorCode TaoGetInequalityBounds(Tao, Vec*, Vec*);

-- PETSC_EXTERN PetscErrorCode TaoSetVariableBoundsRoutine(Tao, PetscErrorCode(*)(Tao, Vec, Vec, void*), void*);

taoSetVariableBoundsRoutine' ::
  Tao ->
  (Tao -> Vec -> Vec -> IO CInt) ->
  IO CInt
taoSetVariableBoundsRoutine' tao f =
  taoSetVariableBoundsRoutine0' tao f' where
    f' t v1 v2 _ = f t v1 v2
    taoSetVariableBoundsRoutine0' tao_ fc =
      [C.exp|int{TaoSetVariableBoundsRoutine($(Tao tao_),
                                             $fun:(int (*fc)(Tao, Vec, Vec, void*)),
                                             NULL)}|]

  
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





-- | -- TaoLineSearch

-- PetscErrorCode TaoLineSearchCreate(MPI_Comm comm, TaoLineSearch *newls)
taoLineSearchCreate' :: Comm -> IO (TaoLineSearch, CInt)
taoLineSearchCreate' cc = withPtr $ \tls -> [C.exp|int{TaoLineSearchCreate($(int c), $(TaoLineSearch* tls))}|] where
  c = unComm cc

-- PetscErrorCode TaoLineSearchDestroy(TaoLineSearch *ls)
taoLineSearchDestroy' :: TaoLineSearch -> IO CInt
taoLineSearchDestroy' t = with t $ \tp -> [C.exp|int{TaoLineSearchDestroy($(TaoLineSearch* tp))}|]


-- PetscErrorCode TaoLineSearchSetType(TaoLineSearch ls, const TaoLineSearchType type)


-- PetscErrorCode TaoLineSearchGetStartingVector(TaoLineSearch ls, Vec *x)
-- Not Collective
-- Input Parameter :
-- ls - the TaoLineSearch context
-- Output :
-- x - The initial point of the line search
taoLineSearchGetStartingVector' :: TaoLineSearch -> IO (Vec, CInt)
taoLineSearchGetStartingVector' ls = withPtr $ \x0 ->
  [C.exp|int{TaoLineSearchGetStartingVector($(TaoLineSearch ls), $(Vec* x0))}|]


-- PetscErrorCode TaoLineSearchGetStepDirection(TaoLineSearch ls, Vec *s)
taoLineSearchGetStepDirection' :: TaoLineSearch -> IO (Vec, CInt)
taoLineSearchGetStepDirection' tls = withPtr $ \v ->
  [C.exp|int{TaoLineSearchGetStepDirection($(TaoLineSearch tls),$(Vec* v))}|]


-- PetscErrorCode TaoLineSearchSetInitialStepLength(TaoLineSearch ls,PetscReal s)
taoLineSearchSetInitialStepLength' :: TaoLineSearch -> PetscReal_ -> IO CInt
taoLineSearchSetInitialStepLength' ls s =
  [C.exp|int{TaoLineSearchSetInitialStepLength($(TaoLineSearch ls), $(PetscReal s))}|]




-- PetscErrorCode TaoLineSearchGetStepLength(TaoLineSearch ls,PetscReal *s)
taoLineSearchGetStepLength' :: TaoLineSearch -> IO (PetscReal_, CInt)
taoLineSearchGetStepLength' ls = withPtr $ \s ->
  [C.exp|int{TaoLineSearchGetStepLength($(TaoLineSearch ls), $(PetscReal* s))}|]


-- PetscErrorCode TaoLineSearchGetNumberFunctionEvaluations(TaoLineSearch ls, PetscInt *nfeval, PetscInt *ngeval, PetscInt *nfgeval)
-- Not Collective
-- Input Parameter :
-- ls -the TaoLineSearch context 
-- Outputs :
-- nfeval	- number of function evaluations
-- ngeval	- number of gradient evaluations
-- nfgeval	- number of function/gradient evaluations
taoLineSearchGetNumberFunctionEvaluations' ::
  TaoLineSearch -> IO ((PetscInt_, PetscInt_, PetscInt_) , CInt)
taoLineSearchGetNumberFunctionEvaluations' ls = withPtr3 $ \nfeval ngeval nfgeval ->
  [C.exp|int{TaoLineSearchGetNumberFunctionEvaluations($(TaoLineSearch ls),$(PetscInt* nfeval),$(PetscInt* ngeval), $(PetscInt* nfgeval))}|]


-- PetscErrorCode TaoLineSearchSetVariableBounds(TaoLineSearch ls,Vec xl, Vec xu)
-- Logically Collective on Tao
-- Input Parameters
-- ls	- the TaoLineSearch context
-- xl	- vector of lower bounds
-- xu	- vector of upper bounds
-- Note: If the variable bounds are not set with this routine, then PETSC_NINFINITY and PETSC_INFINITY are assumed
taoLineSearchSetVariableBounds' :: TaoLineSearch -> Vec -> Vec -> IO CInt
taoLineSearchSetVariableBounds' ls xl xu =
  [C.exp|int{TaoLineSearchSetVariableBounds($(TaoLineSearch ls),$(Vec xl),$(Vec xu))}|]



































-- * Viewer

-- petscViewerStdoutCreate' comm = withPtr $ \v ->
--   [C.exp|int{PETSC_VIEWER_STDOUT_($(int c))}|] where c = unComm comm

    
petscViewerCreate' :: Comm -> IO (PetscViewer, CInt)
petscViewerCreate' cc = withPtr $ \h -> 
  [C.exp|int{PetscViewerCreate($(int c),$(PetscViewer* h))}|] where c = unComm cc

-- PetscErrorCode  PetscViewerSetType(PetscViewer viewer,PetscViewerType type)
petscViewerSetType' :: PetscViewer -> PetscViewerType_ -> IO CInt
petscViewerSetType' v t = withCString ts $ \tsp ->
  [C.exp|int{PetscViewerSetType($(PetscViewer v),$(char* tsp))}|] where
    -- tc = toCInt $ viewerTypeToInt t
    ts = viewerTypeToStr t



-- PetscErrorCode  PetscViewerPushFormat(PetscViewer viewer,PetscViewerFormat format)
petscViewerPushFormat' :: PetscViewer -> PetscViewerFormat_ -> IO CInt
petscViewerPushFormat' v fmt = [C.exp|int{PetscViewerPushFormat($(PetscViewer v), $(int e))}|]
    where
      e = petscViewerFormatToCInt fmt

-- PetscErrorCode  PetscViewerPopFormat(PetscViewer viewer)
petscViewerPopFormat' :: PetscViewer -> IO CInt
petscViewerPopFormat' vi = [C.exp|int{PetscViewerPopFormat($(PetscViewer vi))}|]
  









-- PetscErrorCode PetscViewerFileSetMode(PetscViewer viewer,PetscFileMode type)
petscViewerFileSetMode' :: PetscViewer -> PetscFileMode_ -> IO CInt
petscViewerFileSetMode' v m =
  [C.exp|int{PetscViewerFileSetMode($(PetscViewer v),$(int mp))}|] where
    mp = toCInt $ fileModeToInt m

-- PetscErrorCode  PetscViewerFileSetName(PetscViewer viewer,const char name[])
petscViewerFileSetName' :: PetscViewer -> String -> IO CInt
petscViewerFileSetName' v name = withCString name $ \n -> 
  [C.exp|int{PetscViewerFileSetName($(PetscViewer v),$(char* n))}|] 


-- PetscErrorCode  PetscViewerDestroy(PetscViewer *viewer)
petscViewerDestroy' :: PetscViewer -> IO CInt
petscViewerDestroy' v =
  with v $ \vp -> [C.exp|int{PetscViewerDestroy($(PetscViewer* vp))}|]




-- -- | HDF5 stuff

-- PetscErrorCode  PetscViewerHDF5Open(MPI_Comm comm, const char name[], PetscFileMode type, PetscViewer *hdf5v)
petscViewerHDF5Open' :: Comm -> String -> PetscFileMode_ -> IO (PetscViewer, CInt)
petscViewerHDF5Open' cc name ty =
  withPtr $ \f ->
   withCString name $ \np -> 
  [C.exp|int{PetscViewerHDF5Open($(int c),$(const char* np),$(int t),$(PetscViewer* f))}|]
   where
     c = unComm cc
     t = toCInt $ fileModeToInt ty
    
-- -- -- usage
-- -- 339:   PetscViewerCreate(comm, hdf5v);
-- -- 340:   PetscViewerSetType(*hdf5v, PETSCVIEWERHDF5);
-- -- 341:   PetscViewerFileSetMode(*hdf5v, type);
-- -- 342:   PetscViewerFileSetName(*hdf5v, name);


-- -- -- PetscErrorCode  PetscViewerHDF5PushGroup(PetscViewer viewer, const char *name)
-- petscViewerHDF5PushGroup' v name = withCString name $ \n -> 
--   [C.exp|int{PetscViewerHDF5PushGroup($(PetscViewer v),$(char* n))}|]

-- -- -- PetscErrorCode  PetscViewerHDF5PopGroup(PetscViewer viewer)
-- petscViewerHDF5PopGroup' v =
--   [C.exp|int{PetscViewerHDF5PopGroup($(PetscViewer v))}|]












-- * PETSc misc


-- PetscErrorCode  PetscGetFlops(PetscLogDouble *flops)
petscGetFlops' :: IO (PetscLogDouble, CInt)
petscGetFlops' = withPtr $ \ nflops  ->
  [C.exp|int{PetscGetFlops($(PetscLogDouble* nflops))}|]


-- PetscTime(PetscLogDouble *v)
petscTime' :: IO (PetscLogDouble, CInt)
petscTime' = withPtr $ \t -> [C.exp|int{PetscTime($(PetscLogDouble* t))}|]

-- PetscErrorCode  PetscGetCPUTime(PetscLogDouble *t)
petscGetCPUTime' :: IO (PetscLogDouble, CInt)
petscGetCPUTime' = withPtr $ \t -> [C.exp|int{PetscGetCPUTime($(PetscLogDouble* t))}|]


-- PetscErrorCode  PetscLogDefaultBegin(void)
petscLogDefaultBegin' :: IO CInt
petscLogDefaultBegin' = [C.exp|int{PetscLogDefaultBegin()}|]


-- PetscErrorCode  PetscLogView(PetscViewer viewer)
petscLogView' :: PetscViewer -> IO CInt
petscLogView' vi = [C.exp|int{PetscLogView($(PetscViewer vi))}|]



-- PETSC_EXTERN PetscErrorCode PetscLogStageRegister(const char[],PetscLogStage*);
-- PETSC_EXTERN PetscErrorCode PetscLogStagePush(PetscLogStage);

-- petscLogStageRegister :: String -> PetscLogStage_ -> IO CInt
petscLogStageRegister' :: String -> IO (PetscLogStage, CInt)
petscLogStageRegister' s =
  withCString s $ \c ->
   withPtr $ \lls -> 
    [C.exp|int{PetscLogStageRegister($(char *c), $(PetscLogStage* lls ))}|] 

petscLogStagePush' :: PetscLogStage -> IO CInt
petscLogStagePush' ls = [C.exp|int{PetscLogStagePush($(PetscLogStage ls))}|] 

petscLogStagePop' :: IO CInt
petscLogStagePop' = [C.exp|int{PetscLogStagePop()}|] 




-- PetscErrorCode  PetscClassIdRegister(const char name[],PetscClassId *oclass)
petscClassIdRegister' :: String -> IO (PetscClassId, CInt)
petscClassIdRegister' name =
  withPtr $ \oclass ->
  withCString name $ \namep -> 
  [C.exp|int{PetscClassIdRegister($(const char* namep),$(PetscClassId* oclass))}|]

-- PetscErrorCode  PetscLogEventRegister(const char name[],PetscClassId classid,PetscLogEvent *event) -- Not Collective
-- Input Parameter :
-- name	- The name associated with the event
-- classid	- The classid associated to the class for this event, obtain either with PetscClassIdRegister() or use a predefined one such as KSP_CLASSID, SNES_CLASSID, the predefined ones are only available in C code
-- Output 
-- event -The event id for use with PetscLogEventBegin() and PetscLogEventEnd(). 
-- Example of Usage :
--       PetscLogEvent USER_EVENT;
--       PetscClassId classid;
--       PetscLogDouble user_event_flops;
--       PetscClassIdRegister("class name",&classid);
--       PetscLogEventRegister("User event name",classid,&USER_EVENT);
--       PetscLogEventBegin(USER_EVENT,0,0,0,0);
--          [code segment to monitor]
--          PetscLogFlops(user_event_flops);
--       PetscLogEventEnd(USER_EVENT,0,0,0,0);
petscLogEventRegister' :: String -> PetscClassId -> IO (PetscLogEvent, CInt)
petscLogEventRegister' name classid =
  withPtr $ \ev ->
  withCString name $ \namep -> 
  [C.exp|int{PetscLogEventRegister($(const char* namep), $(PetscClassId classid),$(PetscLogEvent* ev))}|]


-- PetscErrorCode PetscLogEventBegin(int e,PetscObject o1,PetscObject o2,PetscObject o3,PetscObject o4)
petscLogEventBegin0' :: PetscLogEvent -> IO CInt
petscLogEventBegin0' ev =
  [C.exp|int{PetscLogEventBegin($(PetscLogEvent ev), 0,0,0,0)}|] 

petscLogEventEnd0' :: PetscLogEvent -> IO CInt
petscLogEventEnd0' ev =
  [C.exp|int{PetscLogEventEnd($(PetscLogEvent ev), 0,0,0,0)}|]





-- -- * options

type OptionName = String

-- -- PetscErrorCode  PetscOptionsView(PetscOptions opts, PetscViewer viewer)
petscOptionsView0' :: PetscViewer -> IO CInt
petscOptionsView0' vi = [C.exp|int{PetscOptionsView(NULL, $(PetscViewer vi))}|]

-- PetscErrorCode  PetscOptionsSetValue(PetscOptions opts, const char iname[],const char value[])
-- Not collective, but setting values on certain processors could cause problems for parallel objects looking for options.
-- Input  :
-- options	- options database, use NULL for the default global database
-- name	- name of option, this SHOULD have the - prepended
-- value	- the option value (not used for all options)
petscOptionsSetValue0' :: OptionName -> String -> IO CInt
petscOptionsSetValue0' iname val =
  withCString iname $ \inamep ->
  withCString val $ \valp ->
  [C.exp|int{PetscOptionsSetValue(NULL, $(const char* inamep), $(const char* valp))}|]
-- petscOptionsSetValue iname val = [C.exp|int{PetscOptionsSetValue($bs-ptr:iname, $bs-ptr:val)}|]



-- | PetscOptionsGet...0
-- NB: by passing NULL as a first argument, the global options database will be used

-- #include "petscsys.h"   
-- PetscErrorCode  PetscOptionsGetInt(PetscOptions options, const char pre[],const char name[],PetscInt *ivalue,PetscBool  *set)    -- Not Collective
-- Input Parameters :
-- options - the options database, NULL to use the global database
-- pre	- the string to prepend to the name or NULL
-- name	- the option one is seeking
-- Output Parameters :
-- ivalue	- the integer value to return
-- set	- PETSC_TRUE if found, else PETSC_FALSE
petscOptionsGetInt0' :: String -> String -> IO (Maybe Int, CInt)
petscOptionsGetInt0' prefix name = 
  withCString prefix ( \pre ->
   withCString name $ \namep ->
    withPtr2 $ \ptr pb ->
      [C.exp| int{PetscOptionsGetInt(NULL, $(const char *pre), $(const char *namep), $(int *ptr), $(PetscBool *pb))} |] ) >>= optionsGet fi


-- PetscErrorCode  PetscOptionsGetString(PetscOptions options,const char pre[],const char name[],char string[],size_t len,PetscBool  *set)
petscOptionsGetString0' :: String -> String -> Int -> IO (Maybe String, CInt)
petscOptionsGetString0' prefix name len =
  withCString prefix ( \pre ->
   withCString name $ \namep ->
    withPtr $ \pb ->
     getCString len $ \strp ->
      [C.exp|int{PetscOptionsGetString(NULL, $(const char* pre), $(const char* namep), $(char* strp), $(size_t lenc), $(PetscBool* pb))}|] ) >>= \(b, (s, e)) ->
        optionsGet id ((s, b), e)
          where lenc = fromIntegral len :: CSize
    

-- optionsGet : helper for the @petscOptionsGet..@ family of functions

optionsGet :: Monad m => (a -> b) -> ((a, PetscBool), c) -> m (Maybe b, c)    
optionsGet f ((dats, setflag), err) = do
  let b = petscBoolCToBool setflag
  if b then return (Just (f dats), err)
       else return (Nothing, err)






  

-- withPetscOptionsGetInt prefix name f = do
--   x <- petscOptionsGetInt prefix name
--   case x of (Just s) -> f s
--             Nothing  -> error "option not found"




-- PETSC_EXTERN PetscErrorCode PetscGetArgs(int*,char ***);



-- PETSC_EXTERN PetscErrorCode PetscInitialized(PetscBool *);

-- petscInitialized :: IO Bool
petscInitialized :: IO (PetscBool, CInt)
petscInitialized = withPtr ( \b ->
     [C.exp|int{ PetscInitialized($(PetscBool * b)) } |] )   

  

-- PETSC_EXTERN PetscErrorCode PetscFinalized(PetscBool *);

-- petscFinalized :: IO Bool
petscFinalized :: IO (PetscBool, CInt)
petscFinalized = withPtr ( \p ->
  [C.exp|int{ PetscFinalized($(PetscBool * p))  }|] )



-- petscInit0 :: IO ()
petscInit0' :: IO CInt
petscInit0' = [C.exp| int{ PetscInitializeNoArguments()  }|]

-- -- PETSC_EXTERN PetscErrorCode PetscInitialize(int*,char***,const char[],const char[]);
petscInitialize1 :: Argv -> OptsStr -> HelpStr -> IO CInt
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

petscFin' :: IO CInt
petscFin' = [C.block| int{ PetscFinalize(); }|] 









-- | version string

-- -- PetscErrorCode PetscGetVersion(char version[], size_t len)
petscGetVersion0' :: Ptr CChar -> CInt -> IO CInt
petscGetVersion0' version szt =
  [C.exp|int{PetscGetVersion($(char* version), $(int szt))}|]







-- | error codes

-- -- PETSC_EXTERN PetscErrorCode PetscErrorMessage(int,const char*[],char **);
petscErrorMessage' :: CInt -> Ptr (Ptr CChar) -> IO CInt
petscErrorMessage' nn mp =
  [C.exp|int{PetscErrorMessage($(int nn), $(const char** mp), NULL)} |]






-- PETSC_EXTERN PetscErrorCode PetscMemoryShowUsage(PetscViewer,const char[]);

-- pmsu v c = [C.exp|int{PetscMemoryShowUsage($())}|]

-- petscMemoryShowUsage v = do
--   p <- withPtr


-- alloca $ \stringptr -> do
--    ... call some Ptr CString function
--    peek stringptr






  

-- * MPI






-- mpiCommSize c = withPtr $ \p -> [C.exp|int{ MPI_Comm_Size($(int c), $(PetscMPIInt_ *p)) }|] 
mpiCommSize' :: Comm -> IO (CInt, CInt)
mpiCommSize' cc = withPtr (\p -> [C.exp| int{ MPI_Comm_size($(int c), $(int *p))}|] )
  where
   c = unComm cc
-- mpiCommSize c =  unsafePerformIO $ mpiCommSize' c 

mpiCommRank' :: Comm -> IO (CInt, CInt)
mpiCommRank' cc =
  withPtr
   (\p ->
     [C.exp| int{ MPI_Comm_rank($(int c), $(int *p))}|] )
  where
   c = unComm cc
   
-- mpiCommRank c =
--    MkRank $ unsafePerformIO $ mpiCommRank' c   -- FIXME surface it in PutGet


{-# NOINLINE commWorld1 #-}
commWorld1, commSelf1 :: Comm
commWorld1 = Comm $ unsafePerformIO [C.exp| int{ MPI_COMM_WORLD }  |] 
{-# NOINLINE commSelf1 #-}
commSelf1 = Comm $ unsafePerformIO [C.exp| int{ MPI_COMM_SELF }  |]



    -- PetscPrintf - Prints to standard out, only from the first
    -- processor in the communicator. Calls from other processes are ignored.
petscPrintf :: Comm -> String -> IO CInt
petscPrintf cc s =
  withCString s
   ( \s_ -> [C.exp|int{PetscPrintf($(int c), $(char* s_))}|] ) -- >>= handleErr
  where
    c = unComm cc

petscSynchronizedPrintf' :: Comm -> String -> IO CInt
petscSynchronizedPrintf' cc s = withCString s ( \s_ ->
  [C.exp|int{PetscSynchronizedPrintf($(int c), $(char* s_))}|] )
    where c = unComm cc


petscSynchronizedFlushStdout' :: Comm -> IO CInt
petscSynchronizedFlushStdout' cc =
  [C.exp|int{PetscSynchronizedFlush($(int c), 0 )}|] -- 0 == PETSC_STDOUT
    where c = unComm cc


-- syncPrintf c s =
--   petscSynchronizedPrintf c s >> petscSynchronizedFlushStdout c 


-- * misc parallelism stuff













-- | SLEPc signatures

          
-- * EPS


-- EPSCreate(MPI_Comm comm,EPS *eps);
epsCreate' :: Comm -> IO (EPS, CInt)
epsCreate' cc = withPtr $ \e -> [C.exp|int{EPSCreate($(int c), $(EPS* e))}|] where
  c = unComm cc

-- EPSSetOperators(EPS eps,Mat A,Mat B);
epsSetOperators0' :: EPS -> Mat -> IO CInt
epsSetOperators0' e matA =
  [C.exp|int{EPSSetOperators($(EPS e),$(Mat matA), NULL)}|]

epsSetOperators' :: EPS -> Mat -> Mat -> IO CInt
epsSetOperators' e matA matB = [C.exp|int{EPSSetOperators($(EPS e),$(Mat matA),$(Mat matB))}|]


-- PetscErrorCode EPSGetOperators(EPS eps,Mat *A,Mat *B)
-- Collective on EPS and Mat
-- Input Parameter :
-- eps	 - the EPS context
-- Output Parameters :
-- A	 - the matrix associated with the eigensystem
-- B	 - the second matrix in the case of generalized eigenproblems
-- ", simple eigenproblems: A x = lambda x
epsGetOperators0' ee = withPtr $ \matA ->
  [C.exp|int{EPSGetOperators($(EPS ee),$(Mat* matA),NULL)}|]

-- ", for generalized eigenproblems: A x = lambda B x
epsGetOperators' ee = withPtr2 $ \matA matB ->
  [C.exp|int{EPSGetOperators($(EPS ee),$(Mat* matA),$(Mat* matB))}|]

  
-- EPSSetProblemType(EPS eps,EPSProblemType type);
epsSetProblemType' :: EPS -> EpsProblemType_ -> IO CInt
epsSetProblemType' e ty = [C.exp|int{EPSSetProblemType($(EPS e),$(int tyi))}|] where
  tyi = toCInt $ epsProblemTypeToInt ty

-- EPSSetUp(EPS eps)
epsSetup' :: EPS -> IO CInt
epsSetup' e = [C.exp|int{EPSSetUp($(EPS e))}|]

-- EPSSetFromOptions(EPS eps);

-- PetscErrorCode EPSSetWhichEigenpairs(EPS eps,EPSWhich which)
-- Logically Collective on EPS
-- Input Parameters :
-- eps	- eigensolver context obtained from EPSCreate()
-- which	- the portion of the spectrum to be sought
epsSetWhichEigenpairs' :: EPS -> EpsWhich_ -> IO CInt
epsSetWhichEigenpairs' e wh = [C.exp|int{EPSSetWhichEigenpairs($(EPS e),$(int ew))}|] where ew = toCInt $ epsWhichToInt wh


-- PetscErrorCode EPSSetTarget(EPS eps,PetscScalar target)
-- Logically Collective on EPS
-- Input Parameters :
-- eps	- eigensolver context
-- target	- the value of the target
epsSetTarget' :: EPS -> PetscScalar_ -> IO CInt
epsSetTarget' eps target =
  [C.exp|int{EPSSetTarget($(EPS eps),$(PetscScalar target))}|]

              
-- EPSSolve(EPS eps);
epsSolve' :: EPS -> IO CInt
epsSolve' e = [C.exp|int{EPSSolve($(EPS e))}|]

-- EPSGetConverged(EPS eps, int *nconv);
epsGetConverged' :: EPS -> IO (CInt, CInt)
epsGetConverged' e = withPtr $ \nconv ->
  [C.exp|int{EPSGetConverged($(EPS e),$(int* nconv))}|]

-- Computes the error (based on the residual norm) associated with the i-th computed eigenpair.
-- PetscErrorCode EPSComputeError(EPS eps,PetscInt i,EPSErrorType type,PetscReal *error)  -- Collective on EPS
epsComputeError' :: EPS -> Int -> EpsErrorType_ -> IO (PetscReal_, CInt)
epsComputeError' eps i ty = withPtr $ \err ->
  [C.exp|int{EPSComputeError($(EPS eps),$(int ii),$(int et),$(PetscReal* err))}|]
  where
   ii = toCInt i
   et = toCInt $ epsErrorTypeToInt ty
-- Input Parameter :
-- eps	- the eigensolver context
-- i	- the solution index
-- type	- the type of error to compute
-- Output Parameter :
-- error - the error 
-- The error can be computed in various ways, all of them based on the residual norm ||Ax-kBx||_2 where k is the eigenvalue and x is the eigenvector.


-- PetscErrorCode EPSGetEigenvalue(EPS eps,PetscInt i,PetscScalar *eigr,PetscScalar *eigi) -- Not Collective
-- Input Parameters :
-- eps	 - eigensolver context
-- i	 - index of the solution
-- Output Parameters :
-- eigr	 - real part of eigenvalue
-- eigi	 - imaginary part of eigenvalue
-- Notes :
-- If the eigenvalue is real, then eigi is set to zero. If PETSc is configured with complex scalars the eigenvalue is stored directly in eigr (eigi is set to zero).
-- The index i should be a value between 0 and nconv-1 (see EPSGetConverged()). Eigenpairs are indexed according to the ordering criterion established with EPSSetWhichEigenpairs().
epsGetEigenvalue' eps ii = withPtr2 $ \eigr eigi -> 
  [C.exp|int{EPSGetEigenvalue($(EPS eps),$(PetscInt ii),$(PetscScalar* eigr),$(PetscScalar* eigi))}|]

-- PetscErrorCode EPSGetEigenvector(EPS eps,PetscInt i,Vec Vr,Vec Vi)
-- Logically Collective on EPS
-- Input Parameters :
-- eps	 - eigensolver context
-- i	 - index of the solution
-- Output Parameters :
-- Vr	 - real part of eigenvector
-- Vi	 - imaginary part of eigenvector
-- Notes :
-- The caller must provide valid Vec objects, i.e., they must be created by the calling program with e.g. MatCreateVecs().
-- If the corresponding eigenvalue is real, then Vi is set to zero. If PETSc is configured with complex scalars the eigenvector is stored directly in Vr (Vi is set to zero). In both cases, the user can pass NULL in Vi.
epsGetEigenvector' :: EPS -> PetscInt_ -> Vec -> Vec -> IO CInt
epsGetEigenvector' eps ii vr vi =
  [C.exp|int{EPSGetEigenvector($(EPS eps),$(PetscInt ii),$(Vec vr),$(Vec vi))}|]


-- EPSGetEigenpair(EPS eps,int i,PetscScalar *kr,PetscScalar *ki,Vec xr,Vec xi);
epsGetEigenpair0' :: EPS
                    -> CInt              -- eigenpair index
                    -> Ptr PetscScalar_  -- eigenvalue, Re
                    -> Ptr PetscScalar_  -- ", Im
                    -> Vec               -- eigenvector, Re
                    -> Vec               -- ", Im
                    -> IO CInt
epsGetEigenpair0' e i kr ki xr xi =
  [C.exp|int{EPSGetEigenpair($(EPS e),
                             $(int i),           
                             $(PetscScalar* kr), 
                             $(PetscScalar* ki), 
                             $(Vec xr),          
                             $(Vec xi))}|]       

epsGetEigenpair' :: EPS -> Int -> Vec -> Vec -> IO ((PetscScalar_,PetscScalar_),CInt)
epsGetEigenpair' eps i xr xi = withPtr2 $ \kr ki ->
  epsGetEigenpair0' eps (toCInt i) kr ki xr xi

-- | is the operator Hermitian?
epsIsHermitian' :: EPS -> IO (PetscBool, CInt)
epsIsHermitian' e = withPtr $ \ish ->
  [C.exp|int{EPSIsHermitian($(EPS e),$(PetscBool* ish))}|]

-- | is the operator positive definite?
epsIsPositive' :: EPS -> IO (PetscBool, CInt)
epsIsPositive' e = withPtr $ \isp ->
  [C.exp|int{EPSIsPositive($(EPS e),$(PetscBool* isp))}|]


-- | set number of eigenvals to compute and subspace dimension:
-- nev : # eigenvalues
-- ncv : subspace dim
-- mpd : max. projected dimension
epsSetDimensions' :: EPS -> Int -> Int -> Int -> IO CInt
epsSetDimensions' e nev ncv mpd = [C.exp|int{EPSSetDimensions($(EPS e),$(int nevc),$(int ncvc),$(int mpdc))}|] where
  (nevc, ncvc, mpdc) = (toCInt nev, toCInt ncv, toCInt mpd)

epsSetInterval' :: EPS -> PetscReal_ -> PetscReal_ -> IO CInt
epsSetInterval' e smin smax = [C.exp|int{EPSSetInterval($(EPS e),$(PetscReal smin),$(PetscReal smax))}|]

-- | sets subspace (array of Vec's) from which EPSSolve starts to iterate
-- PetscErrorCode EPSSetInitialSpace(EPS eps,PetscInt n,Vec *is)
epsSetInitialSpace' :: EPS -> VS.Vector Vec -> IO CInt
epsSetInitialSpace' e subspace = VS.unsafeWith subspace $ \isp -> 
  [C.exp|int{EPSSetInitialSpace($(EPS e),$(int nc),$(Vec* isp))}|]
    where nc = toCInt n
          n = VS.length subspace


-- PetscErrorCode EPSSetDeflationSpace(EPS eps,PetscInt n,Vec *v)
-- Collective on EPS and Vec
-- Input Parameter :
-- eps	- the eigenproblem solver context
-- n	- number of vectors
-- v	- set of basis vectors of the deflation space
-- When a deflation space is given, the eigensolver seeks the eigensolution in the restriction of the problem to the orthogonal complement of this space. This can be used for instance in the case that an invariant subspace is known beforehand (such as the nullspace of the matrix).
-- These vectors do not persist from one EPSSolve() call to the other, so the deflation space should be set every time.
-- The vectors do not need to be mutually orthonormal, since they are explicitly orthonormalized internally.
epsSetDeflationSpace' :: EPS -> VS.Vector Vec -> IO CInt
epsSetDeflationSpace' e deflspace = VS.unsafeWith deflspace $ \dsp -> [C.exp|int{EPSSetDeflationSpace($(EPS e),$(int nd),$(Vec* dsp))}|]
   where nd = toCInt n
         n = VS.length deflspace
         

-- EPSDestroy(EPS eps);
epsDestroy' :: EPS -> IO CInt
epsDestroy' e = with e $ \ep -> [C.exp|int{EPSDestroy($(EPS* ep))}|]

-- PetscErrorCode EPSView(EPS eps,PetscViewer viewer)
epsView' :: EPS -> PetscViewer -> IO CInt
epsView' eps v = [C.exp|int{EPSView($(EPS eps),$(PetscViewer v))}|]



-- PetscErrorCode EPSVectorsView(EPS eps,PetscViewer viewer)
epsVectorsView' :: EPS -> PetscViewer -> IO CInt
epsVectorsView' eps vi = [C.exp|int{EPSVectorsView($(EPS eps),$(PetscViewer vi))}|]








-- * ST -- spectral transformations

stCreate' :: Comm -> IO (ST, CInt)
stCreate' cc = withPtr $ \s -> [C.exp|int{STCreate($(int c),$(ST* s))}|]
  where c = unComm cc

stSetType' :: ST -> StType_ -> IO CInt
stSetType' st t =
  withCString ts $ \tp -> [C.exp|int{STSetType($(ST st),$(char* tp))}|]
    where ts = stTypeToStr t

stDestroy' :: ST -> IO CInt
stDestroy' st = with st $ \stp -> [C.exp|int{STDestroy($(ST* stp))}|]










-- * SVD

-- | in real symmetric (or complex Hermitian) matrices, singular values coincide with eigenvalues, but in general this is not the case. The SVD is defined for any matrix, even rectangular ones. Singular values are always non-negative real values. 


-- SVDCreate(MPI_Comm comm,SVD *svd);
svdCreate' :: Comm -> IO (SVD, CInt)
svdCreate' cc = withPtr $ \s -> [C.exp|int{SVDCreate($(int c),$(SVD* s))}|] where
  c = unComm cc
  
-- SVDSetOperator(SVD svd,Mat A);
svdSetOperator' :: SVD -> Mat -> IO CInt
svdSetOperator' s matA = [C.exp|int{SVDSetOperator($(SVD s),$(Mat matA))}|]
  
-- SVDSetFromOptions(SVD svd);

-- SVDSolve(SVD svd);
svdSolve' :: SVD -> IO CInt
svdSolve' s = [C.exp|int{SVDSolve($(SVD s))}|]

-- SVDGetConverged(SVD svd, int *nconv);
svdGetConverged' :: SVD -> IO (CInt, CInt)
svdGetConverged' s = withPtr $ \n -> [C.exp|int{SVDGetConverged($(SVD s),$(int* n))}|]

-- SVDGetSingularTriplet(SVD svd,int i,PetscReal *sigma,Vec u,Vec v);
svdGetSingularTriplet' :: SVD -> CInt -> Vec -> Vec -> IO (PetscReal_, CInt)
svdGetSingularTriplet' s i u v =
  withPtr $ \sig ->
  [C.exp|int{SVDGetSingularTriplet($(SVD s),$(int i),$(PetscReal* sig),$(Vec u),$(Vec v))}|]

-- SVDDestroy(SVD svd);
svdDestroy' :: SVD -> IO CInt
svdDestroy' s = with s $ \sp -> [C.exp|int{SVDDestroy($(SVD* sp))}|]













-- * SLEPc misc

-- SlepcInitialize(int *argc,char ***argv,char *file,char *help);
-- ierr = SlepcFinalize();
slepcInitialized' :: IO (PetscBool, CInt)
slepcInitialized' = withPtr ( \b ->
     [C.exp|int{ SlepcInitialized($(PetscBool * b)) } |] )   

  


slepcInit0' :: IO CInt
slepcInit0' = [C.exp| int{ SlepcInitializeNoArguments()  }|]

slepcInitialize' :: Argv -> OptsStr -> HelpStr -> IO CInt
slepcInitialize' args opts help = 
 let acc = fromIntegral $ length args in 
  with acc $ \ac ->
   withCStringArray args $ \aa ->
   with aa $ \a ->
    withCString opts $ \o ->
    withCString help $ \h ->
    [C.exp|int{SlepcInitialize($(int *ac), $(char*** a), $(char* o), $(char *h))}|] 


slepcFin' :: IO CInt
slepcFin' = [C.block| int{ SlepcFinalize(); }|] 



-- -- PetscErrorCode PetscGetVersion(char version[], size_t len)
slepcGetVersion0' :: Ptr CChar -> CInt -> IO CInt
slepcGetVersion0' version szt =
  [C.exp|int{SlepcGetVersion($(char* version), $(int szt))}|]



-- * helpers

withPtr2 ::
  (Storable a, Storable b) =>
  (Ptr a -> Ptr b -> IO x) ->
  IO ((a, b), x)
withPtr2 act = withPtr (withPtr . act) >>= snoc2

withPtr3 ::
  (Storable a, Storable b, Storable c) =>
  (Ptr a -> Ptr b -> Ptr c -> IO x) ->
  IO ((a, b, c), x)
withPtr3 act = withPtr (\x -> withPtr (withPtr . act x)) >>= snoc3

withPtr4 ::
  (Storable a, Storable b, Storable c, Storable d) =>
  (Ptr a -> Ptr b -> Ptr c -> Ptr d -> IO x) ->
  IO ((a, b, c, d), x)
withPtr4 act = withPtr ( \x ->
  withPtr $ \y ->
  withPtr $ \z ->
  withPtr $ \w ->
  act x y z w ) >>= snoc4








