{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.Vec
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | Vec Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.Vec where

import           Numerical.PETSc.Internal.InlineC
import           Numerical.PETSc.Internal.Types
import           Numerical.PETSc.Internal.Exception
import           Numerical.PETSc.Internal.Utils
import           Numerical.PETSc.Internal.Managed

import           Foreign
import           Foreign.ForeignPtr.Unsafe
import           Foreign.C.Types

import           System.IO.Unsafe                   (unsafePerformIO)

import           Control.Monad
import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent
import           Control.Exception

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Class

import Control.Monad.IO.Class
-- import Control.Monad.State.Strict -- for execStateT

import           Data.STRef
import           Control.Monad.ST                   (ST, runST)
import           Control.Monad.ST.Unsafe            (unsafeIOToST)    -- for HMatrix bits

-- import qualified Data.Vector as V
import qualified Data.Vector.Storable               as V
import qualified Data.Vector.Storable.Mutable       as VM



-- | instances

-- -- instance Show Vec where ...





-- | data
            

{- STATE : Vec, VectorData

NB : state is replicated in the distributed memory and in local data. How do we keep these consistent ?

For a given Vec; what stays constant is:
* length (global and local)
* MPI communicator

-}






data PetscVector = PetscVector { vec     :: !Vec,
                                 vecInfo :: !VecInfo }

data VecInfo = VecInfo 
 {vecInfoMpiComm :: Comm ,
  vecInfoSizeLocal :: !Int ,
  vecInfoSizeGlobal :: !Int } deriving (Eq, Show)

-- data VectorData a = VectorData
--                     {vecIdxs :: !(V.Vector Int),
--                      vecDataEntries :: !(V.Vector a)} deriving (Eq, Show)

-- Q : how do we carry data-on-mesh ?
-- 1 -- data VectorData a = VectorData !(V.Vector (Int, a))
-- 2 -- data VectorData a = VectorData (Map.Map Int a)  -- for sparse fills, reads
-- 3 -- isn't this implemented in Data.Vector already ?



data PVector a = PVector !Vec !(V.Vector a)

instance (Storable a, Show a) => Show (PVector a) where
  show (PVector v a) = show a

-- instance (Storable a, Num a) => Num (V.Vector a) where
--   (+) = V.zipWith (+)
--   (-) = V.zipWith (-)
--   (*) = V.zipWith (*)
--   abs = V.map abs
--   signum = V.map signum
--   fromInteger = undefined


-- type ScalarVector = PVector PetscScalar_




-- | "fmap" for PVector

fVdata f (PVector vec vdata) = PVector vec (f vdata)
fVec f (PVector vec vdata) = PVector (f vec) vdata

-- | "bind" for PVector (?!)

-- bindPV :: Monad m => m (PVector a) -> (a -> m (PVector b)) -> m (PVector b)
















withScalarVector ::
  (Monad m, MonadTrans t, MonadResource (t m)) =>
  (PVector PetscScalar_ -> m a) ->
  (Comm, V.Vector PetscScalar_) ->
  t m a 
withScalarVector f = runReaderT $ do
  (comm, v0) <- ask
  (_k, res) <- lift (allocate (vcmpi comm v0) vdestroy)
  x <- lift $ lift $ f res
  lift $ release _k
  return x

vdestroy :: PVector PetscScalar_ -> IO ()
vdestroy (PVector v _) = 
  vecDestroy v

vcmpi ::
  Comm -> V.Vector PetscScalar_ -> IO (PVector PetscScalar_) -- Vector -> create Vec
vcmpi comm vdata = do
     x <- vecCreateMPIFromVector comm n vdata
     return $ PVector x vdata
       where n = V.length vdata

-- vcmpo :: ScalarVector -> IO (V.Vector PetscScalar_)   -- inverse sync










-- | a standardized interface:
-- -- * IO resource management (see e.g. ResourceT or `managed`) with auto-cleanup
-- -- * withNew-, modify- : 
-- -- -- * act on references
-- -- -- * @when@ to copy data (through MVectors) to Hs side?

{- we want to manage a resource of type `a` :
new : x -> IO a
with : IO a -> (a -> IO b) -> IO b
cleanup : a -> IO () 
-}






-- --

-- type Config = VecInfo
-- type Resource = Vec

-- newtype MPVector a =
--   MPV {runMPVector :: ReaderT Config (StateT Resource IO) a}
--     deriving (Functor, Applicative, Monad)

















-- | vecCreate

vecCreate :: Comm -> IO Vec
vecCreate comm = chk1 (vecCreate' comm)

vecCreateMPI_ :: Comm -> Int -> Int -> IO Vec
vecCreateMPI_ comm nLocal nGlobal = chk1 (vecCreateMPI' comm nLocal nGlobal)

vecCreateMPI :: Comm -> Int -> Int -> IO Vec 
vecCreateMPI comm nloc nglob
  | nloc>=0 && nloc<=nglob = vecCreateMPI_ comm nloc nglob
  | otherwise = error "vecCreateMPI: [nloc] must sum to nglob"


vecCreateMPIdecideLocalSize :: Comm -> Int -> IO Vec
vecCreateMPIdecideLocalSize comm nglob
  | nglob > 0 = vcmpidl comm nglob
  | otherwise = error "vecCreateMPIdecideLocalSize: global dim must be > 0"
     where
       vcmpidl c n  = chk1 (vecCreateMPIdecideLoc' c n)



-- | " , using VecInfo

vecCreateMPIInfo :: VecInfo -> IO Vec
vecCreateMPIInfo vi = vecCreateMPI comm nl ng where
  nl = vecInfoSizeLocal vi
  ng = vecInfoSizeGlobal vi
  comm = vecInfoMpiComm vi

vecDestroy :: Vec -> IO ()
vecDestroy v = chk0 (vecDestroy' v)



-- vecSetSizes :: Vec -> Int -> IO ()
-- vecSetSizes v n = chk0 $ vecSetSizes1 v (toCInt n)













-- | `withVec` brackets

withVec :: IO Vec -> (Vec -> IO b) -> IO b
withVec vc = bracket vc vecDestroy


withVecCreate :: VecInfo -> (Vec -> IO a) -> IO a
withVecCreate vv = withVec (vecCreate comm)  where
  comm = vecInfoMpiComm vv


withVecCreateMPI :: VecInfo -> (Vec -> IO a) -> IO a
withVecCreateMPI vi = withVec (vecCreateMPIInfo vi) 



withVecMPIPipeline :: VecInfo -> (Vec -> IO a) -> (Vec -> IO b) -> IO b
withVecMPIPipeline vv pre post = withVecCreateMPI vv $ \v -> do
  pre v
  vecAssemblyChk v
  post v









-- | assembly 

vecAssemblyChk :: Vec -> IO ()
vecAssemblyChk v = chk0 (vecAssemblyBegin' v) >> chk0 (vecAssemblyEnd' v)

-- withVecAssemblyChk v f = chk0 (vecAssemblyBegin' v) >> f >> chk0 (vecAssemblyEnd' v)

-- | withVecAssemblyChk : perform a computation while vector assembly takes place
withVecAssemblyChk :: Vec -> IO a -> IO a
withVecAssemblyChk v = bracket_ (chk0 $ vecAssemblyBegin' v) (chk0 $ vecAssemblyEnd' v)






-- | vecEqual : compares two vectors. Returns true if the two vectors are either pointing to the same memory buffer, or if the two vectors have the same local and global layout as well as bitwise equality of all entries. Does NOT take round-off errors into account.
vecEqual :: Vec -> Vec -> IO Bool
vecEqual v1 v2 = chk1 $ vecEqual1 v1 v2






-- | vecCopy, vecDuplicate

vecCopy_ vorig vcopy = chk0 $ vecCopy1 vorig vcopy
vecCopy vorig vcopy = do {vecCopy_ vorig vcopy ;  return vcopy}

vecDuplicate v = chk1 $ vecDuplicate1 v

-- | vecCopyDuplicate : duplicates Vec and copies content

vecCopyDuplicate :: Vec -> IO Vec
vecCopyDuplicate v = do
  v1 <- vecDuplicate v
  vecCopy v v1

withVecCopyDuplicate :: Vec -> (Vec -> IO a) -> IO a
withVecCopyDuplicate v = bracket
                         ( do
                             v1 <- vecDuplicate v
                             vecCopy v v1
                             return v1 )
                         vecDestroy






-- | setting Vec attributes

vecSetName :: Vec -> String -> IO ()
vecSetName v name = chk0 $ vecSetName1 v name

vecSet_ :: Vec -> PetscScalar_ -> IO ()
vecSet_ v n = chk0 $ vecSet1 v n

vecSet :: Vec -> PetscScalar_ -> IO Vec
vecSet v n = do {vecSet_ v n ; return v}








-- | setting Vec values 

vecSetValuesUnsafe0 ::
  Vec -> CInt -> Ptr CInt -> Ptr PetscScalar_ -> InsertMode_ -> IO ()
vecSetValuesUnsafe0 v ni ix y im = chk0 (vecSetValues' v ni ix y im)

vecSetValuesUnsafe :: Vec -> [CInt] -> [PetscScalar_] -> InsertMode_ -> IO ()
vecSetValuesUnsafe v ix y im =
  withArray ix $ \ixx ->
   withArray y $ \yy -> chk0 $ vecSetValues' v ni ixx yy im 
  where
  ni = toCInt $ length ix

vecSetValuesSafe :: Vec -> [Int] -> [PetscScalar_] -> InsertMode_ -> IO ()
vecSetValuesSafe = safeInsertIndicesVec vsvu
  where vsvu v ix = vecSetValuesUnsafe v (map toCInt ix)

safeInsertIndicesVec ::
  (Vec -> [Int] -> [a] -> b -> c) -> Vec -> [Int] -> [a] -> b -> c
safeInsertIndicesVec f v ix_ y_  im
  |c1 && c2 = f v ix_ y_  im
  |otherwise = error "safeInsertIndicesVec : size error "
   where
  c1 = length ix_ == length y_
  c2 = a >= 0 && b <= ub
  (a, b) = (head ix_, last ix_) -- Hp: ix_ is ordered
  ub = vecGetSizeUnsafe v - 1

-- safeFlag ix_ y_ sv_ = c1 && c2 where
--   c1 = length ix_ == length y_
--   c2 = a >= 0 && b <= sv_
--   ixs = qsort ix_
--   (a, b) = (head ixs, last ixs)

-- safeFlagv ix_ y_ sv_ = c1 && c2 where
--   c1 = V.length ix_ == V.length y_
--   c2 = a >= 0 && b <= sv_
--   ixs = V.sort ix_
--   (a, b) = (V.head ixs, V.last ixs)



-- | setting Vec values, Data.Vector interface

vecSetValuesUnsafeVector ::
  Vec -> V.Vector Int -> V.Vector PetscScalar_ -> InsertMode_ -> IO ()
vecSetValuesUnsafeVector v ix y im =
  V.unsafeWith ixc $ \ixx ->
   V.unsafeWith y $ \yy -> chk0 (vecSetValues' v ni ixx yy im)
    where
      ni = toCInt (V.length ix)
      ixc = V.map toCInt ix











-- | creating Vec reference and setting its content from Data.Vector

vecCreateMPIFromVector :: Comm -> Int -> V.Vector PetscScalar_ -> IO Vec
vecCreateMPIFromVector comm nloc w = do
  let dimv = V.length w
      ix = V.fromList [0 .. dimv - 1]
  v <- vecCreateMPI comm nloc dimv
  vecSetValuesUnsafeVector v ix w InsertValues
  vecAssemblyChk v
  return v

vecCreateMPIFromVectorDecideLocalSize :: Comm -> V.Vector PetscScalar_ -> IO Vec
vecCreateMPIFromVectorDecideLocalSize comm w = do
  let dimv = V.length w
      ix = V.fromList [0 .. dimv - 1]
  v <- vecCreateMPIdecideLocalSize comm dimv
  vecSetValuesUnsafeVector v ix w InsertValues
  vecAssemblyChk v
  return v

  




modifyVecVector ::
  Vec ->
  (V.Vector PetscScalar_ -> V.Vector PetscScalar_) ->
  IO (V.Vector PetscScalar_)
modifyVecVector v f = do
  u <- vecGetVector v
  let y = f u
  vecRestoreVector v y
  return y



-- --











-- | view Vec contents 

vecView :: Vec -> PetscViewer -> IO ()
vecView v vi = chk0 $ vecView1 v vi

vecViewStdout :: Vec -> IO ()
vecViewStdout v = chk0 $ vecViewStdout1 v





-- | get Vec properties 

vecGetOwnershipRange :: Vec -> IO (Int, Int)
vecGetOwnershipRange v = 
  chk1 (vecGetOwnershipRange1 v) 

vecGetSize :: Vec -> IO Int
vecGetSize v = liftM fi $ chk1 ( vecGetSize' v) 

vecGetSizeUnsafe :: Vec -> Int
vecGetSizeUnsafe = unsafePerformIO . vecGetSize

vecSize :: Vec -> Int
vecSize = vecGetSizeUnsafe









-- | getting/restoring a contiguous array from/to a Vec 

-- vecGetArray :: Vec -> Int -> IO [PetscScalar_]
-- vecGetArray v sz = chk1 $ vecGetArray' v sz

-- vecGetArraySafe :: Vec -> IO [PetscScalar_]
-- vecGetArraySafe v = do
--   sz <- vecGetSize v
--   vecGetArray v sz

-- -- PETSC_EXTERN PetscErrorCode VecRestoreArray(Vec,PetscScalar**);
-- vecRestoreArray v c = chk0 $ vecRestoreArray' v c



vecGetArrayPtr :: Vec -> IO (Ptr PetscScalar_)
vecGetArrayPtr v = chk1 (vecGetArray1' v)

vecRestoreArrayPtr :: Vec -> Ptr PetscScalar_ -> IO ()
vecRestoreArrayPtr v ar = chk0 (vecRestoreArrayPtr' v ar)












-- | Vec get/set interface with Data.Vector
-- -- using ".Storable and ".Storable.Mutable

vecGetVector :: Vec -> IO (V.Vector PetscScalar_)
vecGetVector v = do
  p <- vecGetArrayPtr v
  pf <- newForeignPtr_ p
  V.freeze (VM.unsafeFromForeignPtr0 pf len)
   where
     len = vecSize v

vecRestoreVector :: Vec -> V.Vector PetscScalar_ -> IO ()
vecRestoreVector v w = do
  p <- vecGetArrayPtr v
  pf <- newForeignPtr_ p
  V.copy (VM.unsafeFromForeignPtr0 pf len) w
  vecRestoreArrayPtr v p
    where
     len = vecSize v

-- get the first n entries

vecGetVectorN :: Vec -> Int -> IO (Maybe (V.Vector PetscScalar_))
vecGetVectorN v n
  | n > 0 && n <= len = do
     p <- vecGetArrayPtr v
     pf <- newForeignPtr_ p
     y <- V.freeze (VM.unsafeFromForeignPtr0 pf n)
     return $ Just y
  | otherwise = return Nothing
       where
         len = vecSize v









-- | mutation in ST hidden in IO

-- modifyV, modifyV2 :: Vec -> (V.Vector PetscScalar_ -> V.Vector PetscScalar_) -> IO ()
  

modifyV' ::
  Vec ->
  (V.Vector PetscScalar_ -> V.Vector PetscScalar_) ->
  V.Vector PetscScalar_
modifyV' u g = runST $ do
            x <- unsafeIOToST $ vecGetVector u
            s <- newSTRef x
            let y = g x
            writeSTRef s y
            unsafeIOToST $ vecRestoreVector u y
            readSTRef s



-- withSTRef v f = runST $ do
--   s <- newSTRef v
--   let y = f v
--   writeSTRef s y
--   readSTRef s






-- -- | mutating operators, use at own risk

-- withVecGetVectorOverwrite ::
--   Vec ->
--   (V.Vector PetscScalar_ -> V.Vector PetscScalar_) ->
--   IO ()
-- withVecGetVectorOverwrite v modify = do
--   x <- vecGetVector v
--   let y = modify x
--   vecRestoreVector v y

-- -- -- ", monadic version

-- withVecGetVectorOverwriteM ::
--   Vec ->
--   (V.Vector PetscScalar_ -> IO (V.Vector PetscScalar_)) ->
--   IO ()
-- withVecGetVectorOverwriteM v modifyM = do
--   x <- vecGetVector v
--   y <- modifyM x
--   vecRestoreVector v y




    








{-
bracket1 allocate release io = mask $ \restore -> do
  stuff <- allocate
  restore (io stuff) `finally` release stuff
-}








-- -- MVar stuff

-- data PetscVec = PetscVec { unPetscVec :: MVar PVec }

-- makePetscVec v vi = do
--   m <- newMVar (PVec v vi)
--   return $ PetscVec m

-- usePetscVec x f = do
--   let mv = unPetscVec x
--   withMVar mv (`applyVec` f)
--   return $ PetscVec mv

-- updatePetscVec x g = do
--   let mv = unPetscVec x
--   modifyMVar_ mv (`applyVec` g)
--   return $ PetscVec mv

-- applyVec :: PVec -> (Vec -> IO Vec) -> IO PVec
-- applyVec vv fm = do
--   out1 <- fm $ vec vv
--   return $ PVec out1 (vecInfo vv)

-- applyVec' vv fm = do
--   fm (vec vv)
--   return (PVec (vec vv) (vecInfo vv))

-- --













-- | Vec math. operations

vecDot :: Vec -> Vec -> IO PetscScalar_
vecDot v1 v2 = chk1 $ vecDot1 v1 v2

vecNorm :: Vec -> VecNorm_ -> IO PetscScalar_
vecNorm v nt = chk1 $ vecNorm1 nt v

vecSum :: Vec -> IO PetscScalar_
vecSum v = chk1 $ vecSum1 v











-- | Vec math (in-place, destructive) operations

vecLog_, vecExp_, vecAbs_ :: Vec -> IO ()
vecLog, vecExp, vecAbs :: Vec -> IO Vec
vecLog_ v = chk0 $ vecLog' v
vecLog v = do {vecLog_ v; return v}
vecExp_ v = chk0 $ vecExp' v
vecExp v = do {vecExp_ v; return v}
vecAbs_ v = chk0 $ vecAbs' v
vecAbs v = do {vecAbs_ v ; return v}

vecScale_ :: Vec -> PetscScalar_ -> IO ()
vecScale_ v a = chk0 $ vecScale' v a
vecScale :: Vec -> PetscScalar_ -> IO Vec
vecScale v a = do {vecScale_ v a; return v}

-- | AXPY : y = a x + y
-- -- NB : x and y must be different vectors (i.e. distinct pointers)
vecAxpy :: PetscScalar_ -> Vec -> Vec -> IO Vec
vecAxpy a y x = do
  chk0 $ vecAxpy' y a x
  return y

-- | WAXPY : w = a x + y
-- -- NB : w cannot be either x or y, but x and y can be the same
vecWaxpy_ w a x y = chk0 $ vecWaxpy' w a x y
vecWaxpy w a x y = do {vecWaxpy_ w a x y; return w}

vecWaxpySafe a vx vy = withVecCreate vi $ \w ->
  vecWaxpy w a x y  -- NB: w is created on same Comm as x
   where
    vi = vecInfo vx
    x = vec vx
    y = vec vy

vecVecSum , (.+) :: Vec -> Vec -> IO Vec
vecVecSum = vecAxpy 1
(.+) = vecVecSum

vecVecSumSafe = vecWaxpySafe 1

















    
