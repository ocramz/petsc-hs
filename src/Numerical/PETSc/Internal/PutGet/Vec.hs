{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes, FlexibleContexts, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.Vec
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Vec Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.Vec  where

import           Numerical.PETSc.Internal.InlineC
import           Numerical.PETSc.Internal.Types
import           Numerical.PETSc.Internal.Exception
import           Numerical.PETSc.Internal.Utils

import           Numerical.PETSc.Internal.Storable.Vector


import qualified Data.Ix as Ix (range)

import           Foreign
-- import           Foreign.ForeignPtr.Unsafe
import           Foreign.C.Types

import           System.IO.Unsafe                   (unsafePerformIO)

import           Control.Monad
-- import           Control.Applicative
-- import           Control.Arrow
-- import           Control.Concurrent
import           Control.Exception

import           Control.Monad.Trans.Reader
-- import           Control.Monad.Trans.State
-- import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Class

import Control.Monad.IO.Class
-- import Control.Monad.State.Strict -- for execStateT

import           Data.STRef

import           Control.Monad.ST.Unsafe            (unsafeIOToST)    -- for HMatrix bits

import qualified Data.Vector as V
import qualified Data.Vector.Storable               as VS 
import qualified Data.Vector.Storable.Mutable       as VM
import qualified Data.Vector.Generic       as VG




-- | instances

-- instance 

-- -- instance Show Vec where ...





-- | data
            

{- STATE : Vec, VectorData

NB : state is replicated in the distributed memory and in local data. How do we keep these consistent ?

For a given Vec; what stays constant is:
* length (global and local)
* MPI communicator

-}








-- -- --- 


-- data PetscVector = PetscVector { vec     :: !Vec,
--                                  vecInfo :: !VecInfo }

data PetscVector = PetscVector !VecInfo Vec

data PVector = PVector VecInfo Vec (V.Vector Scalar)







-- Q : how do we carry data-on-mesh ?
-- 1 -- data VectorData a = VectorData !(V.Vector (Int, a))
-- 2 -- data VectorData a = VectorData (Map.Map Int a)  -- for sparse fills, reads
-- 3 -- isn't this implemented in Data.Vector already ?






















-- withScalarVector ::
--   (Monad m, MonadTrans t, MonadResource (t m)) =>
--   (PVector PetscScalar_ -> m a) ->
--   (Comm, V.Vector PetscScalar_) ->
--   t m a 
-- withScalarVector f = runReaderT $ do
--   (comm, v0) <- ask
--   (_k, res) <- lift (allocate (vcmpi comm v0) vdestroy)
--   x <- lift $ lift $ f res
--   lift $ release _k
--   return x



















-- --

-- type Config = VecInfo
-- type Resource = Vec

-- newtype MPVector a =
--   MPV {runMPVector :: ReaderT Config (StateT Resource IO) a}
--     deriving (Functor, Applicative, Monad)

















-- | vecCreate

vecCreate :: Comm -> IO Vec
vecCreate c = chk1 (vecCreate' c)


vecCreateMPI :: Comm -> Int -> Int -> IO Vec 
vecCreateMPI c nloc nglob
  | nloc>=0 && nloc<=nglob = vecCreateMPI_ c nloc nglob
  | otherwise = error "vecCreateMPI: [nloc] must sum to nglob"
    where
      vecCreateMPI_ co nLocal nGlobal = chk1 (vecCreateMPI' co nLocal nGlobal)
      

vecCreateMPIdecideLocalSize :: Comm -> Int -> IO Vec
vecCreateMPIdecideLocalSize co nglob
  | nglob > 0 = vcmpidl co nglob
  | otherwise = error "vecCreateMPIdecideLocalSize: global dim must be > 0"
     where
       vcmpidl c n  = chk1 (vecCreateMPIdecideLoc' c n)





-- | " , using VecInfo

vecCreateMPIInfo :: VecInfo -> IO Vec
vecCreateMPIInfo vi = vecCreateMPI co nl ng where
  nl = vecInfoSizeLocal vi
  ng = vecInfoSizeGlobal vi
  co = vecInfoMpiComm vi

vecDestroy :: Vec -> IO ()
vecDestroy v = chk0 (vecDestroy' v)



vecSetSizes :: Vec -> Int -> IO ()
vecSetSizes v n = chk0 $ vecSetSizes1 v (toCInt n)















-- | `withVec` brackets

withVec :: IO Vec -> (Vec -> IO b) -> IO b
withVec vc = bracket vc vecDestroy


withVecCreate :: VecInfo -> (Vec -> IO a) -> IO a
withVecCreate vv = withVec (vecCreate c)  where
  c = vecInfoMpiComm vv


withVecCreateMPI :: VecInfo -> (Vec -> IO a) -> IO a
withVecCreateMPI vi = withVec (vecCreateMPIInfo vi) 



withVecMPIPipeline :: VecInfo -> (Vec -> IO a) -> (Vec -> IO b) -> IO b 
withVecMPIPipeline vv pre post = withVecCreateMPI vv $ \v -> do
  pre v
  vecAssemblyChk v
  post v












-- | assembly 

vecAssemblyBegin, vecAssemblyEnd :: Vec -> IO ()
vecAssemblyBegin v = chk0 (vecAssemblyBegin' v)
vecAssemblyEnd v = chk0 (vecAssemblyEnd' v)

vecAssemblyChk :: Vec -> IO ()
vecAssemblyChk v = vecAssemblyBegin v  >> vecAssemblyEnd v 


-- | withVecAssemblyChk : perform a computation while vector assembly takes place
withVecAssemblyChk :: Vec -> IO a -> IO a
withVecAssemblyChk v = bracket_ (vecAssemblyBegin v) (vecAssemblyEnd v)






-- | vecEqual : compares two vectors. Returns true if the two vectors are either pointing to the same memory buffer, or if the two vectors have the same local and global layout as well as bitwise equality of all entries. Does NOT take round-off errors into account.
vecEqual :: Vec -> Vec -> IO PetscBool
vecEqual v1 v2 = chk1 $ vecEqual1 v1 v2










-- | vecCopy, vecDuplicate

vecCopy_ :: Vec -> Vec -> IO ()
vecCopy_ vorig vcopy = chk0 $ vecCopy1 vorig vcopy

vecCopy :: Vec -> Vec -> IO Vec
vecCopy vorig vcopy = do {vecCopy_ vorig vcopy ;  return vcopy}

vecDuplicate :: Vec -> IO Vec
vecDuplicate v = chk1 $ vecDuplicate1 v

-- | vecCopyDuplicate : duplicates Vec and copies content

vecCopyDuplicate :: Vec -> IO Vec
vecCopyDuplicate v = do
  x <- vecDuplicate v
  vecCopy v x




withVecDuplicate :: Vec -> (Vec -> IO a) -> IO a
withVecDuplicate v = withVec (vecDuplicate v)

withVecCopyDuplicate :: Vec -> (Vec -> IO a) -> IO a
withVecCopyDuplicate v = withVec (vecCopyDuplicate v) 

withVecNew :: Comm -> V.Vector PetscScalar_ -> (Vec -> IO a) -> IO a
withVecNew c v =
  withVec (vecCreateMPIFromVectorDecideLocalSize c v)













-- | setting Vec attributes

vecSetName :: Vec -> String -> IO ()
vecSetName v name = chk0 $ vecSetName1 v name

vecSet :: Vec -> PetscScalar_ -> IO ()
vecSet v n = chk0 $ vecSet1 v n


















-- | setting Vec values


-- common pattern :

-- setWPointers ix y wiv m =
--   wiv ix $ \ixx ->
--    wiv y $ \yy ->
--     m ixx yy

vecSetValuesUnsafe0 ::
  Vec -> CInt -> Ptr CInt -> Ptr PetscScalar_ -> InsertMode_ -> IO ()
vecSetValuesUnsafe0 v ni ix y im = chk0 (vecSetValues' v ni ix y im)

vecSetValuesUnsafe :: Vec -> [CInt] -> [PetscScalar_] -> InsertMode_ -> IO ()
vecSetValuesUnsafe v ix y im =
  withArray ix $ \ixx ->
   withArray y $ \yy -> chk0 $ vecSetValues' v ni ixx yy im 
  where
  ni = toCInt $ length ix

-- vecSetValuesSafe :: Vec -> [Int] -> [PetscScalar_] -> InsertMode_ -> IO ()
-- vecSetValuesSafe = safeInsertIndicesVec vsvu
--   where vsvu v ix = vecSetValuesUnsafe v (map toCInt ix)

-- safeInsertIndicesVec ::
--   (Vec -> V.Vector Int -> V.Vector a -> b -> c) -> Vec -> V.Vector Int -> V.Vector a -> b -> c 
-- safeInsertIndicesVec f v ix_ y_  im
--   |c1 && c2 = f v ix_ y_  im
--   |otherwise = error "safeInsertIndicesVec : size error "
--    where
--   c1 = V.length ix_ == V.length y_
--   c2 = a >= 0 && b <= ub
--   (a, b) = (V.head ix_, V.last ix_) -- Hp: ix_ is ordered
--   ub = vecGetSizeUnsafe v - 1

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
  Vec ->
  V.Vector Int ->
  V.Vector PetscScalar_ ->            -- NB! `y` must be same size as `ix`
  InsertMode_ ->
  IO ()
vecSetValuesUnsafeVector v ix y im =
  VS.unsafeWith ixc $ \ixx ->
   VS.unsafeWith yc $ \yy -> chk0 (vecSetValues' v ni ixx yy im)
    where
      ni = toCInt (V.length ix)
      ixc = V.convert $ V.map toCInt ix
      yc = V.convert y

vecSetValuesUnsafeVector1 ::
  Vec ->
  V.Vector (Int, PetscScalar_) ->      -- (idx, value)
  InsertMode_ ->
  IO ()
vecSetValuesUnsafeVector1 v ixy =
  vecSetValuesUnsafeVector v ix y
    where
      (ix, y) = V.unzip ixy


vecSetValuesRange ::
  VG.Vector v PetscScalar_ =>
  Vec ->
  v PetscScalar_ ->
  InsertMode_ ->
  IO ()
vecSetValuesRange v y im
  | m == ly = 
    VS.unsafeWith ixc $ \ixx ->
     VS.unsafeWith yc $ \yy -> chk0 (vecSetValues' v (toCInt m) ixx yy im)
  | otherwise = error "vecSetValuesRange : v and y must be of the same length !"
  where
    m = vecSize v
    ly = VG.length y
    yc = V.convert y
    ixc = V.convert (V.map toCInt $ V.fromList [0 .. m])




    

-- | generic functions using length of Vec ang VG.Vector :
-- all the `VecSet..` functions should be refactored in terms of these ones


    
-- -- 3 vectors : Vec, Vector and Vector of indices

whenEqualInts3 :: Int -> Int -> Int -> String -> a -> a
whenEqualInts3 l1 l2 l3 c m | l1==l2 && l1==l3 = m | otherwise = error c

withVecVectorLengths3 :: (VG.Vector v1 a1, VG.Vector v a) =>
     Vec ->
     v1 a1 ->
     v a ->
     (Vec -> Int -> v1 a1 -> Int -> v a -> Int -> a2) -> 
     a2
withVecVectorLengths3 v v2 v3 m =
  whenEqualInts3 lv lv2 lv3
    ("withVecVectorLengths3 : length mismatch : " ++
     show lv ++ ", " ++ show lv2 ++ ", " ++ show lv3)
    (m v lv v2 lv2 v3 lv3)
  where
    lv = vecSize v
    lv2 = VG.length v2
    lv3 = VG.length v3

withVecVectorIxPtr :: (VG.Vector v a, VG.Vector vi b, Storable a, Storable b) =>
  Vec ->         -- Vec
  v a ->         -- VG.Vector (contents)
  vi b ->        -- VG.Vector (indices, i.e. b ~ CInt)
  t ->
  (Vec -> Int -> Ptr b -> Int -> Ptr a -> Int -> t -> IO c) ->
  IO c
withVecVectorIxPtr v y ix im m =
  withVecVectorLengths3 v ix y $ \vv lvv ixx lix yy lyy -> do
    let ixc = V.convert ixx           -- position index
    VS.unsafeWith ixc $ \ixcp -> do
      let yc = V.convert yy           -- contents
      VS.unsafeWith yc $ \ycp -> m vv lvv ixcp lix ycp lyy im


vecSetValuesRange :: Vec -> V.Vector PetscScalar_ -> InsertMode_ -> IO ()
vecSetValuesRange v y im = do
  let ix = V.fromList [0 .. toCInt ly-1]
  withVecVectorIxPtr v y ix im $ \vv _ ixp _ yp _ imm ->
    chk0 (vecSetValues' vv (toCInt lv) ixp yp imm)
  where
    lv = vecSize v
    ly = V.length y

  





-- staggeredFill vIn vOut ixDelta








-- | "Assembly" typeclass ?


{- what if we forget to assemble Vec ? CLASS Assembly : data that needs to be assembled before use -}







-- | set Vec values via (idx, value) Vector + assemble

vecSetValuesUnsafeVector1A ::
  Vec ->
  V.Vector (Int, PetscScalar_) ->
  InsertMode_ ->
  IO Vec
vecSetValuesUnsafeVector1A v ixy im = do
  vecSetValuesUnsafeVector1 v ixy im
  vecAssemblyChk v                   
  return v 













-- | creating Vec reference and setting its content from Data.Vector

vecCreateMPIFromVector :: Comm -> Int -> V.Vector PetscScalar_ -> IO Vec
vecCreateMPIFromVector comm nloc w = do
  let dimv = V.length w
      ix = V.fromList $ range0 (dimv - 1)
  v <- vecCreateMPI comm nloc dimv
  vecSetValuesUnsafeVector v ix w InsertValues
  vecAssemblyChk v
  return v

vecCreateMPIFromVectorDecideLocalSize :: Comm -> V.Vector PetscScalar_ -> IO Vec
vecCreateMPIFromVectorDecideLocalSize comm w = do
  let dimv = V.length w
      ix = V.fromList $ range0 (dimv - 1)
  v <- vecCreateMPIdecideLocalSize comm dimv
  vecSetValuesUnsafeVector v ix w InsertValues
  vecAssemblyChk v
  return v
 









modifyVecVector :: 
  (VG.Vector v PetscScalar_, VG.Vector w PetscScalar_) => 
  Vec ->
  (v PetscScalar_ -> w PetscScalar_) ->
  IO (w PetscScalar_)
modifyVecVector v f = do
  u <- vecGetVector v
  let y = f (V.convert u)
  vecRestoreVector v (V.convert y)
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


withVecSize :: Vec -> (Vec -> Int -> t) -> t
withVecSize v f = f v (vecSize v)








-- | getting/restoring a contiguous array from/to a Vec 


vecGetArrayPtr :: Vec -> IO (Ptr PetscScalar_)
vecGetArrayPtr v = chk1 (vecGetArray1' v)

vecRestoreArrayPtr :: Vec -> Ptr PetscScalar_ -> IO ()
vecRestoreArrayPtr v ar = chk0 (vecRestoreArrayPtr' v ar)












-- | Vec get/set interface with Data.Vector
-- -- using ".Storable and ".Storable.Mutable

-- vecGetVector :: Vec -> IO (VS.Vector PetscScalar_)
-- vecGetVector v = do
--   p <- vecGetArrayPtr v
--   pf <- newForeignPtr_ p
--   VS.freeze (VM.unsafeFromForeignPtr0 pf len)
--    where
--      len = vecSize v

vecGetVectorN :: Vec -> Int -> IO (VS.Vector PetscScalar_)
vecGetVectorN v =
  vectorFreezeFromStorablePtr (vecGetArrayPtr v) (vecRestoreArrayPtr v)

vecGetVector :: Vec -> IO (VS.Vector PetscScalar_)
vecGetVector v =
   vecGetVectorN v (vecSize v)

vecRestoreVectorN :: Vec -> Int -> VS.Vector PetscScalar_ -> IO ()
vecRestoreVectorN v =
  vectorCopyToForeignPtr (vecGetArrayPtr v) (vecRestoreArrayPtr v)
  

vecRestoreVector :: Vec -> VS.Vector PetscScalar_ -> IO ()
vecRestoreVector v =
   vecRestoreVectorN v (vecSize v)





     

-- | IOVector <-> Vector.Generic

fromMutableV :: (VG.Vector v a, Storable a) => VM.IOVector a -> IO (v a)
fromMutableV mv = do
  v <- VS.freeze mv
  return (VG.convert v)

toMutableV :: (VG.Vector v a, Storable a) => v a -> IO (VM.IOVector a)
toMutableV = VS.thaw . VG.convert


withVG :: (VG.Vector v a, Storable a) =>
              v a ->
              (VM.IOVector a -> VM.IOVector a) ->
              IO (v a)
withVG v f = do
  x <- toMutableV v
  fromMutableV (f x)


modifyVec ::
  (VG.Vector v PetscScalar_) =>
  Vec ->
  (VM.IOVector PetscScalar_ -> VM.IOVector PetscScalar_) ->
  IO (v PetscScalar_)
modifyVec v f = do
  x <- vecGetIOVector v
  let ym = f x
  vecRestoreIOVector v ym
  fromMutableV ym
  -- x <- vecGetVector v
  -- -- y <- withVG x f
  -- xm <- toMutableV x
  -- let ym = f xm
  -- y <- fromMutableV ym
  -- vecRestoreVector v y





-- | PETSc Vec <-> IOVector

vecGetIOVector :: Vec -> IO ( VM.IOVector PetscScalar_ )
vecGetIOVector v = do
  x <- vecGetVector v
  toMutableV x

vecRestoreIOVector :: Vec -> VM.IOVector PetscScalar_  -> IO ()
vecRestoreIOVector v iov = do
  x <- fromMutableV iov
  vecRestoreVector v x





data V = V Vec

-- instance Num V where








-- get the first n entries

-- vecGetVectorN :: Vec -> Int -> IO (VS.Vector PetscScalar_)
vecGetVectorNSafe v n
  | n > 0 && n <= len = vecGetVectorN v n
  | otherwise = error "vecGetVectorN :" where
     len = vecSize v








-- | mutation of Storable Vectors in ST hidden in IO

-- modifyV, modifyV2 :: Vec -> (V.Vector PetscScalar_ -> V.Vector PetscScalar_) -> IO ()

-- modifyVS ::
--   Vec ->
--   (VS.Vector PetscScalar_ -> VS.Vector PetscScalar_) ->
--   VS.Vector PetscScalar_
-- modifyVS u g = runST $ do
--             x <- unsafeIOToST $ vecGetVector u
--             s <- newSTRef x
--             let y = g x
--             writeSTRef s y
--             unsafeIOToST $ vecRestoreVector u y
--             readSTRef s

-- withSTRef v f = runST $ do
--   s <- newSTRef v
--   let y = f v
--   writeSTRef s y
--   readSTRef s












-- -- | mutating operators, use at own risk


vecVecFunctionAdapt ::
  Vec ->
  Vec ->                 -- SECOND Vec WILL BE MODIFIED
  (VM.IOVector PetscScalar_ -> VM.IOVector PetscScalar_) ->
  IO Vec
vecVecFunctionAdapt vIn vOut fv = 
  withVecSize vIn $ \v vsz -> do
    let ix = V.fromList [0 .. vsz-1] -- hmm
    y <- modifyVec v fv
    let ixy = V.zip ix y
    vecSetValuesUnsafeVector1A vOut ixy InsertValues

petscVecVecFunctionAdapt ::
  Vec ->
  Vec ->                 -- SECOND Vec WILL BE MODIFIED
  (VM.IOVector PetscScalar_ -> VM.IOVector PetscScalar_) ->
  IO CInt
petscVecVecFunctionAdapt v1 v2 fv =
  return0 $ vecVecFunctionAdapt v1 v2 fv










    
















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


-- | Math. operations

-- | -- elementwise

-- unary :  Rn -> Rn

-- exp
-- log
-- reciprocal
-- scale



-- binary :  (Rn * Rn) -> Rn

-- sum
-- product



-- | -- norms




-- --
-- unaryOp :: OpType -> Vec -> IO ()

-- binaryOp :: Op2Type -> Vec -> Vec -> IO ()



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

-- vecWaxpySafe a vx vy = withVecCreate vi $ \w ->
--   vecWaxpy w a x y  -- NB: w is created on same Comm as x
--    where
--     vi = vecInfo vx
--     x = vec vx
--     y = vec vy

vecVecSum , (.+) :: Vec -> Vec -> IO Vec
vecVecSum = vecAxpy 1
(.+) = vecVecSum

-- vecVecSumSafe = vecWaxpySafe 1

















    
