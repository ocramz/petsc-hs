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
module Numerical.PETSc.Internal.PutGet.Vec where

import           Numerical.PETSc.Internal.InlineC
import           Numerical.PETSc.Internal.Types
import           Numerical.PETSc.Internal.C2HsGen.TypesC2HsGen
import           Numerical.PETSc.Internal.Exception
import           Numerical.PETSc.Internal.Utils
import qualified Numerical.PETSc.Internal.PutGet.PetscMisc as P
import qualified Numerical.PETSc.Internal.PutGet.Viewer as View

import           Numerical.PETSc.Internal.Storable.Vector
import           Numerical.PETSc.Internal.Storable.StorableContainer


import qualified Data.Ix as Ix (range, inRange)

import           Foreign
import qualified Foreign.ForeignPtr as FPR
import           Foreign.C.Types

import           System.IO.Unsafe                   (unsafePerformIO)

import           Control.Monad
import           Control.Applicative
import           Control.Exception

-- import           Control.Monad.Trans.Reader
-- import           Control.Monad.Trans.State
-- import           Control.Monad.Trans.Resource
-- import           Control.Monad.Trans.Class

-- import Control.Monad.IO.Class
-- import Control.Monad.State.Strict -- for execStateT

-- import           Data.STRef

-- import           Control.Monad.ST
-- import           Control.Monad.ST.Unsafe            (unsafeIOToST)    -- for HMatrix bits

import qualified Data.Vector as V
import qualified Data.Vector.Storable               as VS 
import qualified Data.Vector.Storable.Mutable       as VM
import qualified Data.Vector.Generic       as VG
import qualified Data.Vector.Generic.Mutable       as VGM








-- | instances
 

-- -- Show

instance Show Vec where        
  show v = show (unsafePerformIO $ vecGetVS v)




-- -- Vector.Generic.Mutable

data MVec s a = MVec 
    {-# UNPACK #-} !Int  -- global vector length (see `vecCreateMPI() `)          
    {-# UNPACK #-} !Vec

-- instance VGM.MVector MVec PetscScalar_ where
--   basicLength (MVec n _) = n



-- | data
            

{- STATE : Vec, VectorData

NB : state is replicated in the distributed memory and in local data. How do we keep these consistent ?

For a given Vec; what stays constant is:
* length (global and local)
* MPI communicator

-}


-- -- StorableContainer

-- instance StorableContainer Vec where
--   type SCInfo Vec = VecInfo
--   type SCLocal Vec = VS.Vector PetscScalar_
--   type SCRemote Vec = Vec
--   initRemote = vecCreateMPIInfo  -- must import PutGet.Vec for these ..
--   updateLocal = vecGetVector
--   updateRemote = vecRestoreVector
--   withRemote v = bracket (vecGetVector v) (vecRestoreVector v)
--   destroyRemote = vecDestroy
















-- -- --- 


data PetscVector = PetscVector !VecInfoBase Vec

data PVector = PVector VecInfoBase Vec (V.Vector PetscScalar_)


data VecInfoBase = VecInfo 
 {vecInfoMpiComm :: Comm ,
  vecInfoSizeLocal :: !Int ,
  vecInfoSizeGlobal :: !Int } deriving (Eq, Show)
























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

vecCreateMPIInfo :: VecInfoBase -> IO Vec
vecCreateMPIInfo vi = vecCreateMPI co nl ng where
  nl = vecInfoSizeLocal vi
  ng = vecInfoSizeGlobal vi
  co = vecInfoMpiComm vi

vecDestroy :: Vec -> IO ()
vecDestroy v = chk0 (vecDestroy' v)

vecDestroyRight :: VecRight -> IO ()
vecDestroyRight (VecRight vr) = vecDestroy vr
vecDestroyLeft :: VecLeft -> IO ()
vecDestroyLeft (VecLeft vl) = vecDestroy vl


vecSetSizes :: Vec -> Int -> IO ()
vecSetSizes v n = chk0 $ vecSetSizes1 v (toCInt n)















-- | `withVec` brackets

withVec :: IO Vec -> (Vec -> IO b) -> IO b
withVec vc = bracket vc vecDestroy

withVecRight :: IO VecRight -> (VecRight -> IO b) -> IO b
withVecRight vcr = bracket vcr vecDestroyRight
withVecLeft :: IO VecLeft -> (VecLeft -> IO b) -> IO b
withVecLeft vcl = bracket vcl vecDestroyLeft

withVecRL :: IO VecRight -> IO VecLeft -> (VecRight -> VecLeft -> IO a) -> IO a
withVecRL vcr vcl f =
  withVecRight vcr $ \r ->
  withVecLeft vcl $ \l ->
   f r l



withVecCreate :: VecInfoBase -> (Vec -> IO a) -> IO a
withVecCreate vv = withVec (vecCreate c)  where
  c = vecInfoMpiComm vv


withVecCreateMPI :: VecInfoBase -> (Vec -> IO a) -> IO a
withVecCreateMPI vi = withVec (vecCreateMPIInfo vi) 



withVecMPIPipeline :: VecInfoBase -> (Vec -> IO a) -> (Vec -> IO b) -> IO b 
withVecMPIPipeline vv pre post = withVecCreateMPI vv $ \v -> do
  pre v
  vecAssembly v
  post v

















-- | assembly 

vecAssemblyBegin, vecAssemblyEnd :: Vec -> IO ()
vecAssemblyBegin v = chk0 (vecAssemblyBegin' v)
vecAssemblyEnd v = chk0 (vecAssemblyEnd' v)

vecAssembly :: Vec -> IO ()
vecAssembly v = vecAssemblyBegin v  >> vecAssemblyEnd v 


-- | withVecAssemblyChk : perform a computation while vector assembly takes place
withVecAssembly :: Vec -> IO a -> IO a
withVecAssembly v = bracket_ (vecAssemblyBegin v) (vecAssemblyEnd v)






-- | vecEqual : compares two vectors. Returns true if the two vectors are either pointing to the same memory buffer, or if the two vectors have the same local and global layout as well as bitwise equality of all entries. Does NOT take round-off errors into account.
vecEqual :: Vec -> Vec -> IO Bool
vecEqual v1 v2 = petscBoolCToBool <$> chk1 (vecEqual' v1 v2)








-- | vecCopy, vecDuplicate -> vecClone

vecCopy :: Vec -> Vec -> IO ()
vecCopy vorig vcopy = chk0 $ vecCopy1 vorig vcopy

-- | vecDuplicate : allocates memory for an indentical Vec

vecDuplicate :: Vec -> IO Vec
vecDuplicate v = chk1 $ vecDuplicate1 v

-- | vecClone : duplicates Vec and copies content

vecClone :: Vec -> IO Vec
vecClone v = do
  vcopy <- vecDuplicate v
  vecCopy v vcopy
  return vcopy




withVecDuplicate :: Vec -> (Vec -> IO a) -> IO a
withVecDuplicate v = withVec (vecDuplicate v)

withVecClone :: Vec -> (Vec -> IO a) -> IO a
withVecClone v = withVec (vecClone v) 

withVecNew :: Comm -> V.Vector PetscScalar_ -> (Vec -> IO a) -> IO a
withVecNew c v =
  withVec (vecCreateMPIFromVectorDecideLocalSize c v)





-- | use a Generic Vector copy of Vec `v` (does NOT modify `v`)

withVecVector :: VG.Vector w PetscScalar_ =>
     Vec -> (w PetscScalar_ -> IO a) -> IO a
withVecVector v io = do
  vv <- vecGetVS v
  y <- io (V.convert vv)
  vecPutVS v vv
  return y -- (y :: V.Vector PetscScalar_)
















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


-- safeFlag ix_ y_ sv_ = c1 && c2 where
--   c1 = length ix_ == length y_
--   c2 = a >= 0 && b <= sv_
--   ixs = qsort ix_
--   (a, b) = (head ixs, last ixs)

-- safeFlag ix y = c1 && c2 where
--   c1 = length ix == length y
--   c2 = safeIndices y ix










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







    

-- | generic functions using length of Vec ang VG.Vector :
-- all the `VecSet..` functions should be refactored in terms of these ones


    
-- -- -- 3 vectors : Vec, Vector and Vector of indices



-- -- | index in range of Vec ?
-- -- validVecIndex :: Vec -> Int -> Bool
-- -- validVecIndex v = Ix.inRange (0, vecSize v - 1)






-- withVecVectorLengths3 :: (VG.Vector v1 a1, VG.Vector v a) =>
--      Vec ->
--      v1 a1 ->
--      v a ->
--      (Vec -> Int -> v1 a1 -> Int -> v a -> Int -> a2) -> 
--      a2
-- withVecVectorLengths3 v v2 v3 m =
--   whenEqualInts3 lv lv2 lv3
--     ("withVecVectorLengths3 : length mismatch : " ++
--      show lv ++ ", " ++ show lv2 ++ ", " ++ show lv3)
--     (m v lv v2 lv2 v3 lv3)
--   where
--     lv = vecSize v
--     lv2 = VG.length v2
--     lv3 = VG.length v3

-- withVecVectorIxPtr :: (VG.Vector v a, VG.Vector vi b, Storable a, Storable b) =>
--   Vec ->         -- Vec
--   v a ->         -- VG.Vector (contents)
--   vi b ->        -- VG.Vector (indices, i.e. b ~ CInt)
--   t ->
--   (Vec -> Int -> Ptr b -> Int -> Ptr a -> Int -> t -> IO c) ->
--   IO c
-- withVecVectorIxPtr v y ix im m =
--   withVecVectorLengths3 v ix y $ \vv lvv ixx lix yy lyy -> do
--     let ixc = V.convert ixx           -- position index
--     VS.unsafeWith ixc $ \ixcp -> do
--       let yc = V.convert yy           -- contents
--       VS.unsafeWith yc $ \ycp -> m vv lvv ixcp lix ycp lyy im


vecSetValuesRangeVector :: Vec -> V.Vector PetscScalar_ -> InsertMode_ -> IO ()
vecSetValuesRangeVector v =
  vecSetValuesUnsafeVector v ix_ where
    ix_ = V.fromList [0 .. m-1]
    m = vecSize v


-- vecSetValuesRange :: Vec -> V.Vector PetscScalar_ -> InsertMode_ -> IO ()
-- vecSetValuesRange v y im = do
--   let ix = V.fromList [0 .. toCInt ly-1]
--   withVecVectorIxPtr v y ix im $ \vv _ ixp _ yp _ imm ->
--     chk0 (vecSetValues' vv (toCInt lv) ixp yp imm)
--   where
--     lv = vecSize v
--     ly = V.length y

  

-- vecSetValuesArbRange vIn vOut ixDelta








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
  vecAssembly v                   
  return v 













-- | creating Vec reference and setting its content from Data.Vector

-- | TODO
-- * abstract out:
-- -- given Vector w and Comm c: create Vec v (decide creation routine from input data), fill it from w, assemble v, return v :

vecCreateMPIFromVector :: Comm -> Int -> V.Vector PetscScalar_ -> IO Vec
vecCreateMPIFromVector cc nloc w = do
  let dimv = V.length w
      ix = V.fromList [0 .. (dimv - 1)]
  v <- vecCreateMPI cc nloc dimv
  vecSetValuesUnsafeVector v ix w InsertValues
  vecAssembly v
  return v

vecCreateMPIFromVectorDecideLocalSize :: Comm -> V.Vector PetscScalar_ -> IO Vec
vecCreateMPIFromVectorDecideLocalSize cc w = do
  let dimv = V.length w
      ix = V.fromList [0 ..(dimv - 1)]
  v <- vecCreateMPIdecideLocalSize cc dimv
  vecSetValuesUnsafeVector v ix w InsertValues
  vecAssembly v
  return v
 









-- | load Vec contents from HDF5

vecLoad :: Vec -> PetscViewer -> IO ()
vecLoad v vi = chk0 $ vecLoad' v vi














-- | view Vec contents 

vecView0 :: Vec -> PetscViewer -> IO ()
vecView0 m v = chk0 (vecView' m v)

vecView :: Vec -> IO ()
vecView v =
  View.withPetscViewerTypeFmt P.commWorld ViewerAscii ViewFmtAsciiInfoDetail (vecView0 v)

vecViewStdout :: Vec -> IO ()
vecViewStdout = vecView












-- | get Vec properties 

vecGetOwnershipRange :: Vec -> IO (Int, Int)
vecGetOwnershipRange v = 
  chk1 (vecGetOwnershipRange1 v) >>= bothMf fi

vecGetSize :: Vec -> IO Int
vecGetSize v = fi <$> chk1 ( vecGetSize' v) 

vecGetSizeUnsafe :: Vec -> Int
vecGetSizeUnsafe = unsafePerformIO . vecGetSize

vecSize :: Vec -> Int
vecSize = vecGetSizeUnsafe


withVecSize :: Vec -> (Int -> Vec -> t) -> t
withVecSize v f = f (vecSize v) v










-- | getting/restoring a contiguous array from/to a Vec 

withVecArrayPtr :: Vec -> (Ptr PetscScalar_ -> IO a) -> IO a
withVecArrayPtr v =
  bracket (vecGetArrayPtr v) (vecRestoreArrayPtr v)
  where
    vecGetArrayPtr u = chk1 (vecGetArray' u)
    vecRestoreArrayPtr u ar = chk0 (vecRestoreArray' u ar)



-- | ",  read only

withVecArrayReadPtr :: Vec -> (Ptr PetscScalar_ -> IO a) -> IO a
withVecArrayReadPtr v =
  bracket (vecGetArrayReadPtr v) (vecRestoreArrayReadPtr v)
  where
    vecGetArrayReadPtr u = chk1 (vecGetArrayRead' u)
    vecRestoreArrayReadPtr u ar = chk0 (vecRestoreArrayRead' u ar)


-- | " accessing size and pointer

withVecSizedPtr :: Vec -> (Int -> Ptr PetscScalar_ -> IO a) -> IO a
withVecSizedPtr w io = withVecSize w (\n v -> withVecArrayPtr v (io n))

-- |", read only

withVecSizedReadPtr :: Vec -> (Int -> Ptr PetscScalar_ -> IO a) -> IO a
withVecSizedReadPtr w io = withVecSize w (\n v -> withVecArrayReadPtr v (io n))







-- | Vec get/set interface with Data.Vector
-- -- using ".Storable 
-- NB : `vecPutVS` overwrites its Vec argument with contents of second argument

vecGetVS :: Vec -> IO (VS.Vector PetscScalar_)
vecGetVS v = withVecSizedPtr v getVS

vecPutVS :: Vec -> VS.Vector PetscScalar_ -> IO ()
vecPutVS v vu = withVecSizedPtr v (putVS vu)  




-- | ", read only

vecGetVSRead :: Vec -> IO (VS.Vector PetscScalar_)
vecGetVSRead v = withVecSizedReadPtr v getVS






-- | ", mutable


-- | overwrite first argument with contents of second argument;

vecOverwriteIOVectorN2_ :: Vec -> V.Vector PetscScalar_ -> IO ()
vecOverwriteIOVectorN2_ v vnew
  | lv == lvnew = withVecArrayPtr v (overwriteCArray lv vnew)
  | otherwise = error "incompatible sizes"
    where (lv, lvnew) = (vecSize v, V.length vnew)


            


 


-- | explicitly passing around the ForeignPtr associated with the Ptr 
getIOVector :: Storable a => Int -> Ptr a -> IO (VM.IOVector a, FPR.ForeignPtr a)
getIOVector = getVM1

putIOVector :: Storable a => Int -> (VM.IOVector a, FPR.ForeignPtr a) -> IO ()
putIOVector = putVM1

-- withIOVector0 ::
--   Storable a => Int -> Ptr a -> ((VM.IOVector a, FPR.ForeignPtr a) -> IO a) -> IO a
-- withIOVector0 n p = bracket (getIOVector n p) (putIOVector n)

-- -- withIOVector :: Storable a => Int -> Ptr a -> V.Vector a -> IO ()
-- -- withIOVector n p v =
-- --   bracket (getIOVector n p) (putIOVector n) $ \(vm, _) -> 
-- --    V.imapM_ (VM.write vm) v


  

-- withIOVectorSized0 vec io = withVecSizedPtr vec $ \n p ->
--   bracket (getIOVector n p) (putIOVector n) io

-- withIOVectorSized vec io = withIOVectorSized0 vec $ \(vm, _) -> io vm






-- | Vec -> Vector.Generic







     

-- | IOVector <-> Vector.Generic

fromMutableV :: (VG.Vector v a, Storable a) => VM.IOVector a -> IO (v a)
fromMutableV mv = do
  v <- VS.freeze mv
  return (VG.convert v)

toMutableV :: (VG.Vector v a, Storable a) => v a -> IO (VM.IOVector a)
toMutableV = VS.thaw . VG.convert

withMutableV ::
  (VG.Vector v a, Storable a) => VM.IOVector a -> (v a -> v a) -> IO (VM.IOVector a)
withMutableV mv f = fromMutableV mv >>= toMutableV . f












-- | PETSc Vec <-> IOVector

vecGetIOVector :: Vec -> IO ( VM.IOVector PetscScalar_ )
vecGetIOVector v = do
  x <- vecGetVS v
  toMutableV x

vecRestoreIOVector :: Vec -> VM.IOVector PetscScalar_  -> IO ()
vecRestoreIOVector v iov = do
  x <- fromMutableV iov
  vecPutVS v x

















-- get the first n entries

vecGetVectorNSafe :: Vec -> Int -> IO (VS.Vector PetscScalar_)
vecGetVectorNSafe v n
  | n > 0 && n <= len = vecGetVS v
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
--   vv <- unsafeIOToST $ vecGetVector v
--   s <- newSTRef vv
--   let y = f vv
--   writeSTRef s y
--   unsafeIOToST $ vecRestoreVector v y
--   readSTRef s























    
















-- -- MVar stuff

-- data PetscVec = PetscVec { unPetscVec :: MVar Vec }

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
-- vecLog, vecExp, vecAbs :: Vec -> IO Vec
vecLog_ v = chk0 $ vecLog' v
-- vecLog v = do {vecLog_ v; return v}
vecExp_ v = chk0 $ vecExp' v
-- vecExp v = do {vecExp_ v; return v}
vecAbs_ v = chk0 $ vecAbs' v
-- vecAbs v = do {vecAbs_ v ; return v}

vecScale_ :: Vec -> PetscScalar_ -> IO ()
vecScale_ v a = chk0 $ vecScale' v a
-- vecScale :: Vec -> PetscScalar_ -> IO Vec
-- vecScale v a = do {vecScale_ v a; return v}

-- | AXPY : y = a x + y
-- -- NB : x and y must be different vectors (i.e. distinct pointers)
vecAxpy :: PetscScalar_ -> Vec -> Vec -> IO ()
vecAxpy a y x = chk0 $ vecAxpy' y a x


-- | WAXPY : w = a x + y
-- -- NB : w cannot be either x or y, but x and y can be the same
vecWaxpy_ :: Vec -> PetscScalar_ -> Vec -> Vec -> IO ()
vecWaxpy_ w a x y = chk0 $ vecWaxpy' w a x y
-- vecWaxpy w a x y = do {vecWaxpy_ w a x y; return w} -- not sure about this one

-- | withVecWaxpySafe :: bracket with allocated Vec for result
withVecWaxpySafe :: PetscScalar_ -> Vec -> Vec -> (Vec -> IO a) -> IO a
withVecWaxpySafe a x y act = withVecClone y $ \w -> do
  vecWaxpy_ w a x y
  act w



vecVecSum , (.+) :: Vec -> Vec -> IO ()
vecVecSum = vecAxpy 1
(.+) = vecVecSum

withVecVecSubtract :: Vec -> Vec -> (Vec -> IO a) -> IO a
withVecVecSubtract = withVecWaxpySafe (-1)



















    
