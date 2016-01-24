{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Storable.StorableContainer
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Storable container class
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Storable.StorableContainer where

import Numerical.PETSc.Internal.Types
-- import Numerical.PETSc.Internal.PutGet.Vec

import Control.Monad

import Data.Ix
import qualified Data.Vector as V

import Data.IORef

-- | mtl
import Control.Monad.Managed
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class

import Foreign.Storable

import Control.Exception




-- | `Element` class from Data.Packed.Internal.Matrix

class Functor f => Element f a where
  type ElementIx a :: *
  elementRange :: f a -> ElementIx a -> ElementIx a -> f a
  elementConst :: a -> Int -> f a

instance Element [] a where
  type ElementIx a = Int
  elementRange xs i1 i2
    | length xs >= i2 && i2 >= i1 = drop i1 (take i2 xs) 
    | otherwise = error "elementRange [a] : indices out of range"
  elementConst = flip replicate

data VecVec a = VV { unVV :: [[a]] }

instance Functor VecVec where
  fmap f x = VV $ (map . map) f (unVV x)

-- instance Element VecVec a where
--   type ElementIx (VecVec a) = (Int, Int)





-- | `Container` class from Data.Packed.Internal.Matrix

-- class Element e => Container c e where
--   type IndexOf (c :: * -> *) :: *
--   type ArgOf (c :: * -> *) a
--   containerSize :: c e -> IndexOf c
--   containerScalar :: e -> c e
--   scale :: e -> c e -> c e






-- | StorableContainer

-- | types that can be exchanged between PETSc and GHC RTS
-- NB :
-- first type : PETSc side (Storable types in Internal.Types)
-- third type : Haskell side (e.g. PetscVector in PutGet.Vec)

-- class (Storable p, Monad m) => StorableContainer p m a where
--   type SCInfo p 
--   type SCLocal a
--   initP :: SCInfo p -> m p
--   updateH :: p -> m (SCLocal a)
--   updateP :: p -> SCLocal a -> m ()
--   withP :: p -> (SCLocal a -> m b) -> m b
--   destroyP :: p -> m ()

{-
-- | usage example :  
instance StorableContainer Vec IO a where
  type PObjInfo Vec = VecInfo
  type PObjLocal Vec a = V.Vector a
-}

-- class Storable p => StorableContainer p where
--   type SCInfo p 
--   type SCLocal p
--   type SCRemote p
--   initRemote :: SCInfo p -> IO (SCRemote p)
--   updateLocal :: SCRemote p -> IO (SCLocal p)
--   updateRemote :: SCRemote p -> SCLocal p -> IO ()
--   withRemote :: SCRemote p -> (SCLocal p -> IO b) -> IO b
--   destroyRemote :: SCRemote p -> IO ()

-- instance StorableContainer Vec where
--   type SCInfo Vec = VecInfo
--   type SCLocal Vec = VS.Vector PetscScalar_
--   type SCRemote Vec = Vec
--   initRemote = vecCreateMPIInfo  -- must import PutGet.Vec for these ..
--   updateLocal = vecGetVector
--   updateRemote = vecRestoreVector
--   withRemote v = bracket (vecGetVector v) (vecRestoreVector v)
--   destroyRemote = vecDestroy







-- | StorableIxContainer

-- class (StorableContainer sc, Ix i) => StorableIxContainer i sc x where
--   sicGet :: sc -> i -> IO x
--   sicPut :: sc -> i -> x -> IO ()


-- class (Storable p) => StorableIxContainer p a where
--   -- indexing depends on `p` :
--   -- e.g. for Vec : SCIdx p == Int
--   -- --   for Mat : SCIdx p == (Int, Int) , etc.
--   type SCIdx p 
--   type SCInfo p
--   type SCLocal p a
--   newSIC :: SCInfo p -> SCLocal p a -> IO p
--   readSIC :: p -> SCIdx p -> IO a
--   writeSIC :: p -> SCIdx p -> a -> IO ()
--   modifySIC :: p -> SCIdx p -> (a -> a) -> IO ()

















-- | ManagedContainer

-- class (Monad m, Storable r) => ManagedContainer m r where
--   type MCInfo r
--   type MCLocalType r
--   manageP :: MCInfo r -> (MCInfo r -> IO r) -> (r -> IO a) -> Managed r
--   manageP info get set = managed (bracket (get info) set)


-- asdf info get set = managed (bracket (get info) set)


-- withSizeArray :: (Storable a, Integral a) => [a] -> (Ptr CULong -> IO b) -> IO b
-- withSizeArray = withArray . liftM fromIntegral
