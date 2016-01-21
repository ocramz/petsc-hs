{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, RankNTypes #-}
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

-- | mtl
import Control.Monad.Managed
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class

import Foreign.Storable

import Control.Exception





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

class Storable p => StorableContainer p where
  type SCInfo p 
  type SCLocal p
  type SCRemote p
  initRemote :: SCInfo p -> IO (SCRemote p)
  updateLocal :: SCRemote p -> IO (SCLocal p)
  updateRemote :: SCRemote p -> SCLocal p -> IO ()
  withRemote :: SCRemote p -> (SCLocal p -> IO b) -> IO b
  destroyRemote :: SCRemote p -> IO ()






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

class (StorableContainer sc, Ix i) => StorableIxContainer i sc x where
  sicGet :: sc -> i -> IO x
  sicPut :: sc -> i -> x -> IO ()

-- instance






-- | ManagedContainer

-- class (Monad m, Storable r) => ManagedContainer m r where
--   type MCInfo r
--   type MCLocalType r
--   manageP :: MCInfo r -> (MCInfo r -> IO r) -> (r -> IO a) -> Managed r
--   manageP info get set = managed (bracket (get info) set)


-- asdf info get set = managed (bracket (get info) set)


-- withSizeArray :: (Storable a, Integral a) => [a] -> (Ptr CULong -> IO b) -> IO b
-- withSizeArray = withArray . liftM fromIntegral
