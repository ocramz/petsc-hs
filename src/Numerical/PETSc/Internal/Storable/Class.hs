{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Storable.Class
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Storable classes
--
-----------------------------------------------------------------------------
{-|

-}

module Numerical.PETSc.Internal.Storable.Class where

import Control.Monad

-- | mtl
import Control.Monad.Managed
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class

import Foreign.Storable

import Control.Exception

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



class (Monad m, Storable r) => ManagedContainer m r where
  type MCInfo r
  type MCLocalType r
  manageP :: MCInfo r -> (MCInfo r -> IO r) -> (r -> IO a) -> Managed r
  manageP info get set = managed (bracket (get info) set)


asdf info get set = managed (bracket (get info) set)


-- withSizeArray :: (Storable a, Integral a) => [a] -> (Ptr CULong -> IO b) -> IO b
-- withSizeArray = withArray . liftM fromIntegral
