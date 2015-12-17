{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.Petsc.Internal.Mutable
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Mutable containers in the IO monad
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Mutable where

import Control.Concurrent.MVar
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.ST (runST, ST)

import Data.IORef
import Foreign.Storable


  














-- class Mutable v where
--   newVar     :: a -> IO (v a)
--   readVar    :: v a -> IO a
--   writeVar   :: v a -> a -> IO ()
--   modifyVar  :: v a -> (a -> a) -> IO ()
--   modifyVar' :: v a -> (a -> (a, b)) -> IO b



-- instance Mutable MVar where
--   newVar = newMVar
--   readVar = takeMVar
--   writeVar = putMVar
--   modifyVar v f = modifyMVar_ v (return . f)
--   modifyVar' v f = modifyMVar v (return . f)

-- instance Mutable TVar where
--   newVar = newTVarIO
--   readVar = readTVarIO
--   writeVar v x = atomically $ writeTVar v x
--   modifyVar v f = atomically $ do x <- readTVar v
--                                   writeTVar v (f x)
--   modifyVar' v f = atomically $ do x <- readTVar v
--                                    let (x', y) = f x
--                                    writeTVar v x'
--                                    return y
