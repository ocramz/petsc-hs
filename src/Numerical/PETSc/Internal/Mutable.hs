{-# LANGUAGE MultiParamTypeClasses, KindSignatures, TypeFamilies #-}
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

import Foreign.Storable

import qualified Control.Monad.Primitive as P




type family MutContainer (mc :: * -> *) :: * -> * -> *


class MContainer c a where
  type MCSize c
  type MCIdx c
  basicSize :: c s a -> MCSize c
  basicUnsafeSlice :: MCIdx c -> MCIdx c -> c s a -> c s a
  -- basicUnsafeThaw :: P.PrimMonad m => c s a -> m (MutContainer c (P.PrimState m) a)
  basicInitialize :: P.PrimMonad m => c (P.PrimState m) a -> m ()
  -- | Yield the element at the given position. This method should not be
  -- called directly, use 'unsafeRead' instead.
  basicUnsafeRead  :: P.PrimMonad m => c (P.PrimState m) a -> MCIdx c -> m a

  -- | Replace the element at the given position. This method should not be
  -- called directly, use 'unsafeWrite' instead.
  basicUnsafeWrite :: P.PrimMonad m => c (P.PrimState m) a -> MCIdx c -> a -> m ()

-- instance G.Vector Vector a where
--   basicUnsafeFreeze (MVector i n marr)
--     = Vector i n `liftM` unsafeFreezeArray marr
--   basicUnsafeThaw (Vector i n arr)
--     = MVector i n `liftM` unsafeThawArray arr
--   basicLength (Vector _ n _) = n
--   basicUnsafeSlice j n (Vector i _ arr) = Vector (i+j) n arr
--   basicUnsafeIndexM (Vector i _ arr) j = indexArrayM arr (i+j)
--   basicUnsafeCopy (MVector i n dst) (Vector j _ src)
--     = copyArray dst i src j n









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
