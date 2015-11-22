-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Raw.Storable.Common
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | common adapters for Storable
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Storable.Common where

import Data.Functor
import Control.Applicative
import Control.Monad

import Foreign.C.Types


  


liftF1_ ::
  Monad m =>
  (r -> m a) ->
  (r -> b -> m c) ->
  r ->
  (a -> b) ->
  m c
liftF1_ get set x f = do
  v <- get x
  set x (f v)

-- -- e.g. 
-- liftVectorF1_ :: Vec -> (V.Vector PetscScalar_ -> V.Vector PetscScalar_) -> IO ()
-- liftVectorF1_ = liftF1_ vecGetVector vecRestoreVector

liftF2 ::
  (Functor m, Monad m) =>
  (a -> b -> c) ->
  a ->
  (d -> m b) ->
  (d -> c -> m e) ->
  d ->
  m CInt
liftF2 f a getter setter b = do
  y <- f a <$> getter b
  setter b y
  return (0 :: CInt)
