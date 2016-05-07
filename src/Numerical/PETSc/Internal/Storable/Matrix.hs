{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Storable.Matrix
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | operations on Storable Matrix, derived from Internal.Storable.Vector
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Storable.Matrix where

import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Utils
import Numerical.PETSc.Internal.Storable.Vector
import Numerical.PETSc.Internal.Storable.Store

import Data.Functor
import Control.Applicative
import Control.Monad

import Control.Exception 

import qualified Foreign.ForeignPtr as FPR

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Complex
import Foreign.C.Types
import Foreign.Marshal.Array
-- import qualified Foreign.Marshal.Alloc as FMA (malloc, finalizerFree)

-- -- import Data.Int(Int64)
-- import System.IO.Unsafe (unsafePerformIO)
-- -- import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import Data.Complex


import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Generic as VG

import qualified Foreign.Marshal.Utils as FMU



-- | binary adapter
adapt2 :: Monad m =>
          (a -> a -> r) -> (b -> c -> m a) -> b -> b -> c -> c -> m r
adapt2 f pfun ni nj iRow jCol = liftM2 f (pfun ni iRow) (pfun nj jCol)


-- | copy `n` and `m` elements, respectively, from the two pointers
--   and apply binary function

adapt2VS :: Storable a =>
            (VS.Vector a -> VS.Vector a -> IO r) ->
            Int -> Int -> Ptr a -> Ptr a -> IO r
adapt2VS f ni nj irow icol = join (adapt2 f getVS ni nj irow icol)

adapt2VG :: (VG.Vector w a, Storable a) =>
            (w a -> w a -> IO r) ->
            Int -> Int -> Ptr a -> Ptr a -> IO r
adapt2VG f ni nj irow icol = join (adapt2 f getVG ni nj irow icol)

