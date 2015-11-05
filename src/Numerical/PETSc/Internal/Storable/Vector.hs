{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Storable.Vector
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | operations on Vector.Storable
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Storable.Vector where

import Numerical.PETSc.Internal.Utils
import Numerical.PETSc.Internal.Storable.Store

import Control.Monad
import Foreign.Marshal.Array (peekArray)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
-- import Data.Int(Int64)
-- import Data.Complex
-- import System.IO.Unsafe (unsafePerformIO)
-- import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS


-- | instances

instance PStore VS.Vector where
  toForeignPtr = VS.unsafeToForeignPtr
  fromForeignPtr = VS.unsafeFromForeignPtr
  smap = VS.map



-- | methods
