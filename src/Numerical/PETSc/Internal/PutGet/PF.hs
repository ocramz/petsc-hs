{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.PF
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | PF Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.PF where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

import Foreign
import Foreign.C.Types

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Control.Arrow
import Control.Concurrent
import Control.Exception

import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST) -- for HMatrix bits

import qualified Data.Vector as V
import qualified Data.Vector.Storable as V (unsafeWith, unsafeFromForeignPtr, unsafeToForeignPtr)


pfCreate ::
  Comm ->
  Int ->     -- dimension of domain
  Int ->     -- dimension of range
  IO PF
pfCreate comm dimin dimout = chk1 $ pfCreate' comm dimin dimout

pfDestroy :: PF -> IO ()
pfDestroy pf = chk0 $ pfDestroy' pf

withPf :: Comm -> Int -> Int -> (PF -> IO a) -> IO a
withPf comm dimin dimout = bracket (pfCreate comm dimin dimout) pfDestroy

dmdaCreatePF :: DM -> IO PF
dmdaCreatePF dm = chk1 $ dmdaCreatePF' dm

-- pfSetType pf t 
