{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.IS
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | IS Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.IS where

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







data IsInfo = IsInfo { isInfoMpiComm :: Comm,
                       isIndexSet :: ![Int]} deriving (Eq, Show)

type ISI = (IS, IsInfo)
data PetscIs = PIs (MVar ISI)


isCreateGeneral cc n idx mode = chk1 (isCreateGeneral' cc n idx mode)

isDestroy is = chk0 $ isDestroy' is



withIsCreateGeneral :: IsInfo -> PetscCopyMode_ -> (IS -> IO a) -> IO a
withIsCreateGeneral iis mode =
  bracket (isCreateGeneral cc n idx mode) isDestroy where
    cc = isInfoMpiComm iis
    n = toCInt $ length idxi
    idx = map toCInt idxi
    idxi = isIndexSet iis

isColoringCreate ::
  Comm ->
  CInt ->
  CInt ->
  [CInt] ->
  PetscCopyMode_ ->
  IO ISColoring
isColoringCreate cc nc n cols cm =
  chk1 (isColoringCreate' cc nc n cols cm)

isColoringDestroy :: ISColoring -> IO ()
isColoringDestroy isc = chk0 (isColoringDestroy' isc)

dmCreateColoring :: DM -> ISColoringType_ -> IO ISColoring
dmCreateColoring dm coloringtype = chk1 $ dmCreateColoring' dm coloringtype


withDmIsColoring :: DM -> ISColoringType_ -> (ISColoring -> IO a) -> IO a
withDmIsColoring dm ct = bracket (dmCreateColoring dm ct) isColoringDestroy
