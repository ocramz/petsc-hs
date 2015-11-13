-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Hybrid.DMVec
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | DM/DMDA + Vec : MPI-distributed regular arrays
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Hybrid.DMVec where

-- import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

import Numerical.PETSc.Internal.PutGet.DM
import Numerical.PETSc.Internal.PutGet.Vec

import Numerical.PETSc.Internal.Storable.Vector

import qualified Data.Vector.Storable as VS

-- import Foreign
-- import Foreign.C.Types

-- import System.IO.Unsafe (unsafePerformIO)

dmdaVecGetVector1 :: DM -> Vec -> Int -> IO (VS.Vector PetscScalar_)
dmdaVecGetVector1 dm v =
  vectorFreezeFromStorablePtr (dmdaVecGetArrayPtr dm v) (dmdaVecRestoreArrayPtr dm v)

-- dmdaVecGetVector :: DM -> Vec -> Int -> IO (V.Vector PetscScalar_)
-- dmdaVecGetVector dm v len = do
--   p <- dmdaVecGetArrayPtr dm v
--   pf <- newForeignPtr_ p
--   V.freeze (VM.unsafeFromForeignPtr0 pf len)

dmdaVecRestoreVector1 :: DM -> Vec -> Int -> VS.Vector PetscScalar_ -> IO ()
dmdaVecRestoreVector1 dm v =
  vectorCopyToForeignPtr (dmdaVecGetArrayPtr dm v) (dmdaVecRestoreArrayPtr dm v)

-- dmdaVecRestoreVector :: DM -> Vec -> Int -> V.Vector PetscScalar_ -> IO ()
-- dmdaVecRestoreVector dm v len w = do
--   p <- dmdaVecGetArrayPtr dm v
--   pf <- newForeignPtr_ p
--   V.copy (VM.unsafeFromForeignPtr0 pf len) w
--   dmdaVecRestoreArrayPtr dm v p
