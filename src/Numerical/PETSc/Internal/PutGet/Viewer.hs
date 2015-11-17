{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.Viewer
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Viewer Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.Viewer where

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







withPetscViewer :: Comm -> (PetscViewer -> IO a) -> IO a
withPetscViewer comm =
  bracketChk (petscViewerCreate' comm) petscViewerDestroy'

withPetscViewerSetup comm ty mode name f = withPetscViewer comm $ \v -> do
  chk0 $ petscViewerSetType' v ty
  chk0 $ petscViewerFileSetName' v name
  chk0 $ petscViewerFileSetMode' v mode
  f v


{- -- -- usage of HDF5 groups: 
 50:   VecView(x1, viewer);
 51:   PetscViewerHDF5PushGroup(viewer, "/testBlockSize");
 52:   VecView(x2, viewer);
 53:   PetscViewerHDF5PushGroup(viewer, "/testTimestep");
-}

withPetscViewerHDF5Group :: PetscViewer -> String -> (PetscViewer -> IO a) -> IO ()
withPetscViewerHDF5Group viewer name f = do
  chk0 $ petscViewerHDF5PushGroup1 viewer name
  f viewer
  chk0 $ petscViewerHDF5PopGroup1 viewer

petscViewerHDF5Open :: Comm -> String -> PetscViewerType_ -> IO PetscViewer
petscViewerHDF5Open comm name ty = chk1 (petscViewerHDF5Open' comm name ty)


petscViewerSetType :: PetscViewer -> PetscViewerType_ -> IO ()
petscViewerSetType v t = chk0 (petscViewerSetType' v t)
