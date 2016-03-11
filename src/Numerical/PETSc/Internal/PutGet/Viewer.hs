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




petscViewerCreate comm = chk1 (petscViewerCreate' comm)

petscViewerDestroy v = chk0 (petscViewerDestroy' v)

withPetscViewer :: Comm -> (PetscViewer -> IO a) -> IO a
withPetscViewer comm =
  bracket (petscViewerCreate comm) petscViewerDestroy

withPetscViewerTypeFmt ::
  Comm -> PetscViewerType_ -> PetscViewerFormat_ -> (PetscViewer -> IO a) -> IO a
withPetscViewerTypeFmt comm ty fmt f = withPetscViewer comm $ \v -> do
  petscViewerSetType v ty
  petscViewerSetFormat v fmt
  f v


withPetscViewerSetup comm ty mode name f = withPetscViewer comm $ \v -> do
  petscViewerSetType v ty
  chk0 $ petscViewerFileSetName' v name
  chk0 $ petscViewerFileSetMode' v mode
  f v



petscViewerSetType :: PetscViewer -> PetscViewerType_ -> IO ()
petscViewerSetType v t = chk0 (petscViewerSetType' v t)

petscViewerSetFormat :: PetscViewer -> PetscViewerFormat_ -> IO ()
petscViewerSetFormat v fmt = chk0 (petscViewerSetFormat' v fmt)


-- petscViewerStdoutCreate comm = chk1 (petscViewerStdoutCreate' comm)



-- -- | HDF5-specific stuff (can we do the same with a generic PetscViewer and setting its type to HDF5 ?)


-- {- -- -- usage of HDF5 groups: 
--  50:   VecView(x1, viewer);
--  51:   PetscViewerHDF5PushGroup(viewer, "/testBlockSize");
--  52:   VecView(x2, viewer);
--  53:   PetscViewerHDF5PushGroup(viewer, "/testTimestep");
-- -}


-- withPetscViewerHDF5Group ::
--   PetscViewer ->
--   (t -> PetscViewer -> IO a) ->
--   [(t, String)] -> IO ()
-- withPetscViewerHDF5Group vi vact vlist = 
--   forM_ vlist $ \(vobj, vname) -> do
--     petscViewerHDF5PushGroup vi vname
--     vact vobj vi
--     petscViewerHDF5PopGroup vi



petscViewerHDF5PushGroup :: PetscViewer -> String -> IO ()
petscViewerHDF5PushGroup vi name = chk0 (petscViewerHDF5PushGroup' vi name)

petscViewerHDF5PopGroup :: PetscViewer -> IO ()
petscViewerHDF5PopGroup vi = chk0 (petscViewerHDF5PopGroup' vi)



petscViewerHDF5Open :: Comm -> String -> PetscFileMode_ -> IO PetscViewer
petscViewerHDF5Open comm name fm = chk1 (petscViewerHDF5Open' comm name fm)


