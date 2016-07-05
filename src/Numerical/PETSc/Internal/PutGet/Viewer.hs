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

-- import Control.Monad.ST (ST, runST)
-- import Control.Monad.ST.Unsafe (unsafeIOToST) -- for HMatrix bits

-- import qualified Data.Vector as V
-- import qualified Data.Vector.Storable as V (unsafeWith, unsafeFromForeignPtr, unsafeToForeignPtr)






withPetscViewer :: Comm -> (PetscViewer -> IO a) -> IO a
withPetscViewer cc = 
  bracket (petscViewerCreate cc) petscViewerDestroy where
   petscViewerCreate k = chk1 (petscViewerCreate' k)
   petscViewerDestroy v = chk0 (petscViewerDestroy' v)

withPetscViewerTypeFmt ::
  Comm -> PetscViewerType_ -> PetscViewerFormat_ -> (PetscViewer -> IO a) -> IO a
withPetscViewerTypeFmt cc ty fmt f = withPetscViewer cc $ \v -> do
  petscViewerSetType v ty
  petscViewerPushFormat v fmt
  x <- f v
  petscViewerPopFormat v
  return x


withPetscViewerSetup cc ty mode name f = withPetscViewer cc $ \v -> do
  petscViewerSetType v ty
  chk0 $ petscViewerFileSetName' v name
  chk0 $ petscViewerFileSetMode' v mode
  f v



petscViewerSetType :: PetscViewer -> PetscViewerType_ -> IO ()
petscViewerSetType v t = chk0 (petscViewerSetType' v t)


petscViewerPushFormat :: PetscViewer -> PetscViewerFormat_ -> IO ()
petscViewerPushFormat v fmt = chk0 (petscViewerPushFormat' v fmt)

petscViewerPopFormat :: PetscViewer -> IO ()
petscViewerPopFormat v = chk0 $ petscViewerPopFormat' v


-- petscViewerStdoutCreate comm = chk1 (petscViewerStdoutCreate' comm)



-- -- | HDF5-specific stuff 


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



-- petscViewerHDF5PushGroup :: PetscViewer -> String -> IO ()
-- petscViewerHDF5PushGroup vi name = chk0 (petscViewerHDF5PushGroup' vi name)

-- petscViewerHDF5PopGroup :: PetscViewer -> IO ()
-- petscViewerHDF5PopGroup vi = chk0 (petscViewerHDF5PopGroup' vi)



-- petscViewerHDF5Open :: Comm -> String -> PetscFileMode_ -> IO PetscViewer
-- petscViewerHDF5Open comm name fm = chk1 (petscViewerHDF5Open' comm name fm)


