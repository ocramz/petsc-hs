{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Raw.PutGet
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | Mid-level interface: catching exceptions and hiding pointers in lexical
--   scope of `bracket`s
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Raw.PutGet where

import Numerical.PETSc.Raw.InlineC
import Numerical.PETSc.Raw.Types
import Numerical.PETSc.Raw.Exception
import Numerical.PETSc.Raw.Utils

import Numerical.PETSc.Raw.Internal

import Numerical.PETSc.Raw.PutGet.IS
import Numerical.PETSc.Raw.PutGet.Vec
import Numerical.PETSc.Raw.PutGet.Mat
import Numerical.PETSc.Raw.PutGet.DM
import Numerical.PETSc.Raw.PutGet.KSP
import Numerical.PETSc.Raw.PutGet.SNES
import Numerical.PETSc.Raw.PutGet.TS
import Numerical.PETSc.Raw.PutGet.TAO

import Control.Exception

-- withPtr f = alloca $ \e -> do
--   err <- f e
--   res <- peek e
--   return (res, err)
  
--------
{- from Base :
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar :: MVar a -> (a -> IO (a,b)) -> IO b  -}
---------



-- * PetscViewer

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




  

-- * MPI

commWorld, commSelf :: Comm
commWorld = commWorld1
commSelf = commSelf1



-- * misc PETSc

-- -- NB : all PETSc functions must appear within a withPetsc* bracket

petscInit0 :: IO ()
petscInit0 = chk0 petscInit01

petscFin :: IO ()
petscFin = chk0 petscFin1

withPetsc0 :: IO a -> IO a
withPetsc0 = bracket_ petscInit0 petscFin

petscInit ::
  [String] ->   -- "argv" list of strings
  String ->     -- options string
  String ->     -- help string
  IO ()
petscInit args opts help = chk0 $ petscInitialize1 args opts help

withPetsc ::
  [String] -> String -> String -> IO a -> IO a
withPetsc a o h = bracket_ (petscInit a o h) petscFin
