{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Raw.PutGet.PetscMisc
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | PETSc misc. functions, Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Raw.PutGet.PetscMisc where

import Numerical.PETSc.Raw.InlineC
import Numerical.PETSc.Raw.Types
import Numerical.PETSc.Raw.Exception
import Numerical.PETSc.Raw.Utils

import Numerical.PETSc.Raw.Internal

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
