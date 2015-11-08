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
module Numerical.PETSc.Internal.PutGet.PetscMisc where

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




-- * MPI

commWorld, commSelf :: Comm
commWorld = commWorld1
commSelf = commSelf1

mpiCommSize comm = chk1 (mpiCommSize' comm)
mpiCommRank comm = chk1 (mpiCommRank' comm)


-- * misc PETSc

-- -- NB : all PETSc functions must appear within a withPetsc* bracket

petscInit0 :: IO ()
petscInit0 = chk0 petscInit01 >> putStrLn "PETSc : initialized with default options"

petscFin :: IO ()
petscFin = chk0 petscFin1 >> putStrLn "PETSc : finalized"

withPetsc0 :: IO a -> IO a
withPetsc0 = bracket_ petscInit0 petscFin

petscInit ::
  [String] ->   -- "argv" list of strings
  String ->     -- options string
  String ->     -- help string
  IO ()
petscInit args opts help = 
  chk0 (petscInitialize1 args opts help) >> putStrLn "PETSc : initialized"

withPetsc ::
  [String] -> String -> String -> IO a -> IO a
withPetsc a o h = bracket_ (petscInit a o h) petscFin
