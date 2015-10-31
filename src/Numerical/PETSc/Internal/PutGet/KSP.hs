{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Raw.PutGet.KSP
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | KSP Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Raw.PutGet.KSP where

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





kspCreate :: Comm -> IO KSP
kspCreate comm = chk1 (kspCreate' comm)

kspDestroy :: KSP -> IO ()
kspDestroy ksp = chk0 (kspDestroy' ksp)

kspSetType :: KSP -> KspType_ -> IO ()
kspSetType ksp kt = chk0 (kspSetType' ksp kt)

withKsp :: Comm -> (KSP -> IO a) -> IO a
withKsp comm =
  bracket (kspCreate comm) kspDestroy

withKspSetup ::
  Comm ->
  KspType_ ->
  Mat ->            -- linear operator
  Mat ->            -- preconditioner
  Bool ->           -- set initial solution guess to nonzero vector
  (KSP -> IO a) ->  -- post-setup actions, i.e. solve with a r.h.s , etc.
  IO a
withKspSetup comm kt amat pmat ignz f = withKsp comm $ \ksp -> do
  kspSetOperators ksp amat pmat
  kspSetType ksp kt
  kspSetInitialGuessNonzero ksp ignz
  kspSetUp ksp
  f ksp

withKspSetupSolve ::
  Comm ->
  KspType_ ->
  Mat ->            -- linear operator
  Mat ->            -- preconditioner
  Bool ->           -- set initial solution guess to nonzero vector
  Vec ->            -- r.h.s
  Vec ->            -- solution (WILL BE OVERWRITTEN)
  (KSP -> IO a) ->  -- post-solve actions
  IO a
withKspSetupSolve comm kt amat pmat ignz rhsv solnv post =
  withKspSetup comm kt amat pmat ignz $ \ksp -> do
    kspSolve ksp rhsv solnv
    post ksp


kspSetOperators :: KSP -> Mat -> Mat -> IO ()
kspSetOperators ksp amat pmat = chk0 (kspSetOperators' ksp amat pmat)

kspSetInitialGuessNonzero :: KSP -> Bool -> IO ()
kspSetInitialGuessNonzero ksp ig = chk0 (kspSetInitialGuessNonzero' ksp ig)

kspSetUp :: KSP -> IO ()
kspSetUp ksp = chk0 (kspSetUp' ksp)

kspSolve, kspSolveTranspose :: 
  KSP -> Vec -> Vec -> IO ()
kspSolve ksp rhsv solnv =  chk0 (kspSolve' ksp rhsv solnv)
kspSolveTranspose ksp rhsv solnv = chk0 (kspSolve' ksp rhsv solnv)

kspSetReusePreconditioner ::
  KSP -> Bool -> IO ()
kspSetReusePreconditioner ksp b = chk0 (kspSetReusePreconditioner' ksp b)

kspGetRhs :: KSP -> IO Vec
kspGetRhs ksp = chk1 (kspGetRhs' ksp)

kspGetSolution :: KSP -> IO Vec
kspGetSolution ksp = chk1 (kspGetSolution' ksp)

kspGetResidualNorm :: KSP -> IO PetscReal_
kspGetResidualNorm ksp = chk1 (kspGetResidualNorm' ksp)

kspGetIterationNumber :: KSP -> IO CInt
kspGetIterationNumber ksp = chk1 (kspGetIterationNumber' ksp)








