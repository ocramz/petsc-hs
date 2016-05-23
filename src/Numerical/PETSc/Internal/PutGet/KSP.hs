-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.KSP
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | KSP Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.KSP where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

import Numerical.PETSc.Internal.PutGet.Vec

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



-- | create KSP

kspCreate :: Comm -> IO KSP
kspCreate cc = chk1 (kspCreate' cc)


-- | with KSP brackets

-- withKsp :: Comm -> (KSP -> IO a) -> IO a
-- withKsp comm =
--   bracket (kspCreate comm) kspDestroy

withKsp_ :: IO KSP -> (KSP -> IO a) -> IO a
withKsp_ kc = bracket kc kspDestroy

withKsp :: Comm -> (KSP -> IO a) -> IO a
withKsp cc = withKsp_ (kspCreate cc)

withKspSetup ::
  Comm ->
  KspType_ ->
  Mat ->            -- linear operator
  Mat ->            -- preconditioner
  Bool ->           -- set initial solution guess to nonzero vector
  (KSP -> IO a) ->  -- post-setup actions, i.e. solve with a r.h.s , etc.
  IO a
withKspSetup cc kt amat pmat ignz f = withKsp cc $ \ksp -> do
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
withKspSetupSolve cc kt amat pmat ignz rhsv solnv post =
  -- withVecDuplicate rhsv $ \solnv -> 
  withKspSetup cc kt amat pmat ignz $ \ksp -> do
    kspSolve ksp rhsv solnv
    post ksp

-- | allocate space for solution internally
withKspSetupSolveAlloc ::
  Comm -> KspType_ -> Mat -> Mat -> Vec -> (KSP -> Vec -> IO a ) -> IO a
withKspSetupSolveAlloc cc kt amat pmat rhsv post =
  withVecDuplicate rhsv $ \soln ->
  withKspSetupSolve cc kt amat pmat True rhsv soln $ \ksp ->
  post ksp soln






-- withKsp cs $ \ksp -> do
     --   kspSetOperators ksp mat mat
     --   kspSetType ksp km 
     --   kspSetInitialGuessNonzero ksp True
     --   kspSetUp ksp
     --   kspSolve ksp x v
     --   soln <- kspGetSolution ksp
     --   -- vecViewStdout soln
     --   showKspStats ksp km
     --   -- kspReasonView ksp
     --   -- return soln



    

-- | destroy KSP

kspDestroy :: KSP -> IO ()
kspDestroy ksp = chk0 (kspDestroy' ksp)







-- | set KSP properties

kspSetType :: KSP -> KspType_ -> IO ()
kspSetType ksp kt = chk0 (kspSetType' ksp kt)




kspSetOperators :: KSP -> Mat -> Mat -> IO ()
kspSetOperators ksp amat pmat = chk0 (kspSetOperators' ksp amat pmat)

-- kspSetInitialGuessNonzero :: KSP -> PetscBool -> IO ()
kspSetInitialGuessNonzero ksp ig = chk0 (kspSetInitialGuessNonzero' ksp ig)

kspSetUp :: KSP -> IO ()
kspSetUp ksp = chk0 (kspSetUp' ksp)

kspSolve, kspSolveTranspose :: 
  KSP -> Vec -> Vec -> IO ()
kspSolve ksp rhsv solnv =  chk0 (kspSolve' ksp rhsv solnv)
kspSolveTranspose ksp rhsv solnv = chk0 (kspSolve' ksp rhsv solnv)

kspSetReusePreconditioner :: KSP -> PetscBool -> IO ()
kspSetReusePreconditioner ksp b = chk0 (kspSetReusePreconditioner' ksp b)

kspSetTolerances ksp rtol abstol dtol maxits = chk0 $ kspSetTolerances' ksp rtol abstol dtol maxits

kspGetRhs :: KSP -> IO Vec
kspGetRhs ksp = chk1 (kspGetRhs' ksp)

kspGetSolution :: KSP -> IO Vec
kspGetSolution ksp = chk1 (kspGetSolution' ksp)

kspGetConvergedReason :: KSP -> IO KspConvergedReason
kspGetConvergedReason ksp = do
  r <- chk1 (kspGetConvergedReason' ksp)
  return $ kspConvergedIntToReason (fi r)

kspGetResidualNorm :: KSP -> IO PetscReal_
kspGetResidualNorm ksp = chk1 (kspGetResidualNorm' ksp)

kspGetIterationNumber :: KSP -> IO CInt
kspGetIterationNumber ksp = chk1 (kspGetIterationNumber' ksp)







kspGetPc :: KSP -> IO PC
kspGetPc k = chk1 (kspGetPc' k)
