{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.EPS
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | EPS Mid-level interface (eigenvalue problem solvers)
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.EPS where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.C2HsGen.TypesC2HsGen
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

import Control.Exception (bracket)

import Numerical.PETSc.Internal.PutGet.Vec
import Numerical.PETSc.Internal.PutGet.Mat




 



-- | setup

-- ordinary eigenproblem : A x = lambda x
epsSetOperators0 :: EPS -> Mat -> IO ()
epsSetOperators0 eps matA  = chk0 $ epsSetOperators0' eps matA

-- generalized eigenproblem : A x = lambda B x
epsSetOperators :: EPS -> Mat -> Mat -> IO ()
epsSetOperators eps matA matB = chk0 $ epsSetOperators' eps matA matB

epsSetProblemType :: EPS -> EpsProblemType_ -> IO ()
epsSetProblemType e ty = chk0 $ epsSetProblemType' e ty

epsSetup :: EPS -> IO ()
epsSetup e = chk0 $ epsSetup' e





-- | solve

epsSolve :: EPS -> IO ()
epsSolve e = chk0 $ epsSolve' e




-- | `with` brackets

withEpsCreate :: Comm -> (EPS -> IO a) -> IO a
withEpsCreate k = bracket (epsCreate k) epsDestroy where
  epsCreate cc = chk1 $ epsCreate' cc
  epsDestroy e = chk0 $ epsDestroy' e

-- | refer to http://slepc.upv.es/documentation/current/src/eps/examples/tutorials/ex31.c.html for EPS setup + solution


withEpsCreateSetup ::
  Comm -> Mat -> EpsProblemType_ -> (EPS -> Vec -> IO a) -> IO a
withEpsCreateSetup cc matA ty post = withEpsCreate cc $ \e -> do
  epsSetOperators0 e matA -- matB
  vr <- matCreateVecRight matA
  epsSetProblemType e ty
  epsSetup e
  post e vr

-- withEpsCreateSetupSolve ::
--   Comm -> Mat -> Mat -> EpsProblemType_ -> (EPS -> IO a) -> IO a
withEpsCreateSetupSolve cc a ty postsolve = withEpsCreateSetup cc a ty $ \e vr -> do
  epsSolve e
  nconv <- epsGetConverged e
  postsolve e nconv

-- | type for retrieving eigenpair results : either the i'th eigenpair or the one specified by EPSWhich_, along with its target value (if this applies)
type EPSGetEigenpair = Either Int (EpsWhich_, Maybe PetscScalar_)


-- | # of converged eigenpairs

epsGetConverged :: EPS -> IO Int
epsGetConverged eps = fmap fi (chk1 $ epsGetConverged' eps)




-- | check properties of eigensolution

epsComputeError :: EPS -> Int -> EpsErrorType_ -> IO PetscReal_
epsComputeError eps i ty = chk1 $ epsComputeError' eps i ty

epsIsHermitian, epsIsPositive :: EPS -> IO PetscBool
epsIsHermitian = chk1 . epsIsHermitian'
epsIsPositive = chk1 . epsIsPositive'



-- | set # of eigenvalues to compute and subspace dimension

epsSetDimensions ::
  EPS ->
  Int ->  -- # eigenvalues
  Int ->  -- subspace dimensionality
  Int ->  -- maximum projected dimensionality
  IO ()
epsSetDimensions e nev ncv mpd  = chk0 $ epsSetDimensions' e nev ncv mpd





epsSetInterval :: EPS -> PetscReal_ -> PetscReal_ -> IO ()
epsSetInterval e smin smax = chk0 $ epsSetInterval' e smin smax






-- | various tricks to improve convergence

-- FIXME : use Vector instead of list
epsSetInitialSpace :: EPS -> [Vec] -> IO ()
epsSetInitialSpace e ss = chk0 $ epsSetInitialSpace' e ss

epsSetDeflationSpace :: EPS -> [Vec] -> IO ()
epsSetDeflationSpace e ds = chk0 $ epsSetDeflationSpace' e ds



-- | target eigenvalue and eigenpair choice criterion

epsSetWhichEigenpairs :: EPS -> EpsWhich_ -> IO ()
epsSetWhichEigenpairs e w = chk0 $ epsSetWhichEigenpairs' e w

epsSetTarget :: EPS -> PetscScalar_ -> IO ()
epsSetTarget e t = chk0 $ epsSetTarget' e t








-- | viewers

epsView :: EPS -> PetscViewer -> IO ()
epsView e viewer = chk0 $ epsView' e viewer

epsVectorsView :: EPS -> PetscViewer -> IO ()
epsVectorsView e viewer = chk0 $ epsVectorsView' e viewer
