{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes, FlexibleContexts#-}
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

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS


 



-- | setup

-- ordinary eigenproblem : A x = lambda x
-- ", setting
epsSetOperators0 :: EPS -> Mat -> IO ()
epsSetOperators0 eps matA  = chk0 $ epsSetOperators0' eps matA

-- ", retrieval
epsGetOperators0 :: EPS -> IO Mat
epsGetOperators0 eps = chk1 $ epsGetOperators0' eps

-- generalized eigenproblem : A x = lambda B x
-- ", setting
epsSetOperators :: EPS -> Mat -> Mat -> IO ()
epsSetOperators eps matA matB = chk0 $ epsSetOperators' eps matA matB

-- ", retrieval
epsGetOperators :: EPS -> IO (Mat, Mat)
epsGetOperators eps = chk1 $ epsGetOperators' eps


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

withEpsCreateSetupSolve ::
  Comm ->
  Mat ->
  EpsProblemType_ ->
  ( EPS -> 
    Int ->           -- # of converged eigenpairs
    IO a) ->
  IO a
withEpsCreateSetupSolve cc a ty postsolve = withEpsCreateSetup cc a ty $ \e vr -> do
  epsSolve e
  nconv <- epsGetConverged e
  postsolve e nconv




-- | # of converged eigenpairs

epsGetConverged :: EPS -> IO Int
epsGetConverged eps = fmap fi (chk1 $ epsGetConverged' eps)





-- | get eigenvalues and eigenvectors
epsGetEigenvalue eps ii = chk1 $ epsGetEigenvalue' eps ii

epsGetEigenvector eps ii = undefined
  where
    ege ee jj vvr vvi = chk0 $ epsGetEigenvector ee jj vvr vvi



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






-- | various tricks to steer convergence

{-- Specify a basis of vectors that constitute the initial space, that is, the subspace from which the solver starts to iterate.  --}
epsSetInitialSpace :: VG.Vector v Vec => EPS -> v Vec -> IO ()
epsSetInitialSpace e ins = chk0 $ epsSetInitialSpace' e ins'
  where ins' = VG.convert ins

{--When a deflation space is given, the eigensolver seeks the eigensolution in the restriction of the problem to the orthogonal complement of this space. This can be used for instance in the case that an invariant subspace is known beforehand (such as the nullspace of the matrix).
These vectors do not persist from one EPSSolve() call to the other, so the deflation space should be set every time.
The vectors do not need to be mutually orthonormal, since they are explicitly orthonormalized internally.--}
epsSetDeflationSpace :: VG.Vector v Vec => EPS -> v Vec -> IO ()
epsSetDeflationSpace e ds = chk0 $ epsSetDeflationSpace' e ds'
  where ds' = VG.convert ds
  



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





-- | hybrid


-- 

withEpsVecRight :: EPS -> (Vec -> IO a) -> IO a
withEpsVecRight e fm = do
  mat <- epsGetOperators0 e 
  withVec (matCreateVecRight mat) fm
