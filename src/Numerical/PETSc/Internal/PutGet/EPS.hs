{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.EPS
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | EPS Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.EPS where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

import Numerical.PETSc.Internal.PutGet.Vec
import Numerical.PETSc.Internal.PutGet.Mat


-- | init/fin

epsCreate :: Comm -> IO EPS
epsCreate comm = chk1 $ epsCreate' comm

epsDestroy :: EPS -> IO ()
epsDestroy eps = chk0 $ epsDestroy' eps



-- | setup

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

withEps :: Comm -> (EPS -> IO a) -> IO a
withEps comm = bracket (epsCreate comm) epsDestroy

withEpsSetup :: Comm -> Mat -> Mat -> EpsProblemType_ -> (EPS -> IO a) -> IO a
withEpsSetup comm matA matB ty post = withEps comm $ \e -> do
  epsSetOperators e matA matB
  epsSetProblemType e ty
  epsSetup e
  post e

withEpsSetupSolve :: Comm -> Mat -> Mat -> EpsProblemType_ -> (EPS -> IO a) -> IO a
withEpsSetupSolve comm a b ty postsolve = withEpsSetup comm a b ty $ \e -> do
  epsSolve e 
  postsolve e

epsComputeError :: EPS -> Int -> EpsErrorType_ -> IO PetscReal_
epsComputeError eps i ty = chk1 $ epsComputeError' eps i ty

epsIsHermitian, epsIsPositive :: EPS -> IO PetscBool
epsIsHermitian = chk1 . epsIsHermitian'
epsIsPositive = chk1 . epsIsPositive'




epsSetDimensions :: EPS -> Int -> Int -> Int -> IO ()
epsSetDimensions e nev ncv mpd  = chk0 $ epsSetDimensions' e nev ncv mpd

epsSetInterval :: EPS -> PetscReal_ -> PetscReal_ -> IO ()
epsSetInterval e smin smax = chk0 $ epsSetInterval' e smin smax



-- FIXME : use Vector instead of list
epsSetInitialSpace :: EPS -> [Vec] -> IO ()
epsSetInitialSpace e ss = chk0 $ epsSetInitialSpace' e ss

epsSetDeflationSpace :: EPS -> [Vec] -> IO ()
epsSetDeflationSpace e ds = chk0 $ epsSetDeflationSpace' e ds




-- | viewers

epsView :: EPS -> PetscViewer -> IO ()
epsView e viewer = chk0 $ epsView' e viewer

epsVectorsView :: EPS -> PetscViewer -> IO ()
epsVectorsView e viewer = chk0 $ epsVectorsView' e viewer
