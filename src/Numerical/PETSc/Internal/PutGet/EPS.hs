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


withEps :: Comm -> (EPS -> IO a) -> IO a
withEps comm = bracketChk (epsCreate' comm) epsDestroy'

withEpsSetup :: Comm -> Mat -> Mat -> EpsProblemType_ -> (EPS -> IO a) -> IO a
withEpsSetup comm matA matB ty post = withEps comm $ \e -> do
  chk0 $ epsSetOperators' e matA matB
  chk0 $ epsSetProblemType' e ty
  chk0 $ epsSetup' e
  post e

withEpsSetupSolve :: Comm -> Mat -> Mat -> EpsProblemType_ -> (EPS -> IO a) -> IO a
withEpsSetupSolve comm a b ty postsolve = withEpsSetup comm a b ty $ \e -> do
  chk0 $ epsSolve' e
  postsolve e

epsIsHermitian, epsIsPositive :: EPS -> IO PetscBool
epsIsHermitian = chk1 . epsIsHermitian'
epsIsPositive = chk1 . epsIsPositive'

epsSetDimensions :: EPS -> Int -> Int -> Int -> IO ()
epsSetDimensions e nev ncv mpd  = chk0 $ epsSetDimensions' e nev ncv mpd

epsSetInterval :: EPS -> PetscReal_ -> PetscReal_ -> IO ()
epsSetInterval e smin smax = chk0 $ epsSetInterval' e smin smax




epsView :: EPS -> PetscViewer -> IO ()
epsView e viewer = chk0 $ epsView' e viewer
