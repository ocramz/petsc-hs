{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.TS
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | TS Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.TS where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

import Numerical.PETSc.Internal.Internal

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





tsCreate :: Comm -> IO TS
tsCreate comm = chk1 $ tsCreate' comm

tsDestroy :: TS -> IO ()
tsDestroy ts = chk0 $ tsDestroy' ts

-- withTs :: Comm -> (TS -> IO a) -> IO a
withTs tsc = bracket tsc tsDestroy

tsSetProblemType :: TS -> TsProblemType -> IO ()
tsSetProblemType ts ty = chk0 $ tsSetProblemType' ts ty

tsSetInitialTimeStep ::
  TS ->
  PetscReal_ -> -- initial time
  PetscReal_ -> -- initial timestep
  IO ()
tsSetInitialTimeStep ts it dt = chk0 $ tsSetInitialTimeStep' ts it dt


-- tsSetRHSFunction ts r f ctx = chk0 $ tsSetRHSFunction0' ts r f ctx

tsSetDuration ::
  TS ->
  Int ->  -- max. # steps
  PetscReal_ -> -- max. time
  IO ()
tsSetDuration ts ms mt = chk0 $ tsSetDuration' ts ms mt

tsSetSolution ::
  TS ->
  Vec ->        -- initial condition
  IO ()
tsSetSolution ts isolnv = chk0 $ tsSetSolution' ts isolnv

tsSolve_ :: TS -> IO ()
tsSolve_ ts = chk0 $ tsSolve_' ts

tsSolveWithInitialCondition :: TS -> Vec -> IO ()
tsSolveWithInitialCondition ts isolnv = do
  tsSetSolution ts isolnv
  tsSolve_ ts

tsSetDm ts dm = chk0 (tsSetDm' ts dm)
