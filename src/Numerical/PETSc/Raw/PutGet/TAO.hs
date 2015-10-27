{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Raw.PutGet.TAO
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | TAO Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Raw.PutGet.TAO where

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




taoCreate :: Comm -> IO Tao
taoCreate comm = chk1 $ taoCreate' comm

taoDestroy :: Tao -> IO ()
taoDestroy tao = chk0 $ taoDestroy' tao

withTao :: Comm -> (Tao -> IO a) -> IO a
withTao comm = bracket (taoCreate comm) taoDestroy

taoSetType :: Tao -> TaoType_ -> IO ()
taoSetType tao ty = chk0 $ taoSetType' tao ty

taoSetInitialVector :: Tao -> Vec -> IO ()
taoSetInitialVector tao x = chk0 $ taoSetInitialVector' tao x

taoSetVariableBounds ::
  Tao ->
  Vec ->  -- min
  Vec ->  -- max
  IO ()
taoSetVariableBounds tao xmin xmax = chk0 $ taoSetVariableBounds' tao xmin xmax

taoSolve :: Tao -> IO ()
taoSolve tao = chk0 $ taoSolve' tao

taoGetSolutionVector :: Tao -> IO Vec
taoGetSolutionVector tao = chk1 $ taoGetSolutionVector' tao

taoComputeObjective :: Tao -> Vec -> IO PetscReal_
taoComputeObjective tao v = chk1 $ taoComputeObjective' tao v

taoComputeGradient :: Tao -> Vec -> IO Vec
taoComputeGradient tao v = chk1 $ taoComputeGradient' tao v

taoIsObjectiveDefined, taoIsGradientDefined :: Tao -> IO PetscBool_
taoIsObjectiveDefined tao = chk1 $ taoIsObjectiveDefined' tao
taoIsGradientDefined tao = chk1 $ taoIsGradientDefined' tao











