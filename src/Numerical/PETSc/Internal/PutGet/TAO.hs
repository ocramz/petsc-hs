{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.TAO
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | TAO Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.TAO where

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




taoCreate :: Comm -> IO Tao
taoCreate cc = chk1 $ taoCreate' cc



withTao :: IO Tao -> (Tao -> IO a) -> IO a
withTao mc = bracket mc taoDestroy where
  taoDestroy :: Tao -> IO ()
  taoDestroy tao = chk0 $ taoDestroy' tao

withTaoCreate :: Comm -> (Tao -> IO a) -> IO a
withTaoCreate c = withTao (taoCreate c)


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




-- | callback setters
-- (NB : callbacks are made to return `0 :: CInt` to PETSc )

taoSetConstraintsRoutine ::
  Tao ->
  Vec ->
  (Tao -> Vec -> Vec -> IO a) ->
  IO ()
taoSetConstraintsRoutine tt v f = chk0 (taoSetConstraintsRoutine' tt v g) where
  g ttt v1 v2 = return0 (f ttt v1 v2)

taoSetJacobianRoutine ::
  Tao ->
  Mat ->
  Mat ->
  (Tao -> Vec -> Mat -> Mat -> IO a) ->
  IO ()
taoSetJacobianRoutine tt m1 m2 f = chk0 (taoSetJacobianRoutine' tt m1 m2 g) where
  g ttt v mm1 mm2 = return0 (f ttt v mm1 mm2)

taoSetVariableBoundsRoutine ::
  Tao ->
  (Tao -> Vec -> Vec -> IO a) ->
  IO ()
taoSetVariableBoundsRoutine tt f = chk0 (taoSetVariableBoundsRoutine' tt g) where
  g ttt v1 v2 = return0 (f ttt v1 v2)







taoSolve :: Tao -> IO ()
taoSolve tao = chk0 $ taoSolve' tao

taoGetSolutionVector :: Tao -> IO Vec
taoGetSolutionVector tao = chk1 $ taoGetSolutionVector' tao

taoComputeObjective :: Tao -> Vec -> IO PetscReal_
taoComputeObjective tao v = chk1 $ taoComputeObjective' tao v

taoComputeGradient0 :: Tao -> Vec -> Vec -> IO ()
taoComputeGradient0 tao v p = chk0 $ taoComputeGradient0' tao v p

-- | allocation bracket for a gradient vector
withTaoComputeGradient ::
  Tao ->
  Vec ->                         -- point at which to evaluate gradient
  (Tao -> Vec -> Vec -> IO a) -> -- 
  IO a
withTaoComputeGradient tao v act = withVecDuplicate v $ \p -> do
  taoComputeGradient0 tao v p
  act tao v p


taoIsObjectiveDefined, taoIsGradientDefined :: Tao -> IO PetscBool
taoIsObjectiveDefined tao = chk1 $ taoIsObjectiveDefined' tao
taoIsGradientDefined tao = chk1 $ taoIsGradientDefined' tao







-- | with- Tao brackets

-- create, set type, set initial vector

withTaoCreateInit ::
  Comm ->
  TaoType_ ->
  Vec ->           -- initial vector (NB : will be modified)
  (Tao -> IO a) ->
  (Tao -> IO b) ->
  IO b 
withTaoCreateInit c ty v pre f = withTaoCreate c $ \tt -> do
  taoSetType tt ty
  taoSetInitialVector tt v
  pre tt
  f tt









