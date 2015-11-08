{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Test
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | tests, but not in the quickcheck sense :D
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Test where

import Data.Functor
import Control.Applicative

import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.PutGet
import Numerical.PETSc.Internal.Internal
import Numerical.PETSc.Internal.Managed


import qualified Data.Vector.Storable as VS
-- import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector as V 

import Control.Monad

import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import System.IO.Unsafe


-- --

v0 :: V.Vector PetscScalar_
v0 = V.fromList [pi .. 10]

lv = V.length v0

v0s = VS.fromList [pi .. 10]


-- --

t1' = do
  -- v <- vecCreateMPIFromVector comm lv v0
  v <- vecCreateMPIFromVectorDecideLocalSize comm v0s
  vecViewStdout v
  vecDestroy v
   where
     comm = commWorld

t1 = withPetsc0 t1'

--

-- toVec = vecCreateMPIFromVectorDecideLocalSize commWorld

vDot v1 v2 = unsafePerformIO $ vecDot v1 v2

-- -- using Managed
vecm :: IO Vec -> Managed Vec
vecm vc = managed (withVec vc)

-- matm :: IO Mat -> Managed Mat
-- matm mc = managed (withMat mc)

-- kspm :: IO KSP -> Managed KSP
-- kspm kc = managed (withKsp_ kc)


t2' v = runManaged $ do
  v1 <- vecm $ do
    x <- vecCreateMPIFromVectorDecideLocalSize comm v
    vecViewStdout x
    return x
  vecm $ vecCopyDuplicate v1
    where comm = commWorld

t2 = withPetsc0 $ t2' v0s


-- kspManage :: Comm -> KspType_ -> Mat -> Mat -> Bool -> Vec -> Vec -> Managed KSP
-- kspManage comm kt a p ignz rhs soln =
--   managed $ withKspSetupSolve comm kt a p ignz rhs soln 




-- t2' n = runManaged $ do
--   vrhs <- vecManage cs n 
--   vsoln <- vecManage cs n 
--   amat <- matManage cs
--   k <- kspManage cs KspChebyshev amat amat True vrhs vsoln
--    where
--      cs = commSelf




-- --


-- vecCreateMPIFromVectorDecideLocalSize :: Comm -> V.Vector PetscScalar_ -> IO Vec

t3' = do
  v <- vecCreateMPIFromVectorDecideLocalSize cs v0s
  vecViewStdout v
  let x = modifyV' v (VS.map (+1))
  print x       -- NB : `print x` cannot occur after `vecDestroy`
  vecViewStdout v
  vecDestroy v
    where cs = commSelf
          
t3 = withPetsc0 t3'

-- --

-- wsv2 :: Show a =>
--         (Comm, V.Vector PetscScalar_) -> (PVector PetscScalar_ -> IO a) -> IO ()
-- wsv2 c f = runResourceT $ do
--   x <- withScalarVector f c
--   lift $ print x

-- withPVectorConfig ::
--   (MonadBaseControl IO m, MonadThrow m, MonadIO m) =>
--   (Comm, V.Vector PetscScalar_) -> (PVector PetscScalar_ -> m a) -> m a
withPVectorConfig config f =
  runResourceT $ withScalarVector f config

t4' :: IO ()
t4' = withPVectorConfig (commWorld, v0s) $ \(PVector v vecdata) -> do
  x <- vecGetVector v
  print x
  print vecdata
  vecRestoreVector v (VS.map (/2) x)
  vecViewStdout v


t4 = withPetsc0 t4'



-- --

vix, viy :: V.Vector Int
va :: V.Vector PetscScalar_
vix = V.fromList [0,1,2,0,1,2]
viy = V.fromList [0,0,0,1,1,1]
va = V.fromList [0 .. 8]

t5''' =
  withMatSetupSetValuesAssembly
    (matCreateSeqAIJConstNZPR commWorld 3 3 3) 3 3 vix viy va InsertValues $ \m ->
       matViewStdout m

t5 = withPetsc0 t5'''

-- t5'' =
--   withMat (matCreateMPIAIJWithVectors commWorld (3,3) (3,3)  vix viy va) $ \m -> do
--     matAssembly m
--     matViewStdout m

-- t5' =
--   withMatSetupSetValuesAssembly (matCreate commWorld) 3 3 vix viy va InsertValues $ \m -> do
--    matViewStdout m





-- --

v0_ :: V.Vector (Int, Int, PetscScalar_)
v0_ = V.zip3 vix viy va

t6' =
  withMatNew commWorld 3 3 v0_ InsertValues $ \m ->
   matViewStdout m

t6 = withPetsc0 t6'


-- -- 










-- -- --

-- vinfo n = VecInfo commWorld n n

-- vecTemplate n f = withVecMPIPipeline vi (`vecSet` pi) $ \v -> do
--   -- withVecGetVectorOverwrite v (V.map exp)    -- contents of Vec are changed 
--   -- print $ exp pi
--   -- f v
--   f
--   vecViewStdout v
--     where
--       vi = vinfo n

-- -- petsc0VecTemplate n f = withPetsc0 $ vecTemplate n f
