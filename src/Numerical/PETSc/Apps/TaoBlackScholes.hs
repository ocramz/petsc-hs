-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Apps.TaoBlackScholes
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Black-Scholes equation solver for American options with Tao
-- (see taoblackscholes.c )
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Apps.TaoBlackScholes where

import Numerical.PETSc.Internal.PutGet
import Numerical.PETSc.Internal.Types

import qualified Data.Vector as V


rate = 0.04  -- interest rate
sigma = 0.4  -- volatility
alpha = 2.0  -- elasticity
delta = 0.01 -- dividend rate
strike = 10  -- strike price
expiry = 1.0 -- expiration date, "exercise time"
mt = 10      -- # of grid points in time
ms = 150     -- "                in space
es = 100     -- end point of spatial discretization

ds, dt :: Double
ds = (fromIntegral es) / fromIntegral (ms - 1)
dt = expiry / fromIntegral mt

sval :: Integral a => a -> a -> Double
sval i g = ds*fromIntegral (g + i)

main = withDmda1d comm DmBNone (ms+1) 1 1 [] $ \dm -> do
  (xs, xm) <- dmdaGetCorners1d dm
  (gxs, gxm) <- dmdaGetGhostCorners1d dm
  withDmCreateGlobalVector dm $ \v -> do
    let
      vt1 = V.generate gxm
            (\i -> case gxs+gxm of ms -> 0
                                   _ -> max (strike - sval i gxs) 0)
      c = V.generate gxm (\i -> (delta - rate) * (sval i gxs))
      d = V.generate gxm (\i -> -0.5 * (sigma**2) * (sval i gxs) **alpha)
      vinfo = VecInfo comm ms ms -- NB : global size == local size
      
    withVecMPIPipeline vinfo (`vecSet` 0.0) $ \x ->
     withVecDuplicate x $ \xcopy ->
      withDmCreateMatrix dm $ \jac ->
        withTaoSetup comm TaoSsils x
         (\tao -> do
             taoSetConstraintsRoutine tao xcopy formConstraints
             taoSetJacobianRoutine tao jac jac formJacobian
             taoSetVariableBoundsRoutine tao computeVarBounds
             -- withVecGetArray
         )
         (\tao -> return ()) -- solve must be done in here
      -- return ()
  where comm = commWorld

varBounds tao xlower xupper dm = do
  let ub = 10^20
  vecSet xupper ub
  (xs, xm) <- dmdaGetCorners1d dm
  -- withVecGetArraySafe xlower $ \xla -> return $ map (\i -> sval)
  return 0

computeVarBounds tao xlower xupper = do
  let ub = 10^20
  vecSet xupper ub
  -- withDmdaGetCorners1d dm 
  return 0

formConstraints tao x = undefined -- do
  -- return 0

formJacobian tao x jac tjpre = 
  return 0


