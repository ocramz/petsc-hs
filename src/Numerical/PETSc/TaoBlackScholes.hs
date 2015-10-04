module Numerical.PETSc.TaoBlackScholes where

import Numerical.PETSc.Test2

import Foreign.C.Types
import Control.Monad
import Control.Concurrent
import Control.Applicative
import Control.Exception
import Foreign

import qualified Data.Vector as V -- hiding (toList)
import Data.Vector.Storable as VS  (toList)
import Data.Vector.Primitive.Mutable as VM

import Data.Array.Repa                  as R hiding ((++), map)
import qualified Data.Array.Repa as R
import Data.Array.Repa.Unsafe           as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RFP

rate = 0.04  -- interest rate
sigma = 0.4  -- volatility
alpha = 2.0  -- elasticity
delta = 0.01 -- dividend rate
strike = 10  -- strike price
expiry = 1.0 -- expiration date, "exercise time"
mt = 10      -- # of grid points in time
ms = 150     -- "                in space
es = 100     -- end point of spatial discretization

ds = es / fromIntegral (ms - 1)
dt = expiry / fromIntegral mt

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
      
    withVecPipeline comm gxm (`vecSet` 0.0) $ \x ->
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


