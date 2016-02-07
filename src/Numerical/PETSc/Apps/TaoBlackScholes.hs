-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Apps.TaoBlackScholes
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Black-Scholes solver for American options with Tao
-- (see src/tao/complementarity/examples/tutorials/blackscholes.c in the PETSc source folder )
--
{-  Notes :  
    American Put Options Pricing using the Black-Scholes Equation

   Background (European Options):
     The standard European option is a contract where the holder has the right
     to either buy (call option) or sell (put option) an underlying asset at
     a designated future time and price.

     The classic Black-Scholes model begins with an assumption that the
     price of the underlying asset behaves as a lognormal random walk.
     Using this assumption and a no-arbitrage argument, the following
     linear parabolic partial differential equation for the value of the
     option results:

       dV/dt + 0.5(sigma**2)(S**alpha)(d2V/dS2) + (r - D)S(dV/dS) - rV = 0.

     Here, sigma is the volatility of the underling asset, alpha is a
     measure of elasticity (typically two), D measures the dividend payments
     on the underling asset, and r is the interest rate.

     To completely specify the problem, we need to impose some boundary
     conditions.  These are as follows:

       V(S, T) = max(E - S, 0)
       V(0, t) = E for all 0 <= t <= T
       V(s, t) = 0 for all 0 <= t <= T and s->infinity

     where T is the exercise time time and E the strike price (price paid
     for the contract).

     An explicit formula for the value of an European option can be
     found.  See the references for examples.

   Background (American Options):
     The American option is similar to its European counterpart.  The
     difference is that the holder of the American option can excercise
     their right to buy or sell the asset at any time prior to the
     expiration.  This additional ability introduce a free boundary into
     the Black-Scholes equation which can be modeled as a linear
     complementarity problem.

       0 <= -(dV/dt + 0.5(sigma**2)(S**alpha)(d2V/dS2) + (r - D)S(dV/dS) - rV)
         complements
       V(S,T) >= max(E-S,0)

     where the variables are the same as before and we have the same boundary
     conditions.

     There is not explicit formula for calculating the value of an American
     option.  Therefore, we discretize the above problem and solve the
     resulting linear complementarity problem.

     We will use backward differences for the time variables and central
     differences for the space variables.  Crank-Nicholson averaging will
     also be used in the discretization.  The algorithm used by the code
     solves for V(S,t) for a fixed t and then uses this value in the
     calculation of V(S,t - dt).  The method stops when V(S,0) has been
     found.

   References:
     Huang and Pang, "Options Pricing and Linear Complementarity,"
       Journal of Computational Finance, volume 2, number 3, 1998.
     Wilmott, "Derivatives: The Theory and Practice of Financial Engineering,"
       John Wiley and Sons, New York, 1998.
-}

-----------------------------------------------------------------------------
module Numerical.PETSc.Apps.TaoBlackScholes where

import Numerical.PETSc.Internal.PutGet
import Numerical.PETSc.Internal.Types

import qualified Data.Vector as V

rate, sigma, alpha, delta, strike, expiry :: Double
rate = 0.04  -- interest rate
sigma = 0.4  -- volatility
alpha = 2.0  -- elasticity
delta = 0.01 -- dividend rate
strike = 10  -- strike price
expiry = 1.0 -- expiration date, "exercise time"

-- discretization options
mt, ms, es :: Int
mt = 10      -- # of grid points in time
ms = 150     -- "                in space
es = 100     -- end point of spatial discretization

ds, dt :: Double
ds = fromIntegral es / fromIntegral ms
dt = expiry / fromIntegral mt

sval :: Integral a => a -> a -> Double
sval i g = ds*fromIntegral (g + i)

ms' :: Int
ms' = ms + 1

main = withDmda1d comm DmBNone ms' 1 1 [] $ \dm -> do
  (xs, xm) <- dmdaGetCorners1d dm
  (gxs, gxm) <- dmdaGetGhostCorners1d dm
  let
    uservt1_ = V.generate gxm
               (\i -> if i==gxm-1 && gxs+gxm==ms' then 0
                      else max (strike - sval i gxs) 0)
  withDmCreateGlobalVector dm $ \x_ -> do    -- line 183
    
    -- | initialize x_ with values from uservt1_ (lines 230-233)
    
    let
      userc_ = V.generate gxm (\i -> (delta - rate) * (sval i gxs))
      userd_ = V.generate gxm (\i -> -0.5 * (sigma**2) * (sval i gxs) ** alpha)

    withVecDuplicate x_ $ \c_ ->
      withDmCreateMatrix dm $ \jac ->
       withTaoCreateInit comm TaoSsils x_
         (\tao -> do
             taoSetConstraintsRoutine tao c_ formConstraints
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


