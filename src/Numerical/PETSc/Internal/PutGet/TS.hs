{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.TS
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
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
tsCreate cc = chk1 $ tsCreate' cc

tsDestroy :: TS -> IO ()
tsDestroy ts = chk0 $ tsDestroy' ts

withTs :: IO TS -> (TS -> IO a) -> IO a
withTs tsc = bracket tsc tsDestroy

tsSetProblemType :: TS -> TsProblemType -> IO ()
tsSetProblemType ts ty = chk0 $ tsSetProblemType' ts ty

tsSetInitialTimeStep ::
  TS ->
  PetscReal_ -> -- initial time
  PetscReal_ -> -- initial timestep
  IO ()
tsSetInitialTimeStep ts it dt = chk0 $ tsSetInitialTimeStep' ts it dt


tsSetExactFinalTime :: TS -> TsExactFinalTimeOption_ -> IO ()
tsSetExactFinalTime ts ft = chk0 (tsSetExactFinalTime' ts ft)

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


tsSolve :: TS -> IO ()
tsSolve ts = chk0 $ tsSolve_' ts


tsStep :: TS -> IO ()
tsStep ts = chk0 (tsStep' ts)


-- -- tsSolveWithInitialCondition :: TS -> Vec -> IO ()
-- tsSolveWithInitialCondition ts isolnv fto = do
--   tsSetSolution ts isolnv
--   tsSetExactFinalTime ts fto   
--   tsSolve ts

withTsSolveWithInitialCondition ::
  TS ->
  Vec ->                      -- initial condition
  TsExactFinalTimeOption_ ->  -- what to do if final time is exceeded
  (TS -> IO a) ->             -- " before setting initial condition
  (TS -> IO b) -> IO b        -- " after solving TS
withTsSolveWithInitialCondition ts v fto before =
  withTsSolve ts fto f0
  where
    f0 ts0 = do
      before ts0
      tsSetSolution ts0 v   -- set initial condition vector `v`


withTsSolve ::
  TS ->
  TsExactFinalTimeOption_ ->
  (TS -> IO a) ->
  (TS -> IO b) ->
  IO b
withTsSolve ts fto before after = do
  before ts
  tsSetExactFinalTime ts fto   -- required as of PETSc 3.7
  tsSolve ts
  after ts




tsSetDm :: TS -> DM -> IO ()
tsSetDm ts dm = chk0 (tsSetDm' ts dm)







-- | 

-- | F(t, u, du/dt)
tsSetIFunction_ ts res f = chk0 (tsSetIFunction0' ts res f)

tsSetIFunction ::
  TS ->
  Vec ->
     (TS ->          
      PetscReal_ ->   -- time
      Vec ->          -- current state : u(tn)
      Vec ->          -- du/dt
      Vec ->          -- updated state : u(tn+1)
      IO CInt) ->
  IO ()
tsSetIFunction ts res f = tsSetIFunction_ ts res g where
  g t r a b c _ = f t r a b c



-- | dF/du = del F / del u_dot + del F / del u

tsSetIJacobian_ ts amat pmat f =
  chk0 (tsSetIJacobian0' ts amat pmat f)

tsSetIJacobian ::
  TS ->
  Mat ->
  Mat ->
      (TS ->
       PetscReal_ ->    -- time
       Vec ->           -- u(tn)
       Vec ->           -- du/dt(tn)
       PetscReal_ ->    -- shift = d(u_dot)/dt
       Mat ->           -- system mtx
       Mat ->           -- preconditioner (can be == amat)
       IO CInt) ->
  IO ()
tsSetIJacobian ts amat pmat fun = tsSetIJacobian_ ts amat pmat fun' where
  fun' t a b c d e f _ = fun t a b c d e f




-- | G(t, u)

tsSetRHSFunction_ ts r f = chk0 (tsSetRHSFunction0' ts r f)

tsSetRHSFunction ::
  TS ->
  Vec ->
      (TS ->          
       PetscReal_ ->  -- time
       Vec ->         -- u(tn) 
       Vec ->         -- u(tn+1)
       IO CInt) ->
  IO ()
tsSetRHSFunction ts r f = tsSetRHSFunction_ ts r g where
  g t a b c _ = f t a b c
  
tsSetRHSJacobian_ ts amat pmat f = chk0 (tsSetRHSJacobian0' ts amat pmat f)

-- | gG/du

tsSetRHSJacobian ::
  TS ->
  Mat ->
  Mat ->
      (TS ->          
       PetscReal_ ->   -- time
       Vec ->          -- u(tn)
       Mat ->
       Mat ->
       IO CInt) ->
  IO ()
tsSetRHSJacobian ts amat pmat f = tsSetRHSJacobian_ ts amat pmat g where
  g t a b c d _ = f t a b c d



-- | adjoint TS solve

tsSetSaveTrajectory :: TS -> IO ()
tsSetSaveTrajectory ts = chk0 $ tsSetSaveTrajectory' ts

tsTrajectoryCreate :: Comm -> IO TSTrajectory
tsTrajectoryCreate cc = chk1 (tsTrajectoryCreate' cc)

tsTrajectoryDestroy :: TSTrajectory -> IO ()
tsTrajectoryDestroy ts = chk0 (tsTrajectoryDestroy' ts)


-- | gradients of Bolza cost functional Psi at _final_ time tF wrt state `y` and parameter `p`
tsSetCostGradients ::
  TS ->
  Int ->          -- # of cost functionals
  [Vec] ->        -- dPsi/dy(tF)       
  [Vec] ->        -- dPsi/dp(tF)
  IO ()
tsSetCostGradients ts numcost lambda_ mu_ =
  withArray lambda_ $ \lp ->
  withArray mu_ $ \mp ->
   chk0 $ tsSetCostGradients' ts n lp mp where
     n = toCInt numcost

tsAdjointSetRHSJacobian ::
  TS ->
  Mat ->
  (TS -> PetscReal_ -> Vec -> Mat -> IO CInt) ->
  IO ()
tsAdjointSetRHSJacobian ts amat f  =
  chk0 $ tsAdjointSetRHSJacobian0' ts amat g where
    g a b c d _ = f a b c d




-- | if nonzero integrand in Bolza cost functional `r(t, y, p)`:

tsSetCostIntegrand_ ts ncostf rf drdyf drdpf =
  chk0 (tsSetCostIntegrand0' ts ncostf rf drdyf drdpf)
  
tsSetCostIntegrand ::
  TS ->
  PetscInt_ ->
  (TS -> PetscReal_ -> Vec -> Vec -> IO CInt) ->      -- value of integrand `r`
  (TS -> PetscReal_ -> Vec -> Ptr Vec -> IO CInt) ->  -- dr/dy
  (TS -> PetscReal_ -> Vec -> Ptr Vec -> IO CInt) ->  -- dr/dp
  IO ()
tsSetCostIntegrand ts n rf drdyf drdpf = tsSetCostIntegrand_ ts n fa fb fc where
  fa a b c d _ = rf a b c d
  fb a b c d _ = drdyf a b c d
  fc a b c d _ = drdpf a b c d





-- | output integral cost at each time step

tsGetCostIntegral :: TS -> IO Vec
tsGetCostIntegral ts = chk1 (tsGetCostIntegral' ts)


-- | solve

tsAdjointSolve :: TS -> IO ()
tsAdjointSolve ts = chk0 (tsAdjointSolve' ts)




tsGetConvergedReason :: TS -> IO TsConvergedReason_
tsGetConvergedReason ksp = do
  r <- chk1 (tsGetConvergedReason' ksp)
  return $ tsConvergedIntToReason r
