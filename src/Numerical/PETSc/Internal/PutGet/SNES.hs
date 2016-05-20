{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes, FlexibleContexts, TypeSynonymInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.SNES
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | SNES Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.SNES where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import qualified Numerical.PETSc.Internal.PutGet.PetscMisc as PM
import qualified Numerical.PETSc.Internal.PutGet.Viewer as Viewer
import Numerical.PETSc.Internal.Utils

import Numerical.PETSc.Internal.PutGet.Vec
import Numerical.PETSc.Internal.PutGet.Mat

import Numerical.PETSc.Internal.Sparse

import Foreign
import Foreign.C.Types

-- import System.IO.Unsafe (unsafePerformIO)

import Data.Functor 

import Control.Monad
import Control.Monad.IO.Class (liftIO)

-- import Control.Applicative
-- import Control.Arrow
-- import Control.Concurrent
import Control.Exception

-- import Control.Monad.ST (ST, runST)
-- import Control.Monad.ST.Unsafe (unsafeIOToST) -- for HMatrix bits


import qualified Foreign.ForeignPtr as FPR

-- Vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS 
import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Generic as VG

-- ad
import qualified Numeric.AD as AD
import qualified Numeric.AD.Internal.Reverse as AD




snesCreate :: Comm -> IO SNES
snesCreate c = chk1 (snesCreate' c)

snesDestroy :: SNES -> IO ()
snesDestroy snes = chk0 (snesDestroy' snes)

snesSetType :: SNES -> SnesType_ -> IO ()
snesSetType snes st = chk0 $ snesSetType' snes st








-- | SNES function adapters:
-- | a callback that dereferences, mutates and copies back data into one of its arguments  


-- a SNES function (either f or grad f) modifies one of its arguments (same for KSP, TS, Tao, .. functions)








-- | `with` brackets



withSnes :: IO SNES -> (SNES -> IO a) -> IO a
withSnes cf = bracket cf snesDestroy

withSnesCreate :: Comm -> (SNES -> IO a) -> IO a
withSnesCreate c = withSnes (snesCreate c)

withSnesCreateSetup ::
  Comm ->
  (SNES -> IO a1) -> -- before SNESSetUp
  (SNES -> IO a) ->  -- after "
  IO a
withSnesCreateSetup c pre post = withSnesCreate c $ \sn -> do
  pre sn
  snesSetUp sn
  post sn

-- withSnesCreateSetupAD0 ::
--   (SNES -> IO a1) ->    -- before setting functions etc.
-- Comm ->
--   Vec ->
--   Mat ->
--   Mat ->
--   (V.Vector PetscScalar_ -> V.Vector PetscScalar_) ->
--   (SNES -> IO a) ->     -- after setup
--   IO a
-- withSnesCreateSetupAD0 pre c v amat pmat f = withSnesCreateSetup c $ \sn -> do
--   pre sn
--   snesSetFunction sn v f
--   snesSetJacobianAD sn amat pmat f

-- withSnesCreateSetupAD ::
--   Comm ->
--   Vec ->
--   Mat ->
--   Mat ->
--   (V.Vector PetscScalar_ -> V.Vector PetscScalar_) ->
--   (SNES -> IO a) ->  -- after SNESSetUp (i.e. solve, view solution ...)
--   IO a
-- withSnesCreateSetupAD = withSnesCreateSetupAD0 return

-- withSnesCreateSetupAD ::
--   Comm ->
--   Vec ->
--   Mat ->
--   Mat ->
--   (V.Vector PetscScalar_ -> V.Vector PetscScalar_) ->
--   (SNES -> IO a) ->
--   IO a
-- withSnesCreateSetupAD c v amat pmat f post = withSnesCreate c $ \s -> do 
--   snesSetFunction s v f
--   snesSetJacobianAD s amat pmat f
--   snesSetUp s
--   post s








-- | snesSetFunction, snesSetJacobian : 
--   Newton-like methods typically solve linear systems of the form
--      f'(x) x = -f(x)
--   where f'(x) denotes the Jacobian matrix and f(x) is the function.

-- | snesSetFunction : Input Parameters : 
-- snes	- the SNES context
-- r	- vector to store function value
-- f	- function evaluation routine;
-- SNESFunction(SNES snes,Vec x,Vec y,void *ctx)
-- Input Parameters :
-- snes	- the SNES context
-- x	- state at which to evaluate residual
-- ctx	- optional user-defined function context, passed in with SNESSetFunction()
-- Output Parameter :
-- y    - vector for residual (function value) 

-- | minimal interface, `y` is not overwritten
snesSetFunction ::
  SNES ->
  Vec ->
  (V.Vector PetscScalar_ -> V.Vector PetscScalar_) ->
  IO ()
snesSetFunction snes r f = chk0 $ snesSetFunction_' snes r g where
  g _snes x y _p = withVecVector x $ \xv -> do
    vecOverwriteIOVectorN2_ y (f xv)
    return (0 :: CInt)






-- snesSetFunction :: (VG.Vector v PetscScalar_, VG.Vector w PetscScalar_) =>
--      SNES ->
--      Vec ->
--      (v PetscScalar_ -> w PetscScalar_) ->
--      IO ()
-- snesSetFunction snes r f = chk0 $ snesSetFunction_' snes r g
--   where
--    g _snes x y _p = 
--      withVecVector x $ \xv -> do
--        -- withVecArrayPtr y $ \yp -> do
--         _ <- vecOverwriteIOVector_ y (V.convert $ f xv)
--         -- do
--         -- vecSetValuesRangeVector y (V.convert $ f xv) InsertValues
--         -- vecAssembly y
--         return (0 :: CInt)


-- asdfs :: Storable a => VS.Vector a -> IO (VM.IOVector a)
-- asdfs = VS.thaw

-- fIO :: CDouble -> C.Nag_Integer -> Ptr CDouble -> Ptr CDouble -> Ptr C.Nag_Comm -> IO ()
--     fIO t n y _yp  _comm = do
--       yFore <- newForeignPtr_ y
--       let yVec = VM.unsafeFromForeignPtr0 yFore $ fromIntegral n
--       ypImm <- f t <$> V.unsafeFreeze yVec
--       V.copy yVec ypImm


-- blap :: (AD.Reverse s a, Floating a) => ([a] -> a) -> [a] -> [a]
-- blap f x = AD.grad f x

-- f0 [x,y] = x**2 + exp(- y)
-- gf0 = AD.grad f0

-- gf1 :: forall a. Floating a => ([a] -> a) -> [a] -> [a]
-- gf1 = AD.grad


-- NB : modifies second Vec argument
snesComputeFunction :: SNES -> Vec -> Vec -> IO ()
snesComputeFunction snes x y = chk0 $ snesComputeFunction' snes x y




-- instance AD.Mode PetscScalar_ where
--   type Scalar PetscScalar_ = PetscScalar_
--   -- isKnownConstant _ = True
--   -- isKnownZero x = 0 == x
--   auto = id
--   -- (^/) = (/)



-- | snesSetJacobianAD : computes Jacobian of `f` via AD.jacobian
snesSetJacobianAD ::  -- (VG.Vector w PetscScalar_, VG.Vector v PetscScalar_) =>
  SNES ->
  Mat ->        -- amat : storage for approximate Jacobian
  Mat ->        -- pmat : storage for preconditioner (usually == amat)
  (V.Vector PetscScalar_ -> V.Vector PetscScalar_) ->
  -- (V.Vector Double -> V.Vector Double) -> 
    -- (SNES ->       
    --  Vec ->        -- vector at which to compute Jacobian
    --  Mat ->        
    --  Mat ->
    --  IO a) ->
  IO ()
snesSetJacobianAD snes amat pmat f = chk0 $ snesSetJacobian_' snes amat pmat gj
  where
    gj _snes x jac _jacp = 
      withVecVector x $ \xv -> do
      -- xv <- vecCopyVector x
      let (m, n) = matSize jac
          jjxv = AD.jacobian (V.map realToFrac . f . V.map realToFrac) xv -- (V.map AD.auto xv)
          -- jj :: V.Vector PetscScalar_ -> V.Vector (V.Vector PetscScalar_)
          -- jj z = AD.jacobian f (V.map AD.auto z)
          vvJac = vvToCSR jjxv
      withMatSetValueVectorSafe jac m n vvJac InsertValues return
      return (0 :: CInt)

snesSetJacobian0 ::
  SNES ->
  Mat ->
  Mat ->
  V.Vector (IdxRow, IdxCol, PetscScalar_) ->
  NumRows ->
  NumCols ->
  IO ()
snesSetJacobian0 snes amat pmat vvJac m n =
  chk0 $ snesSetJacobian_' snes amat pmat gj where
   gj _snes _x jj _jacp = do
     matSetValueVectorSafe jj (m, n) vvJac InsertValues
     matAssembly jj
     return (0 :: CInt)



{- Internal/Sparse :
vvToCSR :: V.Vector (V.Vector a) -> V.Vector (Int, Int, a)
-}
     



-- snesComputeJacobianDefault0 :: SNES -> Vec -> Mat -> Mat -> Ptr () -> IO ()
snesComputeJacobianDefault0 snes x j b ctx =
  chk0 (snesComputeJacobianDefault0' snes x j b ctx)









-- | tamper with SNES line search

snesGetLineSearch :: SNES -> IO SNESLineSearch
snesGetLineSearch snes = chk1 (snesGetLineSearch' snes)

snesSetLineSearch :: SNES -> SNESLineSearch -> IO ()
snesSetLineSearch snes ls = chk0 (snesSetLineSearch' snes ls)

snesLineSearchSetTolerances ::
  SNESLineSearch ->
  PetscReal_ ->
  PetscReal_ ->
  PetscReal_ ->
  PetscReal_ ->
  PetscReal_ ->
  Int ->
  IO ()
snesLineSearchSetTolerances ls steptol maxstep rtol atol ltol maxits =
  chk0 (snesLineSearchSetTolerances' ls steptol maxstep rtol atol ltol maxits)


-- snesLineSearchSetPostCheck ::
--   SNESLineSearch ->
--     (SNESLineSearch ->
--      Vec ->
--      Vec ->
--      Vec ->
--      Ptr PetscBool_ ->
--      Ptr PetscBool_ ->
--      Ptr () ->
--      IO CInt) ->
--     IO ()
snesLineSearchSetPostCheck sls f =
  chk0 (snesLineSearchSetPostCheck0' sls f) 




-- | linesearch steplength parameter `lambda`
  
snesLineSearchGetLambda :: SNESLineSearch -> IO PetscReal_
snesLineSearchGetLambda ls = chk1 (snesLineSearchGetLambda' ls)

snesLineSearchSetLambda :: SNESLineSearch -> PetscReal_ -> IO ()
snesLineSearchSetLambda ls lambda = chk0 (snesLineSearchSetLambda' ls lambda)
      

















-- | setup, solve SNES

snesSetUp :: SNES -> IO ()
snesSetUp snes = chk0 $ snesSetUp' snes

snesSetFromOptions :: SNES -> IO ()
snesSetFromOptions snes = chk0 $ snesSetFromOptions' snes

snesSolve ::  -- solve F(x) = b
  SNES ->
  Vec ->   -- r.h.s
  Vec ->   -- solution (WILL BE OVERWRITTEN)
  IO ()
snesSolve snes rhsv solnv = chk0 $ snesSolve' snes rhsv solnv

snesSolve0 ::  -- this version assumes b = 0
  SNES ->
  Vec ->
  IO ()
snesSolve0 snes solnv = chk0 $ snesSolve0' snes solnv

snesGetSolution :: SNES -> IO Vec
snesGetSolution snes = chk1 $ snesGetSolution' snes



snesSetJacobianComputeDefaultColor ::
  SNES -> Mat -> Mat -> MatFDColoring -> IO ()
snesSetJacobianComputeDefaultColor snes amat pmat fdcol =
  chk0 $ snesSetJacobianComputeDefaultColor' snes amat pmat fdcol



-- | variational inequalities

snesVISetVariableBounds :: SNES -> Vec -> Vec -> IO ()
snesVISetVariableBounds snes xl xu = chk0 $ snesVISetVariableBounds' snes xl xu






-- | misc

snesGetKsp :: SNES -> IO KSP
snesGetKsp s = chk1 $ snesGetKsp' s







-- | view

snesView0 :: SNES -> PetscViewer -> IO ()
snesView0 m v = chk0 (snesView' m v)

snesView :: SNES -> IO ()
snesView m = Viewer.withPetscViewerTypeFmt PM.commWorld ViewerAscii ViewFmtAsciiInfoDetail (snesView0 m)

snesViewStdout :: SNES -> IO ()
snesViewStdout = snesView




-- -- -- junk

-- snesFHs snes r f = snesSetFunction snes r -- (cInt0Wrap r f)

-- cInt0Wrap r f = do
--   r >>= f
--   return (0 :: CInt)
  
-- ioFunction :: IO a -> (a -> IO CInt) -> IO ()
-- ioFunction  m f = m >>= chk0 . f
