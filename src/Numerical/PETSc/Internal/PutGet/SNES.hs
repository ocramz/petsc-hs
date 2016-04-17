{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes, FlexibleContexts #-}
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

withSnes :: IO SNES -> (SNES -> IO a) -> IO a
withSnes cf = bracket cf snesDestroy




-- snesCreateSetup c v amat pmat f post = withSnes c $ \s -> do 
--   snesSetFunction s v f
--   snesSetJacobian s amat pmat f
--   post s






-- | SNES function adapters:
-- | a callback that dereferences, mutates and copies back data into one of its arguments  


-- a SNES function (either f or grad f) modifies one of its arguments (same for KSP, TS, Tao, .. functions)

-- snesFunctionAdapter vIn vOut fv = do --- | what type for this contraption?
--   let ix = V.fromList [0 .. n-1]
--       n = vecSize vIn
--   x <- vecGetVector vIn
--   let y = fv x
--       ixy = V.zip ix y
--   vecSetValuesUnsafeVector1A vOut ixy InsertValues

-- snesFunctionAdapt ::
--   Vec ->
--   Vec ->                 -- SECOND Vec WILL BE MODIFIED
--   (VM.IOVector PetscScalar_ -> VM.IOVector PetscScalar_) ->
--   IO CInt
-- snesFunctionAdapt = petscVecVecFunctionAdapt





-- | `with` brackets

-- withSnesCreateSetup ::
--   Comm ->
--   Vec ->
--   Mat ->
--   Mat ->
--   (SNES -> Vec -> Vec -> IO a) ->
--   (SNES -> Vec -> Mat -> Mat -> IO b) ->
--   (SNES -> IO c) ->
--   IO c
-- withSnesCreateSetup comm v amat pmat f fj =
--   withSnes (snesCreateSetup comm v amat pmat f fj)












-- | snesSetFunction, snesSetJacobian : 
--   Newton-like methods typically solve linear systems of the form
--      f'(x) x = -f(x)
--   where f'(x) denotes the Jacobian matrix and f(x) is the function.



snesSetFunction :: (VG.Vector v PetscScalar_, VG.Vector w PetscScalar_) =>
     SNES ->
     Vec ->
     (w PetscScalar_ -> v PetscScalar_) ->
     IO ()
snesSetFunction snes r f = chk0 $ snesSetFunction_' snes r g
  where
   g _snes x y _p = do
     withVecVector x $ \xv -> do
       _ <- vecGetVector y
       vecRestoreVector y (V.convert $ f xv)   -- `y` is overwritten, as per spec
     return (0 :: CInt)



-- fIO :: CDouble -> C.Nag_Integer -> Ptr CDouble -> Ptr CDouble -> Ptr C.Nag_Comm -> IO ()
--     fIO t n y _yp  _comm = do
--       yFore <- newForeignPtr_ y
--       let yVec = VM.unsafeFromForeignPtr0 yFore $ fromIntegral n
--       ypImm <- f t <$> V.unsafeFreeze yVec
--       V.copy yVec ypImm


-- NB : modifies second Vec argument
snesComputeFunction :: SNES -> Vec -> Vec -> IO ()
snesComputeFunction snes x y = chk0 $ snesComputeFunction' snes x y






-- snesSetJacobian :: -- (VG.Vector w PetscScalar_, VG.Vector v PetscScalar_) =>
--   SNES ->
--   Mat ->        -- amat : storage for approximate Jacobian
--   Mat ->        -- pmat : storage for preconditioner (usually == amat)
--   -- (V.Vector PetscScalar_ -> V.Vector PetscScalar_) ->
--   (V.Vector Double -> V.Vector Double) ->
--     -- (SNES ->       
--     --  Vec ->        -- vector at which to compute Jacobian
--     --  Mat ->        
--     --  Mat ->
--     --  IO a) ->
--   IO ()
snesSetJacobian snes amat pmat f = chk0 $ snesSetJacobian_' snes amat pmat gj
  where
    gj _snes x jac jacp = do
      -- withVecVector x $ \xv -> do
      xv <- vecCopyVector x
      let (m, n) = matSize jac
          -- vvJac :: V.Vector (Int, Int, PetscScalar_)
          vvJac  = vvToCSR (AD.jacobian f xv)
      withMatSetValueVectorSafe jac m n vvJac InsertValues return
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
      



-- snesSetFunctionVector ::
--   SNES ->
--   Vec ->        -- r : storage for function value
--     (V.Vector PetscScalar_ ->       -- NB pure function, SNES not used 
--      V.Vector PetscScalar_ ) ->
--   IO ()
-- snesSetFunctionVector s r f = chk0 $ snesSetFunction' s r f'
--   where f' = liftVectorF f






-- -- liftVectorF ::
-- --   (V.Vector PetscScalar_ -> V.Vector PetscScalar_) -> s -> Vec -> IO CInt
-- modifyVectorWithF f vec = do
--   v <- f <$> vecGetVector vec
--   vecRestoreVector vec v
--   -- return (0 :: CInt)













-- | setup, solve SNES

snesSetUp :: SNES -> IO ()
snesSetUp snes = chk0 $ snesSetUp' snes

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





-- -- -- junk

-- snesFHs snes r f = snesSetFunction snes r -- (cInt0Wrap r f)

-- cInt0Wrap r f = do
--   r >>= f
--   return (0 :: CInt)
  
-- ioFunction :: IO a -> (a -> IO CInt) -> IO ()
-- ioFunction  m f = m >>= chk0 . f
