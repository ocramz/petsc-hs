{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.SNES
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
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

import Numerical.PETSc.Internal.Internal

import Foreign
import Foreign.C.Types

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Exception

import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST) -- for HMatrix bits

import qualified Data.Vector.Storable as V 




snesCreate :: Comm -> IO SNES
snesCreate comm = chk1 (snesCreate' comm)

snesDestroy :: SNES -> IO ()
snesDestroy snes = chk0 (snesDestroy' snes)

snesSetType :: SNES -> SnesType_ -> IO ()
snesSetType snes st = chk0 $ snesSetType' snes st

withSnes :: Comm -> (SNES -> IO a) -> IO a
withSnes comm = bracket (snesCreate comm) snesDestroy

-- | snesSetFunction, snesSetJacobian : 
--   Newton-like methods typically solve linear systems of the form
--      f'(x) x = -f(x)
--   where f'(x) denotes the Jacobian matrix and f(x) is the function.

snesSetFunction ::
  SNES ->
  Vec ->        -- r : storage for function value
    (SNES ->       
     Vec ->        -- vector at which to compute residual
     IO CInt) -> 
  IO ()
snesSetFunction snes r f = chk0 $ snesSetFunction' snes r f


snesSetFunctionVector ::
  SNES ->
  Vec ->        -- r : storage for function value
    (V.Vector PetscScalar_ ->       -- NB pure function, SNES not used 
     V.Vector PetscScalar_ ) ->
  IO ()
snesSetFunctionVector s r f = chk0 $ snesSetFunction' s r f'
  where f' = liftVectorF f


-- liftF1 ::
--   (Functor m, Monad m) =>
--   (a -> b ) ->          -- pure unary f
--   (d -> m a) ->         -- getter
--   (d -> b -> m e) ->    -- setter
--   d ->
--   m CInt
-- liftF1 f getter setter b = do
--   y <- f <$> getter b
--   setter b y
--   return (0 :: CInt)





 
liftVectorF ::
  (V.Vector PetscScalar_ -> V.Vector PetscScalar_) -> s -> Vec -> IO CInt
liftVectorF f s vec = do
  v <- f <$> vecGetVector vec
  vecRestoreVector vec v
  return (0 :: CInt)
  


snesSetJacobian ::
  SNES ->
  Mat ->        -- amat : storage for approximate Jacobian
  Mat ->        -- pmat : storage for preconditioner (usually == amat)
    (SNES ->       
     Vec ->        -- vector at which to compute Jacobian
     Mat ->        
     Mat ->
     IO CInt) ->
  IO ()
snesSetJacobian snes amat pmat f = chk0 $ snesSetJacobian_' snes amat pmat f

snesSetUp :: SNES -> IO ()
snesSetUp snes = chk0 $ snesSetUp' snes

snesSolve ::
  SNES ->
  Vec ->   -- r.h.s
  Vec ->   -- solution (WILL BE OVERWRITTEN)
  IO ()
snesSolve snes rhsv solnv = chk0 $ snesSolve' snes rhsv solnv

snesGetSolution :: SNES -> IO Vec
snesGetSolution snes = chk1 $ snesGetSolution' snes



snesSetJacobianComputeDefaultColor ::
  SNES -> Mat -> Mat -> MatFDColoring -> IO ()
snesSetJacobianComputeDefaultColor snes amat pmat fdcol =
  chk0 $ snesSetJacobianComputeDefaultColor' snes amat pmat fdcol





-- -- -- junk

-- snesFHs snes r f = snesSetFunction snes r -- (cInt0Wrap r f)

-- cInt0Wrap r f = do
--   r >>= f
--   return (0 :: CInt)
  
-- ioFunction :: IO a -> (a -> IO CInt) -> IO ()
-- ioFunction  m f = m >>= chk0 . f
