{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
-- {-# GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Optimization.Class
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Classes for optimization
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Optimization.Class where

import Control.Monad.Writer
import Text.Printf


-- | objective function, most general case
-- NB : `x` should have additional structure

class Optimize x where
  objective :: x -> Double


-- class Optimize x => ObjGradient x where
--   objGrad :: (x -> Double) -> x -> Double

-- -- --
  

-- | from IHaskell conjugate gradient notebook

  
-- class Monad m => GradientDescent m a where
--   -- Type to represent the parameter space.
--   data Params a :: *
  
--   -- Compute the gradient at a location in parameter space.
--   grad :: a -> Params a -> m (Params a)
  
--   -- Move in parameter space.
--   paramMove :: Double        -- Scaling factor.
--             -> Params a      -- Direction vector.
--             -> Params a      -- Original location.
--             -> m (Params a)  -- New location.


class VectorSpace v where
  -- Add two vectors in this inner product space.
  add :: v -> v -> v
  
  -- Scale a vector.
  scale :: Double -> v -> v
  
  -- Take the inner product of two vectors.
  dot :: v -> v -> Double
  
  -- For convenience.
  minus :: v -> v -> v
  minus a b = add a (scale (-1) b)



class (Monad m, VectorSpace (Params a)) => GradientDescent m a where
  -- Type to represent the parameter space.
  data Params a :: *
  
  -- Compute the gradient at a location in parameter space.
  grad :: a -> Params a -> m (Params a)


-- | instances

instance VectorSpace (Params (Double -> Double -> Double)) where
  add (Arg a b) (Arg x y) = Arg (a + x) (b + y)
  dot (Arg a b) (Arg x y) = a * x + b * y
  scale s (Arg a b) = Arg (s * a) (s * b)

instance GradientDescent (Writer [String]) (Double -> Double -> Double) where
  -- The parameter for a function is just its argument.
  data Params (Double -> Double -> Double) = Arg { x :: Double, y :: Double }

  -- Use numeric differentiation for taking the gradient.
  grad f (Arg x y) = do
      let dx = f x y - f (x - epsilon) y
          dy = f x y - f x (y - epsilon)
          gradient = (dx / epsilon, dy / epsilon)
      tell [ "Gradient at\t" ++ show' (x, y) ++ "\tis\t" ++ show' gradient ]
      return $ uncurry Arg gradient
    where epsilon = 0.0001
          show' (x, y) = printf "%.5f, \t%.5f  " x y
