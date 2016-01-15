{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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


-- | objective function, most general case
-- NB : `x` should have additional structure

class Optimize x where
  objective :: x -> Float


-- class Optimize x => ObjGradient x where
--   objGrad :: (x -> Float) -> x -> Float

