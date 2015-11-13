-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Class.TolSpace
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Space for points that can be compared (from `diffgeom`)
-- Instances should satisfy the following laws:
-- > p ~~ p = True  (Reflexivity)
-- > p ~~ q = q ~~ p (Symmetry)
-- This means that 'TolSpace' represents a "tolerance relation".  As such, a
-- common instance is some metric space in which points "close enough"
-- together return True under ~~.
-- The method ~~ is nearly an equivalence relation like ==, but is "fuzzy" in
-- the sense that we relax the Transitivity constraint.
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Class.TolSpace where

-- import Data.Functor


class TolSpace s where
    (~~) :: s -> s -> Bool
    infix 4 ~~
