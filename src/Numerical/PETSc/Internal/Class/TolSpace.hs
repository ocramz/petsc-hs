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

import Foreign.C.Types (CFloat, CDouble)


class TolSpace s where
    (~~) :: s -> s -> Bool
    infix 4 ~~



-- {-
-- -- >>> nearZero (1e-11 :: Double)
-- -- False
-- --
-- -- >>> nearZero (1e-17 :: Double)
-- -- True
-- --
-- -- >>> nearZero (1e-5 :: Float)
-- -- False
-- --
-- -- >>> nearZero (1e-7 :: Float)
-- -- True
-- -}
-- class Num a => TolSpaceEpsilon a where
--   -- | Determine if a quantity is near zero.
--   nearZero :: a -> Bool

-- -- | @'abs' a '<=' 1e-6@
-- instance TolSpaceEpsilon Float where
--   nearZero a = abs a <= 1e-6

-- -- | @'abs' a '<=' 1e-12@
-- instance TolSpaceEpsilon Double where
--   nearZero a = abs a <= 1e-12

-- -- | @'abs' a '<=' 1e-6@
-- instance TolSpaceEpsilon CFloat where
--   nearZero a = abs a <= 1e-6

-- -- | @'abs' a '<=' 1e-12@
-- instance TolSpaceEpsilon CDouble where
--   nearZero a = abs a <= 1e-12
