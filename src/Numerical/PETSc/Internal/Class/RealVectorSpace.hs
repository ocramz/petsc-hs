{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Class.RealVectorSpace
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | real vector spaces, dual pairings, normed spaces
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Class.RealVectorSpace where


import qualified Data.Vector as V

{- |Instances should satisfy the following laws:

> forall x,y. x .+. y = y .+. x -- additive commutativity
> forall x,y,z. x .+. (y .+. z) = (x .+. y) .+. z -- additive associativity
> forall x. vzero .+. x = x -- additive identity
> forall x. x .-. x = vzero -- additive inverse
> forall x,y. x .-. y = x .+. (-1 *. y) -- subtraction is addition
> forall x. 1 *. x = x -- multiplicative identity
> forall a,x. (x .* a) ./ a = x -- multiplicative scalar inverse
> forall r,x. r *. x = x .* r -- scalar multiplication commutes
> forall r,s,x. r *. (s *. x) = (r * s) *. x -- multiplicative associativity 
> forall r,s,x. (r + s) *. x = r *.x + s *.x -- scalar distributivity
> forall r,x,y. r *. (x .+. y) = r *.x + r *.y -- vector distributivity
-}
class RealVectorSpace v where
    (.+.) :: v -> v -> v
    (.-.) :: v -> v -> v
    infixl 6 .+.
    infixl 6 .-.
    (*.) :: Double -> v -> v
    (.*) :: v -> Double -> v
    (./) :: v -> Double -> v
    -- (*.) :: Num a => a -> v -> v
    -- (.*) :: Num a => v -> a -> v
    -- (./) :: Fractional a => v -> a -> v
    infixl 7 *.
    infixl 7 .*
    infixl 7 ./
    vzero :: v
    vnegate :: v -> v
    -- |Default methods
    (.*) = flip (*.)
    (*.) = flip (.*)
    x ./ a = x .* (1/a)
    x .-. y = x .+. vnegate y
    vnegate = ((-1) *.)






{- |We also encode the notion of a dual vector space, which is a space
associated to every real vector space, and consists of all linear
functionals on v.  Note that the dual space is also a vector space.
Instances should satisfy the following laws:

> forall m,x,y. m `pairing` (x .+. y) = m `pairing` x + m `pairing` y -- linearity in v
> forall m,n,x. (m .+. n) `pairing` x = m `pairing` x + n `pairing` x -- linearity in d
> forall a,m,x. m `pairing` (a *. x) = a * (m `pairing` x) -- linearity
> forall a,m,x. (a *. m) `pairing` x = a * (m `pairing` x) -- linearity
> forall m. m `pairing` vzero = 0 -- zero
> forall x. vzero `pairing` x = 0 -- zero
-}


class (RealVectorSpace d, RealVectorSpace v) => DualPair d v where
    pairing :: d -> v -> Double




{- |An inner product space is a vector space in which an inner (dot)
product between vectors exists.
Instances should satisfy the following laws:

> forall x,y. innerProd x y = innerProd y x -- symmetry
> forall a,x,y. innerProd (a *. x) y = a * (innerProd x y) -- linearity
> forall x,y,z. innerProd (x .+. y) z = (innerProd x z) + (innerProd y z) -- linearity
> forall x. innerProd x x = normSquared x -- compatibility
-}
-- class (RealNormedSpace a) => RealInnerProductSpace a where
--     innerProd :: a -> a -> Double

class (RealVectorSpace a) => RealInnerProductSpace a where
    innerProd :: a -> a -> Double    

(<.>) :: RealInnerProductSpace v => v -> v -> Double
(<.>) = innerProd


{- |A normed vector space also includes a 1-homogeneous norm function
satisfying the triangle inequality.
Instances should satisfy the following laws:

> forall x. norm x > 0 -- non-negativity
> forall a,x. norm (a *. x) = (abs a) * (norm x) -- 1-homogeneity
> forall x,y. norm (x .+. y) <= (norm x) + (norm y) -- triangle inequality

In addition, the norm and normSquared functions should be compatible:

> forall x. normSquared x = (norm x) * (norm x)

Minimal definition: one of `norm` or `normSquared`
-}

class (RealInnerProductSpace a) => RealNormedSpace a where
    norm :: a -> Double
    -- normSquared :: a -> Double
    -- -- |Default methods.
    -- norm = sqrt . normSquared
    -- normSquared x = let n = norm x in n * n






-- | testing testing



instance RealVectorSpace (V.Vector Double) where
  (.+.) = V.zipWith (+)
  vzero = V.singleton 0
  (*.) a = V.map (* a)

instance RealInnerProductSpace (V.Vector Double) where
  innerProd a b = V.sum $ V.zipWith (*) a b

instance RealNormedSpace (V.Vector Double) where
  norm a = sqrt $ innerProd a a


v0 = V.fromList [1..10]
v1 = V.fromList [3, pi .. 10]

v2 :: V.Vector Double
v2 = v1 .+. v0

v3 = pi *. v2



nv3 = norm v3

--
