{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Numerical.PETSc.Raw.Algebra2 where

import qualified Data.Vector as V
import Data.Monoid

import Test.QuickCheck


-- from user jhinkle

-- | TolSpace
-- -- reflexive, symmetric
class TolSpace s where
  (~~) :: s -> s -> Bool
  infix 4 ~~










-- -- | fiber bundle
-- -- projection to base manifold : continuous, surjective

-- class (TolSpace (BaseType a)) => FiberBundle a where
--   type BaseType a
--   projBase :: a -> BaseType a


-- -- | vector bundle : a fiber bundle for which each fiber, restricted to a single point is a vector space


-- | Groups : Monoids with inverses
-- inversion: x <> invert x ~~ mempty
--            invert x <> x ~~ mempty
class (TolSpace g, Monoid g) => Group g where
  invert :: g -> g


groupPropInvLeft :: Group a => a -> Property
groupPropInvLeft g = property $ invert g <> g ~~ mempty

-- main = quickCheck groupPropInvLeft









-- | Lie algebras
-- bilinearity (associative algebra):
-- -- -- ad (a *. x .+. b *. y) z = a *. (ad x z) .+. b *. (ad y z)
-- alternating property of ad
-- -- -- ad x y = vnegate (ad y x)
-- Jacobi identity
-- -- -- ad x (ad y z) .+. ad z (ad x y) .+. ad y (ad z x) = vzero
class RealVectorSpace v => LieAlgebra v where
  ad :: v -> v -> v



-- | duality in Lie algebras
-- class ()
  











-- -- | Lie group action:
-- -- --inherits monoidal action
-- -- -- associated infinitesimal action
-- -- -- momentum map (dual of infinites. action)

-- class Monoid m => LeftAction m s where
--   leftAct :: m -> s -> s










-- | metric spaces
class TolSpace m => Metric m where
  dist :: m -> m -> Double
  distSquared :: m -> m -> Double
  -- | defaults
  dist x y = sqrt (distSquared x y)
  distSquared x y = let d = dist x y in d*d












-- | Real vector space
-- addit commutative: a + b = b + a
-- addit associative: a + (b+c) = (a+b) + c
-- addit identity: a + 0 = a
-- addit inverse: x - x = 0
-- subtraction is addition: a - b = a + (- b)
-- multipl identity: a * 1 = a
-- multipl scalar inverse: (a * x) / a = x
-- scalar mult commutes: a * x = x * a
-- multipl associative: a * (b*c) = (a*b) * c
-- scalar distributive: (a + b) * x = a * x + b * x 
-- vector distributive : a * (x + y) = a * x + a * y
class RealVectorSpace v where
  (.+.) :: v -> v -> v
  (.-.) :: v -> v -> v
  infixl 6 .+.
  infixl 6 .-.
  (*.) :: Double -> v -> v -- left scalar multipl
  (.*) :: v -> Double -> v -- right "
  (./) :: v -> Double -> v
  infixl 7 *.
  infixl 7 .*
  infixl 7 ./
  vzero :: v
  vnegate :: v -> v
  -- | defaults:
  (.*) = flip (*.)
  (*.) = flip (.*)
  x ./ a = x .* (1/a)
  x .-. y = x .+. vnegate y
  vnegate = ((-1) *. )


-- | dual space: all liner functionals on primal vector space
class (RealVectorSpace x, RealVectorSpace y) => DualPair x y where
  pairing :: x -> y -> Double










-- | norm :
-- non negative: norm x >= 0
-- 1-homogeneity : norm (a *. x) = abs a * norm x
-- triangle inequality : norm (x .+. y) <= norm x + norm y

-- class RealVectorSpace a => RealNormedSpace a where
--   norm :: a -> Double
--   normSquared :: a -> Double
--   norm = sqrt . normSquared
--   normSquared x = let u = norm x in u*u

-- normalize :: RealNormedSpace v => v -> IO v
-- normalize v | isInfinite b || isNaN b = error "!"
--             | otherwise = return $ v.* b where
--               b = 1 / norm v


-- | inner product

class RealVectorSpace a => RealInnerProductSpace a where
  innerProd :: a -> a -> Double

-- | norm
class RealInnerProductSpace a => RealNormedSpace a where
  normSquared :: a -> Double
  normSquared x = innerProd x x
  norm :: a -> Double
  norm x = sqrt (normSquared x)

normalize v | isInfinite b || isNaN b = error "normalize: NaN or Inf !!!"
            | otherwise = return (v .* b) where
               b = 1 / norm v

normedTolCompare tol u v = normSquared (u .-. v) <= tol * (nSavg + 1) where
  nSavg = 0.5 * (normSquared u + normSquared v)










-- | instances

data Vec a = Vec {unVec :: V.Vector a} deriving Eq
mkVec x= Vec (V.fromList x)

mkVecDouble x = mkVec (x :: [Double])
vecD = mkVecDouble

mkVecInt x = mkVec (x :: [Int])

fi :: Integer -> Int
fi = fromIntegral

instance Show a => Show (Vec a) where
  show x = show (unVec x)

zipWithV f a b = Vec (V.zipWith f (unVec a) (unVec b))

sumV x = V.foldr (+) 0 (unVec x)


instance Functor Vec where
  fmap f x = Vec (fmap f (unVec x))

instance RealVectorSpace (Vec Double) where
  (.+.) = zipWithV (+)
  (.-.) = zipWithV (-)
  a *. x = fmap (* a) x
  vzero = mkVec [0,0 ..]
  vnegate = fmap (* (-1))

dot a b = sumV $ zipWithV (*) a b 

instance RealInnerProductSpace (Vec Double) where
  innerProd = dot

(.*.) :: RealInnerProductSpace a => a -> a -> Double
(.*.) = innerProd
  
instance RealNormedSpace (Vec Double)

instance TolSpace (Vec Double) where
  (~~) = normedTolCompare 1e-5

instance Metric (Vec Double) where
  dist a b = norm (a .-. b)




-- | tests

v1, v2 :: Vec Double
v1 = mkVec [1,2,3]
v2 = mkVec [1,2.0001,3]

t1 = v1 ~~ v2

t2 = norm (v1 .+. v2) == norm (v2 .+. v1)

v3 = vecD [1..3]
v4 = vecD [1..3]

t3 = v3 .*. v4
