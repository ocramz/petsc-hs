{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Numerical.PETSc.Raw.Algebra where

import Data.Complex
import qualified Data.Vector as V
import qualified Data.Foldable as Fold

infixl 4 :+:
infixl 5 :*:, :/:
infixr 6 :^:

data Expr a = Var Char
             | Const a 
             | (Expr a) :+: (Expr a)
             | (Expr a) :-: (Expr a)  
             | (Expr a) :*: (Expr a)
             | (Expr a) :^: (Expr a)  
             | (Expr a) :/: (Expr a)
             | Rec (Expr a)  
             | Log (Expr a)
             | Exp (Expr a) (Expr a)
             | Sin (Expr a)
             | Cos (Expr a)  
             | E  
             deriving (Show, Eq)

instance Num a => Num (Expr a) where
  (+) = (:+:)
  (-) = (:-:)
  (*) = (:*:)

instance Num a => Fractional (Expr a) where
  (/) = (:/:)


instance Floating a => Floating (Expr a) where
  pi = Const pi
  exp = Exp E
  log = Log
  sin = Sin
  cos = Cos
  asin = undefined
  acos = undefined
  sinh = undefined
  cosh = undefined
  asinh = undefined
  acosh = undefined
  atan = undefined
  atanh = undefined

-- instance Floating (Expr a) where
--   -- pi = Const pi

sampleExpr :: Expr Double
sampleExpr = Const 3 :*: Var 'x' :^: Const 2 --3x^2

-- simpl (Expr a) :+: (Expr (Const 0)) = simpl a






-- -- --



data Vector a = Vec { unVec :: V.Vector a} deriving (Eq)
instance Show a => Show (Vector a) where
  show a = show (unVec a)

mkVec = Vec . V.fromList

-- v1, v2 :: Num a => Hilbert (Vector a) a
v1 = mkVec [0,0,1] -- :: Vector Int
v2 = mkVec [1,0,0] -- :: Vector Int
v3 = mkVec [1,2]
-- t1 = v1 `inner` v2

-- f = inner

instance Functor Vector where
  fmap f x = Vec $ V.map f (unVec x)

-- class  Hilbert a b where
--   inner :: a -> a -> b

-- instance Num a =>  Hilbert (Vector a) a where
--   inner x y = V.sum $ V.zipWith (*) (unVec x) (unVec y)

data Matrix a = Mat (V.Vector (V.Vector a))

data MatrixCSR a = MatCSR { matCsrI :: Vector Int,
                            matCsrJ :: Vector Int,
                            matCsrData :: Vector a}

vecSum :: Num a => Vector a -> Vector a -> Vector a
vecSum a b = Vec $ V.zipWith (+) (unVec a) (unVec b)

vecPairF f a b = Vec $ V.zipWith f (unVec a) (unVec b)

-- | elementwise math operations
instance Num a => Num (Vector a) where
  (+) = vecPairF (+)
  (*) = vecPairF (*)
  abs = fmap abs
  signum = fmap signum
  -- fromInteger = fmap fromInteger
  negate = fmap negate



-- -- --

data IOAction a = Return a
                | Put String (IOAction a)
                | Get (String -> IOAction a)

get   = Get Return
put s = Put s (Return ())
