{-# LANGUAGE TypeOperators, DefaultSignatures, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Class.Additive
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Additive class, stripped down wrt `linear`
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Class.Additive where

import Data.Functor
-- import GHC.Generics
import Data.Foldable as Foldable (Foldable,forM_, foldl')

import qualified Data.Vector as V


class Functor f => Additive f where

  zero :: Num a => f a

  (^+^) :: Num a => f a -> f a -> f a
  -- default (^+^) :: Num a => f a -> f a -> f a
  -- (^+^) = liftU2 (+)

  (^-^) :: Num a => f a -> f a -> f a
  -- default (^-^) :: Num a => f a -> f a -> f a
  -- -- (^-^) = liftU2 (-)
  -- (^-^) a b = (^+^) a (negated b)

  -- liftU1 :: (a -> a) -> f a -> f a
  -- liftU2 :: (a -> a -> a) -> f a -> f a -> f a

  (*^) :: Num a => a -> f a -> f a
  (*^) a = fmap (a*)
  {-# INLINE (*^) #-}

  (^*) :: Num a => f a -> a -> f a
  f ^* a = fmap (*a) f
  {-# INLINE (^*) #-}

  -- (^/) :: Fractional a => f a -> a -> f a
  -- f ^/ a = fmap (/a) f
  -- {-# INLINE (^/) #-}
  

-- instance Additive V.Vector where
--   zero = V.singleton 0
--   (^+^) = V.zipWith (+)
--   (^-^) = V.zipWith (-)
--   -- liftU1 = V.map
--   -- liftU2 = V.zipWith


-- n |> lx | length tn == n = V.fromList tn
--         | otherwise = error "list too short"
--   where tn = take n lx

-- v0, v1, v2 :: (Enum a, Num a) => V.Vector a
-- v0 = 5 |> [1..10]
-- v1 = 5 |> [4..15]

-- v2 = v1 ^+^ v2   -- boom! why?



-- -- interface using Generics

-- class GAdditive f where
--   gzero :: Num a => f a
--   gliftU2 :: (a -> a -> a) -> f a -> f a -> f a
--   gliftI2 :: (a -> b -> c) -> f a -> f b -> f c

-- instance GAdditive U1 where
--   gzero = U1
--   {-# INLINE gzero #-}
--   gliftU2 _ U1 U1 = U1
--   {-# INLINE gliftU2 #-}
--   gliftI2 _ U1 U1 = U1
--   {-# INLINE gliftI2 #-}

-- instance (GAdditive f, GAdditive g) => GAdditive (f :*: g) where
--   gzero = gzero :*: gzero
--   {-# INLINE gzero #-}
--   gliftU2 f (a :*: b) (c :*: d) = gliftU2 f a c :*: gliftU2 f b d
--   {-# INLINE gliftU2 #-}
--   gliftI2 f (a :*: b) (c :*: d) = gliftI2 f a c :*: gliftI2 f b d
--   {-# INLINE gliftI2 #-}



-- >>> negated (V2 2 4)
-- V2 (-2) (-4)
negated :: (Functor f, Num a) => f a -> f a
negated = fmap negate
{-# INLINE negated #-}

-- | Sum over multiple vectors
--
-- >>> sumV [V2 1 1, V2 3 4]
-- V2 4 5
sumV :: (Foldable f, Additive v, Num a) => f (v a) -> v a
sumV = Foldable.foldl' (^+^) zero
{-# INLINE sumV #-}

