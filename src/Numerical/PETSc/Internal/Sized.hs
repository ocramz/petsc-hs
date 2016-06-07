{-# language KindSignatures, DataKinds, ScopedTypeVariables #-}
{-# language GeneralizedNewtypeDeriving, RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Sized
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | compile-time checked vector and matrix data
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Sized where

import GHC.TypeLits
import Data.Proxy (Proxy)

import qualified Data.Vector as V

-- | see HMAtrix: Numeric.LinearAlgebra.Static.Internal


newtype Dim (n :: Nat) t = Dim t
  deriving Show

lift1F :: (c t -> c t) -> Dim n (c t) -> Dim n (c t)
lift1F f (Dim v) = Dim (f v)

lift2F :: (c t -> c t -> c t) -> Dim n (c t) -> Dim n (c t) -> Dim n (c t)
lift2F f (Dim u) (Dim v) = Dim (f u v)

---

-- newtype R n = R (Dim n (V.Vector Double))
--   deriving (Num,Fractional,Floating)

type V n t = Dim n (V.Vector t)

type Vd n = V n Double

unV :: Dim n (V.Vector t) -> V.Vector t
unV (Dim v) = v

mkV :: forall (n :: Nat) t . t -> Dim n t
mkV = Dim

vdim :: forall n t . KnownNat n => V n t -> Int
vdim v = fromIntegral . natVal $ (undefined :: Proxy n) :: Int
           

gvect :: forall n t . (Show t, KnownNat n, Num t) => [t] -> V n t
gvect xs'
    | ok = mkV v
    | not (null rest) && null (tail rest) = abort (show xs')
    | not (null rest) = abort (init (show (xs++take 1 rest))++", ... ]")
    | otherwise = abort (show xs)
  where
    (xs,rest) = splitAt d xs'
    ok = V.length v == d && null rest
    v = V.fromList xs
    d = fromIntegral . natVal $ (undefined :: Proxy n) :: Int
    abort info = error $ show d++" can't be created from elements "++info

-- gvect1 = gvect "gvect"

-- -- see Bartosz Milewski's blog on Operads and Tic Tac Toe

-- data Nat = Z | S Nat deriving Show

-- data Vec n a where
--   VNil :: Vec Z a
--   VCons :: a -> Vec n a -> Vec (S n) a





-- -- newtype Matrix n m a = Matrix { unMtx :: Vec n (Vec m a)}




