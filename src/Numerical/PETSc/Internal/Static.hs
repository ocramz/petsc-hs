{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Staic
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Containers with statically checked dimensions
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Static where

import qualified Data.Vector as Vector

import Data.Proxy (Proxy)
import qualified GHC.TypeLits as TL (Nat, KnownNat, natVal)




-- | from HMatrix Numeric.LinearAlgebra.Static.Internal 

newtype Dim (n :: TL.Nat) t = Dim t -- deriving Show
-- instance Show (Dim n a)

newtype Dim2 (n :: TL.Nat) (m :: TL.Nat) t = Dim2 t



-- | sized vector type V

-- mkV :: forall (n :: TL.KnownNat ) t. t -> Dim n t
mkV :: t -> Dim n t
mkV = Dim

type V n a = Dim n (Vector.Vector a)

-- gvect :: forall n t. (Show t, Num t, TL.KnownNat n) => [t] -> V n t
-- gvect xs'
--   | ok = mkV v
--     where
--       (xs, rest) = splitAt d xs'
--       ok = Vector.length v == d && null rest
--       v = Vector.fromList xs
--       d = fromIntegral . TL.natVal $ (undefined :: Proxy n)
