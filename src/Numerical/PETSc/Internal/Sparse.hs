{-# LANGUAGE FlexibleContexts, RankNTypes, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Sparse
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Sparse vector and matrix indexing
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Sparse where

import Numerical.PETSc.Internal.Types

import Data.Functor
import Control.Applicative
import Control.Monad

import Data.Foldable
import Data.Traversable
import Data.Ix
import qualified Data.IntMap as IM

import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

import Foreign.C.Types




-- | `IsSparseList`, inspired by `IsList` in Base

-- class IsSparse s a where
--   type SpIx s :: *
--   fromSparse :: t (SpIx s, a) -> t a
--   toSparse :: Int -> t a -> t (SpIx s, a)



-- IntMap a -> V.Vector (ix, a)
asdfI :: IM.IntMap a -> V.Vector (IM.Key, a)
asdfI = V.fromList . IM.toList

mapMapWithKeys f = IM.mapWithKey $ \i ro ->
  IM.mapWithKey (f i) ro

traverse2 f = IM.traverseWithKey $ \i row ->
  IM.traverseWithKey (f i) row 


imInsert20 i l = IM.insert i (IM.fromList l)

imInsert21 l = IM.fromList 


-- | IntMap (IntMap a) -> V.Vector (ixy, ixy, a)
-- imm2v f = 

-- | " reverse :
-- | V.Vector (ix, iy, a) -> IntMap (IntMap a)
-- v2i f = V.map $ \(i, j, x) -> 
--   IM.insert i


-- insertImm i j x imm = IM.insert j (IM.insert i x imm)



toSparseList ::
  (Ix i, Num i, Applicative f) =>
  i ->
  (i -> a -> f b) ->
  [(i, a)] ->
  f ()
toSparseList imax f = traverse_ (uncurry fm) where
  fm i v | inRange (0, imax-1) i = f i v

toSV = SV
assembleFromSV (SV imax v) f = toSparseList imax f (V.toList v)


-- toSV isize f = SV isize (V.fromList $ toSparseList isize f)

-- class IsSparseList l where
--   type SpIx l   -- e.g. Int, or (Int, Int) or Ix i => i
--   fromSparseList :: [SpIx l] -> l
--   fromSparseListN :: Int -> [SpIx l] -> l -- first arg: list length
--   toSparseList :: l -> [SpIx l]
  

  

-- instance IsSparseTraversable [] [a] a where -- ewww



-- | from Data.Vector.Generic

-- type family Mutable (v :: * -> *) :: * -> * -> *

-- class MVector v a where
--   -- | Length of the mutable vector. This method should not be
--   -- called directly, use 'length' instead.
--   basicLength       :: v s a -> Int

-- class MVector (Mutable v) a => Vect v a where -- Vector from Data.Vector.Generic.Base
--   basicUnsafeFreeze :: PrimMonad m => Mutable v (PrimState m) a -> m (v a)



-- | example of TypeFamily usage

class Container c where
  type CElem c
  cempty :: c
  cinsert :: CElem c -> c -> c
  ctoList :: c -> [CElem c]

instance Container [e] where
  type CElem [e] = e
  cempty = []
  cinsert e l = e : l
  ctoList l = l

-- sumContainer :: Container c => c -> c -> c
-- sumContainer c1 c2 = foldr cinsert c2 (ctoList c1)




-- | a sparse container typeclass using TypeFamilies
  
-- -- type SIdx : indexing, 1D Int, 2D (Int, Int), ...
-- -- type SCDim : container dimensions: 1D Int, 2D (Int, Int), ...


-- class SContainer c where
--   type SIdx c
--   type SCe c
--   type SCDim c
--   scIdxValid :: SCDim c -> SIdx c -> Bool
--   scBuild :: SCDim c ->
--              V.Vector (SIdx c) ->
--              V.Vector (SCe c) ->
--              V.Vector (SIdx c, SCe c)
--   -- scBuild d vi ve | q i = V.cons (i, e) (scBuild d vis ves) -- FIXME broken
--   --              | otherwise = scBuild d vis ves 
--   --    where i = V.head vi 
--   --          e = V.head ve 
--   --          vis = V.tail vi
--   --          ves = V.tail ve
--   --          q = scIdxValid d 

-- instance SContainer (SV x) where
--   type SIdx (SV x) = Int
--   type SCe (SV x) = x
--   type SCDim (SV x) = Int
--   scIdxValid d i = i >= 0 && i <= d









-- | Vector

data SV a = SV { spVectorDim :: Int ,
                 spVector :: V.Vector (Int, a) }





-- | Matrix

data CSR a = CSR { csrNrows :: Int,
                   csrNcols :: Int,
                   csr :: V.Vector (Int, Int, a) }





-- |

llToCSR :: [[c]] -> [(Int, Int, c)]
llToCSR u = go (0 :: Int) u where
  m = length (head u)
  go j (v:vs) = zip3 irows (replicate m j) v ++ go (j+1) vs where
    irows = [0..m-1]
  go _ [] = []

vvToCSR :: V.Vector (V.Vector c) -> V.Vector (Int, Int, c)
vvToCSR vv = V.fromList (llToCSR (V.toList (V.map V.toList vv)))

