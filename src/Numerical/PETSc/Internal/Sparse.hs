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
import Control.Monad

import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

import Foreign.C.Types




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

