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

instance Container (V.Vector x) where
  type CElem (V.Vector x) = x
  cempty = V.empty
  cinsert = V.cons
  ctoList = V.toList

sumContainer :: Container c => c -> c -> c
sumContainer c1 c2 = foldr cinsert c2 (ctoList c1)

c0 = cinsert 2 cempty :: [Int]
c1 = sumContainer c0 c0


-- |




-- | Vector

data SV a = SV { spVectorDim :: Int ,
                 spVector :: V.Vector (Int, a) }





-- | Matrix

data CSR a = CSR { csrNrows :: Int,
                   csrNcols :: Int,
                   csr :: V.Vector ((Int, Int), a) }







