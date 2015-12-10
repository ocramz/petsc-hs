{-# LANGUAGE FlexibleContexts, RankNTypes, TypeFamilies, MultiParamTypeClasses #-}
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

instance Eq e => Container [e] where
  type CElem [e] = e
  cempty = []
  cinsert e l = e : l
  ctoList l = l
  



-- |

class Sparse e where
  type SparseIdx e :: *
  type SparseDim e :: *
  type SparseContainer e :: *
  sparseAssemble :: SparseDim e ->
                    V.Vector (SparseIdx e, e) -> SparseContainer e


-- | Vector

data SV a = SV { spVectorDim :: Int ,
                 spVector :: V.Vector (Int, a) }

-- instance Sparse (SV e) where
--   type SparseIdx (SV e) = Int
--   type SparseContainer (SV e) = SV e
--   -- sparseAssemble = SV



-- | Matrix

data CSR a = CSR { csrNrows :: Int,
                   csrNcols :: Int,
                   csr :: V.Vector ((Int, Int), a) }







