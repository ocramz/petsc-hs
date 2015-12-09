{-# LANGUAGE FlexibleContexts, RankNTypes, TypeFamilies #-}
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





-- | experiment


class Z a where
  type IdxZ a :: *
  type ContnrZ a :: *
  assembleZ :: V.Vector (IdxZ a, a) -> ContnrZ a

instance Z (SV a) where
  type IdxZ (SV a)  = Int
  type ContnrZ (SV a) = (SV a)
  -- assembleZ 



-- |

class Num a => Sparse a where
  type SparseIdx a :: *
  type SparseDim a :: *
  type SparseContainer a :: *
  sparseIndex :: SparseIdx a -> SparseContainer a -> a
  sparseAssemble :: V.Vector (SparseIdx a) -> SparseDim a -> SparseContainer a





-- | Vector

data SV a = SV { spVectorDim :: Int ,
                 spVector :: V.Vector (Int, a) }





-- | Matrix

data CSR a = CSR { csrNrows :: Int,
                   csrNcols :: Int,
                   csr :: V.Vector ((Int, Int), a) }







