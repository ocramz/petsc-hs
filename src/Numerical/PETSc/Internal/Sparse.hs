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



-- type AssocMatrix = [((Int,Int),Double)]
type AssocMatrix a = V.Vector ((Int, Int), a)
type PetscAssocMatrix = AssocMatrix PetscScalar_



-- -- | more or less lifted from HMatrix 
-- data CSR = CSR
--         { csrVals  :: V.Vector Double
--         , csrCols  :: V.Vector CInt
--         , csrRows  :: V.Vector CInt
--         , csrNRows :: Int
--         , csrNCols :: Int
--         } deriving Show

-- data CSC = CSC
--         { cscVals  :: V.Vector Double
--         , cscRows  :: V.Vector CInt
--         , cscCols  :: V.Vector CInt
--         , cscNRows :: Int
--         , cscNCols :: Int
--         } deriving Show

-- data CSR a = CSR
--              { csrVals :: V.Vector a
--              , csrCols :: V.Vector CInt
--              , csrRows :: V.Vector CInt
--              , csrNRows :: Int
--              , csrNCols :: Int
--              }

data CSR a = CSR { csr :: AssocMatrix a,
                   csrNrows :: Int,
                   csrNcols :: Int}

class Sparse a where
  type SparseIdx a
  assemble :: a -> V.Vector (SparseIdx a) -> a

instance Sparse (CSR a) where
  type SparseIdx (CSR a) = (Int, Int)



