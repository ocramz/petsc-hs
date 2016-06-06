-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Product
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Mat, Vec product compatibility interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Product where

import Numerical.PETSc.Internal.PutGet.Mat

import qualified Data.Foldable as Fold

import qualified Data.Vector as V


-- sizeCompat2 a b sz sz2
--   | sz a == sz2 b = True
--   | otherwise = False

-- | staggered pairwise comparison

sizeCompatN :: (Eq a) => (t -> a) -> (t -> a) -> [t] -> Bool
sizeCompatN sz sz2 tt =
  all (\(x, y) -> sz x == sz2 y) $ zip tt (tail tt)

sizeCompatNV :: (Eq a) => (t -> a) -> (t -> a) -> V.Vector t -> Bool
sizeCompatNV sz sz2 tt =
  V.all (\(x, y) -> sz x == sz2 y) $ V.zip tt (V.tail tt)

-- |", specialized to matrix dimensions: "are all pairs of PetscMatrix in the Vector such that the # of columns in the first element match the # of rows in the second?"
sizeCompatMat = sizeCompatNV fm1 fm2 where
  fm1 = matCols . getMatrixInfoBase
  fm2 = matRows . getMatrixInfoBase
