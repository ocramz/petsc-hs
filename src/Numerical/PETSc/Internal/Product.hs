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



sizeCompat2 a b sz sz2
  | sz a == sz2 b = True
  | otherwise = False

sizeCompatN sz sz2 = all (\x y -> sz x == sz2 y) -- (head t)
