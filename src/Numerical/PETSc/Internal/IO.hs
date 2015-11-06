-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.IO
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | misc. I/O operations
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.IO where

import Data.List

-- import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils
-- import Numerical.PETSc.Internal.PutGet
-- import Numerical.PETSc.Internal.Internal

import qualified Data.Vector as V

-- | printing

v0 = V.fromList [1..21]


-- | show only first n elements and last of Vector v

showVn :: Show a => V.Vector a -> Int -> IO ()
showVn l n = print $ "[" ++ s0 ++ s1 ++ "]"
  where
   len = V.length l
   s0 = intercalate ", " $ map show $ V.toList (V.take n l)
   s1
     | len == n + 1 = []
     | len > n = " .. " ++ show (V.last l)
     | otherwise = []

showV l = showVn l 20



-- instance Show a => Show (V.Vector a) where
--   show = flip showN 80
