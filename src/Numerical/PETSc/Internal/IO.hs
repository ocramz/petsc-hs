-- | ------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.IO
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | misc. I/O operations
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.IO where

import           Data.List

-- import Numerical.PETSc.Internal.InlineC
import           Numerical.PETSc.Internal.Types
import           Numerical.PETSc.Internal.Utils
-- import Numerical.PETSc.Internal.PutGet
-- import Numerical.PETSc.Internal.Internal

import qualified Data.Vector                    as V
import qualified Data.Vector.Generic            as VG


-- | printing

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





-- ", for Vector (Vector a)
-- showVVn v_

-- padSparse (i, x) v = if i==v then show x else "."

padSparse [] _ = []
padSparse _ [] = []
padSparse xx@((i,x):xs) (v:vs)
  | i==v = show x : padSparse xs vs
  | otherwise = "." : padSparse xx vs

asdf = padSparse [(1, 'x'), (3, 'w')] [1..10]

