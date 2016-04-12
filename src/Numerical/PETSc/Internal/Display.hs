-- | ------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.IO
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | misc. display operations
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Display where

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

showSparse :: Show a => [(Int, a)] -> Int -> String
showSparse x n = unwords $ padSparse x [0..n-1]

-- | padSparse : display sparse (index, data) array with dots instead of the 0 entries
padSparse [] _ = []
padSparse _ [] = []
padSparse xx@((i,x):xs) (v:vs)
  | i==v = show x : padSparse xs vs
  | otherwise = "." : padSparse xx vs




-- | fq generalizes padSparse
fq :: (a -> b -> Bool) -> (a -> c) -> c -> [a] -> [b] -> [c]
fq q f t xx@(a:as) (v:vs) 
  | q a v = f a : fq q f t as vs
  | otherwise = t : fq q f t xx vs
fq _ _ _ [] _ = []
fq _ _ _ _ [] = []

padSparse' :: (Show b, Eq a) => [(a, b)] -> [a] -> [String]
padSparse' = fq (\a b -> fst a == b) (show . snd) "."

showSparse' x n = unwords (padSparse' x [0..n-1])


-- -- examples:
asdf = showSparse' [(1, "x"), (3, "w")] 10
fdsa = showSparse' [(1, 1), (3, -2), (4, 4), (7, 9)] 10
