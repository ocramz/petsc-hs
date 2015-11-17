-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Indexing
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Mesh indexing primitives
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Indexing where

import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

-- import Numerical.PETSc.Internal.PutGet

import Data.Array
import qualified Data.Vector as V




{-| Specs :

inputs:

* Nd : # of spatial dimenstions 
* [(xmin, xmax)] : bounds / dimension
* [BdryType] : bound. type / dimension
* Sw : stencil width (i)


queries :

* indices <-> coordinates (topology <-> metric)
* point within boundaries

-}



{-|

* staggered grids, e.g. dual node/element meshes

-}



-- | Data.Array tests

-- n = 5

-- a0 = array (0, n-1) [(i, i^2) | i <- [0 .. n-1]]  

array1d :: Ix i => i -> i -> [(i, e)] -> Array i e
array1d xmin xmax = array (xmin, xmax)

array2d :: (Ix t, Ix t1) =>
           (t, t1) -> (t, t1) -> [((t, t1), e)] -> Array (t, t1) e
array2d (x1,y1) (x2, y2) = array ((x1,y1),(x2, y2))

-- array3d :: (Ix t, Ix t1, Ix t2) =>
--      (t, t1) -> (t, t1) -> (t2, t2) -> [((t, t1, t2), e)] -> Array (t, t1, t2) e
array3d :: Ix t  =>
     (t, t) -> (t, t) -> (t, t) -> [((t, t, t), e)] -> Array (t, t, t) e
array3d (x1,y1) (x2, y2) (z1, z2) = array ((x1,y1,z1),(x2, y2, z2))

-- iota a b
--   | b >= a = [a .. b-1]
--   | otherwise = error $ "incompatible indices : " ++ show (a, b)

iota a b = [a .. b-1]

iota0 :: Int -> [Int]
iota0 = iota 0



-- |





-- | notes

-- mpiComm = mkMPIComm commWorld

-- (comm, commSize, commRank) = getMPICommData mpiComm

-- localRange :: Int -> (Int, Int)
-- localRange m = ( istart, iend) where
--   istart = cr * (m `div` cs) + if t < cr then t else cr
--   iend = istart + (m `div` cs) + if t > cr then 1 else 0
--   t = m `mod` cs
--   cs = commSize
--   cr = commRank

--   -- start = rank*(mm /size) + ((mm %size) < rank ? (mm %size) : rank);
--   -- end   = start + mm /size + ((mm %size) > rank);
