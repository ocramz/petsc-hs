{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Hybrid.Indexing
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Mesh indexing primitives
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Hybrid.Indexing where

import Data.Functor
import Control.Applicative 

-- import Numerical.PETSc.Internal.Types
-- import Numerical.PETSc.Internal.Exception
-- import Numerical.PETSc.Internal.Utils

-- import Numerical.PETSc.Internal.PutGet

-- import Data.Set

-- import Data.Array
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

-- import qualified Data.Map.Strict as Map
-- -- import Math.Geometry.Grid.Square
-- import Data.Ix

import Data.List (nub, nubBy, (\\))

{-
see `grid` :

https://hackage.haskell.org/package/grid-7.8.4/src/src/Math/Geometry/GridInternal.hs

https://hackage.haskell.org/package/grid-7.8.4/src/src/Math/Geometry/Grid/SquareInternal.hs

-}


class Grid g where
  type Index g
  type Direction g
  
  indices :: g -> [Index g]

  distance :: g -> Index g -> Index g -> Int

  minDistance :: g -> [Index g] -> Index g -> Int
  minDistance = defaultMinDistance

  directionTo :: g -> Index g -> Index g -> [Direction g]

  neighbours :: Eq (Index g) => g -> Index g -> [Index g]
  neighbours = defaultNeighbours

  neighboursOfSet :: Eq (Index g) => g -> [Index g] -> [Index g]
  neighboursOfSet = defaultNeighboursOfSet

  neighbour :: (Eq (Index g), Eq (Direction g)) =>
    g -> Index g -> Direction g -> Maybe (Index g)
  neighbour = defaultNeighbour

  numNeighbours :: Eq (Index g) => g -> Index g -> Int
  numNeighbours g = length . neighbours g

  contains :: Eq (Index g) => g -> Index g -> Bool
  contains g a = a `elem` indices g

  tileCount :: g -> Int
  tileCount = length . indices

  nullG :: g -> Bool
  nullG g = tileCount g == 0

  notNullG :: g -> Bool
  notNullG = not . nullG

  edges :: Eq (Index g) => g -> [(Index g,Index g)]
  edges = defaultEdges

  -- These default implementations are broken out to make it easier to
  -- compare the results with custom implementations (for testing).
  
  defaultMinDistance :: g -> [Index g] -> Index g -> Int
  defaultMinDistance g xs a = minimum . map (distance g a) $ xs

  -- WARNING: this implementation won't work for wrapped grids
  defaultNeighbours :: g -> Index g -> [Index g]
  defaultNeighbours g a = filter (\b -> distance g a b == 1 ) $ indices g

  -- This should work for wrapped grids, though.
  defaultNeighboursOfSet :: Eq (Index g) => g -> [Index g] -> [Index g]
  defaultNeighboursOfSet g as = ns \\ as
    where ns = nub . concatMap (neighbours g) $ as

  -- WARNING: this implementation won't work for wrapped grids
  defaultNeighbour :: (Eq (Index g), Eq (Direction g))
    => g -> Index g -> Direction g -> Maybe (Index g)
  defaultNeighbour g a d =
    maybeHead . filter (\b -> [d] == directionTo g a b) . neighbours g $ a
    where maybeHead (x:_) = Just x
          maybeHead _ = Nothing

  defaultTileCount :: g -> Int

    -- WARNING: this implementation won't work for wrapped grids
  defaultEdges :: Eq (Index g) => g -> [(Index g,Index g)]
  defaultEdges g = nubBy sameEdge $ concatMap (`adjacentEdges` g) $ indices g


-- Helper functions
--

sameEdge :: Eq t => (t, t) -> (t, t) -> Bool
sameEdge (a,b) (c,d) = (a,b) == (c,d) || (a,b) == (d,c)

adjacentEdges :: (Grid g, Eq (Index g)) => Index g -> g -> [(Index g, Index g)]
adjacentEdges i g = map (\j -> (i,j)) $ neighbours g i

cartesianIndices
  :: (Enum r, Enum c, Num r, Num c, Ord r, Ord c) =>
     (r, c) -> [(c, r)]
cartesianIndices (r, c) = west ++ north ++ east ++ south
  where west = [(0,k) | k <- [0,1..r-1], c>0]
        north = [(k,r-1) | k <- [1,2..c-1], r>0]
        east = [(c-1,k) | k <- [r-2,r-3..0], c>1]
        south = [(k,0) | k <- [c-2,c-3..1], r>1]

cartesianCentre :: (Int, Int) -> [(Int, Int)]
cartesianCentre (r,c) = [(i,j) | i <- cartesianMidpoints c, j <- cartesianMidpoints r]

cartesianMidpoints :: Int -> [Int]
cartesianMidpoints k = if even k then [m-1,m] else [m]
  where m = k `div` 2

{-
node -> [edge] , edge -> [node]
edge -> [face] , face -> [edge]
face -> [node] , node -> [face]
-}
  
-- type Node = Int
-- type Edge = Int
-- type Face = Int

-- class (Ix n, Ix e) => Mesh n e where
--   cone :: Map.Map n (V.Vector e)
--   cap :: Map.Map e (V.Vector n)






{- e.g. numbering ccw

     4
   1---4
 1 |   | 3 
   2---3
     2

   1---4
   | 1 |
   2---5  
   | 2 |
   3---6



-}


-- class Grid g where
--   type Index g
--   indices :: g -> [Index g]

-- data Ix1 = Ix1 [Int]

-- instance Grid Ix1 where
--   type Index Ix1 = Int
--   indices (Ix1 xs) = xs

-- data Ix2 = Ix2 [(Int, Int)]

-- instance Grid Ix2 where
--   type Index Ix2 = (Int, Int)
--   indices (Ix2 xs) = xs

-- data Iota = Iota ![Int]

-- iota :: Int -> Iota
-- iota n = Iota [0 .. n-1]

-- iotaV (Iota iot) = V.fromList iot

-- -- asdf n m = (,) <$> iota m <*> iota n

-- idxRegularMesh2d n m = map (zip iotn . replicate n) iotm where
--   (Iota iotn, Iota iotm) = (iota n, iota m)



{-|

* staggered grids, e.g. dual node/element meshes

-}



-- -- | Data.Array tests

-- -- n = 5

-- -- a0 = array (0, n-1) [(i, i^2) | i <- [0 .. n-1]]  

-- array1d :: Ix i => i -> i -> [(i, e)] -> Array i e
-- array1d xmin xmax = array (xmin, xmax)

-- array2d :: (Ix t, Ix t1) =>
--            (t, t1) -> (t, t1) -> [((t, t1), e)] -> Array (t, t1) e
-- array2d (x1,y1) (x2, y2) = array ((x1,y1),(x2, y2))

-- -- array3d :: (Ix t, Ix t1, Ix t2) =>
-- --      (t, t1) -> (t, t1) -> (t2, t2) -> [((t, t1, t2), e)] -> Array (t, t1, t2) e
-- array3d :: Ix t  =>
--      (t, t) -> (t, t) -> (t, t) -> [((t, t, t), e)] -> Array (t, t, t) e
-- array3d (x1,y1) (x2, y2) (z1, z2) = array ((x1,y1,z1),(x2, y2, z2))





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
