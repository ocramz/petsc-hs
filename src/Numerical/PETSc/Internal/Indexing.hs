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

n = 5

a0 = array (0, n-1) [(i, i^2) | i <- [0 .. n-1]]  







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
