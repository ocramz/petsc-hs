-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Indexing
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | Mesh indexing primitives
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Indexing where

import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

import Numerical.PETSc.Internal.PutGet

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
