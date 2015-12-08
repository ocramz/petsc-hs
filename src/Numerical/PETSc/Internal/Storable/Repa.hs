{-# LANGUAGE FlexibleContexts, TypeFamilies, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Sparse
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Repa <-> Storable Vector interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Storable.Repa where

import Numerical.PETSc.Internal.Types

import Foreign.C.Types

import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

-- import qualified Data.Array.Repa as R
-- import qualified Data.Array.Repa.Repr.Vector as RV
