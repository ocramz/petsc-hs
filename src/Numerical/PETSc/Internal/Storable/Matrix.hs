{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Storable.Matrix
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | operations on Storable Matrix, derived from Internal.Storable.Vector
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Storable.Matrix where

import Numerical.PETSc.Internal.Utils
import Numerical.PETSc.Internal.Storable.Store

import Control.Monad
