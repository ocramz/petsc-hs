{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.Vec
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | tests, but not in the quickcheck sense :D
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Test where

import Numerical.PETSc.Internal.PutGet
import Numerical.PETSc.Internal.Internal

import qualified Data.Vector.Storable as V



vinfo n = VecInfo commWorld n n

vtemplate n f = withVecMPIPipeline vi (`vecSet` pi) $ \v -> do
  withVecGetVectorOverwrite v (V.map exp)    -- contents of Vec are changed 
  print $ exp pi
  f v
  vecViewStdout v
    where
      vi = vinfo n
