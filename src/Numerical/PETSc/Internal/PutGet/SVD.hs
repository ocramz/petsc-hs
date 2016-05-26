{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.SVD
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | SVD Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.SVD where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

import Numerical.PETSc.Internal.PutGet.Vec
import Numerical.PETSc.Internal.PutGet.Mat

import Control.Exception


withSvdCreate :: Comm -> (SVD -> IO a) -> IO a
withSvdCreate k = bracket (svdCreate k) svdDestroy where
  svdCreate cc = chk1 $ svdCreate' cc
  svdDestroy s = chk0 $ svdDestroy' s

withSvdCreateSetup :: Comm -> Mat -> (SVD -> IO a) -> IO a
withSvdCreateSetup cc oper act = withSvdCreate cc $ \s -> do
  chk0 $ svdSetOperator' s oper
  act s

withSvdCreateSetupSolve :: Comm -> Mat -> (SVD -> IO a) -> IO a
withSvdCreateSetupSolve cc oper postsolve = withSvdCreateSetup cc oper $ \s -> do
  chk0 $ svdSolve' s
  postsolve s
  
