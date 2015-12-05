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



withSvd :: Comm -> (SVD -> IO a) -> IO a
withSvd comm = bracketChk (svdCreate' comm) svdDestroy'

withSvdSetup :: Comm -> Mat -> (SVD -> IO a) -> IO a
withSvdSetup comm oper act = withSvd comm $ \s -> do
  chk0 $ svdSetOperator' s oper
  act s

withSvdSetupSolve :: Comm -> Mat -> (SVD -> IO a) -> IO a
withSvdSetupSolve comm oper postsolve = withSvdSetup comm oper $ \s -> do
  chk0 $ svdSolve' s
  postsolve s
  
