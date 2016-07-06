-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Mid-level interface: catching exceptions and hiding pointers in lexical
--   scope of `bracket`s
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet
       ( logViewStdout, 
         module X ) where

import Numerical.PETSc.Internal.PutGet.IS as X 
import Numerical.PETSc.Internal.PutGet.Vec as X
import Numerical.PETSc.Internal.PutGet.Mat as X
import Numerical.PETSc.Internal.PutGet.DM as X
import Numerical.PETSc.Internal.PutGet.KSP as X
import Numerical.PETSc.Internal.PutGet.PC as X
import Numerical.PETSc.Internal.PutGet.SNES as X
import Numerical.PETSc.Internal.PutGet.TS as X
import Numerical.PETSc.Internal.PutGet.TAO as X
import Numerical.PETSc.Internal.PutGet.Viewer as X
import Numerical.PETSc.Internal.PutGet.PetscMisc as X

import Numerical.PETSc.Internal.PutGet.EPS as X
import Numerical.PETSc.Internal.PutGet.SVD as X
import Numerical.PETSc.Internal.PutGet.SlepcMisc as X
import Numerical.PETSc.Internal.Types as X





  

-- | logging

logViewStdout :: Comm -> IO ()
logViewStdout cc = do
  petscLogDefaultBegin
  withPetscViewStdout cc petscLogView
