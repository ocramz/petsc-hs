-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.SF
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Mid-level interface: catching exceptions and hiding pointers in lexical
--   scope of `bracket`s
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.SF where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

import Foreign
import Foreign.C.Types

import Control.Exception (bracket)

petscSFCreate :: Comm -> IO PetscSF
petscSFCreate cc = chk1 $ petscSFCreate' cc

petscSFDestroy :: PetscSF -> IO ()
petscSFDestroy sf = chk0 $ petscSFDestroy' sf

-- |

withPetscSF :: Comm -> (PetscSF -> IO a) -> IO a
withPetscSF cc = bracket (petscSFCreate cc) petscSFDestroy
