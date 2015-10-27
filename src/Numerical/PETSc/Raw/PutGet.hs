{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Raw.PutGet
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | Mid-level interface: catching exceptions and hiding pointers in lexical
--   scope of `bracket`s
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Raw.PutGet
       (
         module Numerical.PETSc.Raw.PutGet.IS,
         module Numerical.PETSc.Raw.PutGet.Vec,
         module Numerical.PETSc.Raw.PutGet.Mat,
         module Numerical.PETSc.Raw.PutGet.DM,
         module Numerical.PETSc.Raw.PutGet.KSP,
         module Numerical.PETSc.Raw.PutGet.SNES,
         module Numerical.PETSc.Raw.PutGet.TS,
         module Numerical.PETSc.Raw.PutGet.TAO,
         module Numerical.PETSc.Raw.PutGet.Viewer,
         module Numerical.PETSc.Raw.PutGet.PetscMisc
       )
       where

-- import Numerical.PETSc.Raw.InlineC
-- import Numerical.PETSc.Raw.Types
-- import Numerical.PETSc.Raw.Exception
-- import Numerical.PETSc.Raw.Utils

-- import Numerical.PETSc.Raw.Internal

import Numerical.PETSc.Raw.PutGet.IS
import Numerical.PETSc.Raw.PutGet.Vec
import Numerical.PETSc.Raw.PutGet.Mat
import Numerical.PETSc.Raw.PutGet.DM
import Numerical.PETSc.Raw.PutGet.KSP
import Numerical.PETSc.Raw.PutGet.SNES
import Numerical.PETSc.Raw.PutGet.TS
import Numerical.PETSc.Raw.PutGet.TAO
import Numerical.PETSc.Raw.PutGet.Viewer
import Numerical.PETSc.Raw.PutGet.PetscMisc







  


