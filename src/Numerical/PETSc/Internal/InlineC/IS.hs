{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.InlineC.IS
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Foreign signatures, + everything that requires an inline-c pass
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.InlineC.IS where

import           Numerical.PETSc.Internal.Internal
import           Numerical.PETSc.Internal.Types
import           Numerical.PETSc.Internal.Utils

import           Language.C.Inline                 as C
import           Language.C.Inline.Context

context petscCtx

C.include "<petscsnes.h>"

isCreateStride_ c n first step is = [C.exp|
     int{ISCreateStride(
            $(int c),
            $(PetscInt n),
            $(PetscInt first),
            $(PetscInt step),
            $(IS* is)) }|]

isCreateStride' comm n first step =
  withPtr $ \is -> isCreateStride_ c n first step is
   where c = unComm comm
