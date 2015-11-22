{-# LANGUAGE TemplateHaskell #-}
module Numerical.PETSc.Internal.Class where
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Class
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | typeclasses
--
-----------------------------------------------------------------------------

import Numerical.PETSc.Internal.PutGet.PetscMisc
import Numerical.PETSc.Internal.Types

import Control.Monad.Managed
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Control.Lens









-- -- 
-- data PetscObj a = PetscObj { _comm :: Comm,
--                              _obj :: a }

-- makeLenses ''PetscObj

-- createPetscObj f = do
--   c <- asks _comm
--   lift $ f c 
