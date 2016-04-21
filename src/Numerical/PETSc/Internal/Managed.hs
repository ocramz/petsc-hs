-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.Petsc.Internal.Managed
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Managed actions
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Managed where


import Control.Exception
import Control.Monad.Managed


liftManaged :: (t -> IO a) -> (t -> a -> IO b) -> t -> Managed a
liftManaged get set x = 
  managed $ bracket (get x) (set x)
