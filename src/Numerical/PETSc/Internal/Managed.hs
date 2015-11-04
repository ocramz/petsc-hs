{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Managed
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | IO resource management abstraction
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Managed where

import Foreign.C.Types

-- newtype Managed a r = Managed {runManaged :: IO a -> (a -> IO r) -> IO r}
-- newtype Managed a = Managed { (>>-) :: forall r . (a -> IO r) -> IO r}

newtype Managed0 a r1 r2 = Managed { (>>-) :: (a -> IO r1) -> IO r2}

managed = Managed

-- withManaged :: Managed a -> (a -> IO b) -> IO b
withManaged = (>>-)


runManaged m = m >>- return
