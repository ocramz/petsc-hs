-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.ST
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | In-place manipulation in the ST monad
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.ST where

-- import Internal.Vector
-- import Internal.Matrix
-- import Internal.Vectorized
import Control.Monad.ST (ST, runST)
import Foreign.Storable (Storable, peekElemOff, pokeElemOff)
import Control.Monad.ST.Unsafe (unsafeIOToST)
