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

import Control.Monad.ST (ST, runST)
import Foreign.Storable (Storable, peekElemOff, pokeElemOff)
import Control.Monad.ST.Unsafe (unsafeIOToST)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

import qualified Numerical.PETSc.Internal.PutGet.Vec as PV


-- newtype STVector s = STVector PV.PetscVec


-- runSTVector :: Storable t => (forall s . ST s (STVector s t)) -> Vector t
runSTVector st = runST (st >>= unsafeFreezeVector)

-- unsafeFreezeVector :: (Storable t) => STVector s t -> ST s (Vector t)
-- unsafeFreezeVector (STVector x) = (unsafeIOToST . return)  x






