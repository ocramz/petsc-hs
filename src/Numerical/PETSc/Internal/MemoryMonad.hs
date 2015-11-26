{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, RankNTypes #-}
-- {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
module Numerical.PETSc.Internal.MemoryMonad where
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Free
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | atomic operations on shared memory
--
-----------------------------------------------------------------------------

import Data.Functor

import Control.Concurrent.STM.TVar
import Control.Monad.Free.Church
import qualified Control.Monad.Managed as M
import Control.Monad.STM

import qualified Control.Monad.Trans.Class as T
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy

import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

import Foreign.Storable

-- | PETsc - specific

-- import Numerical.PETSc.Internal.Types
-- import Numerical.PETSc.Internal.PutGet

-- import System.IO.Unsafe

-- | -- -- -- -- -- -




asdfm = M.managed


 


-- class (Storable p, Monad m) => PetscObj m p where
--   type PObjInfo p
--   type PObjPtr p
--   initO :: PObjInfo p -> m p
--   finalizeO :: p -> m ()
--   withO :: p -> (PObjPtr p -> m a) -> m a
--   modifyO :: p -> (PObjPtr p -> m a) -> m ()  



-- type Vx = V.Vector PetscScalar_

-- data VecMonad a = GetVec (Vec -> a)
--                 | ModifyVec (Vx -> Vx) a  
--                 deriving Functor

-- getVec :: F VecMonad (V.Vector PetscScalar_)
-- getVec = liftF (GetVec vecGetVectorU) where
--   vecGetVectorU = VG.convert . unsafePerformIO . vecGetVector

-- modifyVec :: Vec -> (Vx -> Vx) -> F VecMonad Vec
-- modifyVec v f = liftF (ModifyVec f v)


