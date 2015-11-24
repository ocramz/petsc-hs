{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
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

import Numerical.PETSc.Internal.PutGet.Vec
import Numerical.PETSc.Internal.PutGet.Mat

import Data.Functor
import Control.Applicative
import Control.Monad

import Storable

-- import Control.Monad.Managed
-- import Control.Monad.Reader

-- import Control.Monad.Trans.Class
-- -- import Control.Monad.Trans.Reader
-- import Control.Monad.Trans.State

-- import Control.Lens

-- | a standardized interface:
-- -- * IO resource management (see e.g. ResourceT or `managed`) with auto-cleanup
-- -- * withNew-, modify- : 
-- -- -- * act on references
-- -- -- * @when@ to copy data (through MVectors) to Hs side?

{- we want to manage a resource of type `a` :
new : x -> IO a
with : IO a -> (a -> IO b) -> IO b
modify : IO a -> (a -> IO b) -> IO ()
cleanup : a -> IO () 
-}

-- | types that can be exchanged between PETSc and GHC RTS
-- NB :
-- first type : PETSc side (Storable types in Internal.Types)
-- second type: Haskell side (e.g. PetscVector in PutGet.Vec)

-- class (Storable p, Monad m) => PetscObject i p h m | p -> i where
--   initO :: i -> p -> m h
--   updateH :: p -> m h
--   updateP :: p -> h -> m ()
--   destroyO :: p -> m () 

class (Storable p, Monad m) => PObj p m where
  type PObjInfo p
  type PObjLocal p
  initO :: PObjInfo p -> p
  updateH :: p -> PObjLocal p
  updateP :: p -> PObjLocal p -> m ()
  destroyO :: p -> m ()

instance PObj Vec where
  type PObjInfo PetscVector = VecInfo
  type PObjLocal PetscVector = V.Vector



-- newtype PetscObj a =
--   PetscObj (ReaderT MPIComm IO a)
--   deriving (Functor, Applicative, Monad, MonadIO)--, MonadReader MPIComm)

-- runPetscObj (PetscObj x) c = liftIO (runReaderT x c)



-- -- 
-- data PetscObj a = PetscObj { _comm :: Comm,
--                              _obj :: a }

-- makeLenses ''PetscObj

-- createPetscObj f = do
--   c <- asks _comm
--   lift $ f c 

