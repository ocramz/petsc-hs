{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Raw.PutGet.PetscMisc
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | PETSc misc. functions, Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.PetscMisc
       -- (
       --   commWorld, commSelf, mkMPIComm, getMPICommData,
       --   petscInit0, petscInit, petscFin,
       --   withPetsc0, withPetsc,
       --   petscGetVersion
       -- )
       where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

-- import qualified Data.ByteString as BS

import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import Foreign
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String

import Language.C.Inline

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Control.Arrow
import Control.Concurrent
import Control.Exception

-- * MPI

commWorld, commSelf :: Comm
commWorld = commWorld1
commSelf = commSelf1

-- -- interface : mkMPIComm , getMPICommData

mpiCommSize comm = unsafePerformIO $ liftM fi $ chk1 (mpiCommSize' comm)
mpiCommRank comm = unsafePerformIO $ liftM fi $ chk1 (mpiCommRank' comm)

data MpiCommSize = MpiCommSz Int deriving (Eq, Show)
mkMpiCommSize comm = MpiCommSz (mpiCommSize comm)
data MpiCommRank = MpiCommRk Int deriving (Eq, Show)
mkMpiCommRank comm = MpiCommRk (mpiCommRank comm)
data MPIComm = MPIComm Comm MpiCommSize MpiCommRank deriving (Eq, Show)
mkMPIComm c = MPIComm c (mkMpiCommSize c) (mkMpiCommRank c)
getMPICommData (MPIComm c sz rk) = (c, getMpiCommSize sz, getMpiCommRank rk)

getMpiCommSize (MpiCommSz s) = s
getMpiCommRank (MpiCommRk r) = r



-- * misc PETSc

-- -- NB : all PETSc functions must appear within a withPetsc* bracket

petscInit0 :: IO ()
petscInit0 = do
  chk0 petscInit01
  putStrLn (vs ++ ": initialized with default options")

petscFin :: IO ()
petscFin = chk0 petscFin1 >> putStrLn "PETSc : finalized"

withPetsc0 :: IO a -> IO a
withPetsc0 = bracket_ petscInit0 petscFin

petscInit ::
  [String] ->   -- "argv" list of strings
  String ->     -- options string
  String ->     -- help string
  IO ()
petscInit args opts help = do
  chk0 (petscInitialize1 args opts help)
  putStrLn ( vs ++ ": initialized"  )

withPetsc ::
  [String] -> String -> String -> IO a -> IO a
withPetsc a o h = bracket_ (petscInit a o h) petscFin




-- | Version string

{-# NOINLINE vs #-}
vs :: String
vs = unsafePerformIO $ petscGetVersion 50 

petscGetVersion :: Int -> IO String
petscGetVersion l = do
  fp <- mallocPlainForeignPtrBytes l  -- see Data.Bytestring.Internal.create
  withForeignPtr fp $ \p -> do
    pgv p (toCInt l)
    peekCString p
    where
     pgv v sz = chk0 (petscGetVersion0' v sz)
