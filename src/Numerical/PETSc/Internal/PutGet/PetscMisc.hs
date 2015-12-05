{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.PetscMisc
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | PETSc misc. functions, Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.PetscMisc
       (
         commWorld, commSelf, mkMPIComm, getMPICommData,
         petscInit0, petscInit, petscFin,
         withPetsc0, withPetsc
       )
       where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import Foreign
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Control.Arrow
import Control.Concurrent
import Control.Exception

-- * MPI

commWorld, commSelf :: Comm
commWorld = commWorld1
commSelf = commSelf1

commWorld' = mkMPIComm commWorld
commSelf' = mkMPIComm commSelf

-- -- interface : mkMPIComm , getMPICommData

mpiCommSize, mpiCommRank :: Comm -> Int
mpiCommSize comm = unsafePerformIO $ liftM fi $ chk1 (mpiCommSize' comm)
mpiCommRank comm = unsafePerformIO $ liftM fi $ chk1 (mpiCommRank' comm)



mkMpiCommSize comm = MpiCommSz (mpiCommSize comm)
mkMpiCommRank comm = MpiCommRk (mpiCommRank comm)

mkMPIComm :: Comm -> MPIComm
mkMPIComm c = MPIComm c (mkMpiCommSize c) (mkMpiCommRank c)

getMPICommData :: MPIComm -> (Comm, Int, Int)
getMPICommData (MPIComm c sz rk) = (c, getMpiCommSize sz, getMpiCommRank rk)

getMpiCommSize (MpiCommSz s) = s
getMpiCommRank (MpiCommRk r) = r



-- * misc PETSc

-- -- NB : all PETSc functions must appear within a withPetsc* bracket

petscInit0 :: IO ()
petscInit0 = do
  chk0 petscInit01
  putStrLn (header ++ " with default options\n")

petscFin :: IO ()
petscFin = chk0 petscFin1 >> putStrLn ("\nPETSc : finalized\n" ++ sep)

withPetsc0 :: IO a -> IO a
withPetsc0 = bracket_ petscInit0 petscFin

petscInit ::
  [String] ->   -- "argv" list of strings
  String ->     -- options string
  String ->     -- help string
  IO ()
petscInit args opts help = do
  chk0 (petscInitialize1 args opts help)
  putStrLn header

withPetsc ::
  [String] -> String -> String -> IO a -> IO a
withPetsc a o h = bracket_ (petscInit a o h) petscFin

sep, header :: String 
sep = "======"

header =
  sep ++ "\npetsc-hs : Haskell bindings for PETSc\n" ++
  "\nPETSc " ++ vs ++ ": initialized"


-- | Version string

{-# NOINLINE vs #-}
vs :: String
vs = drop 6 $ unsafePerformIO $ pv

pv = petscGetVersion 50 

petscGetVersion :: Int -> IO String
petscGetVersion l = do
  fp <- mallocPlainForeignPtrBytes l  -- see Data.Bytestring.Internal.create
  withForeignPtr fp $ \p -> do
    pgv p (toCInt l)
    peekCString p
    where
     pgv v sz = chk0 (petscGetVersion0' v sz)


getCString :: Int -> (Ptr CChar -> IO a) -> IO String
getCString l act = do
  fp <- mallocPlainForeignPtrBytes l
  withForeignPtr fp $ \p -> do
    act p
    peekCString p



-- | Error messages :
-- text strings directly from PETSc 
-- -- usually we generate them using the CInt return code (see Internal.Exception)

{-
BROKEN :

petscErrorMessage :: Int -> IO String
petscErrorMessage n = alloca $ \p -> do
  chk0 $ petscErrorMessage' (toCInt n) p
  x <- peek p
  peekCString x

-- petscErrorMessage n = do
--   (x, _) <- withPtr $ \p -> chk0 (petscErrorMessage' (toCInt n) p)
--   peekCString x

-- pem n len = getCString len (\p -> chk0 (petscErrorMessage' (toCInt n) p))

-- petscErrorMessage n = getCString n $ \p l ->
--   petscErrorMessage' (toCInt l) p
-
-}
