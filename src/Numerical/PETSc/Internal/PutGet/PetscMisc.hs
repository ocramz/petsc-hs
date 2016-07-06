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
         petscTime,
         petscGetCPUTime, petscGetFlops,
         petscLogDefaultBegin,
         petscLogView, petscLogStageRegister, petscLogStagePush, petscLogStagePop,
         commWorld, commSelf,
         commWorldC, commSelfC,
         mkMPIComm, getMPICommSize, getMPICommRank,
         -- withMPIEnv,
         petscOptionsView, petscOptionsSetValue,
         petscInit0, petscInit, petscFin,
         withPetsc0, withPetsc,
         withPetsc0MpiComm, withPetscMpiComm,
         isMpiRoot
       )
       where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

import Data.List (intercalate)
import Data.List.Split (splitOn, splitOneOf)

import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import Foreign
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String

import System.IO.Unsafe (unsafePerformIO)

import Control.Applicative ((<$>))
import Control.Monad
-- import Control.Concurrent
import Control.Exception

-- import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import Control.Arrow ((***), (&&&))

import System.Process (readProcess)




-- a `reader` bracket for reading MPI environmemt information

-- withMPIEnv f = runReaderT $ do
--   c <- ask
--   return $ f $ getMPICommSize c
--   -- let
--   --   (cs, cr) = (getMPICommSize &&& getMPICommRank) c
               
--   -- lift (f cs cr)
--   -- lift (f c')




-- * FLOPs

petscGetFlops :: IO PetscLogDouble
petscGetFlops = chk1 petscGetFlops'



-- * timing

petscTime :: IO PetscLogDouble
petscTime = chk1 petscTime'

petscGetCPUTime :: IO PetscLogDouble
petscGetCPUTime = chk1 petscGetCPUTime'






-- * logging

petscLogDefaultBegin :: IO ()
petscLogDefaultBegin = chk0 petscLogDefaultBegin'

petscLogView :: PetscViewer -> IO ()
petscLogView vi = chk0 $ petscLogView' vi

petscLogStageRegister :: String -> IO PetscLogStage
petscLogStageRegister logstagename = chk1 $ petscLogStageRegister' logstagename

petscLogStagePush :: PetscLogStage -> IO ()
petscLogStagePush ls = chk0 $ petscLogStagePush' ls

petscLogStagePop :: IO ()
petscLogStagePop = chk0 petscLogStagePop'



-- * MPI

commWorld, commSelf :: Comm
commWorld = commWorld1
commSelf = commSelf1

commWorldC, commSelfC :: MPIComm
commWorldC = mkMPIComm commWorld
commSelfC = mkMPIComm commSelf

-- -- interface : mkMPIComm , getMPICommRank, getMPICommSize

mpiCommSize, mpiCommRank :: Comm -> Int
mpiCommSize c = unsafePerformIO $ fi <$> chk1 (mpiCommSize' c)
mpiCommRank c = unsafePerformIO $ fi <$> chk1 (mpiCommRank' c)


mkMpiCommSize :: Comm -> MpiCommSize
mkMpiCommSize c = MkMpiCommSz (mpiCommSize c)

mkMpiCommRank :: Comm -> MpiCommRank
mkMpiCommRank c = MkMpiCommRk (mpiCommRank c)

mkMPIComm :: Comm -> MPIComm
mkMPIComm c = MPIComm c (mkMpiCommSize c) (mkMpiCommRank c)

-- | MPIComm accessors
getMPICommSize, getMPICommRank :: MPIComm -> Int
getMPICommSize c = fromEnum (commSize c)
getMPICommRank c = fromEnum (commRank c)


  




-- * misc PETSc


-- options handling

petscOptionsView :: PetscViewer -> IO ()
petscOptionsView vi = chk0 $ petscOptionsView0' vi

petscOptionsSetValue :: OptionName -> String -> IO ()
petscOptionsSetValue opt val = chk0 $ petscOptionsSetValue0' opt val

-- -- NB : all PETSc functions must appear within a withPetsc* bracket

petscInit0 :: IO ()
petscInit0 = 
  chk0 petscInit0' >> putStrLn (petscHeader ++ " with default options\n")

petscFin :: IO ()
petscFin =
  chk0 petscFin' >> putStrLn ("\nPETSc : finalized\n" ++ sep)


withPetsc0 :: IO a -> IO a
withPetsc0 = bracket_ petscInit0 petscFin


withPetsc0MpiComm :: MPIComm -> (MpiCommSize -> MpiCommRank -> IO a) -> IO a
withPetsc0MpiComm c act =
  withPetsc0 $ do
    let size = commSize c
        rank = commRank c
    act size rank


petscInit ::
  [String] ->   -- "argv" list of strings
  String ->     -- options string
  String ->     -- help string
  IO ()
petscInit args opts help = do
  chk0 (petscInitialize1 args opts help)
  putStrLn petscHeader

withPetsc ::
  [String] -> String -> String -> IO a -> IO a
withPetsc a o h = bracket_ (petscInit a o h) petscFin

withPetscMpiComm :: [String] -> String -> String ->
     MPIComm ->
     (MpiCommSize -> MpiCommRank -> IO a) -> 
     IO a
withPetscMpiComm a o h c act =
  withPetsc a o h $ do
    let size = commSize c
        rank = commRank c
    act size rank


isMpiRoot :: MPIComm -> Bool
isMpiRoot c = getMPICommRank c == 0





-- | git commit hash (for debugging)
-- Hp: `git` command is available
{-# noinline gitHash #-}
gitHash :: String
gitHash = unsafePerformIO $ readProcess "git" ["rev-parse","--verify","HEAD"] []




-- | header

sep, petscHeader :: String 
sep = "======"

petscHeader =
  sep ++ "\npetsc-hs : Haskell bindings for PETSc" ++
  "\ncommit " ++ gitHash ++ 
  "\nPETSc " ++ petscVersionString ++ ": initialized"


-- | Version string

{-# NOINLINE petscVersionString #-}
petscVersionString :: String
petscVersionString = ver ++ ", rel. " ++ date
  where
    ver = strs!!3
    date = unwords [strs!!5, strs!!7, strs!!9]
    strs = splitOneOf ", " vstrRaw
    vstrRaw = unsafePerformIO (petscGetVersion 50)

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
