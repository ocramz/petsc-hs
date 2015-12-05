{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.SlepcMisc
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | SLEPc Mid-level interface miscellaneous functions
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.SlepcMisc
       (slepcInit0, slepcInit, slepcFin, withSlepc, withSlepc0,
        slepcVersionString)
       where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

import Control.Exception

import Foreign
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String

import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import System.IO.Unsafe (unsafePerformIO)



slepcInit :: Argv -> OptsStr -> HelpStr -> IO ()
slepcInit a o h = chk0 $ slepcInitialize' a o h

slepcInit0, slepcFin :: IO ()
slepcInit0 = do 
  chk0 slepcInit01
  putStrLn (slepcHeader ++ " with default options\n")
slepcFin = chk0 slepcFin1 >> putStrLn ("\nSLEPc : finalized\n" ++ sep)


-- | FIXME: move into specialized monad
withSlepc0 :: IO a -> IO a
withSlepc0 = bracket_ slepcInit0 slepcFin

withSlepc :: Argv -> OptsStr -> HelpStr -> IO a -> IO a
withSlepc a o h = bracket_ (slepcInit a o h) slepcFin



sep, slepcHeader :: String 
sep = "======"

slepcHeader =
  sep ++ "\npetsc-hs : Haskell bindings for PETSc\n" ++
  "\nSLEPc " ++ slepcVersionString ++ ": initialized"

-- | SLEPc version string

{-# NOINLINE slepcVersionString #-}
slepcVersionString :: String
slepcVersionString = drop 6 $ unsafePerformIO $ slepcGetVersion 50

slepcGetVersion :: Int -> IO String
slepcGetVersion l = do
  fp <- mallocPlainForeignPtrBytes l  -- see Data.Bytestring.Internal.create
  withForeignPtr fp $ \p -> do
    pgv p (toCInt l)
    peekCString p
    where
     pgv v sz = chk0 (slepcGetVersion0' v sz)
