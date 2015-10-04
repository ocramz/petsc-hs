{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
module Numerical.PETSc.Test2_1
       -- (
       --   withPetsc0
       -- )
       where

import Control.Concurrent
import Control.Exception

-- import Numerical.PETSc.Types
import           Data.Functor ((<$>))
import Foreign
import Control.Monad
-- import           Foreign.C.String (peekCString)
import Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable

import Numerical.PETSc.Internal2
import Language.C.Inline as C

context petscCtx

-- include "<petscksp.h>"
C.include "<petscksp.h>"
C.include "<petsctao.h>"

-- * Vec

vecCreate :: Comm -> IO Vec
vecCreate c = withPtr (vC c) >>= handleErrTup where
  vC comm p = [C.exp|int{VecCreate($(int cx), $(Vec *p))} |]
    where cx = unComm comm

vecDestroy :: Vec -> IO CInt
vecDestroy p = with p vD where 
  vD px = [C.exp|int{VecDestroy($(Vec *px))}|]

withVec :: Comm -> (Vec -> IO a) -> IO a
withVec c = bracket (vecCreate c) vecDestroy

commWorld :: Comm
commWorld = Comm $ unsafePerformIO commWorld' where
  commWorld' = [C.exp| int{ MPI_COMM_WORLD }  |]
 
petscInit0 = [C.block| int{ PetscInitializeNoArguments();  }|]

petscFin = [C.block| int{ PetscFinalize(); }|]  

withPetsc0 f = do {petscInit0; f; petscFin}

-- class PETSCVec

-- -- errors

handleErrTup :: (a, CInt) -> IO a
handleErrTup (res, ie)
  | ie /= 0 = throwIOErr ie
  | otherwise = return res

handleErr :: CInt -> IO ()
handleErr 0 = return ()
handleErr n = throwIOErr n

throwIOErr :: CInt -> IO a
throwIOErr n = throwIO (ErrorCall $ "PETSc error " ++ show (n' :: Int)) where
  n' = fromIntegral (n :: CInt)
-- throwIOErr _ = return ()

-- handleErr f = f >>= handleIntErr

-- handleIntErr :: Monad m => CInt -> m ()
-- handleIntErr = decodeErr . convErrCode


dIO f = do
  e <- f
  unless (e==0) $ throwIO (ErrorCall $ "error" ++ show e) where

-- f = dIO petscFin  `catch` \e -> print (e :: NotInitialized)

-- -- exceptions
  

data PetscException = NotInitialized
                    | Finalized
                    | Other
                    deriving (Show,Typeable)
instance Exception PetscException

-- f1 = try petscInit0

-- -- async

-- data Async a = Async (MVar a)

-- async :: IO a -> IO (Async a)
-- async act = do
--   v <- newEmptyMVar
--   forkIO (do r <- act; putMVar v r)
--   return $ Async v

-- wait :: Async a -> IO a
-- wait (Async v) = readMVar v -- NB: readMVar: multiple wait calls to same MVar possible

data Async a = Async (MVar (Either SomeException a))

async' :: IO a -> IO (Async a)
async' act = do
  var <- newEmptyMVar
  forkIO $ do
    r <- try act
    putMVar var r
  return $ Async var

waitCatch :: Async t -> IO (Either SomeException t)
waitCatch (Async v) = readMVar v

wait' :: Async b -> IO b
wait' a = do
  r <- waitCatch a
  case r of
    Left e -> throwIO e
    Right x -> return x


-- --

bracketPetsc = bracket_ petscInit0 petscFin

-- t0' = do
--   v <- vecCreate commWorld
--   vecDestroy v

-- t0 = bracketPetsc t0'





-- t0 = do
--   a1 <- async' petscInit0 -- WRONG, pair must be bracketed
--   a2 <- async' petscFin --  "
--   r1 <- wait' a1
--   r2 <- wait' a2
--   print (r1, r2)

-- $ ghc -c Main.hs
-- $ cc -c Main.c -o Main_c.o
-- $ ghc Foo.hs
-- $ ghc Bar.hs
-- $ cc -c Bar.c -o Bar_c.o
-- $ ghc Main.o Foo.o Bar.o Main_c.o Bar_c.o -lm -o Main
