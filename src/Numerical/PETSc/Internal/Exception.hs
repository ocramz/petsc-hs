{-# LANGUAGE DeriveDataTypeable, TypeFamilies, FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Exception
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Exception handling for calls wrapped by PutGet interfaces
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Exception where

import Foreign.C.Types
import Foreign.Ptr
import Foreign

import Data.Maybe
import Data.Functor
import Data.Typeable

import Control.Exception
import Control.Concurrent
-- import Control.Concurrent.Async

import Control.Monad
-- import Control.Monad.Trans.Reader
-- import Control.Monad.Trans

-- import Control.Arrow hiding (left, right)



-- | Petsc error codes and strings (from $PETSC/src/sys/error/err.c )

data PetscErrCode_ =
  OutOfMemory | UnsupportedOperOnThisType | UnsupportedOperOnThisSystem | WrongOperationOrder | SignalReceived
  | NonconformingObjSizes | InvalidArgument | ArgOutOfRange | CorruptArgument
  | OpenFileFailed
  | ReadFileFailed | WriteFileFailed | PointerInvalid | ArgsMustHaveSameType
  | InaccessiblePointerLocation | ZeroPivotInLUFact | FloatException
  | ObjIsInWrongState | PetscObjectCorrupted | IncompatibleArgs
  | ExternalLibraryErr | PetscGeneratedInconsistentData | MemoryCorruption
  | UnexpectedDataInFile | ArgsMustBeOnSameComm
  | ZeroPivotInCholeskyFact | OverflowIntegerOper | ValidPointerExpected
  | UnknownType | NotUsed
  | SysCallError | ObjTypeNotSet | NoError | UndefinedException
  deriving (Eq, Show, Typeable)

instance Exception PetscErrCode_


petscErrCodeFromInt n =
  case n of 55 -> OutOfMemory
            56 -> UnsupportedOperOnThisType
            57 -> UnsupportedOperOnThisSystem
            58 -> WrongOperationOrder
            59 -> SignalReceived
            60 -> NonconformingObjSizes
            62 -> InvalidArgument
            63 -> ArgOutOfRange
            64 -> CorruptArgument
            65 -> OpenFileFailed
            66 -> ReadFileFailed
            67 -> WriteFileFailed
            68 -> PointerInvalid
            69 -> ArgsMustHaveSameType
            70 -> InaccessiblePointerLocation
            71 -> ZeroPivotInLUFact
            72 -> FloatException
            73 -> ObjIsInWrongState
            74 -> PetscObjectCorrupted
            75 -> IncompatibleArgs
            76 -> ExternalLibraryErr
            77 -> PetscGeneratedInconsistentData
            78 -> MemoryCorruption
            79 -> UnexpectedDataInFile
            80 -> ArgsMustBeOnSameComm
            81 -> ZeroPivotInCholeskyFact
            84 -> OverflowIntegerOper
            85 -> ValidPointerExpected
            86 -> UnknownType
            87 -> NotUsed
            88 -> SysCallError
            89 -> ObjTypeNotSet
            0  -> NoError
            -- n  -> ErrorCall $ show n
            _  -> UndefinedException -- WATCH OUT HERE



errCodeInRange n = (n < maxErrCode && n >= minErrCode) || n == 0 || n==256 where
  maxErrCode = 90
  minErrCode = 55


throwPetscException n = throwIO (petscErrCodeFromInt n)

{- bracket' before after action = 
     mask $ \restore -> do
      a <- chk1 before
      r <- restore (action a) `onException` chk0 (after a)
      _ <- after a
      return r        -}

bracketChk a o = bracket (chk1 a) (chk0 . o)  

chk1 :: IO (a, CInt) -> IO a
chk1 act = do
  r <- act
  let (v, e) = r
  if errCodeInRange e
    then
     case e of 0 -> return v
               256 -> return v
               m -> throwPetscException m
    else error ("unforeseen case : " ++ show e)


chk0 :: IO CInt -> IO ()
chk0 act = do
  e <- act
  if errCodeInRange e
    then
     case e of 0 -> return ()
               256 -> return ()
               m -> throwPetscException m
    else  error ("unforeseen case : " ++ show e)             





-- from HMatrix.Internal.Devel

-- -- | check the error code
-- check :: String -> IO CInt -> IO ()
-- check msg f = do
-- --  finit
--     err <- f
--     when (err/=0) $ error (msg++": "++errorCode err)
--     return ()


-- infixl 1 #
-- a # b = apply a b
-- {-# INLINE (#) #-}

-- -- | postfix error code check
-- infixl 0 #|
-- (#|) = flip check

-- -- | Error capture and conversion to Maybe
-- mbCatch :: IO x -> IO (Maybe x)
-- mbCatch act = E.catch (Just `fmap` act) f
--     where f :: SomeException -> IO (Maybe x)
--           f _ = return Nothing













-- -- with EitherT

-- type PetscIO a = IO (Either PetscError a)

-- runCheck' :: Checkable a => IO a -> PetscIO (OutType a)
-- runCheck' act = runEitherT (check' act)

-- check' act = do
--     res <- lift act
--     let s = status res
--         v = proj res
--     case s of 0 -> right v
--               _ -> left $ PetscError (fromIntegral s)

-- -- with ReaderT, EitherT

-- check'' act = do
--   res <- lift $ liftIO act
--   a <- ask
--   let s = status res
--       v = proj res
--   lift $ case s of 0 -> right v
--                    _ -> left $ PetscError (fromIntegral s)


-- check''' :: PetscCheckable a => IO a -> EitherT PetscError' IO (PetscOutType a)
-- check''' act = do
--   res <- liftIO act
--   let s = checkStatus res
--       v = projStatus res
--   case s of NoError -> right v
--             m -> left $ PError m

-- runCheck''' :: (PetscCheckable a) => IO a -> PetscIO' (PetscOutType a)
-- runCheck''' a = runEitherT (check''' a)

-- type PetscIO' a = IO (Either PetscError' a )



-- bracketCheck''' alpha omega =
--   bracket (runCheck'''  alpha)  (runCheck''' . omega )

{-
-- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
-- bracket before after thing =
--   mask $ \restore -> do
--     a <- before
--     r <- restore (thing a) `onException` after a
--     _ <- after a
--     return r

-- onException :: IO a -> IO b -> IO a
-- onException io what = io `catch` \e -> do _ <- what
--                                           throwIO (e :: SomeException)

-- -- | Given a pair of actions, one to perform in case of failure, and one to 
-- -- perform in case of success, run an 'EitherT' and get back a monadic result.
-- eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
-- eitherT f g (EitherT m) = m >>= \z -> case z of
--   Left a -> f a
--   Right b -> g b
-}








------             


-- newtype PetscIO'' a = PetscIO'' { runPetscIO'' :: EitherT PetscError' IO a}






------


-- -- -- -- --



-- -- | Perform an IO action that returns a tuple with an integer status
-- -- and some return values, processing errors.
-- chk :: Checkable a => IO a -> Access (OutType a)
-- chk act = do
--   res <- lift $ liftIO $ act
--   let st = status res
--       val = proj res
--   (f, p) <- ask
--   lift $ if st == 0
--     then right val
--     else left $ NcError f st (nc_strerror st) p


-------- from haskell-mpi
  

-- checkError :: CInt -> IO ()
-- checkError code = do
--    -- We ignore the error code from the call to Internal.errorClass
--    -- because we call errorClass from checkError. We'd end up
--    -- with an infinite loop if we called checkError here.
--    (_, errClassRaw) <- errorClass code
--    let errClass = cToEnum errClassRaw
--    unless (errClass == Success) $ do
--       errStr <- errorString code
--       throwIO $ MPIError errClass errStr

-- -- | Convert MPI error code human-readable error description. Corresponds to @MPI_Error_string@.
-- errorString :: CInt -> IO String
-- errorString code =
--   allocaBytes (fromIntegral maxErrorString) $ \ptr ->
--     alloca $ \lenPtr -> do
--        -- We ignore the error code from the call to Internal.errorString
--        -- because we call errorString from checkError. We'd end up
--        -- with an infinite loop if we called checkError here.
--        _ <- errorString' code ptr lenPtr
--        len <- peek lenPtr
--        peekCStringLen (ptr, fromIntegral len)
--   where
--     errorString' = {# call unsafe Error_string as errorString_ #}
