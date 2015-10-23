{-# LANGUAGE DeriveDataTypeable, TypeFamilies, FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Raw.Exception
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  provisional
--
-- | Exception handling for calls wrapped by PutGet interfaces
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Raw.Exception where

import Foreign.C.Types
import Foreign.Ptr
import Foreign

import Data.Maybe
import Data.Functor
import Data.Typeable

import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans

import Control.Arrow hiding (left, right)

-- -- Checkable

-- -- -- taken from HNetCDF/Utils.hs
-- https://hackage.haskell.org/package/hnetcdf-0.3.0.0/src/Data/NetCDF/Utils.hs

class Checkable a where
  type OutType a :: *
  status :: a -> Int
  proj :: a -> OutType a

instance Checkable CInt where
  type OutType CInt = ()
  status = fromIntegral
  proj _ = ()

instance Checkable (a, CInt) where
  type OutType (a, CInt) = a
  status (_, s) = fromIntegral s
  proj (x, _) = x

instance Checkable (a, (a, CInt)) where
  type OutType (a, (a, CInt)) = (a, a)
  status (_, (_, s)) = fromIntegral s
  proj (x, (y, _)) = (x, y)

-- to be used more or less like:

-- -- -- | Simple synonym to tidy up signatures.
-- type PetscIO a = IO (Either PetscError a)

-- -- -- -- | Monad stack to help with handling errors from FFI functions.
-- type Access a = ReaderT (String, FilePath) (EitherT PetscError IO) a

-- -- -- | Utility function to run access monad stack.
-- runAccess :: String -> String -> Access a -> PetscIO a
-- runAccess f p = runEitherT . flip runReaderT (f, p)

--

{- -- -- reminders :
-- liftIO :: MonadIO m => IO a -> m a                 -- Control.Monad.IO.Class
-- lift :: (MonadTrans t, Monad m) => m a -> t m a    -- Control.Monad.Trans.Class
-- right :: Monad m => a -> EitherT e m a             -- ..Trans.Either
-- left :: Monad m => e -> EitherT e m a              -- "
-- ask :: Monad m => ReaderT r m r                    -- Control.Monad.Trans.Reader
-- tell :: (Monoid w, Monad m) => w -> WriterT w m () -- .. Trans.Writer
-}


withAsyncWait :: IO a -> (a -> IO b) -> IO b
withAsyncWait io act = withAsync io $ \a -> do
  r <- wait a
  act r 

check :: Checkable a => IO a -> IO (OutType a)
check act = 
  withAsyncWait act $ \res -> do
   let s = status res
       v = proj res
   case s of 0 -> return v
             _ -> throw $ PetscError (fromIntegral s)

     
bracketCheck :: (Checkable a, Checkable b) =>
                IO a -> (OutType a -> IO b) -> (OutType a -> IO c) -> IO c 
bracketCheck alpha omega = bracket (check alpha) (check . omega)


-- -- --


class PetscCheckable a where
  type PetscOutType a :: *
  checkStatus :: a -> PetscErrCode_
  projStatus :: a -> PetscOutType a

instance PetscCheckable CInt where
  type PetscOutType CInt = ()
  checkStatus = petscErrCodeFromInt
  projStatus _ = ()

instance PetscCheckable (a, CInt) where
  type PetscOutType (a, CInt) = a
  checkStatus (_, s) = petscErrCodeFromInt s
  projStatus (x, _) = x

  

checkE act = do
  res <- act
  let s = checkStatus res
      v = projStatus res
  case s of NoError -> return v
            n       -> throw $ PError n

bracketCheckErr alpha omega action =
  bracket (checkE alpha) (checkE omega) (checkE action)


  

-- -- datatypes

data PetscError = PetscError
                  { petscErrCode :: CInt } deriving (Eq, Show, Typeable)
instance Exception PetscError

-- 
data PetscError' = PError {errCode :: PetscErrCode_ }
              deriving (Eq, Show, Typeable)
instance Exception PetscError'

-- mkPetscError' = PError . petscErrCodeToString







-- -- -- -- --
-- | adapted from OpenCLRaw 
wrapError :: IO CInt -> IO (Maybe PetscErrCode_)
wrapError thunk = thunk >>= \errcode -> do
  let pe = petscErrCodeFromInt errcode
  if pe == NoError
    then return Nothing
    else return $ Just pe


wrapErrorEither thunk = alloca $ \errorP -> do
    ret <- thunk errorP
    err <- petscErrCodeFromInt <$> peek errorP
    if err == NoError
        then return . Right $ ret
        else return . Left $ err 

wrapGetX c f = alloca $ \p -> do
  pd  <- (mallocForeignPtrBytes . fromIntegral $ c)
  ret <- wrapError $ withForeignPtr pd f
  -- if isNothing ret
  --   then peek p >>= \valsz -> return $ (pd, valsz)
  --   else throwIO $ fromJust ret 
  maybe (peek p >>= \v -> return (pd, v)) throwIO ret

-- nest :: [(r -> a) -> a] -> ([r] -> a) -> a
-- nest xs = runCont (sequence (map Cont xs))

-- withCStringArray0 :: [String] -> (Ptr CString -> IO a) -> IO a
-- withCStringArray0 strings act = nest (map withCString strings)
--                                      (\rs -> withArray0 nullPtr rs act)

-- -- -- -- -- 











-- | from `judy`
-- -- ( https://hackage.haskell.org/package/judy-0.2.2/docs/Data-Judy.html )
-- {-
newtype JudyL a = JudyL { unJudyL :: MVar (ForeignPtr JudyL_) }
type JudyL_ = Ptr JudyLArray
data JudyLArray

instance Show (JudyL a) where show _ = "<Judy a>"

-- new :: JE a => IO (JudyL a)
new = do
    -- we allocate the structure on the Haskell heap (just a pointer)
    fp <- mallocForeignPtrBytes (sizeOf (undefined :: Ptr Word))
    -- note that the Haskell GC doesn't really know costly the arrays are.
    addForeignPtrFinalizer finalizerFree fp
    withForeignPtr fp $ \p -> poke p (castPtr nullPtr)
    -- and make it threadsafe.
    mv <- newMVar fp
    return $! JudyL mv
                              
{-

foreign import ccall "&hs_judyl_free"
    c_judyl_free_ptr :: FunPtr (Ptr JudyL_ -> IO ())

foreign import ccall unsafe "JudyLIns"
    c_judy_lins :: Ptr JudyL_ -> Key -> JError -> IO (Ptr Word)

-- | Insert a key and value pair into the JudyL array.
-- Any existing key will be overwritten.
insert :: JE a => Key -> a -> JudyL a -> IO ()
insert k v m = do
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
        v_ptr <- c_judy_lins p (fromIntegral k) nullError
        if v_ptr == judyErrorPtr
            then memoryError
            else poke v_ptr =<< toWord v
{-# INLINE insert #-}


-- JudyLGet: read a value from a JudyL array
--
-- > JudyLGet(PJLArray, Index, &JError)
-- used as:
-- > #define JLG(PValue, PJLArray, Index)  \
-- >   PValue = JudyLGet(PJLArray, Index, PJE0)
--
-- Get the pointer PValue associated with Index in the PJLArray Judy array.
-- Return PValue pointing to Value. Return PValue set to NULL if the Index was
-- not present. Return PValue set to PJERR if a malloc() fail occured.
--
foreign import ccall unsafe "JudyLGet"
    c_judy_lget :: JudyL_ -> Key -> JError -> IO (Ptr Word)


-- | Update a value at a specific key with the result of the provided
-- function. When the key is not a member of the map, no change is made.
adjust :: JE a => (a -> a) -> Key -> JudyL a -> IO ()
adjust f k m = do
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
        q     <- peek p                  -- get the actual judy array
        v_ptr <- c_judy_lget q (fromIntegral k) nullError
        if v_ptr == judyErrorPtr
            then memoryError
            else if v_ptr == nullPtr
                    then return ()
                    else do
                        old_v <- fromWord =<< peek v_ptr
                        new_v <- toWord (f old_v)
                        poke v_ptr new_v
 -}

adju f k m get_c = 
  withMVar m $ \mp ->
    withForeignPtr mp $ \p -> do
      q <- peek p
      cp <- get_c q k
      when (cp /= nullPtr) $
       do
         oldv <- peek cp
         newv <- f oldv
         poke cp newv
      -- if cp /= nullPtr
      --   then do
      --    oldv <- peek cp
      --    newv <- f oldv
      --    poke cp newv
      --   else return ()
        
{-
foreign import ccall unsafe "JudyLDel"
    c_judy_ldel :: Ptr JudyL_ -> Key -> JError -> IO CInt

-- | Delete the Index\/Value pair from the JudyL array.
--
delete :: Key -> JudyL a -> IO ()
delete k m = do
    withMVar (unJudyL m) $ \m_ ->
      withForeignPtr m_ $ \p -> do
        i <- c_judy_ldel p (fromIntegral k) nullError
        if i == judyError then memoryError else return ()
-}

dele k m del_c prop = 
  withMVar m $ \mp ->
    withForeignPtr mp $ \p -> do
      i <- del_c p k
      when (prop i) $ return ()














-- -- -- -- -- 





errCodeInRange n = (n < maxErrCode && n > minErrCode) || n == 0 || n==256 where
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

chk1 act = do
  r <- act
  let (v, e) = r
  if errCodeInRange e
    then
     case e of 0 -> return v
               256 -> return v
               m -> throwPetscException m
    else error ("unforeseen case : " ++ show e)

chk0 act = do
  e <- act
  if errCodeInRange e
    then
     case e of 0 -> return ()
               256 -> return ()
               m -> throwPetscException m
    else  error ("unforeseen case : " ++ show e)             




-- chk1 act = do
--   r <- act
--   let (v, e) = r
--   case e of 0 -> return v
--             m -> throwPetscException m

-- chk0 act = do
--   n <- act
--   case n of 0 -> return ()
--             m -> throwPetscException m



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

-- | Petsc error strings (from $PETSC/src/sys/error/err.c )
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
