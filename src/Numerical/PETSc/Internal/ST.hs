{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.ST
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | In-place manipulation in the ST monad
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.ST where

import Numerical.PETSc.Internal.Storable.Vector

-- import Linear
-- import Data.Foldable (for_, Foldable)
import Data.Functor
import Control.Applicative
import Control.Monad

import Control.Monad.ST (ST, runST)
import Data.STRef

import Foreign
-- import Foreign.ForeignPtr
import GHC.ForeignPtr

-- import Foreign.Storable (Storable, peekElemOff, pokeElemOff)

import Control.Monad.ST.Unsafe (unsafeIOToST)

import qualified Data.Vector.Storable as V
import Data.Vector.Storable (unsafeWith)
import qualified Data.Vector.Storable.Mutable as VM

-- import Control.Monad.Primitive -- for V.copy

-- import qualified Numerical.PETSc.Internal.PutGet.Vec as PV





{-

(State (Managed a)

-}




{-
newSTRef ::
   a -> ST s (STRef s a)    --Build a new STRef in the current state thread

readSTRef ::
   STRef s a -> ST s a     -- Read the value of an STRef

writeSTRef ::
   STRef s a -> a -> ST s ()      -- Write a new value into an STRef

modifySTRef ::
   STRef s a -> (a -> a) -> ST s ()     -- Mutate the contents of an STRef.

Be warned that modifySTRef does not apply the function strictly.
This means if the program calls modifySTRef many times, but seldomly uses the value,
thunks will pile up in memory resulting in a space leak.
This is a common mistake made when using an STRef as a counter.
To avoid this problem, use modifySTRef' instead.

modifySTRef' :: STRef s a -> (a -> a) -> ST s ()  -- Strict version of modifySTRef-}

{-
new :: a -> ST s (Muv s a)
read :: Muv s a -> ST s a
write :: Muv s a -> a -> ST s ()

thenST :: ST s a -> (a -> ST s b) -> ST s b

-}








--

modifyV0 :: Storable a => V.Vector a -> (a -> a) -> V.Vector a
modifyV0 v f = runST $ do
  x <- newSTRef v
  modifySTRef x (V.map f)
  readSTRef x



-- | some basics: State and StateT

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  (State m) >>= k = State $ \s ->
    let (v, s') = m s
    in runState (k v) s'
       
fmapp :: Functor f => (a -> b) -> StateT s f a -> StateT s f b
fmapp f m = StateT $ \ s ->
         (\ ~(a, s') -> (f a, s')) <$> runStateT m s







newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}


returnStateT :: Monad m => a -> StateT s m a
returnStateT a = StateT $ \s -> return (a, s)

bindStateT :: Monad m => StateT s m a -> (a -> StateT s m b) -> StateT s m b
(StateT x) `bindStateT` k = StateT $ \s -> do
  (v, s') <- x s
  runStateT (k v) s'

instance Monad m => Monad (StateT s m) where
  return = returnStateT
  (>>=) = bindStateT


-- -- --







-- from HMatrix : Internal.ST


{-# INLINE ioReadV #-}
ioReadV :: Storable t => V.Vector t -> Int -> IO t
ioReadV v k = unsafeWith v $ \s -> peekElemOff s k

{-# INLINE ioWriteV #-}
ioWriteV :: Storable t => V.Vector t -> Int -> t -> IO ()
ioWriteV v k x = unsafeWith v $ \s -> pokeElemOff s k x

newtype STVector s t = STVector (V.Vector t)

runSTVector :: Storable t => (forall s . ST s (STVector s t)) -> V.Vector t
runSTVector st = runST (st >>= unsafeFreezeVector)




thawVector :: Storable t => V.Vector t -> ST s (STVector s t)
thawVector = unsafeIOToST . fmap STVector . cloneVector

unsafeThawVector :: Storable t => V.Vector t -> ST s (STVector s t)
unsafeThawVector = unsafeIOToST . return . STVector



{-# INLINE unsafeReadVector #-}
unsafeReadVector :: Storable t => STVector s t -> Int -> ST s t
unsafeReadVector   (STVector x) = unsafeIOToST . ioReadV x

{-# INLINE unsafeWriteVector #-}
unsafeWriteVector :: Storable t => STVector s t -> Int -> t -> ST s ()
unsafeWriteVector  (STVector x) k = unsafeIOToST . ioWriteV x k

-- {-# INLINE modifyVector #-}
-- modifyVector :: (Storable t) => STVector s t -> Int -> (t -> t) -> ST s ()
-- modifyVector x k f = readVector x k >>= return . f >>= unsafeWriteVector x k

liftSTVector :: (Storable t) => (V.Vector t -> a) -> STVector s t -> ST s a
liftSTVector f (STVector x) = (unsafeIOToST . fmap f . cloneVector) x




-- freezeVector :: (Storable t) => STVector s t -> ST s (Vector t)
-- freezeVector v = liftSTVector id v

unsafeFreezeVector :: (Storable t) => STVector s t -> ST s (V.Vector t)
unsafeFreezeVector (STVector x) = unsafeIOToST . return $ x

{-# INLINE safeIndexV #-}
safeIndexV ::
   Storable c => 
     (STVector s c -> Int -> a) -> STVector a c -> Int -> a
safeIndexV f (STVector v) k
    | k < 0 || k>= vdim v = error $ "out of range error in vector (dim="
                                   ++show (vdim v)++", pos="++show k++")"
    | otherwise = f (STVector v) k



-- {-# INLINE readVector #-}
-- readVector :: Storable a => STVector s a -> Int -> s
-- readVector = safeIndexV unsafeReadVector

-- {-# INLINE writeVector #-}
-- writeVector :: Storable t => STVector s t -> Int -> t -> ST s ()
-- writeVector = safeIndexV unsafeWriteVector



{-# INLINE newVector #-}
newVector :: Storable t => t -> Int -> ST s (STVector s t)
newVector x n = do
    v <- newUndefinedVector n
    let go (-1) = return v
        go !k = unsafeWriteVector v k x >> go (k-1 :: Int)
    go (n-1)

newUndefinedVector :: Storable t => Int -> ST s (STVector s t)
newUndefinedVector = unsafeIOToST . fmap STVector . createVector
















    



---

-- newtype STVector s = STVector PV.PetscVector


-- runSTVector :: Storable t => (forall s . ST s (STVector s t)) -> Vector t
-- runSTVector st = runST (st >>= unsafeFreezeVector)

-- unsafeFreezeVector :: (Storable t) => STVector s t -> ST s (Vector t)
-- unsafeFreezeVector (STVector x) = (unsafeIOToST . return)  x






-- | testing testing

-- newtype Vtest a = Vtest {unV :: Int} deriving Show

-- v0 = Vtest 0

-- testf0 = runST $ do
--   x <- newSTRef v0
--   writeSTRef x (Vtest 1)
--   readSTRef x

-- main = print testf0


-- -- sumST :: Num a => [a] -> a
-- sumST :: (Foldable t, Num a) => t a -> a
-- sumST xs = runST $ do
--     n <- newSTRef 0
--     for_ xs $ \x ->
--         modifySTRef n (+x)
--     readSTRef n
