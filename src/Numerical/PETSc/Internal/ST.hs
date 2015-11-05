{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.ST
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | In-place manipulation in the ST monad
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.ST where

import Control.Monad

-- import Linear
-- import Data.Foldable (for_, Foldable)

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

Be warned that modifySTRef does not apply the function strictly. This means if the program calls modifySTRef many times, but seldomly uses the value, thunks will pile up in memory resulting in a space leak. This is a common mistake made when using an STRef as a counter.   To avoid this problem, use modifySTRef' instead.

modifySTRef' :: STRef s a -> (a -> a) -> ST s ()  -- Strict version of modifySTRef-}

{-
new :: a -> ST s (Muv s a)
read :: Muv s a -> ST s a
write :: Muv s a -> a -> ST s ()

thenST :: ST s a -> (a -> ST s b) -> ST s b

-}








--


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
       
-- instance Functor (State s) where
--   fmap = fmapState

-- fmapState f a = State $ \s -> runState f s


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


cloneVector :: Storable t => V.Vector t -> IO (V.Vector t)
cloneVector v = do
        let n = V.length v
        r <- VM.new n
        V.copy r v
        -- r <- createVector n
        -- let f _ s _ d =  copyArray d s n >> return 0
        -- f $ v $ r 
        -- return r
        V.freeze r

-- freezeVector :: (Storable t) => STVector s t -> ST s (Vector t)
-- freezeVector v = liftSTVector id v

unsafeFreezeVector :: (Storable t) => STVector s t -> ST s (V.Vector t)
unsafeFreezeVector (STVector x) = unsafeIOToST . return $ x

{-# INLINE safeIndexV #-}
safeIndexV ::
   Storable c => 
     (STVector s c -> Int -> a) -> STVector a c -> Int -> a
safeIndexV f (STVector v) k
    | k < 0 || k>= dim v = error $ "out of range error in vector (dim="
                                   ++show (dim v)++", pos="++show k++")"
    | otherwise = f (STVector v) k

dim :: Storable a => V.Vector a -> Int
dim = V.length


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

createVector :: Storable a => Int -> IO (V.Vector a)
createVector n = do
    when (n < 0) $ error ("trying to createVector of negative dim: "++show n)
    fp <- doMalloc undefined
    return $ V.unsafeFromForeignPtr fp 0 n
  where
    -- Use the much cheaper Haskell heap allocated storage
    -- for foreign pointer space we control
    doMalloc :: Storable b => b -> IO (ForeignPtr b)
    doMalloc dummy = 
        mallocPlainForeignPtrBytes (n * sizeOf dummy)










vjoin :: Storable t => [V.Vector t] -> IO (V.Vector t)
vjoin [] = return $ V.fromList []
vjoin [v] = return v
vjoin as = do
    let tot = sum (map dim as)
    r <- createVector tot
    unsafeWith r $ \ptr ->
        joiner as tot ptr
    return r
  where joiner [] _ _ = return ()
        joiner (v:cs) _ p = do
            let n = dim v
            unsafeWith v $ \pb -> copyArray p pb n
            joiner cs 0 (advancePtr p n)



    



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
