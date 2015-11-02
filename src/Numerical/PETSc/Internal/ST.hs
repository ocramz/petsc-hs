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

-- import Linear
import Data.Foldable (for_, Foldable)

import Control.Monad.ST (ST, runST)
import Data.STRef

import Foreign.Storable (Storable, peekElemOff, pokeElemOff)
import Control.Monad.ST.Unsafe (unsafeIOToST)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

-- import qualified Numerical.PETSc.Internal.PutGet.Vec as PV


{-
Instances   : Eq (STRef s a)

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

-- newtype ST

-- sumST :: Num a => [a] -> a
sumST :: (Foldable t, Num a) => t a -> a
sumST xs = runST $ do
    n <- newSTRef 0
    for_ xs $ \x ->
        modifySTRef n (+x)
    readSTRef n


---

-- newtype STVector s = STVector PV.PetscVector


-- runSTVector :: Storable t => (forall s . ST s (STVector s t)) -> Vector t
-- runSTVector st = runST (st >>= unsafeFreezeVector)

-- unsafeFreezeVector :: (Storable t) => STVector s t -> ST s (Vector t)
-- unsafeFreezeVector (STVector x) = (unsafeIOToST . return)  x






