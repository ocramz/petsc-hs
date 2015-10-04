{-# LANGUAGE ForeignFunctionInterface #-}
module Tests where

-- import Numerical.PETSc.Types

import Foreign
import Foreign.C.Types
import Control.Monad

v1 :: Storable t2 => (t -> Ptr t2 -> IO t1) -> t -> IO (t1, t2)
v1 f c =
  alloca $ \p -> 
    f c p >>= \res ->
       peek  p >>= \ptrA -> 
         return (res, ptrA)

-- vecCreate'_ :: CInt -> Ptr (Ptr (Vec)) -> IO CInt

v1d f a1 =
  with a1 $ \p -> 
    f p >>= \res ->
       return res

v1d' f a =
 with a $ f >=> \r -> return r

newtype St a b = St {unSt :: b -> (a, b)}

class PMonad m where
  unitP :: a -> m p p a
  bindP :: m p q a -> (a -> m q r b) -> m p r b
  
newtype LockM p q a = LockM { unLockM :: IO a}
