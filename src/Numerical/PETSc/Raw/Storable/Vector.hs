{-# LANGUAGE BangPatterns #-}
module Numerical.PETSc.Raw.Storable.Vector where

import Numerical.PETSc.Raw.Utils

import Control.Monad

import Foreign.Marshal.Array (peekArray)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types (CInt)
-- import Data.Int(Int64)
-- import Data.Complex
import System.IO.Unsafe (unsafePerformIO)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (fromList, unsafeToForeignPtr, unsafeFromForeignPtr, unsafeWith)



-- C-Haskell vector adapter
{-# INLINE avec #-}
avec :: Storable a => (CInt -> Ptr a -> b) -> VS.Vector a -> b
avec f v = unsafePerformIO (unsafeWith v (return . f (fromIntegral (VS.length v))))
infixl 1 `avec`

safeAvec :: Storable a => (CInt -> Ptr a -> b) -> VS.Vector a -> IO b
safeAvec f v = unsafeWith v (return . f (fromIntegral $ VS.length v))



createVector :: Storable a => Int -> IO (VS.Vector a)
createVector n = do
    -- when (n < 0) $ error ("trying to createVector of negative dim: "++show n)
    (n < 0) ~!~ ("createVector : cannot allocate negative dim : "++show n)
    fp <- doMalloc undefined
    return $ unsafeFromForeignPtr fp 0 n
  where
    -- Use the much cheaper Haskell heap allocated storage
    -- for foreign pointer space we control
    doMalloc :: Storable b => b -> IO (ForeignPtr b)
    doMalloc dummy = 
        mallocPlainForeignPtrBytes (n * sizeOf dummy)

readVector :: Storable a => VS.Vector a -> (Ptr a -> IO b) -> b
readVector v = unsafePerformIO . safeReadVector v
    
safeReadVector :: Storable a => VS.Vector a -> (Ptr a -> IO b) -> IO b
safeReadVector = unsafeWith 

vdim :: Storable a => VS.Vector a -> Int
vdim = VS.length

toList :: Storable a => VS.Vector a -> [a]
toList v = readVector v $ peekArray (vdim v)




--  5 |> [1..]
-- fromList [1.0,2.0,3.0,4.0,5.0]

(|>) :: (Storable a) => Int -> [a] -> VS.Vector a
infixl 9 |>
n |> l
    | length l' == n = fromList l'
    | otherwise      = error "list too short for |>"
  where
    l' = take n l

fromSizedList :: Storable a => Int -> [a] -> VS.Vector a
fromSizedList n l =
  listLongEnoughOrError n l (fromList $ take n l) "fromSizedList : list too short"





-- | access to Vector elements with range checking

atIndex :: Storable a => VS.Vector a -> Int -> a
atIndex v idx = inBoundsOrError idx (0, vdim v) (at' v idx) "@> : index out of bounds"

atIndexSafe, (@:) :: Storable a => VS.Vector a -> Int -> IO a
atIndexSafe v idx =
  inBoundsOrError idx (0, vdim v) (at'' v idx) "@> : index out of bounds"

(@:) = atIndexSafe

-- | access to Vector elements without range checking

at' :: Storable a => VS.Vector a -> Int -> a
at' v n = readVector v $ flip peekElemOff n
{-# INLINE at' #-}

at'' :: Storable a => VS.Vector a -> Int -> IO a
at'' v n = safeReadVector v $ flip peekElemOff n
{-# INLINE at'' #-}

mapVectorM ::
  (Storable a, Storable b) => (a -> IO b) -> VS.Vector a -> IO (VS.Vector b)
mapVectorM f v = do
  w <- return $! unsafePerformIO $! createVector vd -- allocate space
  go w 0 (vd - 1)
  return w
  where
    go u !k !t
      | k == t = do
          x <- unsafeWith v (`peekElemOff` k )
          y <- f x
          unsafeWith u ( \q -> pokeElemOff q k y )
      | otherwise = do
          x <- unsafeWith v (`peekElemOff` k)
          y <- f x
          _ <- unsafeWith u (\q -> pokeElemOff q k y)
          go u (k+1) t
      
    vd = vdim v

mapVectorM0 ::
  (Storable a, Storable b, Monad m) => (a -> m b) -> VS.Vector a -> m (VS.Vector b)
mapVectorM0 f v = do
    w <- return $! unsafePerformIO $! createVector (vdim v)
    go w 0 (vdim v -1)
    return w
    where
      go w' !k !t
        | k == t  = do
           x <- return $! unsafePerformIO $! unsafeWith v (`peekElemOff` k) 
           y <- f x
           return $! unsafePerformIO $! unsafeWith w' ( \q -> pokeElemOff q k y)
        | otherwise = do
           x <- return $! unsafePerformIO $! unsafeWith v (`peekElemOff` k) 
           y <- f x
           _ <- return $! unsafePerformIO $! unsafeWith w' ( \q -> pokeElemOff q k y )
           go w' (k+1) t


