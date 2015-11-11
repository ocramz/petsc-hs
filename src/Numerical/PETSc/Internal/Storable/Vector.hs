{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Storable.Vector
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | operations on Vector.Storable
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Storable.Vector where

import Numerical.PETSc.Internal.Utils
import Numerical.PETSc.Internal.Storable.Store

import Control.Monad
import Foreign.Marshal.Array (peekArray)
import qualified GHC.ForeignPtr as FPR
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
-- import Data.Int(Int64)
-- import Data.Complex
import System.IO.Unsafe (unsafePerformIO)
-- import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import Data.Complex

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS


-- | instances

-- instance PStore VS.Vector where
--   toForeignPtr = VS.unsafeToForeignPtr
--   fromForeignPtr = VS.unsafeFromForeignPtr
--   smap = VS.map




-- | methods

vdim :: Storable a => VS.Vector a -> Int
vdim = VS.length




-- | creation

createVector :: Storable a => Int -> IO (VS.Vector a)
createVector n = do
  fp <- malloc n undefined
  return $ VS.unsafeFromForeignPtr fp 0 n

createVectorSized :: Storable a => Int -> a -> IO (VS.Vector a)
createVectorSized n x  = do
  fp <- malloc n x
  return $ VS.unsafeFromForeignPtr fp 0 n

malloc :: Storable a => Int -> a -> IO (FPR.ForeignPtr a)
malloc n d = do
  when (n < 0) $ error ("createVector : cannot allocate negative dim : "++show n)
  FPR.mallocPlainForeignPtrBytes (n * sizeOf d)


buildVectorFromIdxs ::
  (Storable b, Num a, Enum a) => a -> (a -> b) -> VS.Vector b
buildVectorFromIdxs len f =
    VS.fromList $ map f [0 .. (len - 1)]

iota :: (Num a, Storable a, Enum a) => a -> VS.Vector a
iota n = buildVectorFromIdxs n id


mapVectorFromList :: Storable b => [a] -> (a -> b) -> VS.Vector b
mapVectorFromList l f = VS.fromList (map f l)



toList :: Storable a => VS.Vector a -> [a]
toList = VS.toList



subVector :: Storable t => Int       -- ^ index of the starting element
                        -> Int       -- ^ number of elements to extract
                        -> VS.Vector t  -- ^ source
                        -> VS.Vector t  -- ^ result
subVector = VS.slice

  






-- | indexing into a Vector :
--   Vector.! and Vector.!? : read from Vector elements with range checking

atIndex, (@:) :: Storable a => VS.Vector a -> Int -> a
atIndex = (VS.!)

{-# INLINE (@:) #-}
(@:) = atIndex

{-# INLINE safeRead #-}
safeRead :: Storable a => VS.Vector a -> (Ptr a -> IO a) -> a
safeRead v = unsafePerformIO . VS.unsafeWith v

at' :: Storable a => VS.Vector a -> Int -> a
at' v n = safeRead v $ flip peekElemOff n 

at :: Storable a => VS.Vector a -> Int -> a
at v n
  | in0m (vdim v) n = at' v n
  | otherwise = error "at : index out of bounds"


slice :: Storable a => Int -> Int -> VS.Vector a -> VS.Vector a
slice = VS.slice










-- | create Storable Vector from sized list

--  5 |> [1..]
-- fromList [1.0,2.0,3.0,4.0,5.0]

(|>) :: Storable a => Int -> [a] -> VS.Vector a
infixl 9 |>
n |> l
    | length l' == n = VS.fromList l'
    | otherwise      = error "list too short for |>"
  where
    l' = take n l






-- | modify Storable Vector

modifyVectorAt, (//) :: Storable a => VS.Vector a -> [(Int, a)] -> VS.Vector a
modifyVectorAt = (VS.//)

(//) = modifyVectorAt








-- | function adapter if either argument is :t `a` rather than `V.Vector a` 

adaptScalar :: (Storable a, Storable b) =>
     (a -> VS.Vector b -> x) ->
     (VS.Vector a -> VS.Vector b -> x) ->
     (VS.Vector a -> b  -> x) ->
     VS.Vector a ->
     VS.Vector b ->
     x
adaptScalar f1 f2 f3 x y
    | vdim x == 1 = f1   (x @: 0) y
    | vdim y == 1 = f3 x (y @: 0)
    | otherwise = f2 x y








-- | Vector.mapM

mapVectorM0 ::
  (Storable a, Storable b, Monad m) => (a -> m b) -> VS.Vector a -> m (VS.Vector b)
mapVectorM0 f v = do
    w <- return $! unsafePerformIO $! createVector vd
    go w 0 (vd -1)
    return w
    where
      vd = vdim v
      go w' !k !t
        | k == t  = do
           x <- return $! unsafePerformIO $! VS.unsafeWith v (`peekElemOff` k) 
           y <- f x
           return $! unsafePerformIO $! VS.unsafeWith w' ( \q -> pokeElemOff q k y)
        | otherwise = do
           x <- return $! unsafePerformIO $! VS.unsafeWith v (`peekElemOff` k) 
           y <- f x
           _ <- return $! unsafePerformIO $! VS.unsafeWith w' ( \q -> pokeElemOff q k y )
           go w' (k+1) t











-- | Vector.imapM in IO : 

mapVectorWithIndex ::
  (Storable a, Storable b) => (Int -> a -> b) -> VS.Vector a -> IO (VS.Vector b)
mapVectorWithIndex f v = do
    w <- createVector vd
    VS.unsafeWith v $ \p ->
        VS.unsafeWith w $ \q -> do
            let go (-1) = return ()
                go !k = do x <- peekElemOff p k
                           pokeElemOff      q k (f k x)
                           go (k-1)
            go (vd -1)
    return w
      where
        vd = vdim v
{-# INLINE mapVectorWithIndex #-}








-- | real <-> complex

-- | transforms a complex vector into a real vector with alternating real and imaginary parts 
asReal ::
  (RealFloat a, Storable a, Storable (Complex a)) =>
  VS.Vector (Complex a) -> VS.Vector a
asReal v = VS.unsafeFromForeignPtr (FPR.castForeignPtr fp) (2*i) (2*n)
    where (fp,i,n) = VS.unsafeToForeignPtr v

-- | transforms a real vector into a complex vector with alternating real and imaginary parts
asComplex :: (RealFloat a, Storable a, Storable (Complex a)) =>
             VS.Vector a -> VS.Vector (Complex a)
asComplex v = VS.unsafeFromForeignPtr (FPR.castForeignPtr fp) (i `div` 2) (n `div` 2)
    where (fp,i,n) = VS.unsafeToForeignPtr v









-- | testing testing

v0 :: (Storable a, Num a, Enum a) => VS.Vector a
v0 = 10 |> [0 .. 9]
