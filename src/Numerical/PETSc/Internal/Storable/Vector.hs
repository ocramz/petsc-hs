{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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

import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Utils
import Numerical.PETSc.Internal.Storable.Store

import Data.Functor
import Control.Applicative

import Control.Exception 
import Control.Monad
-- import qualified GHC.ForeignPtr as FPR
import qualified Foreign.ForeignPtr.Safe as FPR

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Complex
import Foreign.C.Types
import Foreign.Marshal.Array
-- import qualified Foreign.Marshal.Alloc as FMA (malloc, finalizerFree)

-- import Data.Int(Int64)
import System.IO.Unsafe (unsafePerformIO)
-- import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import Data.Complex


import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Generic as VG

import qualified Foreign.Marshal.Utils as FMU







-- | instances

-- instance PStore VS.Vector where
--   toForeignPtr = VS.unsafeToForeignPtr
--   fromForeignPtr = VS.unsafeFromForeignPtr
--   smap = VS.map




-- | methods

vdim :: Storable a => VS.Vector a -> Int
vdim = VS.length




-- | creation

-- createVector :: Storable a => Int -> IO (VS.Vector a)
-- createVector n = do
--   fp <- malloc n undefined
--   return $ VS.unsafeFromForeignPtr fp 0 n



-- mallocCDouble n = malloc n (undefined :: CDouble)

-- -- instance for Storable (Complex CDouble) from `storable-complex`
-- mallocCComplexD n = malloc n (undefined :: Complex CDouble) 


malloc :: Storable a => Int -> a -> IO (FPR.ForeignPtr a)
malloc n d = assert (n>0) $ FPR.mallocForeignPtrBytes n' where
  n' = n * sizeOf d


-- -- NB : according to Foreign.ForeignPtr, it is advisable to use `mallocForeignPtr` ranther than `newForeignPtr` with a finalizer:

createVector :: Storable a => Int -> IO (VS.Vector a)
createVector n = do
    -- fp <- FPR.mallocForeignPtr
    fp <- malloc n undefined
    return $ VS.unsafeFromForeignPtr0 fp n

 -- copyArray : Copy the given number of elements from the second array (source) into the first array (destination); the copied areas may not overlap

cloneVector :: Storable a => VS.Vector a -> IO (VS.Vector a)
cloneVector v = do
  let n = vdim v
  r <- createVector n  
  VS.unsafeWith v $ \vp ->
    VS.unsafeWith r $ \rp -> 
     copyArray rp vp n 
  return r




joinVector :: Storable t => [VS.Vector t] -> IO (VS.Vector t)
joinVector [] = return $ VS.fromList []
joinVector [v] = return v
joinVector as = do
    let tot = sum (map vdim as)
    r <- createVector tot
    VS.unsafeWith r $ \ptr ->
        joiner as tot ptr
    return r
  where joiner [] _ _ = return ()
        joiner (v:cs) _ p = do
            let n = vdim v
            VS.unsafeWith v $ \pb -> copyArray p pb n
            joiner cs 0 (advancePtr p n)







-- | to-from pointers to Storable data


-- Ptr a <-> VS.Vector (bracket'ed)

vectorFreezeFromStorablePtr ::
  Storable a =>
  IO (Ptr a) -> (Ptr a -> IO b) -> Int -> IO (VS.Vector a)
vectorFreezeFromStorablePtr get restore n =
  bracket get restore (getVS n)

vectorCopyToForeignPtr ::
  Storable a =>
  IO (Ptr a) -> (Ptr a -> IO b) -> Int -> VS.Vector a -> IO ()
vectorCopyToForeignPtr get restore n w = bracket get restore (putVS w n) 




-- Ptr <-> Vector.Storable

getVS :: Storable a => Int -> Ptr a -> IO (VS.Vector a)
getVS n p = do
  fp <- FPR.newForeignPtr_  p
  VS.freeze (VM.unsafeFromForeignPtr0 fp n)

putVS :: Storable a => VS.Vector a -> Int -> Ptr a -> IO ()
putVS v n p = do 
  pf <- FPR.newForeignPtr_ p
  VS.copy (VM.unsafeFromForeignPtr0 pf n) v



-- Ptr <-> Vector.Generic

getVG :: (VG.Vector v a, Storable a) => Int -> Ptr a -> IO (v a)
getVG n p = do
  w <- getVS n p
  return $ VG.convert w

putVG :: (VG.Vector v a, Storable a) => v a -> Int -> Ptr a -> IO ()
putVG w = putVS (VG.convert w)


withGetVG :: (VG.Vector v a, Storable a) => Int -> Ptr a -> (v a -> IO b) -> IO b
withGetVG n p = bracket (getVG n p) (\v -> putVG v n p)



-- | modify `n` entries of packed array via local VG.Vector of Storable

_vectorModifyVG :: (Storable a, VG.Vector v a, VG.Vector w a) =>
                   (v a -> w a) -> Int -> Ptr a -> IO ()
_vectorModifyVG fun n p = do
  yim <- fun <$> getVG n p
  putVG yim n p


vectorModifyVG :: (Storable a, VG.Vector v a, VG.Vector w a) =>
                  (v a -> w a) -> Int -> VM.IOVector a -> IO ()
vectorModifyVG fun n vin =
  VM.unsafeWith vin $ \pin -> do
   x <- getVG n pin
   putVG (fun x) n pin
        







-- | building VS.Vector from indices
  

buildVectorFromIdxs ::
  (Storable b, Num a, Enum a) => a -> (a -> b) -> VS.Vector b
buildVectorFromIdxs len f =
    VS.fromList $ map f [0 .. (len - 1)]

-- iota :: (Num a, Storable a, Enum a) => Int -> VS.Vector a
-- iota n = buildVectorFromIdxs n id


mapVectorFromList :: Storable b => [a] -> (a -> b) -> VS.Vector b
mapVectorFromList l f = VS.fromList (map f l)

fromList :: Storable a => [a] -> VS.Vector a 
fromList = VS.fromList

toList :: Storable a => VS.Vector a -> [a]
toList = VS.toList



-- | Storable Vector slice (sub-vector)

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





-- | mutable <-> generic

-- type VMS a = VM.IOVector a 

-- -- fromMutableV :: (VG.Vector v a, Storable a) => VM.IOVector a -> IO (v a)
-- fromMutableV :: (Storable a, VG.Vector v a) => VMS a -> IO (v a)
-- fromMutableV mv = do
--   v <- VS.freeze mv
--   return (VG.convert v)

-- toMutableV :: (VG.Vector v a, Storable a) => v a -> IO (VM.IOVector a)
-- toMutableV = VS.thaw . VG.convert

-- withVG :: (VG.Vector v a, Storable a) =>
--               v a ->
--               (VM.IOVector a -> VM.IOVector a) ->
--               IO (v a)
-- withVG v f = do
--   x <- toMutableV v
--   fromMutableV (f x)

-- asdf v f = VM.unsafeWith v (f . castPtr)








-- | testing testing

v0 :: (Storable a, Num a, Enum a) => VS.Vector a
v0 = 10 |> [0 .. 9]
