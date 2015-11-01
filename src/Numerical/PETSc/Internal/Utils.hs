{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Utils
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | Miscellaneous utilities
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Utils where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign

import Control.Monad
import Control.Arrow

import GHC.Arr -- Ix





withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings ss f = case ss of
  [] -> f []
  (s:ss') -> withCString s $ \cs -> 
    withCStrings ss' $ \css -> f (cs:css)

withCStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray ss f = withCStrings ss $ \css -> withArray css f

withCStringArrayPtr :: [String] -> (Ptr (Ptr CString) -> IO a) -> IO a
withCStringArrayPtr ss f = withCStringArray ss $ \css -> with css f




-- vFromC l p = do
--   ptr <- newForeignPtr_ p
--   V.freeze $ VM.unsafeFromForeignPtr0 ptr l 

-- vectorFromC :: Storable a => Int -> Ptr a -> IO (V.Vector a)
-- vectorFromC len ptr = do
--   ptr' <- newForeignPtr_ ptr
--   V.freeze $ VM.unsafeFromForeignPtr0 ptr' len

-- vectorToC :: Storable a => V.Vector a -> Int -> Ptr a -> IO ()
-- vectorToC vec len ptr = do
--   ptr' <- newForeignPtr_ ptr
--   V.copy (VM.unsafeFromForeignPtr0 ptr' len) vec




-- * array sorting (hmm..)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort l ++ [x] ++ qsort r where
  l = filter (< x) xs
  r = filter (> x) xs





(~!~) :: Monad m => Bool -> String -> m ()
c ~!~ es = when c (error es)

ifThenElse :: Bool -> a -> a -> a
ifThenElse q i e
  | q = i
  | otherwise = e

ifThenElseE q a b
  | q = Left a
  | otherwise = Right b





-- indexing

ifNegE n = ifThenElseE (n<0)

listTooShortE n l = ifThenElseE (length (take n l) /= n)

listTooShortEorError n l = listTooShortE n l (error "list too short") l




ifNeg :: Int -> a -> a -> a
ifNeg n = ifThenElse (n < 0)

listLongerThan :: Int -> [a] -> Bool
listLongerThan n l = length (take n l) == n

-- listLongEnoughOrError n l f es
--   | length l' == n = f
--   | otherwise = error es where
--       l' = take n l

-- inBoundsOrError n (a,b) f es
--   | n >= a && n <= b = f
--   | otherwise = error es

inBounds :: Ord a => a -> (a, a) -> Bool
inBounds !n !(a,b) = n >= a && n <= b

inBoundsOrError :: Ord a => a -> (a, a) -> t -> String -> t
inBoundsOrError n b x es 
  | inBounds n b = x
  | otherwise = error es


  


ifNegError :: Int -> String -> a -> a
ifNegError n es = ifNeg n (error es)


listLongEnoughOrError n l f es
  | length l' == n = f
  | otherwise = error es where
      l' = take n l

-- inBoundsOrError :: Ord a => a -> (a, a) -> t -> String -> t
-- inBoundsOrError n (a,b) f es
--   | n >= a && n <= b = f
--   | otherwise = error es


-- inBounds :: 

-- | tests whether smallest element of list is in bounds

inBoundsUnsortedList lx (x1, x2)
  | validX = s
  | otherwise = error "inBoundsUnsortedList : invalid bounds x1 >= x2 "where
  (l1, l2) = extremaUnsortedList lx
  s = l1 >= x1 && l2 <= x2
  validX = x1 < x2

extremaUnsortedList lx = (head lxs, last lxs) where
  lxs = qsort lx




-- * misc

linspace1, linspace1a :: (Enum a, Fractional a) => Int -> a -> a -> [a]
linspace1 n a b = take n [a, a + dt .. ] where
  dt = (b-a) / fromIntegral n

linspace1a n a b = mv  where
  mv1 = take n [a, a+dt ..]
  mv2 = reverse $ take n [b, b-dt ..]
  mv = map (\(x,y) -> 1/2 * (x+y)) $ zip mv1 mv2
  dt = (b-a) / fromIntegral n

mean :: Fractional a => [a] -> a
mean x = sum x / fromIntegral (length x)



sndM :: Monad m => m (a, b) -> m b
sndM = liftM snd

fstM :: Monad m => m (a, b) -> m a
fstM = liftM fst

fst2 :: (a, (b, x)) -> ((a, b), x)
fst2 x = ((y1, y2), t ) where
  y1 = fst x
  y2 = (fst . snd) x
  t = (snd . snd) x

fst2M :: Monad m => (a, (b, c)) -> m ((a, b), c)
fst2M = return . fst2

-- -- tuple unpacking stuff


fstOf2 = fst . snd

sndOf2 =  snd . snd

both' f =  f *** f

both (a, b) f = (f a, f b)

bothF (a, b) f = (fmap f a, fmap f b)

all3 (a,b,c) f = (f a, f b, f c)

bothM t f = return (both t f)


-- withTup'' (a, b) f g = (f a, g b)

-- withTup (a, b) f g = (f *** g) (a, b)

-- withTup' :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
-- withTup' f g = f *** g

-- firstM'' f (a, b) = fmap (, b) (f a)  

(_1) f (a, b) =
  f a >>= \x -> return (x, b) -- a special case of _1 from Control.Lens

(_2) f (a, b) =
  f b >>= \y -> return (a, y) -- _2, "


-- -- utils

-- isSorted x = all (\(a,b) -> a <= b) $ zip x (tail x)



fromIntegralTup t = both t fi
fromIntegralTup2 t = both t fromIntegralTup
fromIntegralTup3 t = both t (`all3` fi)


fi :: CInt -> Int
fi = fromIntegral

toCInt :: Int -> CInt
toCInt = CInt . fromIntegral
