{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Raw.Exception
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  provisional
--
-- | miscellaneous utilities
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Raw.Utils where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign

import Control.Monad
import Control.Arrow


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












c ~!~ es = when c (error es)

-- ifThenElse :: Bool -> a -> a -> a
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

listLongEnough n l = ifThenElse (length (take n l) == n)

-- listLongEnoughOrError n l f es
--   | length l' == n = f
--   | otherwise = error es where
--       l' = take n l

-- inBoundsOrError n (a,b) f es
--   | n >= a && n <= b = f
--   | otherwise = error es



ifNegError :: Int -> String -> a -> a
ifNegError n es = ifNeg n (error es)


listLongEnoughOrError n l f es
  | length l' == n = f
  | otherwise = error es where
      l' = take n l

inBoundsOrError n (a,b) f es
  | n >= a && n <= b = f
  | otherwise = error es







-- * misc

linspace' n a b = take n [a, a + dt .. ] where
  dt = (b-a) / fromIntegral n

-- linspace1 n a b = mv  where
--   mv1 = take n [a, a+dt ..]
--   mv2 = reverse $ take n [b, b-dt ..]
--   mv = Data.List.map (\(x,y) -> 1/2 * (x+y)) $ zip mv1 mv2
--   dt = (b-a) / fromIntegral n

mean x = sum x / fromIntegral (length x)



sndM :: Monad m => m (a, b) -> m b
sndM = liftM snd

fstM :: Monad m => m (a, b) -> m a
fstM = liftM fst

fst2 x = ((y1, y2), t ) where
  y1 = fst x
  y2 = (fst . snd) x
  t = (snd . snd) x

fst2M :: Monad m => (a, (b, c)) -> m ((a, b), c)
fst2M = return . fst2

-- -- tuple unpacking stuff

-- fst2 :: (a, (b, c)) -> (a,b)
fstOf2 = fst . snd
-- snd2 :: (a, (b, c)) -> c
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
