{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Utils
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
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

import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Control.Monad
import Control.Arrow

-- import GHC.Arr -- Ix
import qualified Data.Ix as Ix
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG



-- | [String] -> C arrays

withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings ss f = case ss of
  [] -> f []
  (s:ss') -> withCString s $ \cs -> 
    withCStrings ss' $ \css -> f (cs:css)

withCStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray ss f = withCStrings ss $ \css -> withArray css f

withCStringArrayPtr :: [String] -> (Ptr (Ptr CString) -> IO a) -> IO a
withCStringArrayPtr ss f = withCStringArray ss $ \css -> with css f





-- | C array -> String

getCString :: Int -> (Ptr CChar -> IO a) -> IO (String, a)
getCString len io = 
  allocaArray len $ \p -> do
    x <- io p
    s <- peekCString p
    return (s, x)




-- * lists

filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap _ _ [] = []
filterMap p f (x:xs)
  | p x = f x : filterMap p f xs
  | otherwise = filterMap p f xs




-- * list sorting (hmm..)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort l ++ [x] ++ qsort r where
  l = filter (< x) xs
  r = filter (>= x) xs




-- * error control

(~!~), (~?~) :: Monad m => Bool -> String -> m ()
c ~!~ es = when c (error es)

c ~?~ es = unless c (error es)


  










-- * Vector


-- | filterMapV : if head of vector satisfies a predicate, apply a function to it

filterMapV :: (a -> Bool) -> (a -> b) -> V.Vector a -> V.Vector b
filterMapV _ _ vv
  | V.null vv = V.empty
filterMapV p f vv
  | p x = V.cons (f x) $ filterMapV p f (V.tail vv) 
  | otherwise = filterMapV p f (V.tail vv)
  where
      x = V.head vv



-- * traversal

traverseFilter_ :: Monad m => (a -> Bool) -> (a -> m ()) -> V.Vector a -> m ()
traverseFilter_ p mf = V.mapM_ mg where
  mg x | p x = mf x
       | otherwise = return ()








-- * indexing and integer ranges, Data.Ix derived




inRange0 :: Int -> Int -> Bool
inRange0 l = Ix.inRange (0, l-1)

safeIndices :: [a] -> [Int] -> Bool
safeIndices w = all (inRange0 (length w))

safeIndicesV :: V.Vector a -> V.Vector Int -> Bool
safeIndicesV w = V.all (inRange0 (V.length w))





-- * index-data vector for sparse matrix assembly : V.Vector (Int, Int, a) 

filterMapSafeIndicesV ::
  Int -> Int -> ((Int, Int, a) -> b) -> V.Vector (Int, Int, a) -> V.Vector b
filterMapSafeIndicesV m n = filterMapV (\(i,j,_) -> in0m m i && in0m n j)










-- * indexing





listLongerThan :: Int -> [a] -> Bool
listLongerThan n l = length (take n l) == n

-- listLongEnoughOrError n l f es
--   | length l' == n = f
--   | otherwise = error es where
--       l' = take n l

-- inBoundsOrError n (a,b) f es
--   | n >= a && n <= b = f
--   | otherwise = error es

-- allInBounds b = all (inBounds' b)

inBounds' :: Ord a => (a, a) -> a -> Bool
inBounds' = flip inBounds

inBounds :: Ord a => a -> (a, a) -> Bool
inBounds !n !(a,b) = n >= a && n <= b



in0m :: Int -> Int -> Bool
in0m m i = i >= 0 && i < m

allIn0m :: Int -> [Int] -> Bool
allIn0m = all . in0m

allIn0mV :: Int -> V.Vector Int -> Bool
allIn0mV = V.all . in0m

inBoundsOrError :: Ord a => a -> (a, a) -> t -> String -> t
inBoundsOrError n b x es 
  | inBounds n b = x
  | otherwise = error es


  




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












-- |-- tuple unpacking stuff

sndM :: Monad m => m (a, b) -> m b
sndM = liftM snd

fstM :: Monad m => m (a, b) -> m a
fstM = liftM fst

fst2 :: (a, (b, x)) -> ((a, b), x)
fst2 (y1, (y2, t)) = ((y1, y2), t)




-- tuple reordering

snoc2 :: Monad m => (t1, (t2, t)) -> m ((t1, t2), t)
snoc2 x = do
  let (a, (b, z)) = x
  return ((a, b), z)

snoc3 :: Monad m => (t1, (t2, (t3, t))) -> m ((t1, t2, t3), t)
snoc3 x = do
  let (a, (b, (c, z))) = x
  return ((a, b, c), z)

snoc4 :: Monad m => (t1, (t2, (t3, (t4, t)))) -> m ((t1, t2, t3, t4), t)
snoc4 x = do
  let (a, (b, (c, (d, z)))) = x
  return ((a,b,c,d), z)


-- -- used in DMDAGetCornersX and DMDAGetGhostCornersX :

f1d :: (t1, (t2, t)) -> ((t1, t2), t)
f1d (a, (b, c)) = ((a, b), c)

f2d :: (t1, (t2, (t3, (t4, t)))) -> (((t1, t2), (t3, t4)), t)
f2d (a,(b,(c,(d,e)))) = (((a,b), (c, d)), e)

f3d :: (t1, (t2, (t3, (t4, (t5, (t6, t)))))) -> (((t1, t2, t3), (t4, t5, t6)), t)
f3d (a, (b, (c, (d, (e, (f, g)))))) = (((a, b, c), (d, e, f)), g)



-- projections
  
fstOf2 :: (a, (c, b)) -> c
fstOf2 = fst . snd

sndOf2 :: (a1, (a, c)) -> c
sndOf2 =  snd . snd



both' :: Arrow a => a b' c' -> a (b', b') (c', c')
both' f =  f *** f

both :: (a, a) -> (a -> b) -> (b, b)
both (a, b) f = (f a, f b)

bothFst2 :: (a -> b) -> (a, (a, t)) -> ((b, b), t)
bothFst2 f (a, (b, c)) = (both t1 f, c) where
  t1 = (a, b)

-- mapFst2 f g (a, (b, c)) = ((f a, g b), c)



fmapBoth :: (Functor f) => (a -> b) -> (f a, f a) -> (f b, f b)
fmapBoth f (a, b) = (fmap f a, fmap f b)


-- vFmapBoth f (va, vb) = (V.map f va, V.map f vb)

-- fmapBoth' f = fmap f *** fmap f  -- so arrow, much category theory, wow

all3 :: (a, a, a) -> (a -> b) -> (b, b, b)
all3 (a,b,c) f = (f a, f b, f c)

bothM :: Monad m => (a, a) -> (a -> b) -> m (b, b)
bothM t f = return (both t f)

bothMf :: Monad m => (a -> b) -> (a, a) -> m (b, b)
bothMf = flip bothM




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


fromIntegralTup :: (CInt, CInt) -> (Int, Int)
fromIntegralTup t = both t fi

fromIntegralTup2 :: ((CInt, CInt),(CInt, CInt)) -> ((Int, Int), (Int, Int))
fromIntegralTup2 t = both t fromIntegralTup

fromIntegralTup3 :: ((CInt, CInt, CInt),(CInt, CInt, CInt)) -> ((Int, Int, Int), (Int, Int, Int))
fromIntegralTup3 t = both t (`all3` fi)













-- | lift pure function in get/set monad

liftF1 ::
  (Functor m, Monad m) =>
  (a -> b) ->           -- pure unary f
  (d -> m a) ->         -- getter
  (d -> b -> m e) ->    -- setter
  d ->
  m e
liftF1 f getter setter b = do
  y <- f <$> getter b
  setter b y

liftF1' ::             -- ", no Functor constraint
  Monad m =>
  (a -> b) ->          -- pure unary f
  (d -> m a) ->        -- getter
  (d -> b -> m e) ->   -- setter
  d ->
  m e
liftF1' f get set b = get b >>= \x -> set b (f x)








-- | callbacks : forget Ptr () that usually comes as last argument. Lexical context is better than implicit state.

-- maybe a GADT would look better here? 

lanp1 :: (t1 -> Ptr a -> t) -> t1 -> t
lanp1 f x = f x nullPtr

lanp2 :: (t1 -> t2 -> Ptr a -> t) -> t1 -> t2 -> t
lanp2 f x y = f x y nullPtr

lanp3 :: (t1 -> t2 -> t3 -> Ptr a -> t) -> t1 -> t2 -> t3 -> t
lanp3 f x y z = f x y z nullPtr

lanp4 :: (t1 -> t2 -> t3 -> t4 -> Ptr a -> t) -> t1 -> t2 -> t3 -> t4 -> t
lanp4 f a b c d = f a b c d nullPtr
lanp5 f a b c d e = f a b c d e nullPtr
lanp6 f a b c d e x = f a b c d e x nullPtr

-- | forget last arg of function and return (0 :: CInt) . Really

-- wrapCb1 :: Monad m => (t -> Ptr a1 -> m a) -> t -> m CInt
wrapCb1 x f = return0 (lanp1 f x)

-- wrapCb2 :: Monad m => (t -> t1 -> Ptr a1 -> m a) -> t -> t1 -> m CInt
wrapCb2 x y f = return0 (lanp2 f x y)

-- wrapCb3 :: Monad m => (t -> t1 -> t2 -> Ptr a1 -> m a) -> t -> t1 -> t2 -> m CInt
wrapCb3 x y z f = return0 (lanp3 f x y z)

-- wrapCb4 :: Monad m => (t -> t1 -> t2 -> t3 -> Ptr a1 -> m a) -> t -> t1 -> t2 -> t3 -> m CInt
wrapCb4 f x y z w = return0 (lanp4 f x y z w)
wrapCb5 f x y z w a = return0 (lanp5 f x y z w a)
wrapCb6 f x y z w a b = return0 (lanp6 f x y z w a b)









-- | C-style ("return 0") function adapters





return0 :: Monad m => m a -> m CInt
return0 m = do
  m
  return (0 :: CInt)


-- nested `with`
-- cInt7Adapt f s v1 v2 v3 pb1 pb2 pv = return0 $
--   with pb1 $ \b1 ->
--   with pb2 $ \b2 -> 
--   f s v1 v2 v3 b1 b2 pv









-- |  C <-> Haskell numeric formats

fi :: CInt -> Int
fi = fromIntegral

fs :: CSize -> Int
fs = fromIntegral

toCInt :: Int -> CInt
toCInt = CInt . fromIntegral

fromCDouble :: CDouble -> Double
fromCDouble (CDouble x) = x

toCDouble :: Double -> CDouble
toCDouble = CDouble




-- | classes

