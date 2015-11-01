{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Numerical.PETSc.Internal.Storable.Vector where

import Numerical.PETSc.Internal.Utils

import Control.Monad

-- import GHC.Arr -- for Ix

import Foreign.Marshal.Array (peekArray)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types (CInt)
-- import Data.Int(Int64)
-- import Data.Complex
import System.IO.Unsafe (unsafePerformIO)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import qualified Data.Vector.Storable as V
import Data.Vector.Storable (fromList, unsafeToForeignPtr, unsafeFromForeignPtr, unsafeWith)

-- import qualified Data.Vector.Storable.Mutable as VM



-- -- | C-Haskell vector adapter
-- {-# INLINE avec #-}
-- avec :: Storable a => (CInt -> Ptr a -> b) -> V.Vector a -> b
-- avec f v = unsafePerformIO (unsafeWith v (return . f (fromIntegral (V.length v))))
-- infixl 1 `avec`

-- safeAvec :: Storable a => (Int -> Ptr a -> b) -> V.Vector a -> IO b
-- safeAvec f v = unsafeWith v (return . f (fromIntegral $ V.length v))




createVector :: Storable a => Int -> IO (V.Vector a)
createVector n = do
  fp <- malloc n undefined
  return $ unsafeFromForeignPtr fp 0 n

malloc :: Storable a => Int -> a -> IO (ForeignPtr a)
malloc n d = do
  when (n < 0) $ error ("createVector : cannot allocate negative dim : "++show n)
  mallocPlainForeignPtrBytes (n * sizeOf d)

  
-- readVector :: Storable a => V.Vector a -> (Ptr a -> IO b) -> b
-- readVector v = unsafePerformIO . readVectorSafe v
    
-- readVectorSafe :: Storable a => V.Vector a -> (Ptr a -> IO b) -> IO b
-- readVectorSafe = unsafeWith 



toList :: Storable a => V.Vector a -> [a]
toList = unsafePerformIO . toListSafe

toListSafe :: Storable a => V.Vector a -> IO [a]
toListSafe v = unsafeWith v $ peekArray (V.length v)



--  5 |> [1..]
-- fromList [1.0,2.0,3.0,4.0,5.0]

(|>) :: (Storable a) => Int -> [a] -> V.Vector a
infixl 9 |>
n |> l
    | length l' == n = fromList l'
    | otherwise      = error "list too short for |>"
  where
    l' = take n l

fromSizedList :: Storable a => Int -> [a] -> V.Vector a
fromSizedList n l =
  listLongEnoughOrError n l (fromList $ take n l) "fromSizedList : list too short"





-- | read from Vector elements with range checking

atIndex, (@:) :: Storable a => V.Vector a -> Int -> a
atIndex = (V.!)

{-# INLINE (@:) #-}
(@:) = atIndex




modifyAt :: Storable a => V.Vector a -> Int -> a -> IO ()
modifyAt v i x = unsafeWith v $ \p -> pokeElemOff p i x

-- modifyAt :: Storable a => V.Vector a -> [(Int, a)] -> V.Vector a   -- safe
-- modifyAt = (V.//)




vBounds :: Storable a => V.Vector a -> (Int, Int)
vBounds v = (0, V.length v - 1)

 




-- | maps

-- | map over storable vector : allocate w, map over v (peek from v[i], poke f v[i] into w), return w
-- | which one is faster ? the HMatrix version uses `inlinePerformIO` as seen in Data.Text.Unsafe


mapVectorM ::
  (Storable a, Storable b) => (a -> IO b) -> V.Vector a -> IO (V.Vector b)
mapVectorM f v = do
  w <- createVector vd -- allocate space
  go w 0 (vd - 1)
  return w
  where
    vd = V.length v
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


mapVectorM0 ::
  (Storable a, Storable b, Monad m) => (a -> m b) -> V.Vector a -> m (V.Vector b)
mapVectorM0 f v = do
    w <- return $! unsafePerformIO $! createVector vd
    go w 0 (vd -1)
    return w
    where
      vd = V.length v
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


-- | map over storable vector and passes index to mapping function

mapVectorWithIndex ::
  (Storable a, Storable b) => (Int -> a -> b) -> V.Vector a -> IO (V.Vector b)
mapVectorWithIndex f v = do
    w <- createVector vd
    unsafeWith v $ \p ->
        unsafeWith w $ \q -> do
            let go (-1) = return ()
                go !k = do x <- peekElemOff p k
                           pokeElemOff      q k (f k x)
                           go (k-1)
            go (vd -1)
    return w
      where
        vd = V.length v
{-# INLINE mapVectorWithIndex #-}



buildVectorFromIdxs :: Storable a => Int -> (Int -> a) -> V.Vector a
buildVectorFromIdxs len f =
    fromList $ map f [0 .. (len - 1)]

mapFromList :: Storable b => [a] -> (a -> b) -> V.Vector b
mapFromList l f = fromList (map f l)













-- | zips 

-- | zipWith for Vectors
zipVectorWith ::
  (Storable a, Storable b, Storable c) =>
     (a-> b -> c) -> V.Vector a -> V.Vector b -> V.Vector c
zipVectorWith f u v = unsafePerformIO $ do
    let n = min (V.length u) (V.length v)
    w <- createVector n
    unsafeWith u $ \pu ->
        unsafeWith v $ \pv ->
            unsafeWith w $ \pw -> do
                let go (-1) = return ()
                    go !k = do x <- peekElemOff pu k
                               y <- peekElemOff pv k
                               pokeElemOff      pw k (f x y)
                               go (k-1)
                go (n -1)
    return w
{-# INLINE zipVectorWith #-}

-- | unzipWith for Vectors
unzipVectorWith :: (Storable (a,b), Storable c, Storable d) 
                   => ((a,b) -> (c,d)) -> V.Vector (a,b) -> (V.Vector c,V.Vector d)
unzipVectorWith f u = unsafePerformIO $ do
      let n = V.length u
      v <- createVector n
      w <- createVector n
      unsafeWith u $ \pu ->
          unsafeWith v $ \pv ->
              unsafeWith w $ \pw -> do
                  let go (-1) = return ()
                      go !k   = do z <- peekElemOff pu k
                                   let (x,y) = f z 
                                   pokeElemOff      pv k x
                                   pokeElemOff      pw k y
                                   go (k-1)
                  go (n-1)
      return (v,w)
{-# INLINE unzipVectorWith #-}


-- | zip for Vectors
zipVector ::
  (Storable a, Storable b, Storable (a,b)) =>
    V.Vector a -> V.Vector b -> V.Vector (a,b)
zipVector = zipVectorWith (,)

-- -- | unzip for Vectors
unzipVector ::
  (Storable a, Storable b, Storable (a,b)) =>
    V.Vector (a,b) -> (V.Vector a,V.Vector b)
unzipVector = unzipVectorWith id









-- | testing testing

v1 = 5 |> [3, 2 ..] :: V.Vector Double

v2 = mapVectorM0 (return . (^2)) v1 :: IO (V.Vector Double)
v2u = unsafePerformIO v2

v3 = zipVectorWith (+) v1 v2u




-- -- -- -- --

-- unsafeFromForeignPtr0 :: Storable a => ForeignPtr a -> Int -> VSM.Vector a
-- unsafeFromForeignPtr0 = VSM.unsafeFromForeignPtr0

-- f fp t = do
--   p <- newForeignPtr fp
--   t p

-- f :: Storable a => VS.Vector a -> (Ptr a -> IO b) -> IO b
-- f = unsafeWith
