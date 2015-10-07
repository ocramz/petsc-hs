module Numerical.PETSc.Raw.Storable.Vector where

import Control.Monad

-- import Foreign.Marshal.Array
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
    when (n < 0) $ error ("trying to createVector of negative dim: "++show n)
    fp <- doMalloc undefined
    return $ unsafeFromForeignPtr fp 0 n
  where
    -- Use the much cheaper Haskell heap allocated storage
    -- for foreign pointer space we control
    doMalloc :: Storable b => b -> IO (ForeignPtr b)
    doMalloc dummy = 
        mallocPlainForeignPtrBytes (n * sizeOf dummy)

safeReadVector :: Storable a => VS.Vector a -> (Ptr a -> IO b) -> b
safeReadVector v = unsafePerformIO . readVector v
    
readVector :: Storable a => VS.Vector a -> (Ptr a -> IO b) -> IO b
readVector = unsafeWith 
