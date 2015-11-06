{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Storable.Vector
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
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
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
-- import Data.Int(Int64)
-- import Data.Complex
-- import System.IO.Unsafe (unsafePerformIO)
-- import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS


-- | instances

-- instance PStore VS.Vector where
--   toForeignPtr = VS.unsafeToForeignPtr
--   fromForeignPtr = VS.unsafeFromForeignPtr
--   smap = VS.map



-- log_b a = log_c a / log_c b


-- ln a = log10 a / log10 e





-- | methods






-- | indexing into a Vector :
--   Vector.! and Vector.!? : read from Vector elements with range checking

atIndex, (@:) :: V.Vector a -> Int -> a
atIndex = (V.!)

{-# INLINE (@:) #-}
(@:) = atIndex







-- | create Vector from sized list

--  5 |> [1..]
-- fromList [1.0,2.0,3.0,4.0,5.0]

(|>) :: Int -> [a] -> V.Vector a
infixl 9 |>
n |> l
    | length l' == n = V.fromList l'
    | otherwise      = error "list too short for |>"
  where
    l' = take n l







-- | function adapter if either argument is :t `a` rather than `V.Vector a` 

adaptScalar ::
     (a -> V.Vector b -> x) ->
     (V.Vector a -> V.Vector b -> x) ->
     (V.Vector a -> b  -> x) ->
     V.Vector a ->
     V.Vector b ->
     x
adaptScalar f1 f2 f3 x y
    | V.length x == 1 = f1   (x @: 0) y
    | V.length y == 1 = f3 x (y @: 0)
    | otherwise = f2 x y
