{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Storable.StorableContainer
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Storable container class
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Storable.StorableContainer
       -- (StorableContainer(..)
       -- )
       where

-- import Numerical.PETSc.Internal.Types
-- import Numerical.PETSc.Internal.PutGet.Vec

import Control.Monad

import Data.Complex
import Data.Ix
import qualified Data.Vector as V

import Data.IORef

-- | mtl
import Control.Monad.Managed
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class

import Foreign.Storable
import Foreign.C.Types

import Control.Exception



-- | statically encoded dimensions

-- data Nat = Z | S Nat deriving (Eq, Show)

-- natToInt :: Nat -> Int
-- natToInt Z = 0
-- natToInt (S x) = 1 + natToInt x

-- newtype Dim2 = Dim2 { unDim2 :: (Nat, Nat)}



-- --




-- -- | algebraic hierarchy from Blanco et al. `Towards a functional run-time for dense NLA domain`, FHPC'13

-- -- class (Eq e, Fractional e) => Element e where
-- class Num e => Element e where
--   conj :: e -> e
--   conj = id      -- default

-- instance Element Int
-- instance Element Double
-- instance Element CDouble
-- instance Element Float
-- instance RealFloat e => Element (Complex e) where
--   conj = conjugate

-- -- data TransposedState = NotTransposed | Transposed deriving (Eq, Show)

-- {-- | NB : complex conjugation and transposition are aspects of a more general concept, that of Hermitian transposition.
-- -}

-- -- class Transposable t where
-- --   data Transposed -- = T | NT

-- class (Functor c, Element e) => Container c e where
--   type IxC :: *    -- Int for Vectors, (Int, Int) for Matrices ..
--   type DimC :: *
--   generateC :: DimC -> (IxC -> e) -> c e
--   selectC :: c e -> IxC -> e   -- or Maybe e ?
--   dimC :: c e -> DimC
--   subC :: c e -> IxC -> IxC -> c e
--   mapC :: (e -> e) -> c e -> c e

-- -- instance Element a => Container [] a where
-- --   type IxC = Int
-- --   type DimC = Int
-- --   dimC = length
-- --   selectC = (!!)
-- --   subC l a b = drop a (take b l)
-- --   mapC = map
-- --   generateC n f = map f [0 .. n-1]

-- -- -- example :
-- -- -- generateC 3 (^2) :: [Int]



-- class Container c e => Vector c e where
--   fromListV :: [e] -> c e
--   toListV :: c e -> [e]
--   concatV :: [c e] -> c e
--   foldrV :: (e -> a -> a) -> a -> c e -> a  -- NB : works IFF args have same dims
--   zipWithV :: (e -> e -> e) -> c e -> c e -> c e -- "



-- class Container c e => Matrix c e where
--   fromListM :: Int -> Int -> [e] -> c e
--   -- transposeM :: c e -> c e    -- NB : transposition shd be reflected in type








-- | `Element` class from Data.Packed.Internal.Matrix

-- class Functor f => Element f a where
--   type ElementIx a :: *
--   elementRange :: f a -> ElementIx a -> ElementIx a -> f a
--   elementConst :: a -> f a

-- instance Element [] a where
--   type ElementIx a = Int
--   elementRange xs i1 i2
--     | length xs >= i2 && i2 >= i1 = drop i1 (take i2 xs) 
--     | otherwise = error "elementRange [a] : indices out of range"
--   elementConst = flip replicate







-- | `Container` class from Data.Packed.Internal.Matrix

-- class (Functor c, Element c e) => Container c e where
--   -- type IndexOf (c :: * -> *) :: *
--   type SizeOf (c :: * -> *) :: *
--   -- type ArgOf (c :: * -> *) a
--   containerSize :: c e -> SizeOf c
--   containerConst :: e -> c e
--   scale :: e -> c e -> c e







-- | IxContainer

-- class Container c a => IxContainer c a where
--   type IndexOf (c :: * -> *) :: *






-- | StorableContainer

-- | types that can be exchanged between PETSc and GHC RTS
-- NB :
-- first type : PETSc side (Storable types in Internal.Types)
-- third type : Haskell side (e.g. PetscVector in PutGet.Vec)

-- class (Storable p, Monad m) => StorableContainer p m a where
--   type SCInfo p 
--   type SCLocal p
--   initP :: SCInfo p -> m p
--   updateH :: p -> m (SCLocal p)
--   updateP :: p -> SCLocal p -> m ()
--   withP :: p -> (SCLocal p -> m b) -> m b
--   destroyP :: p -> m ()

{-
-- | usage example :  
instance StorableContainer Vec IO a where
  type PObjInfo Vec = VecInfo
  type PObjLocal Vec a = V.Vector a
-}

class Storable p => StorableContainer p where
  type SCInfo p 
  type SCLocal p
  type SCRemote p
  initRemote :: SCInfo p -> IO (SCRemote p)
  updateLocal :: SCRemote p -> IO (SCLocal p)
  updateRemote :: SCRemote p -> SCLocal p -> IO ()
  withRemote :: SCRemote p -> (SCLocal p -> IO b) -> IO b
  destroyRemote :: SCRemote p -> IO ()

-- instance StorableContainer Vec where
--   type SCInfo Vec = VecInfo
--   type SCLocal Vec = VS.Vector PetscScalar_
--   type SCRemote Vec = Vec
--   initRemote = vecCreateMPIInfo  -- must import PutGet.Vec for these ..
--   updateLocal = vecGetVector
--   updateRemote = vecRestoreVector
--   withRemote v = bracket (vecGetVector v) (vecRestoreVector v)
--   destroyRemote = vecDestroy







-- | StorableIxContainer

-- class (StorableContainer sc, Ix i) => StorableIxContainer i sc x where
--   sicGet :: sc -> i -> IO x
--   sicPut :: sc -> i -> x -> IO ()


-- class (Storable p) => StorableIxContainer p a where
--   -- indexing depends on `p` :
--   -- e.g. for Vec : SCIdx p == Int
--   -- --   for Mat : SCIdx p == (Int, Int) , etc.
--   type SCIdx p 
--   type SCInfo p
--   type SCLocal p a
--   newSIC :: SCInfo p -> SCLocal p a -> IO p
--   readSIC :: p -> SCIdx p -> IO a
--   writeSIC :: p -> SCIdx p -> a -> IO ()
--   modifySIC :: p -> SCIdx p -> (a -> a) -> IO ()

















-- | ManagedContainer

-- class (Monad m, Storable r) => ManagedContainer m r where
--   type MCInfo r
--   type MCLocType r
--   manageP :: MCInfo r -> (MCInfo r -> IO (MCLocType r)) -> (MCLocType r -> IO a) -> Managed (MCLocType r)
--   manageP info get set = managed (bracket (get info) set)


-- asdf info ini fin = managed (bracket (ini info) fin)


-- withSizeArray :: (Storable a, Integral a) => [a] -> (Ptr CULong -> IO b) -> IO b
-- withSizeArray = withArray . liftM fromIntegral
