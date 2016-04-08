{-# LANGUAGE FlexibleInstances, ConstraintKinds, TypeFamilies, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Storable.Store
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | classes for store polymorphism
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Storable.Store where

-- | from HNetCDF

-- | The /store polymorphism/ for the functions to get the values of
-- NetCDF variables relies on a simple `NcStore` typeclass for
-- converting between /store/ values and @ForeignPtr@s.

import Foreign.Storable
import Foreign.ForeignPtr

import GHC.Exts

-- class PStore s where
--   toForeignPtr :: Storable e => s e -> (ForeignPtr e, Int, Int)
--   fromForeignPtr :: Storable e => ForeignPtr e -> Int -> Int -> s e
--   smap :: (Storable a, Storable b) => (a -> b) -> s a -> s b





class PetscStore s where
  type PetscStoreExtraCon s a :: Constraint
  type PetscStoreExtraCon s a = ()
  toForeignPtr :: (Storable e, PetscStoreExtraCon s e) =>
                  s e -> ForeignPtr e
  fromForeignPtr :: (Storable e, PetscStoreExtraCon s e) =>
                    ForeignPtr e -> [Int] -> s e
  smap :: (Storable a, Storable b, PetscStoreExtraCon s a, PetscStoreExtraCon s b) =>
          (a -> b) -> s a -> s b


-- -- | Class representing containers suitable for storing values read
-- -- from NetCDF variables.  Just has methods to convert back and forth
-- -- between the store and a foreign pointer, and to perform simple
-- -- mapping over the store.  The NcStoreExtraCon associated
-- -- constraint-kinded type is used to track extra class constraints on
-- -- elements needed for some store types.
-- class NcStore s where
--   type NcStoreExtraCon s a :: Constraint
--   type NcStoreExtraCon s a = ()
--   toForeignPtr :: (Storable e, NcStoreExtraCon s e) =>
--                   s e -> ForeignPtr e
--   fromForeignPtr :: (Storable e, NcStoreExtraCon s e) =>
--                     ForeignPtr e -> [Int] -> s e
--   smap :: (Storable a, Storable b, NcStoreExtraCon s a, NcStoreExtraCon s b) =>
--           (a -> b) -> s a -> s b



  
class Storable a => FromStorable a b where
  fromStorable :: Ptr a -> V.Vector a
