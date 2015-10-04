{-# LANGUAGE FlexibleInstances, ConstraintKinds, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | PETSc store instance for Storable Vectors.
module Numerical.PETSc.Raw.Vector where

import Numerical.PETSc.Raw.Store

import Data.Vector.Storable as SV




instance PStore SV.Vector where
  toForeignPtr = fst . SV.unsafeToForeignPtr0
  fromForeignPtr p s = SV.unsafeFromForeignPtr0 p (Prelude.product s)
  smap = SV.map
