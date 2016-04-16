{-# LANGUAGE FlexibleContexts, TypeFamilies, MultiParamTypeClasses #-}
{-# language ParallelListComp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Spec
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Hspec tests
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Spec where


import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.PutGet
import Numerical.PETSc.Internal.Utils


import Foreign
import Foreign.C.Types

import qualified Data.Vector.Storable as VS
-- import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector as V

import Test.Hspec

-- | utils



tests = withPetsc0 $ hspec $ do
  t7
  -- t7



-- | tests

t7 = 
  describe "t7'" $ 
    it "computes the inner product of two orthogonal vectors in R2" t7'

t7' = withVecNew commWorld vd1 $ \e1 ->
  withVecNew commWorld vd2 $ \e2 -> do
    x <- vecDot e1 e2
    x `shouldBe` (0 :: PetscScalar_)
      where
        vd1 = V.convert $ V.fromList [0 , 1]
        vd2 = V.convert $ V.fromList [1 , 0]





