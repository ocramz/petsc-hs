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


specs :: IO ()
specs = withPetsc0 $ hspec $ do
  t_vecDot_r2_1
  t_linSys_r3_1
  t_linSys_3x4_1



-- | tests

com = commWorld

t_vecDot_r2_1 = 
  describe "t_vecDot_r2_1" $ 
    it "computes the inner product of two orthogonal vectors in R2" $
 withVecNew com vd1 $ \e1 ->
  withVecNew com vd2 $ \e2 -> do
    x <- vecDot e1 e2
    x `shouldBe` (0 :: PetscScalar_)
      where
        vd1 = V.convert $ V.fromList [0 , 1]
        vd2 = V.convert $ V.fromList [1 , 0]


t_linSys_r3_1 = describe "t_linSys_r3_1" $
  it "solves a 3x3 linear system" $
   withPetscMatrix com m n ixd nz InsertValues $ \mat ->
    withVecNew com vd $ \rhs -> do
     let (_, _, _, mu) = fromPetscMatrix mat
     withKspSetupSolveAlloc com KspGmres mu mu rhs $ \ksp soln -> do
       x <- vecGetVS soln
       let xl = V.toList $ V.convert x
       xl `shouldBe` ([1,1,1] :: [PetscScalar_]) 
      where
        (m, n) = (3, 3)
        vd = V.fromList [3, 7, 18]
        ixd = listToCSR m n [1,2,0,0,3,4,5,6,7]
        nz = ConstNZPR (3,3)

t_linSys_3x4_1 = describe "t_linSys_3x4_1" $
  it "solves a 3x4 linear system" $
   withPetscMatrix com m n ixd nz InsertValues $ \mat ->
    withVecNew com vd $ \rhs -> do
     let (_, _, _, mu) = fromPetscMatrix mat
     withKspSetupSolveAlloc com KspGmres mu mu rhs $ \ksp soln -> do
       x <- vecGetVS soln
       let xl = V.toList $ V.convert x
       xl `shouldBe` ([1,1,1,1] :: [PetscScalar_]) 
      where
        (m, n) = (3, 4)
        vd = V.fromList [7, 14, 16]
        ixd = listToCSR m n [3,4,0,1,2,0,5,7,9,3,1,3]
        nz = ConstNZPR (m, n)





