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
import Numerical.PETSc.Internal.C2HsGen.TypesC2HsGen
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
specs = 
  withPetsc0 $
   withSlepc0 $ hspec $ do
   --  t_vecDot_r2_1
   --  t_linSys_r3_1
   -- --  -- 
   -- -- withSlepc0 $ hspec $ do
    t_eigen_r3_1






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
   withPetscMatrix com m n MatAij ixd nz InsertValues $ \mat ->
    withVecNew com vrhs $ \rhs -> do
     let (_, _, _, mu) = fromPetscMatrix mat
     withKspSetupSolveAlloc com KspGmres mu mu rhs $ \ksp soln -> 
       withVecNew com vsolnTrue $ \solnTrue -> 
        withVecVecSubtract soln solnTrue $ \solnDiff -> do
          nd <- vecNorm solnDiff VecNorm2
          nd < diffNormTol `shouldBe` True 
      where
        (m, n) = (3, 3)                      -- matrix size
        vrhs = V.fromList [3, 7, 18]         -- r.h.s
        vsolnTrue = V.fromList [1, 1, 1]     -- exact solution  
        ixd = ixd3x3                         -- matrix elements, by rows
        diffNormTol = 1e-300                 -- lin.solv. convergence tol.
        nz = nz3x3 -- ConstNZPR (3,3)        -- matrix nonzero pattern



t_eigen_r3_1 = describe "t_eigen_r3_1" $
  it "solves a 3x3 linear eigenproblem" $
   withPetscMatrix com m n  MatAij ixd nz InsertValues $ \mat -> do
    let (_, _, _, mu) = fromPetscMatrix mat
    withEpsCreateSetupSolve com mu Nothing EpsHep $ \eps nev vrr _ -> do
      _ <- withEpsEigenvectors eps $ \(VecRight vr) (VecRight vi) -> do
             print vr
             print vi
      ve <- epsGetEigenvalues eps
      let (er, ei) = V.unzip ve
      print er
      print ei
      where
        (m, n) = (3, 3)                   
        ixd = ixd3x3                      
        nz = nz3x3





-- | test data

-- 3x3 asymmetric matrix :
-- elements
ixd3x3 = listToCSR 3 3 [1,2,0,0,3,4,5,6,7]
-- nonzero pattern
nz3x3 = VarNZPR (dnnz, onnz) where
   dnnz = V.convert $ V.fromList [1,1,1]
   onnz = V.convert $ V.fromList [1,1,2]
