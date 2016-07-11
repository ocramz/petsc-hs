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


import Numerical.PETSc.Internal

import Foreign
import Foreign.C.Types

import Data.Complex

import qualified Data.Vector.Storable as VS
-- import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector as V

import Test.Hspec

-- | utils


specs :: IO ()
specs = 
  withPetsc0 $
   withSlepc0 $
   hspec $ do
    t_vecDot_r2_1
    t_linSys_r3_1

    -- | SLEPc
    t_eigen_r3_1
    t_eigen_r3_1_spd
    
    -- | HDF5 
    t_hdf5_int
    t_hdf5_float





-- | tests

com :: Comm
com = commWorld

t_vecDot_r2_1 = 
  describe "t_vecDot_r2_1" $ 
    it "computes the inner product of two orthogonal vectors in R2" $
 withVecNew com vd1 $ \e1 ->
  withVecNew com vd2 $ \e2 -> do
    x <- vecDot e1 e2
    x `shouldBe` (0 :: PetscScalar_)
    petscGetFlops >>= \nfl -> putStrLn $ "total # FLOPs : " ++ show nfl
      where
        vd1 = V.convert $ V.fromList [0 , 1]
        vd2 = V.convert $ V.fromList [1 , 0]


t_linSys_r3_1 = describe "t_linSys_r3_1" $
  it "solves a 3x3 linear system" $
   withPetscMatrix com m n MatAij ixd nz InsertValues $ \mat ->
    withVecNew com vrhs $ \rhs -> do
     let (_, _, _, mu) = fromPetscMatrix mat
     withKspSetupSolveAlloc com KspGmres mu mu rhs $ \ksp soln -> do
       withVecNew com vsolnTrue $ \solnTrue -> 
        withVecVecSubtract soln solnTrue $ \solnDiff -> do
          nd <- vecNorm solnDiff VecNorm2
          nd < diffNormTol `shouldBe` True
          petscGetFlops >>= \nfl -> putStrLn $ "total # FLOPs : " ++ show nfl
      where
        (m, n) = (3, 3)                      -- matrix size
        vrhs = V.fromList [3, 7, 18]         -- r.h.s
        vsolnTrue = V.fromList [1, 1, 1]     -- exact solution  
        ixd = ixd3x3                         -- matrix elements, by rows
        diffNormTol = 1e-16                  -- lin.solv. convergence tol.
        nz = nz3x3 -- ConstNZPR (3,3)        -- matrix nonzero pattern




-- | SLEPc tests

t_eigen_r3_1 = describe "t_eigen_r3_1" $
  it "solves a 3x3 (real, asymmetric) linear eigenproblem: eigenvalues are real numbers" $
   withPetscMatrix com m n  MatAij ixd nz InsertValues $ \mat -> do
    let (_, _, _, mu) = fromPetscMatrix mat
    withEpsCreateSetupSolve com mu Nothing EpsHep $ \eps nev vrr _ -> do
      -- putStrLn "Eigenvectors : (real, imag)"
      -- _ <- withEpsEigenvectors eps $ \(VecRight vr) (VecRight vi) -> do
      --        print (vr, vi)
      -- putStrLn "Eigenvalues : (real, imag)"
      petscGetFlops >>= \nfl -> putStrLn $ "total # FLOPs : " ++ show nfl
      ve <- epsGetEigenvalues eps
      let (_, ei) = V.unzip ve
      V.all (<= imzTol) ei `shouldBe` True -- real mtx
      -- logViewStdout com
      where
        (m, n) = (3, 3)                   
        ixd = ixd3x3                      
        nz = nz3x3
        imzTol = 1e-16



-- | data for3x3 asymmetric matrix used in examples above :

-- elements
ixd3x3 = listToCSR 3 3 [1,2,0,0,3,4,5,6,7]
-- nonzero pattern
nz3x3 = VarNZPR (dnnz, onnz) where
   dnnz = V.convert $ V.fromList [1,1,1]
   onnz = V.convert $ V.fromList [1,1,2]



t_eigen_r3_1_spd = describe "t_eigen_r3_1_symm" $
  it "solves a 3x3 (symmetric positive definite) linear eigenproblem: eigenvalues are real positive numbers" $
   withPetscMatrix com m n  MatAij ixd nz InsertValues $ \mat -> do
    let (_, _, _, mu) = fromPetscMatrix mat
    withEpsCreateSetupSolve com mu Nothing EpsHep $ \eps nev vrr _ -> do
      -- putStrLn "Eigenvalues : (real :+ imag)"
      ve <- epsGetEigenvalues eps
      -- let evc = V.map (\(a,b) -> a :+ b) ve
      -- print evc
      let (er, ei) = V.unzip ve
      V.all (>0) er `shouldBe` True -- SPD matrices have real positive eigenvalues
      V.all (<= imzTol) ei `shouldBe` True 
      -- matViewStdout mu
      where
        (m, n) = (3, 3)                   
        ixd = listToCSR m n [2,-1,0, -1,2,-1, 0,-1,2] -- SPD mtx        
        nz = ConstNZPR (3,3)
        imzTol = 1e-16


-- | HDF5 tests

t_hdf5_int = describe "t_hdf5_int" $
  it "writes and reads back a Vec of integers to HDF5" $
     hdf5RoundtripTest vhtest1 "intvec" fname
     where
       vhtest1 = V.fromList [1,2,3,4,5]
       fname = "test.hdf5"

t_hdf5_float = describe "t_hdf5_float" $
  it "writes and reads back a Vec of floats to HDF5" $
    hdf5RoundtripTest vhtest2 "floatvec" fname
     where
       vhtest2 = V.fromList [pi, exp 1, sqrt 2]
       fname = "test.hdf5"

-- NB: HDF5 data are named, so VecLoad expects the same name as was used in the Vec that was stored in the file (using PetscObjectSetName()):
hdf5RoundtripTest w vname fname = withVecNew com w $ \vt -> do
    vecSetName vt vname
    withVecDuplicate vt $ \vhat -> do
     vecSetName vhat vname
     withHDF5Write com fname (vecView0 vt) -- roundtrip: write named Ved to HDF 
     withHDF5Read com fname (vecLoad vhat) -- --, expect Vec with same name
     eqq <- vecEqual vt vhat
     eqq `shouldBe` True
