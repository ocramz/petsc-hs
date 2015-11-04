-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Test
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | tests, but not in the quickcheck sense :D
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Test where

import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.PutGet
import Numerical.PETSc.Internal.Internal
import Numerical.PETSc.Internal.Managed

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM





-- --

v0 :: V.Vector PetscScalar_
v0 = V.fromList [pi .. 10]

lv = V.length v0


-- --

t1' = do
  -- v <- vecCreateMPIFromVector comm lv v0
  v <- vecCreateMPIFromVectorDecideLocalSize comm v0
  vecViewStdout v
  vecDestroy v
   where
     comm = commWorld

t1 = withPetsc0 t1'

--

-- toVec = vecCreateMPIFromVectorDecideLocalSize commWorld



-- -- using Managed

vecManage :: Comm -> Managed Vec
vecManage comm = managed $ withVec (vecCreate comm)

matManage :: Comm -> Managed Mat
matManage comm = managed $ withMat (matCreate comm)

-- kspManage :: Comm -> Managed KSP
kspManage comm = managed $ withKsp1 (kspCreate comm)



-- t2' = runManaged $ do
--   v1 <- vecManage cs
--   m1 <- matManage cs
--   k <- kspManage cs
--    where
--      cs = commSelf



-- --





-- -- 
















-- -- --

-- vinfo n = VecInfo commWorld n n

-- vecTemplate n f = withVecMPIPipeline vi (`vecSet` pi) $ \v -> do
--   -- withVecGetVectorOverwrite v (V.map exp)    -- contents of Vec are changed 
--   -- print $ exp pi
--   -- f v
--   f
--   vecViewStdout v
--     where
--       vi = vinfo n

-- -- petsc0VecTemplate n f = withPetsc0 $ vecTemplate n f
