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

import Numerical.PETSc.Internal.PutGet
import Numerical.PETSc.Internal.Internal

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

toVec = vecCreateMPIFromVectorDecideLocalSize commWorld





-- combine = \ a b f -> (b >>=) . (return .) . f =<< a




-- -- now let's package the above in ST
















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
