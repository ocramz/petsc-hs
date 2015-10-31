module TestMain2 where

-- import Numerical.PETSc.Raw
import Numerical.PETSc.Internal.PutGet

import qualified Data.Vector.Storable as V


vinfo n = VecInfo commWorld n n


t2' = withVecMPIPipeline (vinfo 5) (`vecSet` pi) $ \v ->
  vecViewStdout v

t2 = withPetsc0 t2'


-- --

-- t6' = withVecMPIPipeline vi (`vecSet` pi) $ \v -> do
--   withSnes comm $ \snes ->
--    withMat comm $ \jac ->
--     snesSetFunction snes v vf
--     snesSetjacobian snes jac jac
--     snesSolve snes
--     where
--       vi = vinfo 5
--       comm = vecInfoMpiComm vi

-- vf u = do
--   w <- vecDot u u
--   return 0

-- vj u = do
--   j <- vecScale u 2
--   return 0

--


-- --

t8' = withVecMPIPipeline vi (`vecSet` pi) $ \v -> do
  vecViewStdout v
  x <- vecGetVector v
  print x
  let y = V.map sqrt x
  print y
  vecRestoreVector v y
  vecViewStdout v
    where
      vi = vinfo 3

t8 = withPetsc0 t8'

-- --

t9' = withVecMPIPipeline vi (`vecSet` pi) $ \v -> do
  withVecGetVectorOverwrite v (V.map exp)    -- contents of Vec are changed 
  print $ exp pi
  vecViewStdout v
    where
      vi = vinfo 3

t9 = withPetsc0 t9'


