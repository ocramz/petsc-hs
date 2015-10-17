module TestMain2 where

-- import Numerical.PETSc.Raw
import Numerical.PETSc.Raw.PutGet



vinfo n = VecInfo commWorld n n

t1' = 
  withVecMPIPipeline (vinfo 5) (`vecSet` pi) $ \v1 -> do
   v3 <- vecCopyDuplicate v1 
   v3 <- v1 .+ v3
   a <- vecGetArraySafe v3
   let a2 = map (+1) a
   print a2
   vecRestoreArray v3 a2
   vecViewStdout v3

t1 = withPetsc0 t1'

t2' = withVecMPIPipeline (vinfo 5) (`vecSet` pi) $ \v ->
  vecViewStdout v

t2 = withPetsc0 t2'


-- | vecGet, modify array, restore modified array in original Vec:
-- -- Vec -> array -> fmap f array -> Vec
t3' = withVecMPIPipeline (vinfo 5) (`vecSet` (2*pi))  $ \v -> do
  vecViewStdout v
  a <- vecGetArraySafe v
  let b = [1,2,3,4,5]
  vecRestoreArray v b
  vecAssemblyChk v
  vecViewStdout v

t3 = withPetsc0 t3'

-- --


t4'= withVecMPIPipeline (vinfo 5) (`vecSet` pi) $ \v -> do
  a <- vecGetArraySafe v
  let b = take 6 [1, 1 ..] --laziness: size of b is not issue to vecRestoreArray (?!)
  vecRestoreArray v b       
  vecViewStdout v

t4 = withPetsc0 t4'

-- -- 

t5' = do
  v <- vecCreateMPIInfo vi
  vecSet v pi
  a <- vecGetArraySafe v
  let b = map (+1) a
  vecRestoreArray v b
  vecViewStdout v
  vecDestroy v

    where
      vi = vinfo 5

t5 = withPetsc0 $ t5'

-- -- 
