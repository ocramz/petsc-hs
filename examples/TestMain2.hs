module TestMain2 where

-- import Numerical.PETSc.Raw
import Numerical.PETSc.Raw.PutGet



vinfo n = VecInfo commWorld n n

t1' = 
  withVecMPIPipeline (vinfo 5) (`vecSet` pi) $ \v1 -> do
   -- v2 <- vecCopyDuplicate v1
   v3 <- vecCopyDuplicate v1 
   -- vecSet v2 2
   -- v3 <- vecAxpy 1 v1 v2
   -- v3 <- vecVecSumSafe (PVec v1 vinfo) (PVec v2 vinfo)
   -- v3 <- vecWaxpy v3 1 v1 v2
   -- vecViewStdout v3
   -- av3 <- vecGetArraySafe v3
   -- let av3' = map (+1) av3
   -- vecRestoreArray v3 av3'
   -- withVecGetArrayMVarModify v3 $ \a -> do
   --   print a
   --   let a2 = map (+2) a
   --   print "so far so good"
   --   print a2
   --   return a2
   -- withVecGetArray v3 $ \a -> do
   --   print a
   --   let a2 = map (+2) a
   --   print a2
   --   return a2
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
  -- print b
  vecRestoreArray v b
  vecAssemblyChk v
  vecViewStdout v

t3 = withPetsc0 t3'



testv1 = withVecMPIPipeline (vinfo 5) (`vecSet` pi)

-- t4'= testv1 $ \v -> do
--   a <- vecGetArraySafe v
--   let b = map (+1) a
--   vecRestoreArrayB v b
--   vecViewStdout v

-- t4 = withPetsc0 t4'




-- tests = withPetsc0 $ sequence [t4']
