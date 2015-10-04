{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-} -- for repa
module Numerical.PETSc.TestMain where

-- import Numerical.PETSc.Types

import Numerical.PETSc.Test2

-- import qualified Numerical.PETSc.Test2 as P

import Foreign.C.Types
import Control.Monad
import Control.Concurrent
import Control.Applicative
import Control.Exception
import Foreign

import qualified Data.Vector as V -- hiding (toList)
import Data.Vector.Storable as VS  (toList) 

import Data.Array.Repa                  as R hiding ((++), map)
import qualified Data.Array.Repa as R
import Data.Array.Repa.Unsafe           as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RFP

-- * tests

t1 = do
  petscInitialize [] [] []
  print commWorld
  petscFin

t2' = do
  v1 <- vecCreateMPILocal commWorld 5
  v2 <- vecCreateMPILocal commWorld 5
  -- m1 <- fstM $ matCreate commWorld
  -- vecSetSizes v1 3
  -- vecSetSizes v2 3
  vecSet v1 2.0
  vecSet v2 3.0
  vecAssembly v1
  vecAssembly v2
-- ownr <- vecGetOwnershipRange v1
  -- print $ map unPInt ownr
  d <- vecDot v1 v2
  vecDestroy v1
  vecDestroy v2
  print d
  return d

-- t2'' = H.hspec $ do
--   H.describe "t2''" $ do
--     H.it "checks inner product" $ do
--       t2' `H.shouldBe` (30.0 :: Double )

t2 = withPetsc0 t2'

t3' = do
  v <- vecCreateMPI commWorld 5 5
  -- v <- vecCreate commWorld
  vecSet v 2.0
  vecAssembly v
  vecDestroy v

t3 = withPetsc0 t3'

-- t4' = withVecPipeline commWorld 5 $ \v1 ->
--         withVecPipeline commWorld 5 $ \v2 -> do
--            d <- vecDot v1 v2
--            print d

-- makeVecCW = withVecMPIPipeline commWorld

-- t4' =
--   makeVecCW 5 (`vecSet` 2.0) $ \v1 ->
--    makeVecCW 5 (`vecSet` 3.0) $ \v2 -> do
--     d <- vecDot v1 v2
--     print d

-- t4 = withPetsc0 t4'

-- t4 = withPetsc [] [] []
--        t4'


showKspStats ksp km = do
  iterno <- kspGetIterationNumber ksp
  resnorm <-  kspGetResidualNorm ksp
  -- km <-  kspGetType ksp
  putStrLn $ "method: " ++ show km ++ ", # iter.: " ++ show iterno ++ ", residual norm: " ++ show resnorm 


-- t5' :: KspType_ -> IO ()
t5' km  =
  withMatSeqAIJPipeline cs m m nz nnz (`matSetDiagonal` 200.0) $ \mat ->
   withVecMPIPipeline cs m (`vecSet` 1.0) $ \v ->
    withVecMPIPipeline cs m (`vecSet` 1.0) $ \x -> 
     withKsp cs $ \ksp -> do
       kspSetOperators ksp mat mat
       kspSetType ksp km 
       kspSetInitialGuessNonzero ksp True
       kspSetUp ksp
       kspSolve ksp x v
       soln <- kspGetSolution ksp
       -- vecViewStdout soln
       showKspStats ksp km
       -- kspReasonView ksp
       -- return soln
  where
   cs = commSelf
   m = 5
   nz = sum nnz
   nnz = replicate (fromIntegral m) 1


t5 = withPetsc [] [] [] $
      forM [KspGmres, KspCg, KspBicg, KspChebyshev, KspRichardson] t5'

t51' kt =
  withMatSeqAIJPipeline cs m m nz nnz (`matSetDiagonal` 200.0) $ \mat ->
   withVecMPIPipeline cs m (`vecSet` 1.0) $ \soln ->
    withVecMPIPipeline cs m (`vecSet` 1.0) $ \rhs -> 
     withKspSetupSolve cs mat mat True kt rhs soln $ \ksp -> do
       showKspStats ksp kt
       kspGetConvergedReason ksp
  where
   cs = commSelf
   m = 5
   nz = sum nnz
   nnz = replicate (fromIntegral m) 1

t51 = withPetsc0 $ forM [KspGmres, KspCg, KspBicg, KspChebyshev, KspRichardson]  t51'


t6' =
  withVecMPIPipeline cs m (`vecSet` pi) $ \x ->
   withVecDuplicateCopy x $ \xd -> do
    b <- vecEqual x xd
    print b
    vecViewStdout x
    -- vecViewStdout xd
  where
    cs = commWorld
    m = 5
  
t6 = withPetsc0 t6'

-- t7' = withVecMPIPipeline cs m (\x -> vecSetValues x ix y InsertValues ) $ \v -> do
--   vecViewStdout v
--   where y = replicate m pi
--         ix = map fromIntegral [0,1,2,3,4]
--         cs = commSelf
--         m = 5

-- t7' kt =
--   withMatMPIAIJWithArraysPipeline cs i j a $ \mat ->
--    withVecMPIPipeline cs m (`vecSet` pi) $ \x ->
--     withVecMPIPipeline cs m (`vecSet` 1.0) $ \rhs -> 
--      withKspSetupSolve cs mat mat True kt rhs x $ \ksp ->
--       showKspStats ksp kt
--   where
--     cs = commWorld
--     m = 2
--     i = [0,0,1,1]
--     j = [0,1,0,1]
--     a = [2,3,4,5]

-- t7 = withPetsc0 $ t7' KspCg

-- -- local row ownership range (imin, imax)
t7' =  print $ localRange c 10 
  where
    c = commWorld
t7 = withPetsc0 t7'

-- -- getting an integer from the options database, printing
t8' = 
  withPetscOptionsGetInt "" "-m" $ \m ->
   
    print m
    -- petscSynchronizedPrintf c ( "Hello from PETSc, rankId " ++ mm  ++ "\n")
    -- petscPrintf c "hello! "

t8 = withPetsc ["-m 10"] [] [] t8'


helloFromRank c = syncPrintf c $ "hello from rank " ++ mm ++ "!\n" where
  mm = show $ rankId (mpiCommRank c)

numMPIProcs c = syncPrintf c $ "# of MPI processes: " ++ show (mpiCommSize c) ++"\n"


t9' = -- hello MPI!
 mapM_ (const hello) [0 .. np - 1]
  where
  np = toInteger $ mpiCommSize c
  hello = syncPrintf c $ "hello from rank " ++ show (rankId (mpiCommRank c)) ++ "\n"
  c = commWorld

t9 = withPetsc0 t9'



-- t10' kt =
--   withMatSeqAIJPipeline cs m m nz nnz (`assemble1` 1) $ \mat -> do
--     matViewStdout mat
--    -- withVecMPIPipeline cs m (`vecSet` 1.0) $ \soln ->
--    --  withVecMPIPipeline cs m (`vecSet` 1.0) $ \rhs -> 
--    --   withKspSetupSolve cs mat mat True kt rhs soln $ \ksp -> do
--    --     showKspStats ksp kt
--    --     kspGetConvergedReason ksp
--   where
--    cs = commSelf
--    m = 5
--    nz = sum nnz
--    nnz = replicate (fromIntegral m) 1

-- t10 = withPetsc0 $ forM [KspGmres, KspCg]  t10'

t10' =
  withMatPipeline cs mN mN MatMPIAij 
  (\w -> assemble1 w 1 m mM)
    matViewStdout
   where
     cs = commWorld
     m = 5 -- # elements / side
     mN = (m+1) ^ 2 -- # nodes total 
     mM = m ^ 2     -- # elements total

t10 = withPetsc0 t10'

-----------------
t11' h m n =
  withMatAssembleQ4 cs m n (ke h) $ \mat ->
    matViewStdout mat
   where cs = commWorld

t11 = withPetsc0 $ t11' 2.0 10 11

withMatAssembleQ4 comm m n localmat
  | length localmat == 16 =
      withMatPipeline comm nNodes nNodes MatMPIAij (\mat -> assembleQ4 mat m n localmat)
  | otherwise = error "withMatAssembleQ4: localmat must be length 16"
  where
    nnrow = m+1
    nncol = n+1
    nNodes = nnrow * nncol

assembleQ4 mat m n localmat =
  forM_ [0 .. nElem] (\j -> matSetValues mat (idx j) (idx j) localmat AddValues)
   where
    nElem = m * n
    idx j = indices4 j m

indices4 i m = [idx0, idx1, idx2, idx3] where
      idx0 = (m+1)*(i `div` m) + (i `mod` m)
      idx1 = idx0 + 1
      idx2 = idx1 + m + 1
      idx3 = idx1 + m

assemble1 mat h m mM =
  forM_ [0 .. mM] (\i -> matSetValuesAdd mat (idx i) (idx i) localmat )
   where
    idx i = indices4 i m
    localmat = ke h

ke0 :: Fractional a => [a]
ke0 = [1/6, -1/8, 1/12, -1/8,
        -1/8, 1/6, -1/8, 1/12,
        1/12, -1/8, 1/6, -1/8,
        -1/8, 1/12, -1/8, 1/6] 

ke h = map (*h) ke0


---------------------------------------------------

t12' n =
  withDmda1d cs DmBNone n 1 1 [n] $ \dm ->
   withDmCreateGlobalVector dm $ \v -> 
    withVecDuplicateCopy v $ \vc -> do
      vecSet vc pi
      vecViewStdout vc
  where
    cs = commWorld

t12 = withPetsc0 $ t12' 10

---------------------------------------------------
type Sten = CInt -> [CInt]
data Kernel a = Kern { vals :: [a],
                     sten :: Sten }
-------------------------------------
assembleLaplacian mat h = assembleKernelByRows mat (makeLapl h)

makeLapl h = Kern { vals = [1/h', 2/h', 1/h'],
                    sten = \i -> [i-1, i, i+1] } where h' = h^2
                                                       
assembleKernelByRows mat k im =
  withMatSize mat $ \(nrows, _) ->
    forM_ [0..nrows-1] (\j -> matSetValues mat [j] (sten k j) (vals k) im )

t13' =
  withMatPipeline cs m n MatMPIAij (\w -> assembleLaplacian w h AddValues) $ \mat -> 
   matViewStdout mat
  where
    cs = commWorld
    m = 7
    n = m
    h = 2

t13 = withPetsc0 t13'
------------------------------

t14' =
  withMatPipeline cs m n MatMPIAij (`assembleLaplacian1` h) $ \mat ->
   matViewStdout mat
     where
       (cs, m, n, h) = (commWorld, 7, m, 2)
t14 = withPetsc0 t14'

assembleLaplacian1 mat h = assembleKernelByRowsAddSafe mat (makeLapl h)

assembleKernelByRowsAddSafe mat k =
  withMatRowsSafe mat k (\mm jj ss vv -> matSetValues mm [jj] ss vv AddValues)
  
withMatRowsSafe mat kern f =
  withMatSize mat $ \(nr, nc) ->
   forM_ [0..nr-1] $ \ii -> do
     let
       (skf, vkf) =
         unzip $ filter (\a -> fst a < nc) (zip (sten kern ii) (vals kern))
     f mat ii skf vkf

-- withMatSizeSafe mat kern f = withMatSize mat $ \(nr, nc) ->
--   (forM_ [0..nr-1] ( \ii -> do
--     let (skf, vkf) = unzip $ filter (\a -> fst a < nc || fst a < nr) $
--            zip (sten kern ii) (vals kern)
--     f mat skf vkf ))




matSetValuesRow mat kern idx =
  matSetValues mat [idx] (sten kern idx) (vals kern)
matSetValuesRowAdd k i m = matSetValuesRow m k i AddValues
  
withMatRows mat f =
  withMatSize mat $ \(nrows, _) -> forM_ [0 .. nrows-1] (`f` mat)

assembleKernelByRowsAdd mat k =
  withMatRows mat (matSetValuesRowAdd k)

------------------
-- --more on the safe assembly of global matrices 

data Kernel2 a = Kern2 { valsKernel2 :: [a],
                         stenKernel2x :: Sten,
                         stenKernel2y :: Sten}

withMatSizeSafe mat kern f = withMatSize mat $ \(nr,nc) ->
  (forM_ [0..nr-1] (\ii -> do
     let
       (sx,sy,v) =
         (stenKernel2x kern ii, stenKernel2y kern ii, valsKernel2 kern)
       (skfx, skfy, vkf) =
           unzip3 $ filter (\(a,b,_) -> a < nc || b < nr) $
             zip3 sx sy v
     -- print (skfx, skfy, vkf)
     print (sx, sy, v)
     f mat skfx skfy vkf))

matSetValuesSafe mat k im =
  withMatSizeSafe mat k (\mm idxx idxy v -> matSetValues mm idxx idxy v im)

q4 m h = Kern2 {valsKernel2 = ke h,
                stenKernel2x = (`indices4` m),
                stenKernel2y = (`indices4` m)}

t15' =
  withMatPipeline cs m n MatMPIAij (\w -> matSetValuesSafe w kern AddValues) $ \mat ->
    matViewStdout mat
    where
    cs = commWorld
    m = 3
    n = 2
    h = 0.2
    kern = q4 m h
t15 = withPetsc0 t15'

-----------------------------------
-- -- Vec to array

t16' =
  withVecMPIPipeline cs n (`vecSet` (- 3.14)) $ \v -> 
   withVecGetArray v m $ \a ->
     print a
   where
     cs = commWorld
     n = 10
     m = n + 2 -- NB: elem>n are gibberish: need static guarantees on arrays: repa !
t16 = withPetsc0 t16'

t16a' =
  withVecMPIPipeline commWorld n (`vecSet` 23.95) $ \v -> 
    withVecGetArraySafe' v $ \a -> -- looks up the size of v
     print a
    where
      n = 5
t16a = withPetsc0 t16a'

--------------------------------
-- -- repa

xrepa :: Array U DIM3 Int;
xrepa = fromListUnboxed (Z :. (3::Int) :. (3::Int) :. (3::Int)) [1..27]

t17' = withVecMPIPipeline commWorld n (`vecSet` 3.14) $ \v -> 
  withVecGetRepa v $ \r ->
    return $ R.map (^2) r
   where n = 5
t17 = withPetsc0' t17' 

-- f arr = undefined
--   where
--     _ :. height :. width = extent arr

-- f = unsafeTraverse

-- relaxLaplace
--         :: Monad m
--         => Array U DIM2 Double  -- ^ Boundary condition mask
--         -> Array U DIM2 Double  -- ^ Boundary condition values
--         -> Array U DIM2 Double  -- ^ Initial matrix
--         -> m (Array U DIM2 Double)
-- relaxLaplace arrBoundMask arrBoundValue arr
--   = computeP
--   $ R.zipWith (+) arrBoundValue
--   $ R.zipWith (*) arrBoundMask
--   $ unsafeTraverse arr id elemFn
--   where
--         _ :. height :. width    
--                 = extent arr

--         {-# INLINE elemFn #-}
--         elemFn !get d@(sh :. i :. j)
--          = if isBorder i j
--                  then  get d
--                  else (get (sh :. (i-1) :. j)
--                    +   get (sh :. i     :. (j-1))
--                    +   get (sh :. (i+1) :. j)
--                    +   get (sh :. i     :. (j+1))) / 4

--         -- Check if this element is on the border of the matrix.
--         -- If so we can't apply the stencil because we don't have all the neighbours.
--         {-# INLINE isBorder #-}
--         isBorder !i !j
--                 =  (i == 0) || (i >= width  - 1) 
--                 || (j == 0) || (j >= height - 1)


t18' = withVecMPIPipeline commWorld n (`vecSet` 3.14) $ \v -> do
  vecViewStdout v
  withVecGetVector v $ \vv -> -- vv is read-only
   print $ VS.toList vv
   where n = 5
t18 = withPetsc0 t18'

---------------------------

t19' =
  withVecMPIPipeline commWorld n (`vecSet` 3.14) $ \v -> do
   withVecGetArrayUnsafe v (map (^2)) -- v doesn't change because at this point it's already been assembled
   vecViewStdout v
     where n = 5
t19 = withPetsc0 t19'

t20' =
  withVecMPIPipeline commWorld n (`vecSet` 3.14) $ \v -> do
    withVecGetArrayUnsafe' v (map (^2))
    vecViewStdout v
    where n = 5
t20 = withPetsc0 t20'
