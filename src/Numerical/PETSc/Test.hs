-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Test
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | misc. tests
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Test where

import Numerical.PETSc.Internal

import qualified Numerical.PETSc.Internal.Sparse as PSparse -- (vvToCSR, vv)
import Numerical.PETSc.Internal.Sized

import Foreign
import Foreign.C.Types

import qualified Data.Vector.Storable as VS
-- import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

-- import Control.Applicative
import Control.Arrow ((***),(&&&))
import Control.Monad

import qualified Control.Monad.Managed as Mng

import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
-- import System.IO.Unsafe

import qualified Numeric.AD as AD





-- | test battery

ts = [t1', t7']
testLabels = ["1", "61", "61a", "7", "13a"]  --- ugh


tests = withPetsc0 testActions where
  testActions = prependTestNo (zip testLabels ts)
  prependTestNo = sequence2_ (printTestNo . fst) snd
  printTestNo x = putStrLn ("\n---\nTEST " ++ x ++ "\n---\n")



sequence2_ :: Monad m => (t -> m a) -> (t -> m b) -> [t] -> m ()
sequence2_ f1 f2 (x:xs) = (f1 x >> f2 x) >> sequence2_ f1 f2 xs
sequence2_ _ _ [] = return ()






-- --

t1' = do
  -- v <- vecCreateMPIFromVector comm lv v0
  v <- vecCreateMPIFromVectorDecideLocalSize comm v0
  vecViewStdout v
  vx <- vecGetVS v
  print vx
  vecPutVS v vx
  vecDestroy v
   where
     comm = commWorld

t1 = withPetsc0 t1'

--



-- | using Managed
-- vecm :: IO Vec -> Managed Vec
-- vecm vc = managed (withVec vc)

-- matm :: IO Mat -> Managed Mat
-- matm mc = managed (withMat mc)

-- kspm :: IO KSP -> Managed KSP
-- kspm kc = managed (withKsp_ kc)


-- t2' v = runManaged $ do
--   v1 <- vecm $ do
--     x <- vecCreateMPIFromVectorDecideLocalSize comm v
--     vecViewStdout x
--     return x
--   vecm $ vecCopyDuplicate v1
--     where comm = commWorld

-- t2 = withPetsc0 $ t2' v0


-- kspManage :: Comm -> KspType_ -> Mat -> Mat -> Bool -> Vec -> Vec -> Managed KSP
-- kspManage comm kt a p ignz rhs soln =
--   managed $ withKspSetupSolve comm kt a p ignz rhs soln 

-- t2' n = runManaged $ do
--   vrhs <- vecManage cs n 
--   vsoln <- vecManage cs n 
--   amat <- matManage cs
--   k <- kspManage cs KspChebyshev amat amat True vrhs vsoln
--    where
--      cs = commSelf




-- --

-- | managing config and resource abstractly with ReaderT and ResourceT

-- wsv2 :: Show a =>
--         (Comm, V.Vector PetscScalar_) -> (PVector PetscScalar_ -> IO a) -> IO ()
-- wsv2 c f = runResourceT $ do
--   x <- withScalarVector f c
--   lift $ print x

-- -- withPVectorConfig ::
-- --   (MonadBaseControl IO m, MonadThrow m, MonadIO m) =>
-- --   (Comm, V.Vector PetscScalar_) -> (PVector PetscScalar_ -> m a) -> m a
-- withPVectorConfig config f =
--   runResourceT $ withScalarVector f config

-- t4' :: IO ()
-- t4' = withPVectorConfig (commWorld, v0) $ \(PVector v vecdata) -> do
--   x <- vecGetVector v
--   print x
--   print vecdata
--   vecRestoreVector v (VS.map (/2) x)
--   vecViewStdout v


-- t4 = withPetsc0 t4'









-- --

-- | simplified interface for Mat setup, usage with Vector

v0_ :: V.Vector (Int, Int, PetscScalar_)
v0_ = V.zip3 vix viy va

-- t6' n =
--   withPetscMatrix commWorld n n MatAij vi InsertValues $ \m ->
--    matViewStdout m where
--      vi = csrAllNxN_ n    -- test mtx ( NB : dense )
 
-- t6 = withPetsc0 $ t6' 5


-- t61' = withPetscMatrix commWorld 3 3 MatAij vi InsertValues $ \m -> -- do
--   -- (n, i_, v_) <- matViewRow m 0
--   -- print (n, i_, v_)
--   matViewStdout m where
--     vi = csrSome3

-- t61 = withPetsc0 t61'


-- t61a' = withMatCreateSetup commWorld 3 3 MatAij $ \mat -> do
--   matSeqAIJSetPreallocationConstNZPR mat 3
--   matSetValuesVector2 mat vi InsertValues
--   matAssembly mat
--   matViewStdout mat where
--     vi = csrAllNxN_ 3


-- t62' nz = withMatAIJDecideConstNZPRNew commWorld 3 3 nz nz vi InsertValues $ \m -> do
--   matSetType m MatAij
--   matViewStdout m where
--     vi = csrSome3

-- t62 = withPetsc0 (t62' 1)




-- -- 
t7' = withVecNew commWorld vd1 $ \e1 ->
  withVecNew commWorld vd2 $ \e2 -> do
    x <- vecDot e1 e2
    print x
    print e1 >> print e2
      where
        vd1 = V.convert $ V.fromList [0 , 1]
        vd2 = V.convert $ V.fromList [1 , 0]

t7 = withPetsc0 t7'

-- 


-- t7' n = withMatNew cw n n vi InsertValues $ \amat ->
--   withVec (vecCreateMPIFromVectorDecideLocalSize cw v0) $ \v1 ->
--   withSnesCreateSetup cw v1 amat amat f fj
--    where
--      cw = commWorld

--

-- | SNES ex.3
--    www.mcs.anl.gov/petsc/petsc-current/src/snes/examples/tutorials/ex3.c.html

-- t8rhsAndSolution :: (Storable a, Num a, Enum a) => Int -> Float -> Int -> VS.Vector a





-- iot :: (Storable a, Num a, Enum a) => a -> a -> Int -> VS.Vector a
-- iot x0 h n = VS.fromList $ take n [x0, x0+h ..]

-- t8snesEx3 = do           
--   let n = 10
--       h = 1 / fromIntegral (n-1) 
--   withDmda1d cw DmBNone n 1 1 [] $ \da ->             -- DA
--    withMatCreateSetup cw n n $ \jac -> do             -- Jacobian
--     matSeqAIJSetPreallocation jac 3 []               -- " preallocation
--     withDmCreateGlobalVector da $ \x ->              -- solution
--      withVecDuplicate x $ \r ->
--      withVecDuplicate x $ \f ->
--      withVecDuplicate x $ \u -> do
--        (xs, nloc) <- dmdaGetCorners1d da
--        let xp = h * fromIntegral xs
--            localIdx :: VS.Vector PetscScalar_          -- local index array
--            localIdx = iot xp h (fromIntegral nloc)
--            f1 x = 6*x + (x + 1e-12)^6                  
--            u1 x = x^3
--            (f_, u_) = (VS.map f1 localIdx, VS.map u1 localIdx)
--        dmdaVecReplaceWVectorF da f nloc (const $ return f_)
--        dmdaVecReplaceWVectorF da u nloc (const $ return u_)
--        vecSet x 0.5                                    -- set initial solution
--        let formFunc sns vx vf =
--              withDmGetLocalVector da $ \xlocal -> do
--               dmG2L da x InsertValues xlocal           -- fill local vector
--               let d = 1 /  (h**2) 
--               withDmdaVecGetVector da xlocal nloc $ \xx ->
--                 return xx
--            formJacF = undefined
--        withSnesCreateSetup cw x jac jac formFunc formJacF $ \snes -> do
--         snesSolve0 snes x                              -- solve
--         xsoln <- snesGetSolution snes           
--         vecViewStdout xsoln                            -- view solution
--           where
--             cw = commWorld


-- t8 = withPetsc0 t8snesEx3

-- -- t9

dotS, (<.>) :: (Num a, Storable a) => VS.Vector a -> VS.Vector a -> a
dotS x y = VS.sum $ VS.zipWith (*) x y

(<.>) = dotS

constS :: (Storable a) => Int -> a -> VS.Vector a
constS = VS.replicate

timesConstS :: (Storable a, Num a) => a -> VS.Vector a -> VS.Vector a
timesConstS a = VS.map (* a) 

-- t9' = do
--   let
--     formFunction snes x f = do
--         xv <- vecGetVector x
--         fv <- vecGetVector f
--         vecRestoreVector f (constS n $ xv <.> xv) -- state transform `f`
--         vecRestoreVector x xv 
--         return (0 :: CInt)
--     formJacobian snes x amat = -- Vec -> IO Mat : state transform `amat`
--       withMatSetValueVectorSafe amat n n idxv InsertValues $ \amatp -> undefined
--        where
--          idxv = undefined
         
--     n = 1
--   withDmda1d0 cw DmBNone n 1 1 $ \da ->   -- distributed array
--     withMatCreateSetup cw n n $ \jac -> 
--       matMPIAIJSetPreallocationConstNZPR jac 1 0 
--     where
--       cw = commWorld


-- --

t10' = withDmda1d0 cw DmBoundaryNone n 1 1 $ \da ->
  withPetscViewer cw $ \vi -> do
   petscViewerSetType vi ViewerAscii
   -- petscViewerSetFormat vi ViewFmtAsciiInfoDetail
   dmView da vi
  where
    cw = commWorld
    n = 10
        
t10 = withPetsc0 t10'



-- -- -- 

-- | managed resources, obj stdout

t11'  = Mng.runManaged $ do
  -- dm <- managed $ withDmda1d0 cw DmBoundaryNone n 1 1
  dm <- Mng.managed $ withDmda2d0 cw (DmBoundaryNone, DmBoundaryNone) DmdaStencilStar (n,n) 1 1
  vi <- Mng.managed $ withPetscViewerTypeFmt cw ViewerAscii ViewFmtAsciiInfoDetail 
  Mng.liftIO $ dmView dm vi
    where
      n = 10
      cw = commWorld



-- fmts = [ViewFmtDefault, ViewFmtAsciiIndex ]--, ViewFmtAsciiInfo, ViewFmtAsciiInfoDetail]

t11 = withPetsc0  t11'

-- --


-- -- | block matrix assembly

-- asdf = idxV ne i where
--   i = 1
--   ne = 4


  
-- t12' =
--   withMatCreateSetup1 cw mm mm MatAij -- MatMPIBaij
--   (`matSetBlockSize` ne)
--   (\mat -> do
--       -- matSeqAIJSetPreallocation mat nzpr (replicate ne ne)
--       matMPIAIJSetPreallocationConstNZPR mat nzpr nzpr
--       matSetup mat
--   )
--   (\mat -> do
--       matSetValuesBlocked0 mat idxs idxs vs InsertValues
--       matAssembly mat
--       matViewStdout mat)
--   where
--     cw = commWorld
--     ne = 4 -- :: CInt
--     nzpr = 10
--     mm = fromIntegral ne^2
--     idxs = idxV ne 0
--     vs = V.fromList $ replicate mm pi -- [0..mm-1 :: CDouble]

-- -- idx[4] = {Ii, Ii+1, Ii + (ne+1) + 1, Ii + (ne+1)}
-- idxV ne i = V.fromList [i, i+1, i+ne+2, i+ne+1] 

-- t12 = withPetsc0 t12'




-- --

-- class (Storable p, Monad m) => PObj p m where
--   type PObjInfo p
--   type PObjLocal p
--   initO :: PObjInfo p -> m p
--   updateH :: p -> m ( PObjLocal p )
--   updateP :: p -> PObjLocal p -> m ()
--   destroyO :: p -> m ()

-- instance PObj Vec IO  where
--   type PObjInfo Vec = VecInfo
--   type PObjLocal Vec = V.Vector PetscScalar_
--   initO = vecCreateMPIInfo
--   -- updateH = vecGetVector

-- -- --


-- mca = matCreateMPIAIJWithArrays comm ix iy v


-- t13a' = 
--   withPetscMatrix comm m n MatAij vv InsertValues $ \mat -> 
--     matViewStdout mat
--     where
--       comm = commWorld
--       m = 2
--       n = m
--       vv = V.fromList [(0,1,pi), (1,0, exp 1), (1,1, sqrt 2)]

-- t13a = withPetsc0 t13a' 

-- t13b = withSlepc0 t13a' 



-- |  nesting PETSc and SLEPc

-- t14 = withSlepc0 $ t6' 5
-- t14 = withPetsc0 $ withSlepc0 t13a'



-- | vectorized Mat assembly with matSetValues 

-- t15' = withMatCreateSetup cw m n MatAij $ \mat -> do
--   matZeroEntries mat
--   matSetValuesVector mat vx vy vv InsertValues
--   matAssembly mat
--   matViewStdout mat
--    where
--      cw = commWorld
--      (m, n) = (3, 3)
--      vx = V.fromList [0,1]
--      vy = V.fromList [1,1]
--      vv = V.fromList [pi , exp 1]

-- t15 = withSlepc0 t15'


-- | StorableContainer

-- t16' = do
--   let vinfo = VecInfo commWorld 5 5
--   v <- initRemote vinfo



-- | vecSetValuesRange

-- t17' :: Int -> IO ()
-- t17' m = withVecMPIPipeline vinfo
--        (\v -> vecSetValuesRange v vIn InsertValues)
--        vecViewStdout
--   where
--   vinfo = VecInfo cw n n
--   cw = commWorld
--   n = 5
--   vIn = V.fromList $ [0.3, pi, (exp 1), (sqrt 2), (sqrt 5)] :: [CDouble] 
--   -- lIn = [0 .. CDouble (fromIntegral m)-1]

-- t17 = withPetsc0 (t17' 5)


-- |

-- kron a b v = [(x,y, z)| x<-a, y<-b | z<-v]



-- | SNES

-- t18' =
--   -- withVecMPIPipeline (VecInfo commWorld n n)
--   withVec (vecCreateMPIFromVectorDecideLocalSize cw w) $ \x ->
--   withVecClone x $ \xtemp ->
--   withMatCreateSetup cw n n MatAij $ \jac ->
--   withSnesCreateSetupAD cw xtemp jac jac f $ \snes -> do
--     -- -- vecViewStdout x
--     -- matAssembly jac
--     -- snesViewStdout snes
--     -- matViewStdout jac
--     snesSolve0 snes x
--     xsol <- snesGetSolution snes
--     vecViewStdout xsol
--     -- vecDot xtemp xtemp
--   where
--     cw = commSelf
--     n = 5
--     w = V.replicate n 0.3
--     f = V.map (**2)

-- t18 = withPetsc0 t18'


-- | t18debug : why does t18 segfault?

-- -- 1 : initialize, fill Jacobian matrix using AD and visualize it

-- t18d1' = withMatNew c n n MatAij vcsr InsertValues $ \mat -> do
--   print jac
--   matViewStdout mat
--   where
--    c = commWorld
--    n = 3
--    fun v = V.fromList [exp (x*y), exp y, exp z] -- this works
--      where [x,y,z] = V.toList v
--    -- fun y = V.map exp y
--    xv = V.replicate n (1.0 :: PetscScalar_)
--    jac = AD.jacobian fun xv -- (V.map realToFrac . fun . V.map realToFrac) xv
--    vcsr = PSparse.vvToCSRsparse jac

-- t18d1 = withPetsc0 t18d1'

-- -- 2 : ", passing in a function to be differentiated as a parameter

-- -- 3 : compute SNES function
t18d3' =
  -- withVecMPIPipeline (VecInfo commWorld n n)
  withVec (vecCreateMPIFromVectorDecideLocalSize cw w) $ \x ->
  withVecClone x $ \xtemp ->
  withMatCreateSetup cw n n MatAij $ \jac ->
  withSnesCreateSetup cw (\snes -> do
                             snesSetFunction snes xtemp f
                             snesSetJacobian0 snes jac jac (vvDiag n jd) n n 
                         ) $ \snes -> do -- return ()
    snesViewStdout snes
    withPetscViewerTypeFmt cw ViewerAscii ViewFmtAsciiCommon petscOptionsView
    -- snesComputeFunction snes xtemp x
    snesSolve0 snes x
    -- vecViewStdout xtemp
    
  where
    -- preSNS s = snesSetFunction s xtemp f
    cw = commWorld
    n = 5
    w = V.replicate n 0.3
    f = V.map (**2)       -- f  
    jd = V.replicate n 2  -- diagonal of Jacobian of f

-- matSetDiagonalVectorSafe 


t18d3 = withPetsc a o "" t18d3' where
  a = []
  o = "" -- name of options file 


asdfg = withPetsc


-- -- 4 : snes ex.1 

t18d4' = withSnesCreate cw $ \snes ->  -- line 49
  withVecNew cw vx0 $ \x -> do
    vecAssembly x 
    withVecClone x $ \r -> 
      withMatCreateSetup cw n n MatAij $ \jac -> do -- line 68
        snesSetFromOptions snes
        snesSetFunction snes r fun
        snesSetJacobian0 snes jac jac vvJac n n
        -- matAssembly jac
        -- vecAssembly r
        -- matViewStdout jac
        -- vecViewStdout r
        -- snesGetKsp snes >>= kspGetPc >>= (`pcSetType` PcJacobi) -- eh
        -- snesSolve0 snes x
        -- snesViewStdout snes


  where
    cw = commWorld
    n = 2
    vx0 = V.replicate n 0.5  -- starting guess
    fun = V.map (** 2)
    vvJac = vvDiag n (V.replicate n 2)

t18d4 = withPetsc0 t18d4'



-- snes ex 1test template

snesEx1 ::
  Floating b =>
     (SNES ->
      Vec ->
      Vec ->
      Mat ->
      (V.Vector b -> V.Vector b) ->
      IO a)
     -> IO a
snesEx1 io = withSnesCreate cw $ \snes ->  -- line 49
  withVecNew cw vx0 $ \x ->
  withVecClone x $ \r -> 
  withMatCreateSetup cw n n MatAij $ \jac ->  -- line 68
   io snes x r jac fun
    where
    cw = commWorld
    n = 2
    vx0 = V.replicate n 0.5  -- starting guess
    fun = V.map (** 2)
    -- vvJac = vvDiag n (V.replicate n 2)



snesEx1_t1 sfun vvsj = snesEx1 $ \snes x r jac f -> do
  snesSetFunction snes r sfun
  -- snesSetJacobian0 snes jac jac vvsj n n
  -- snesSetFromOptions snes
  
  snesSolve0 snes x
  where
    n = 2



-- | Vec destructive update in IO : fill Vec, get Vector from Vec, store modified contents in original Vec

t19 = withPetsc0 $ 
  withVecNew cw v0 $ \xvec -> do
    xv <- vecGetVS xvec
    print xv
    let yv = VG.map (/ 2) xv
    vecPutVS xvec $ V.convert yv
    vecViewStdout xvec
    xv1 <- vecGetVS xvec
    print xv1
    where
      cw = commWorld



t20 = withPetsc0 $
  withPetscMatrix cw m n MatAij vv (ConstNZPR (2,2)) InsertValues $ \mat -> do
   let (_, _, _, matm) = fromPetscMatrix mat
   matViewStdout matm
  where
    cw = commWorld
    (m, n) = (3, 3)
    vv = csrSome3


-- Sized









-- | test data

-- --

v0 :: V.Vector PetscScalar_
v0 = V.fromList [pi .. 10]

lv :: Int
lv = V.length v0

v0s :: (Storable a, Floating a, Enum a) => VS.Vector a
v0s = VS.fromList [pi .. 10]

vix, viy :: V.Vector Int
va :: V.Vector PetscScalar_
vix = V.fromList [0,1,2,0,1,2]
viy = V.fromList [0,0,0,1,1,1]
va = V.fromList [0 .. 8]

csrAllNxN_ :: Int -> V.Vector (Int, Int, PetscScalar_)
csrAllNxN_ n = V.zip3 x y a where
  x = V.fromList $ concat $ replicate n [0 .. n-1]
  y = V.fromList $ concatMap (replicate n) [0 .. n-1]
  a = V.fromList [0 .. nc^2-1 :: PetscScalar_ ] where
    nc = fromIntegral n 

csrSome3 :: V.Vector (Int, Int, PetscScalar_)
csrSome3 = V.zip3 x y a where
  x = V.fromList [2, 0, 1]
  y = V.fromList [1, 1, 2]
  a = V.fromList [pi, exp 1, sqrt 2]
