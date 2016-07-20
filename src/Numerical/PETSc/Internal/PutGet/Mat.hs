{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.Mat
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Mat Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.Mat where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.C2HsGen.TypesC2HsGen
import Numerical.PETSc.Internal.Exception
import qualified Numerical.PETSc.Internal.PutGet.PetscMisc as PM
import qualified Numerical.PETSc.Internal.PutGet.Viewer as Viewer
import Numerical.PETSc.Internal.Utils -- (both, fi, toCInt, in0m, allIn0mV)

import Numerical.PETSc.Internal.Storable.Vector
import Numerical.PETSc.Internal.Storable.Matrix
import Numerical.PETSc.Internal.Storable.Common (unsafeWithVS)
import Numerical.PETSc.Internal.Sparse

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Utils

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Control.Arrow
import Control.Concurrent
import Control.Exception

import qualified Data.IntMap as IM

import Data.Traversable

import qualified Data.Vector as V 
import qualified Data.Vector.Storable as VS (unsafeWith, Vector)
import qualified Data.Vector.Generic as VG



-- | instances

-- instance Show Mat where
--   show m = show 




-- | a datatype encapsulating matrix information and the typed pointer

data PetscMatrix a = PetscMatrix MatrixInfoBase NZPR (MatrixDataZ a) Mat

toPetscMatrix :: MatrixInfoBase -> NZPR -> MatrixDataZ a -> Mat -> PetscMatrix a
toPetscMatrix = PetscMatrix

fromPetscMatrix :: PetscMatrix t -> (MatrixInfoBase, NZPR, MatrixDataZ t, Mat)
fromPetscMatrix (PetscMatrix mib nz md m) = (mib, nz, md, m)


-- | (dnz, onz) and (dnnz, onnz) : # NZ/row ON-DIAGONAL BLOCK and OFF-DIAGONAL BLOCK, constant and variable-number case respectively
type DNZ = Int -- diagonal
type ONZ = Int -- off-diagonal
data NZPR = ConstNZPR (DNZ, ONZ)
          | VarNZPR (VS.Vector DNZ, VS.Vector ONZ) deriving (Eq, Show) 

type NumRows = Int
type NumCols = Int
data MatrixInfoBase =
  MatrixInfoBase { matComm  :: Comm
                  ,matRows  :: !NumRows
                  ,matCols  :: !NumCols
                  -- ,matOrder :: !MatrixOrder
                 } deriving (Eq, Show)


type IdxRow = Int
type IdxCol = Int
data MatrixDataZ a =
  MatrixDataZ {matDataZ :: V.Vector (IdxRow, IdxCol, a)} deriving (Eq, Show)



-- | PETSc matrix usage :
{-
* create
* configure
* setup  (preallocation : required unless one uses DMCreateMatrix )
* fill
* assemble
* _use_
* destroy

-}




petscMatrixCreate
  :: Comm
     -> NumRows
     -> NumCols
     -> MatType_
     -> V.Vector (IdxRow, IdxCol, PetscScalar_)
     -> NZPR
     -> InsertMode_
     -> IO (PetscMatrix PetscScalar_)
petscMatrixCreate cc m n mty ixd nz imode = do
  let mib = MatrixInfoBase cc m n
      mixd = MatrixDataZ ixd
  mat <- matCreateAdapter cc m n mty nz
  matSetValueVectorSafe mat (m, n) ixd imode
  matAssembly mat
  let pm = PetscMatrix mib nz mixd mat
  return pm

petscMatrixDestroy :: PetscMatrix t -> IO ()
petscMatrixDestroy (PetscMatrix _ _ _ mat) = matDestroy mat

withPetscMatrix
  :: Comm
     -> NumRows
     -> NumCols
     -> MatType_
     -> V.Vector (IdxRow, IdxCol, PetscScalar_)
     -> NZPR
     -> InsertMode_
     -> (PetscMatrix PetscScalar_ -> IO c)
     -> IO c
withPetscMatrix cc m n mty ixd nz imode =
  bracket (petscMatrixCreate cc m n mty ixd nz imode) petscMatrixDestroy
    


-- | adapter for matrix create/settype/setsizes/preallocation/setup

matCreateAdapter :: Comm -> NumRows -> NumCols -> MatType_ -> NZPR -> IO Mat
matCreateAdapter cc m n mty nz = do
  mat <- matCreate cc
  matSetType mat mty
  matSetSizes mat m n 
  case mty of
    MatAij ->
     case nz of
      (ConstNZPR (dnz, onz)) -> matMPIAIJSetPreallocationConstNZPR mat dnz onz
      (VarNZPR (dnnz, onnz)) -> matMPIAIJSetPreallocationVarNZPR mat dnnz onnz
  matSetup mat
  return mat




-- | predicates

-- | predicates for MatrixData

checkMatrixData :: MatrixDataZ a -> Bool
checkMatrixData (MatrixDataZ z) = (lr == lc) && (lr == le) where
  (lr, lc, le) = (V.length idxx, V.length idxy, V.length vals)
  (idxx, idxy, vals) = V.unzip3 z


-- | predicates for PetscMatrix

inMatRowRange, inMatColRange :: PetscMatrix a -> Int -> Bool
inMatRowRange m = in0m (getMatRows m)
inMatColRange m = in0m (getMatCols m)

inMatrixBounds :: PetscMatrix a -> (Int, Int) -> Bool
inMatrixBounds m (ii, jj) = inMatRowRange m ii && inMatColRange m jj








-- | PetscMatrix getters

getMatrixInfoBase :: PetscMatrix a -> MatrixInfoBase
getMatrixInfoBase (PetscMatrix mib _ _ _) = mib

getMatComm :: PetscMatrix a -> Comm
getMatComm = matComm . getMatrixInfoBase

getMatRows, getMatCols :: PetscMatrix a -> Int
getMatRows = matRows . getMatrixInfoBase
getMatCols = matCols . getMatrixInfoBase













                            
-- | create Mat

matCreate ::
  Comm -> IO Mat
matCreate c = chk1 (matCreate' c)









matCreateMPIAIJWithVectors ::
  Comm -> (Int, Int) -> (Int, Int) ->
  V.Vector Int ->
  V.Vector Int ->
  V.Vector PetscScalar_ ->
  IO Mat
matCreateMPIAIJWithVectors c (m, n) (mm, nn) ix iy ia =
  VS.unsafeWith ixc $ \ip ->
  VS.unsafeWith iyc $ \jp ->
  VS.unsafeWith iac $ \aap ->
   chk1 (matCreateMPIAIJWithArrays0' c m' n' mm' nn' ip jp aap)
     where
           ixc = V.convert $ V.map toCInt ix :: VS.Vector CInt
           iyc = V.convert $ V.map toCInt iy :: VS.Vector CInt
           iac = V.convert ia :: VS.Vector PetscScalar_
           (m', n') = (toCInt m, toCInt n)
           (mm', nn') = (toCInt mm, toCInt nn)


matTranspose :: Mat -> MatReuse_ -> IO Mat
matTranspose mat reuse = chk1 (matTranspose' mat reuse)

-- -- NB : the actual transpose is NOT created by using `matCreateTranspose`, but the definition of matvec will use MatMultTranspose()

matCreateTranspose :: Mat -> IO Mat
matCreateTranspose mat = chk1 (matCreateTranspose' mat)





  

-- | destroy Mat

matDestroy :: Mat -> IO ()
matDestroy = chk0 . matDestroy'












-- | `with` Mat brackets

withMat :: IO Mat -> (Mat -> IO a) -> IO a
withMat mc = bracket mc matDestroy 

withMatCreate :: Comm -> (Mat -> IO a) -> IO a
withMatCreate cc = withMat (matCreate cc)



-- -- | withMatCreateSetup : (create, setSizes, setup, <body>, cleanup) bracket
withMatCreateSetup ::
  Comm ->
  NumRows ->
  NumCols ->
  MatType_ ->
  (Mat -> IO a) ->
  IO a
withMatCreateSetup c m n ty after = withMatCreate c $ \mat -> do
  matSetSizes mat m n
  matSetType mat ty
  matSetup mat
  after mat         -- set values, assemble can be done here




  
-- -- create, setup AND fill





   

-- -- -- | withMatSetValueVectorSafe :  fill + setup Mat with index bound checks
withMatSetValueVectorSafe ::
  Mat ->
  NumRows ->
  NumCols ->
  V.Vector (IdxRow, IdxCol, PetscScalar_) ->
  InsertMode_ ->
  (Mat -> IO a) ->                            -- after assembly
  IO a
withMatSetValueVectorSafe mat m n v_ imode after = do
  matSetValueVectorSafe mat (m, n) v_ imode
  matAssembly mat
  after mat 


matSetDiagonalVectorSafe ::
  Mat ->
  NumRows ->
  V.Vector PetscScalar_ ->   -- diagonal values
  InsertMode_ ->
  (Mat -> IO a) ->
  IO a
matSetDiagonalVectorSafe mat m v = withMatSetValueVectorSafe mat m m (vvDiag m v)




















-- | set Mat values

matZeroEntries :: Mat -> IO ()
matZeroEntries mat = chk0 (matZeroEntries' mat)

matSetValue ::
  Mat -> IdxRow -> IdxCol -> PetscScalar_ -> InsertMode_ -> IO ()
matSetValue m irow icol val mode = chk0 (matSetValueUnsafe' m irow icol val mode)


-- | matSetValueVectorSafe
-- NB !
-- * sets one value at a time 
-- * drops entries that would be written outside of matrix index bounds

matSetValueVectorSafe ::
  Mat ->
  (NumRows, NumCols) ->
  V.Vector (IdxRow, IdxCol, PetscScalar_) ->
  InsertMode_ ->
  IO ()
matSetValueVectorSafe mat (mx, my) v_ mode =
  V.mapM_ mf v_ where
    mf (ix,iy,val)
      | in0m mx ix && in0m my iy = matSetValue mat ix iy val mode
      | otherwise = return ()





-- matSetValuesVectorSafe2 ::
--   Mat ->
--   (NumRows, NumCols) -> 
--   V.Vector (IdxRow, IdxCol, PetscScalar_) ->
--   InsertMode_ ->
--   IO ()
-- matSetValuesVectorSafe2 mat (nrow, ncol) z_ imode =
--   VS.unsafeWith (V.convert ix) $ \ixp ->
--   VS.unsafeWith (V.convert iy) $ \iyp ->
--   VS.unsafeWith (V.convert iv) $ \ivp ->
--      chk0 $ matSetValues0' mat nx ixp ny iyp ivp imode where
--   (nx, ny) = (toCInt $ V.length ix, toCInt $ V.length iy)
--   (ix, iy, iv) = V.unzip3 zu
--   zu = filterMapSafeIndicesV nrow ncol (\(i,j,v) -> (toCInt i,toCInt j,v)) z_

-- | matSetValues using inline-c vecCtx

-- matSetValuesVectorSafe3 ::
--   Mat ->
--   (NumRows, NumCols) -> 
--   V.Vector (IdxRow, IdxCol, PetscScalar_) ->
--   InsertMode_ ->
--   IO ()
-- matSetValuesVectorSafe3 mat (nrow, ncol) z_ imode =
--   chk0 $ matSetValues0vs' mat nvals (V.convert ix) nvals (V.convert iy) (V.convert iv) imode where
--    zu = filterMapSafeIndicesV nrow ncol (\(i,j,v) -> (toCInt i,toCInt j,v)) z_
--    (ix, iy, iv) = V.unzip3 zu
--    nvals = toCInt $ V.length ix












-- | modify Mat

modifyMat ::
  PetscMatrix PetscScalar_ ->
  V.Vector (IdxRow, IdxCol, PetscScalar_) ->
  InsertMode_ ->
  IO ()
modifyMat mm vv imode = do
  matSetValueVectorSafe mat (m, n) vv imode
  matAssembly mat
   where
     (mib, _, _, mat) = fromPetscMatrix mm
     (m, n) = (matRows mib, matCols mib) -- (matRows &&& matCols) mib




    










-- | set Mat properties

matSetType :: Mat -> MatType_ -> IO ()
matSetType mat ty = chk0 (matSetType' mat ty)

matSetSizes0 ::
  Mat ->
  Int ->            -- # local rows
  Int ->            -- # local columns
  NumRows ->            -- # global rows
  NumCols ->            -- # global columns
  IO ()
matSetSizes0 mat mloc nloc mm nn =
  chk0 (matSetSizes0' mat mloc nloc mm nn)


matSetSizes ::
  Mat ->
  NumRows ->            -- # global rows
  NumCols ->            -- # global columns
  IO ()
matSetSizes mat m n
  | m > 0 && n > 0 = chk0 (matSetSizes' mat m n)
  | otherwise = error $ "matSetSizes : invalid size " ++ show (m,n)








-- | nonzero preallocation
-- -- NB : if (onnz, dnnz) are specified, (onz,dnz) are ignored


-- | SEQAIJ


matSeqAIJSetPreallocationConstNZPR :: Mat -> Int -> IO ()
matSeqAIJSetPreallocationConstNZPR mat nz = chk0 (matSeqAIJSetPreallocationConstNZPR' mat nz') where
  nz' = toCInt nz

matSeqAIJSetPreallocationVarNZPR :: Mat -> VS.Vector Int -> IO ()
matSeqAIJSetPreallocationVarNZPR mat nnz = chk0 (matSeqAIJSetPreallocationVarNZPR' mat nnz') where
  nnz' = VG.map toCInt nnz
  
  

-- | MPIAIJ 
matMPIAIJSetPreallocationConstNZPR ::
  Mat ->
  Int ->    -- # nonzeros/row in diagonal block of process-local matrix block
  Int ->    -- # NZ/row in off-diagonal block of local mtx block
  IO ()
matMPIAIJSetPreallocationConstNZPR mat dnz onz =
  chk0 (matMPIAIJSetPreallocationConstNZPR' mat dnz' onz') where
    (dnz', onz') = both (dnz, onz) toCInt


matMPIAIJSetPreallocationDiagonal :: Mat -> IO ()
matMPIAIJSetPreallocationDiagonal mat =
  matMPIAIJSetPreallocationConstNZPR mat 1 0




matMPIAIJSetPreallocationVarNZPR :: Mat -> VS.Vector DNZ -> VS.Vector ONZ -> IO ()
matMPIAIJSetPreallocationVarNZPR mat dnnz onnz =
  chk0 (matMPIAIJSetPreallocationVarNZPR' mat dnnz' onnz')
  where
    dnnz' = VG.map toCInt dnnz
    onnz' = VG.map toCInt onnz







-- | block Mat assembly

matSetBlockSize :: Mat -> Int -> IO ()
matSetBlockSize mat bs = chk0 (matSetBlockSize' mat bs)

matSetValuesBlocked0 ::  (VG.Vector v PetscScalar_) =>
  Mat ->
  V.Vector Int ->
  V.Vector Int ->
  v PetscScalar_ ->
  InsertMode_ ->
  IO ()
matSetValuesBlocked0 mat idxm idxn v imode =
  unsafeWithVS imc $ \idxmp ->
  unsafeWithVS inc $ \idxnp ->
  unsafeWithVS v $ \vp -> 
    -- print (m,n)   -- debug
    chk0 (matSetValuesBlocked0' mat m idxmp n idxnp vp imode)
   where
     (m ,n) = both (V.length imc, V.length inc) toCInt
     imc = V.map toCInt idxm
     inc = V.map toCInt idxn
     -- vc = V.map toCDouble v
     








-- | setup Mat

matSetup :: Mat -> IO ()
matSetup = chk0 . matSetup'










-- | assemble Mat

matAssemblyBegin, matAssemblyEnd :: 
  Mat -> IO ()
matAssemblyBegin = chk0 . matAssemblyBegin'
matAssemblyEnd = chk0 . matAssemblyEnd'

matAssembly :: Mat -> IO ()
matAssembly = matAssemblyBegin >> matAssemblyEnd


-- | withMatAssembly : we can perform some computation while data are in flight

withMatAssembly ::
  Mat -> IO a -> IO ()
withMatAssembly m f = do
  matAssemblyBegin m
  f 
  matAssemblyEnd m








-- | create compatible vectors
matCreateVecs :: Mat -> IO (VecRight, VecLeft)
matCreateVecs m = do
  (vr, vl) <- chk1 $ matCreateVecs' m
  return (VecRight vr, VecLeft vl)

-- vector in which the matrix vector product can be stored
matCreateVecLeft :: Mat -> IO VecLeft
matCreateVecLeft m = liftM VecLeft $ chk1 (matCreateVecLeft' m) 

-- vector that the matrix can be multiplied against
matCreateVecRight :: Mat -> IO VecRight
matCreateVecRight m = liftM VecRight $ chk1 ( matCreateVecRight' m ) 






-- | get Mat properties

matGetOwnershipRange :: Mat -> IO (Int, Int)
matGetOwnershipRange m = chk1 (matGetOwnershipRange' m) >>= bothMf fi

matGetSizeCInt :: Mat -> IO (CInt, CInt)
matGetSizeCInt m = chk1 (matGetSize' m)

matGetSize :: Mat -> IO (NumRows, NumCols)
matGetSize mat = matGetSizeCInt mat >>= \(m,n) -> return (fi m, fi n)

matGetSizeUnsafe, matSize :: Mat -> (NumRows, NumCols)
matGetSizeUnsafe = unsafePerformIO . matGetSize

matSize = matGetSizeUnsafe

matGetSizeCIntUnsafe :: Mat -> (CInt, CInt)
matGetSizeCIntUnsafe = unsafePerformIO . matGetSizeCInt

-- data MatrixOrder = RowMajor | ColMajor deriving (Eq, Show)
-- transposeOrder RowMajor = ColMajor
-- transposeOrder ColMajor = RowMajor
-- -- matrixTranspose (Matrix r c d o)  = Matrix r c d (transposeOrder o)


matGetInfo :: Mat -> MatInfoType_ -> IO MatInfo
matGetInfo mat infotype = chk1 (matGetInfo' mat infotype)

-- matGetRow
--   :: (VG.Vector v PetscScalar_, VG.Vector v CInt,
--       VG.Vector v (CInt, PetscScalar_)) =>
--      Mat -> Int -> IO (v (CInt, PetscScalar_))
matGetRow mat ro = do
  (nc, cols, vals) <- chk1 (matGetRow' mat r)
  let n = fi nc             -- # nonzeros
  colsv <- getIVG n cols
  valsv <- getVG n vals
  return $ VG.zip colsv valsv where
    r = toCInt ro

matRestoreRow :: Mat -> Int -> IO ()
matRestoreRow m ro = chk0 (matRestoreRow0Safe' m r) where
  r = toCInt ro

-- matViewRow :: Mat -> Int -> IO (Int, VS.Vector CInt, VS.Vector PetscScalar_)
matViewRow mat r = bracket (matGetRow mat r) (const $ matRestoreRow mat r) return


matViewRows mat r_ = forM r_ $ \r -> do
  x <- matViewRow mat r
  return (r, x)










matIsStructurallySymmetric :: Mat -> IO PetscBool
matIsStructurallySymmetric mat = chk1 (matIsStructurallySymmetric' mat)



-- | # of diagonals that carry at most f% of the Frobenius norm of mat
matComputeBandwidth :: Mat -> PetscReal_ -> IO CInt
matComputeBandwidth mat f = chk1 (matComputeBandwidth' mat f)

-- | matrix norm
matNorm :: Mat -> MatNorm_ -> IO PetscReal_
matNorm mat nt = chk1 (matNorm' mat nt)

-- | matrix trace
matGetTrace :: Mat -> IO PetscScalar_
matGetTrace mat = chk1 (matGetTrace' mat)






-- | view Mat on stdout

matView0 :: Mat -> PetscViewer -> IO ()
matView0 m v = chk0 (matView' m v)

matView :: Mat -> IO ()
matView m = Viewer.withPetscViewerTypeFmt PM.commWorld ViewerAscii ViewFmtAsciiInfoDetail (matView0 m)

matViewStdout :: Mat -> IO ()
matViewStdout = matView



-- matViewStdoutSelf :: Mat -> IO ()
-- matViewStdoutSelf m = chk0 (matViewStdoutSelf' m)


-- matViewStdoutWorld :: Mat -> IO ()
-- matViewStdoutWorld m = chk0 (matViewStdoutWorld' m)






-- identityMatrix :: Comm -> Int -> Mat -> PetscMatrix
identityMatrix cc n =
  PetscMatrix (MatrixInfoBase cc n n) (ConstNZPR (1, 0))













-- -- petscMatrixBounds :: PetscMatrix a -> ((Int, Int), (Int, Int))
-- petscMatrixBounds pm = pmib (petscMatrixInfoB pm) where
--  pmib mi = (ibx, iby) where
--   ibx = (0, matRows mi - 1)-- :: (Int, Int)
--   iby = (0, matCols mi - 1)-- :: (Int, Int)

petscMatrixInfoB :: PetscMatrix a -> MatrixInfoBase
petscMatrixInfoB (PetscMatrix mi _ _ _) = mi


petscMatrixMat :: PetscMatrix a -> Mat
petscMatrixMat (PetscMatrix _ _ _ m) = m



validDims' :: MatrixInfoBase -> Bool
validDims' mi = nr > 0 && nc > 0
      where (nr, nc) = (matRows &&& matCols) mi

validDims :: MatrixInfoBase -> NZPR -> Bool
validDims mi (ConstNZPR (dnz, onz)) =
  validDims' mi && nz >= 0 && nz <= matCols mi where
   nz = dnz+onz
validDims mi (VarNZPR (dnnz, onnz)) =
  validDims' mi &&
  -- V.length dnnz == matRows mi &&
  -- V.length onnz == matCols mi &&
  VG.all withinCols dnnz && VG.all withinCols onnz where
    withinCols x = x >= 0 && x <= matCols mi
    



-- mkMatrixInfoConstNZPR :: Comm -> Int -> Int -> Int -> MatrixInfo
-- mkMatrixInfoConstNZPR comm nr nc = MIConstNZPR (mkMatrixInfoBase comm nr nc)

-- mkMatrixInfoVarNZPR :: Comm -> Int -> Int -> [Int] -> MatrixInfo
-- mkMatrixInfoVarNZPR comm nr nc = MIVarNZPR (mkMatrixInfoBase comm nr nc)

-- mkMatrix :: (Num a, Eq a) => MatrixInfo -> IO (Mat, a) -> IO PetscMatrix
-- mkMatrix mi matcreatef
--   | validDims mi = do
--       m <- chk1 matcreatef
--       return $ PetscMatrix mi m
--   | otherwise = error "mkMatrix : invalid sizes : no matrix allocated"

-- matCreateSeqAIJConstNZPR :: Comm -> Int -> Int -> Int -> IO PetscMatrix
-- matCreateSeqAIJConstNZPR comm nr nc nz =
--   mkMatrix
--     (mkMatrixInfoConstNZPR comm nr nc nz)
--     (matCreateSeqAIJconstNZperRow1 comm nr nc nz)

-- matCreateSeqAIJVarNZPR :: Comm -> Int -> Int -> [Int] -> IO PetscMatrix
-- matCreateSeqAIJVarNZPR comm nr nc nnz =
--   mkMatrix
--     (mkMatrixInfoVarNZPR comm nr nc nnz)
--     (matCreateSeqAIJ1 comm nr nc nnz)





-- data Matrix a = Matrix {
--   matrixInfo    :: !MatInfo,
--   matrixRowIdxs :: !(V.Vector Int),
--   matrixColIdxs :: !(V.Vector Int),
--   matrixData    :: !(V.Vector a),
--   matrixOrder   :: !MatrixOrder } deriving (Eq, Show)

-- isValidMatrix m = (mx==my) && (V.length mri == V.length mci) where
--   (mx, my) = (matInfoSize . matrixInfo) m
--   mri = matrixRowIdxs m
--   mci = matrixColIdxs m 
  


  


matFDColoringCreate ::
  Mat -> ISColoring -> IO MatFDColoring
matFDColoringCreate mat isc = chk1 $ matFDColoringCreate' mat isc

matFDColoringDestroy ::
  MatFDColoring -> IO ()
matFDColoringDestroy mfc = chk0 $ matFDColoringDestroy' mfc

withMatFDColoring :: Mat -> ISColoring -> (MatFDColoring -> IO a) -> IO a
withMatFDColoring mat iscoloring =
  bracket (matFDColoringCreate mat iscoloring) matFDColoringDestroy










-- | -- MatNullSpace

matNullSpaceCreate :: Comm -> Bool -> Int -> VS.Vector Vec -> IO MatNullSpace
matNullSpaceCreate cc hasc n vecs = chk1 $ matNullSpaceCreate' cc hasc n vecs

matNullSpaceDestroy :: MatNullSpace -> IO ()
matNullSpaceDestroy sp = chk0 $  matNullSpaceDestroy' sp

matNullSpaceRemove :: MatNullSpace -> Vec -> IO ()
matNullSpaceRemove sp v = chk0 $ matNullSpaceRemove' sp v

matSetNullSpace :: Mat -> MatNullSpace -> IO ()
matSetNullSpace amat sp = chk0 $ matSetNullSpace' amat sp









-- | some math operations that use Mat

-- Mat, scalar

matScale :: Mat -> PetscScalar_ -> IO ()
matScale m s = chk0 (matScale' m s)

matShift :: Mat -> PetscScalar_ -> IO ()
matShift m s = chk0 (matShift' m s)

-- Mat, Vec

-- | vresult = m * v
matMult :: Mat -> Vec -> Vec -> IO ()
matMult m v vresult = chk0 (matMult' m v vresult)

-- | vresult = m' * v
matMultTranspose :: Mat -> Vec -> Vec -> IO ()
matMultTranspose m v vresult = chk0 (matMultTranspose' m v vresult)


-- | v3 = m * v1 + v2
matMultAdd :: Mat -> Vec -> Vec -> Vec -> IO ()
matMultAdd m v1 v2 v3 = chk0 (matMultAdd' m v1 v2 v3)

-- | v3 = m' * v1 + v2
matMultTransposeAdd :: Mat -> Vec -> Vec -> Vec -> IO ()
matMultTransposeAdd m v1 v2 v3 = chk0 (matMultTransposeAdd' m v1 v2 v3)


-- | vresult = m' * v
matMultHermitianTranspose :: Mat -> Vec -> Vec -> IO ()
matMultHermitianTranspose m v vresult = chk0 (matMultHermitianTranspose' m v vresult)


-- | v3 = m' * v1 + v2
matMultHermitianTransposeAdd :: Mat -> Vec -> Vec -> Vec -> IO ()
matMultHermitianTransposeAdd m v1 v2 v3 = chk0 (matMultHermitianTransposeAdd' m v1 v2 v3)


-- Mat, Mat

-- matMatMult ma mb reuse fill = ch





-- | helpers

vvDiag :: Int -> V.Vector a -> V.Vector (Int, Int, a)
vvDiag m = V.zip3 ii ii where
  ii = V.fromList [0..m-1]



listToCSR :: Int -> Int -> [a] -> V.Vector (Int, Int, a)
listToCSR m n ll
  | length ll >= (m*n) = V.zip3 ii jj (V.fromList ll)
  | otherwise = error "listToCSR : list too short"
  where
      ii = V.fromList $ concat $ map (replicate n) [0 .. m-1]
      jj = V.fromList $ concat $ replicate m [0 .. n-1]
