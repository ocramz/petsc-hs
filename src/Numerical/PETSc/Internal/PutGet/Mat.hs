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
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils (both, fi, toCInt, in0m, allIn0mV)

import Numerical.PETSc.Internal.Storable.Vector
import Numerical.PETSc.Internal.Storable.Matrix
import Numerical.PETSc.Internal.Storable.Common (unsafeWithVS)

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Control.Arrow
import Control.Concurrent
import Control.Exception

import qualified Data.Vector as V 
import qualified Data.Vector.Storable as VS (unsafeWith, Vector)
import qualified Data.Vector.Generic as VG





-- | a datatype encapsulating matrix information and the typed pointer
data PetscMatrix = PetscMatrix !MatrixInfo Mat

data MatrixInfo =
  MIConstNZPR MatrixInfoBase !Int
  | MIVarNZPR MatrixInfoBase !(V.Vector Int)

data MatrixInfoBase =
  MatrixInfoBase { matComm  :: Comm
                  ,matRows  :: !Int
                  ,matCols  :: !Int
                  -- ,matOrder :: !MatrixOrder
                 } deriving (Eq, Show)


data MatrixData a =
  MatrixData { matDataRowIdxs :: !(V.Vector Int),
               matDataColIdxs :: !(V.Vector Int),
               matDataEntries :: !(V.Vector a)} deriving (Eq, Show)






-- | predicates

-- | predicates for MatrixData

checkMatrixData :: MatrixData a -> Bool
checkMatrixData (MatrixData idxx idxy vals) = (lr == lc) && (lr == le) where
  (lr, lc, le) = (V.length idxx, V.length idxy, V.length vals)


-- | predicates for PetscMatrix

inMatRowRange, inMatColRange :: PetscMatrix -> Int -> Bool
inMatRowRange m = in0m (getMatRows m)
inMatColRange m = in0m (getMatCols m)

inMatrixBounds :: PetscMatrix -> (Int, Int) -> Bool
inMatrixBounds m (ii, jj) = inMatRowRange m ii && inMatColRange m jj




-- | PetscMatrix getters

getMatrixInfoBase :: PetscMatrix -> MatrixInfoBase
getMatrixInfoBase (PetscMatrix (MIConstNZPR mib _) _) = mib

getMatComm :: PetscMatrix -> Comm
getMatComm = matComm . getMatrixInfoBase

getMatRows, getMatCols :: PetscMatrix -> Int
getMatRows = matRows . getMatrixInfoBase
getMatCols = matCols . getMatrixInfoBase




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





                            
-- | create Mat

matCreate ::
  Comm -> IO Mat
matCreate comm = chk1 (matCreate' comm)

matCreateSeqAIJVarNZPR ::
  Comm -> Int -> Int -> [Int] -> IO Mat
matCreateSeqAIJVarNZPR comm m n nnz =
  chk1 (matCreateSeqAIJ1 comm m n nnz)

matCreateSeqAIJConstNZPR ::
  Comm -> Int -> Int -> Int -> IO Mat
matCreateSeqAIJConstNZPR comm m n nz =
  chk1 (matCreateSeqAIJconstNZperRow1 comm m n nz)

matCreateAIJ :: Comm -> PetscInt_ -> PetscInt_ -> PetscInt_ -> PetscInt_ ->
      PetscInt_ -> [PetscInt_] ->
      PetscInt_ -> [PetscInt_] ->
      IO Mat
matCreateAIJ comm m n mm nn dnz dnnz onz onnz =
  chk1 ( matCreateAIJ' comm m n mm nn dnz dnnz onz onnz)

matCreateAIJDecideV ::
  Comm -> Int -> Int -> Int -> V.Vector Int -> Int -> V.Vector Int -> IO Mat
matCreateAIJDecideV comm mm nn dnz dnnz onz onnz =
  chk1 (matCreateAIJDecideVS' comm mmc nnc dnzc dnnz_ onzc onnz_) where
    dnnz_ = V.convert (V.map toCInt dnnz)
    onnz_ = V.convert (V.map toCInt onnz)
    mmc = toCInt mm
    nnc = toCInt nn
    dnzc = toCInt dnz
    onzc = toCInt onz

matCreateAIJDecideConstNZPR :: Comm -> Int -> Int -> Int -> Int -> IO Mat
matCreateAIJDecideConstNZPR comm mm nn dnz onz =
  chk1 (matCreateAIJ0DecideConstNZPR' comm mmc nnc dnzc onzc) where
    mmc = toCInt mm
    nnc = toCInt nn
    dnzc = toCInt dnz
    onzc = toCInt onz


-- | matCreateMPIAIJWithArrays

matCreateMPIAIJWithArrays ::
  Comm -> [PetscInt_] -> [PetscInt_] -> [PetscScalar_] -> IO Mat
matCreateMPIAIJWithArrays comm idxx idxy vals =
  chk1 (matCreateMPIAIJWithArrays' comm idxx idxy vals)


matCreateMPIAIJWithVectors ::
  Comm -> (Int, Int) -> (Int, Int) ->
  V.Vector Int ->
  V.Vector Int ->
  V.Vector PetscScalar_ ->
  IO Mat
matCreateMPIAIJWithVectors comm (m, n) (mm, nn) ix iy ia =
  VS.unsafeWith ixc $ \ip ->
  VS.unsafeWith iyc $ \jp ->
  VS.unsafeWith iac $ \aap ->
   chk1 (matCreateMPIAIJWithArrays0' comm m' n' mm' nn' ip jp aap)
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
withMatCreate comm = withMat (matCreate comm)

-- | withMatCreateSetup : (create, setSizes, setup, <body>, cleanup) bracket
withMatCreateSetup ::
  Comm ->
  Int ->
  Int ->
  MatType_ ->
  (Mat -> IO a) ->
  IO a
withMatCreateSetup comm m n ty after = withMatCreate comm $ \mat -> do
  matSetSizes mat m n
  matSetType mat ty
  matSetup mat
  after mat         -- set values, assemble can be done here

-- withMatCreateSetup1 ::
--   Comm ->
--   Int ->
--   Int ->
--   MatType_ -> 
--   (Mat -> IO a) ->
--   (Mat -> IO b) ->
--   (Mat -> IO c) ->
--   IO c
-- withMatCreateSetup1 comm m n ty before setup  after =
--   withMatCreate comm $ \mat -> do
--     matSetSizes mat m n
--     matSetType mat ty
--     before mat    -- e.g. set block size for MPIBAIJ
--     setup mat     -- matMPISetPreallocation or matSetup
--     after mat



  
-- -- create, setup AND fill

-- | withMatNew :  creation, setup, fill, use, cleanup ; batteries included
withMatNew ::
  Comm ->                               -- MPI communicator
  Int ->                                -- # rows
  Int ->                                -- # cols
  MatType_ ->
  V.Vector (Int, Int, PetscScalar_) ->  -- (rowIdx, colIdx, value)
  InsertMode_ ->                        -- `InsertValues` or `AddValues`
  (Mat -> IO a) ->                      -- bracket body
  IO a 
withMatNew comm m n ty v_ imode after =
  withMatCreateSetup comm m n ty $ \mat -> 
    withMatSetValueVectorSafe mat m n v_ imode after

-- | withMatSetValueVectorSafe :  fill + setup Mat with index bound checks
withMatSetValueVectorSafe ::
  Mat ->
  Int -> Int ->
  V.Vector (Int, Int, PetscScalar_) ->
  InsertMode_ ->
  (Mat -> IO a) ->
  IO a
withMatSetValueVectorSafe mat m n v_ imode after = do
  matSetValueVectorSafe mat (m, n) v_ imode
  matAssembly mat
  after mat 






{- -- BROKEN due to matSetValuesVector, see t5 -}
withMatSetupSetValuesAssembly ::  
  IO Mat ->
  Int -> Int ->              -- Mat sizes
  V.Vector Int ->            -- i indices
  V.Vector Int ->            -- j " 
  V.Vector PetscScalar_ ->   -- mat values
  InsertMode_ ->             
  (Mat -> IO a) ->
  IO a
withMatSetupSetValuesAssembly mc m n ix iy vals imode after =
  withMat mc $ \mat -> do
   -- matSetSizes mat m n
   matSetup mat
   matSetValuesVectorSafe mat ix iy vals imode      
   matAssembly mat
   after mat
{- -- -}














-- | set Mat values

matZeroEntries :: Mat -> IO ()
matZeroEntries mat = chk0 (matZeroEntries' mat)

matSetValue ::
  Mat -> Int -> Int -> PetscScalar_ -> InsertMode_ -> IO ()
matSetValue m irow icol val mode = chk0 (matSetValueUnsafe' m irow icol val mode)

matSetValueSafe ::
  Mat -> (Int, Int) -> Int -> Int -> PetscScalar_ -> InsertMode_ -> IO ()
matSetValueSafe m (mm, nn) irow icol val mode
  | in0m mm irow && in0m nn icol = matSetValue m irow icol val mode
  | otherwise =
     error $ "matSetValueSafe : index "++ show (irow,icol) ++" out of bounds"

matSetValueVectorSafe ::
  Mat -> (Int, Int) -> V.Vector (Int, Int, PetscScalar_) -> InsertMode_ -> IO ()
matSetValueVectorSafe m (mx, my) v_ mode =
  V.mapM_ (\(ix,iy,val) -> matSetValueSafe m (mx, my) ix iy val mode) v_





matSetValuesVector1 ::
  Mat ->
  V.Vector CInt ->
  V.Vector CInt ->
  V.Vector PetscScalar_ ->
  InsertMode_ ->
  IO ()
matSetValuesVector1 ma ix iy iv im =
  unsafeWithVS ix $ \ixp ->
  unsafeWithVS iy $ \iyp ->
  unsafeWithVS iv $ \ivp -> chk0 (matSetValues0' ma nx ixp ny iyp ivp im)
   where
     (nx, ny) = both (V.length ix, V.length iy) toCInt







{-| the matSetValues Vector interface is broken (see t5) -}

{-| -- --     DO NOT USE      -- --  -}

matSetValuesVector ::
  Mat ->
  V.Vector Int ->
  V.Vector Int ->
  V.Vector PetscScalar_ ->
  InsertMode_ ->
  IO ()
matSetValuesVector m x y v = msvv0 m nx0 ny0 xc yc vc
  where
    xc = V.convert $ V.map toCInt x :: VS.Vector CInt
    yc = V.convert $ V.map toCInt y :: VS.Vector CInt
    vc = V.convert v :: VS.Vector PetscScalar_
    nx0 = toCInt $ V.length x
    ny0 = toCInt $ V.length y
    msvv0 ma nx ny idxx idxy vals im =
      VS.unsafeWith idxx $ \ix ->
      VS.unsafeWith idxy $ \iy ->
      VS.unsafeWith vals $ \iv -> chk0 (matSetValues0' ma nx ix ny iy iv im)

matSetValuesVectorSafe ::
  Mat ->
  V.Vector Int ->
  V.Vector Int ->
  V.Vector PetscScalar_ ->
  InsertMode_ ->
  IO ()
matSetValuesVectorSafe m ix iy v
  | c1 && c2 = matSetValuesVector m ix iy v
  | otherwise = error "matSetValuesVectorSafe : incompatible indices"
     where
       (mx, my) = matSize m
       (lx, ly) = (V.length ix, V.length iy)
       c1 = lx == ly
       c2 = allIn0mV mx ix && allIn0mV my iy

{-| --    UNTIL HERE    -}







    










-- | set Mat properties

matSetType :: Mat -> MatType_ -> IO ()
matSetType mat ty = chk0 (matSetType' mat ty)

matSetSizes0 ::
  Mat ->
  Int ->            -- # local rows
  Int ->            -- # local columns
  Int ->            -- # global rows
  Int ->            -- # global columns
  IO ()
matSetSizes0 mat mloc nloc mm nn =
  chk0 (matSetSizes0' mat mloc nloc mm nn)


matSetSizes ::
  Mat ->
  Int ->            -- # global rows
  Int ->            -- # global columns
  IO ()
matSetSizes mat m n
  | m > 0 && n > 0 = chk0 (matSetSizes' mat m n)
  | otherwise = error $ "matSetSizes : invalid size " ++ show (m,n)


matSeqAIJSetPreallocation :: Mat -> Int -> [Int] -> IO ()
matSeqAIJSetPreallocation mat nz nnz = chk0 (matSeqAIJSetPreallocation' mat nz' nnz') where
  nz' = toCInt nz 
  nnz' = map toCInt nnz 


-- | nonzero preallocation of AIJ parallel matrix format
-- -- NB : if (onnz, dnnz) are specified, (onz,dnz) are ignored

matMPIAIJSetPreallocation0 ::
  Mat ->
  CInt ->    -- # nonzeros/row in diagonal block of process-local matrix slice
  [CInt] ->  -- # " in various rows of diagonal part of "" (alternative to ^)
  CInt ->    -- # NZ/row in off-diagonal block of local mtx slice
  [CInt] ->  -- # " in various rows of off-diagonal part of " (alt.to ^)
  IO ()
matMPIAIJSetPreallocation0 mat dnz dnnz onz onnz =
  chk0 (matMPIAIJSetPreallocation' mat dnz dnnz onz onnz)

matMPIAIJSetPreallocationConstNZPR ::
  Mat ->
  Int ->    -- # nonzeros/row in diagonal block of process-local matrix block
  Int ->    -- # NZ/row in off-diagonal block of local mtx block
  IO ()
matMPIAIJSetPreallocationConstNZPR mat dnz onz =
  chk0 (matMPIAIJSetPreallocationConstNZPR' mat dnz' onz') where
    (dnz', onz') = both (dnz, onz) toCInt



-- -- block Mat assembly

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












-- | get Mat properties

matGetOwnershipRange :: Mat -> IO (Int, Int)
matGetOwnershipRange m = chk1 (matGetOwnershipRange' m)

matGetSizeCInt :: Mat -> IO (CInt, CInt)
matGetSizeCInt m = chk1 (matGetSize' m)

matGetSize :: Mat -> IO (Int, Int)
matGetSize mat = matGetSizeCInt mat >>= \(m,n) -> return (fi m, fi n)

matGetSizeUnsafe, matSize :: Mat -> (Int, Int)
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

matViewStdout :: Mat -> IO ()
matViewStdout m = chk0 (matViewStdout' m)








identityMatrix :: Comm -> Int -> Mat -> PetscMatrix
identityMatrix comm n =
  PetscMatrix (MIConstNZPR (MatrixInfoBase comm n n) 1)




mkMatrixInfoBase :: Comm -> MatrixData a -> MatrixInfoBase
mkMatrixInfoBase comm (MatrixData idxx idxy vals) =
  MatrixInfoBase comm (V.length idxx) (V.length idxy)







petscMatrixBounds :: PetscMatrix -> ((Int, Int), (Int, Int))
petscMatrixBounds pm = pmib (petscMatrixInfoB pm) where
 pmib mi = (ibx, iby) where
  ibx = (0, matRows mi - 1) :: (Int, Int)
  iby = (0, matCols mi - 1) :: (Int, Int)

petscMatrixInfoB :: PetscMatrix -> MatrixInfoBase
petscMatrixInfoB (PetscMatrix (MIConstNZPR mi _) _) = mi
petscMatrixInfoB (PetscMatrix (MIVarNZPR mi _) _) = mi

petscMatrixMat :: PetscMatrix -> Mat
petscMatrixMat (PetscMatrix (MIConstNZPR _ _ ) m) = m
petscMatrixMat (PetscMatrix (MIVarNZPR _ _ ) m) = m

validDims0 :: Int -> Int -> Bool
validDims0 nr nc = nr > 0 && nc > 0 

validDims' :: MatrixInfoBase -> Bool
validDims' mi = validDims0 nr nc
      where (nr, nc) = (matRows &&& matCols) mi

validDims :: MatrixInfo -> Bool
validDims (MIConstNZPR mi nz) = validDims' mi && nz >= 0 && nz <= matCols mi
validDims (MIVarNZPR mi nnz) =
  validDims' mi && V.length nnz == matRows mi && V.all withinCols nnz where
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
  


-- matGetOwnershipRange m = chk1 ( matGetOwnershipRange1 m ) >>= (`bothM` fi)
  


matFDColoringCreate ::
  Mat -> ISColoring -> IO MatFDColoring
matFDColoringCreate mat isc = chk1 $ matFDColoringCreate' mat isc

matFDColoringDestroy ::
  MatFDColoring -> IO ()
matFDColoringDestroy mfc = chk0 $ matFDColoringDestroy' mfc

withMatFDColoring :: Mat -> ISColoring -> (MatFDColoring -> IO a) -> IO a
withMatFDColoring mat iscoloring =
  bracket (matFDColoringCreate mat iscoloring) matFDColoringDestroy














-- | some math operations that use Mat

matScale :: Mat -> PetscScalar_ -> IO ()
matScale m s = chk0 (matScale' m s)

matShift :: Mat -> PetscScalar_ -> IO ()
matShift m s = chk0 (matShift' m s)


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
