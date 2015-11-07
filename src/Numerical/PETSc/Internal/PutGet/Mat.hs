{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.Mat
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | Mat Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.Mat where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils
import Numerical.PETSc.Internal.Managed

import Numerical.PETSc.Internal.Internal

-- import Foreign
import Foreign.C.Types

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State


import Control.Arrow
import Control.Concurrent
import Control.Exception

import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST) -- for HMatrix bits

import qualified Data.Vector as V 
import qualified Data.Vector.Storable as VS (unsafeWith, Vector)



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

checkMatrixData :: MatrixData a -> Bool
checkMatrixData (MatrixData idxx idxy vals) = (lr == lc) && (lr == le) where
  (lr, lc, le) = (V.length idxx, V.length idxy, V.length vals)





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






  

-- | destroy Mat

matDestroy :: Mat -> IO ()
matDestroy = chk0 . matDestroy'












-- | `with` Mat brackets

withMat :: IO Mat -> (Mat -> IO a) -> IO a
withMat mc = bracket mc matDestroy 


     
withMatSetupSetValuesAssembly ::
  IO Mat ->
  V.Vector Int ->
  V.Vector Int ->
  V.Vector PetscScalar_ ->
  InsertMode_ ->
  (Mat -> IO a) ->
  IO a
withMatSetupSetValuesAssembly mc ix iy vals imode f =
  withMat mc $ \mat -> do
   matSetup mat
   matSetValuesVectorSafe mat ix iy vals imode
   matAssembly mat
   f mat


   











-- | set Mat values


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



-- convert :: (Vector v a, Vector w a) => v a -> w a


    

matSetValues ::
  Mat ->
  [CInt] ->         -- first index positions
  [CInt] ->         -- second index positions
  [PetscScalar_] -> -- values to be inserted
  InsertMode_ ->    -- `AddValues` or `InsertValues`
  IO () 
matSetValues m idxx idxy vals im = chk0 (matSetValues' m idxx idxy vals im)

matSetValuesAdd, matSetValuesInsert :: 
  Mat -> [CInt] -> [CInt] -> [PetscScalar_] -> IO ()
matSetValuesAdd m idxx idxy vals = chk0 (matSetValuesAdd' m idxx idxy vals)
matSetValuesInsert m idxx idxy vals = chk0 (matSetValuesInsert' m idxx idxy vals)

matSetValuesSafe ::
  Mat ->
  [CInt] ->          -- first dimension index array 
  [CInt] ->          -- second " " "
  [PetscScalar_] ->  -- values to fill the matrix with
  InsertMode_ ->     -- `AddValues` or `InsertValues`
  IO ()
matSetValuesSafe m idxx idxy vals im
  | safeFlag = matSetValues m idxx idxy vals im
  | otherwise = error "matSetValuesSafe : invalid indices "
     where
       safeFlag = c1 && c2
       (lix, liy, lv) = (length idxx, length idxy, length vals)
       c1 = lix == liy && lix == lv    -- compatible array lengths
       (mx,my) = matGetSizeCIntUnsafe m
       c2 = inBoundsUnsortedList idxx (0, mx) &&  -- all indices in bounds
            inBoundsUnsortedList idxy (0, my)






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

matGetOwnershipRange ::
  Mat -> IO (Int, Int)
matGetOwnershipRange m = chk1 (matGetOwnershipRange' m)

matGetSizeCInt ::
  Mat -> IO (CInt, CInt)
matGetSizeCInt m = chk1 (matGetSize' m)

matGetSize ::
  Mat -> IO (Int, Int)
matGetSize mat = matGetSizeCInt mat >>= \(m,n) -> return (fi m, fi n)

matGetSizeUnsafe, matSize :: Mat -> (Int, Int)
matGetSizeUnsafe = unsafePerformIO . matGetSize

matSize = matGetSizeUnsafe

matGetSizeCIntUnsafe ::
  Mat -> (CInt, CInt)
matGetSizeCIntUnsafe = unsafePerformIO . matGetSizeCInt

-- data MatrixOrder = RowMajor | ColMajor deriving (Eq, Show)
-- transposeOrder RowMajor = ColMajor
-- transposeOrder ColMajor = RowMajor
-- -- matrixTranspose (Matrix r c d o)  = Matrix r c d (transposeOrder o)




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


















