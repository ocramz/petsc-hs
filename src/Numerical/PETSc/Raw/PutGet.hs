{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Raw.Internal
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | Mid-level interface: catching exceptions and hiding pointers in lexical
--   scope of `bracket`s
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Raw.PutGet where

import Numerical.PETSc.Raw.InlineC
import Numerical.PETSc.Raw.Types
import Numerical.PETSc.Raw.Exception
import Numerical.PETSc.Raw.Utils

import Numerical.PETSc.Raw.Internal

import Foreign
-- import Foreign.Storable
-- import Foreign.Marshal.Array
-- import Foreign.Ptr
-- import Foreign.C.String
import Foreign.C.Types
-- import Foreign.C

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Control.Arrow
import Control.Concurrent
import Control.Exception

import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST) -- for HMatrix bits

import qualified Data.Vector as V
import qualified Data.Vector.Storable as V (unsafeWith, unsafeFromForeignPtr, unsafeToForeignPtr)



withPtr f = alloca $ \e -> do
  err <- f e
  res <- peek e
  return (res, err)
  

--------
{- from Base :
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar :: MVar a -> (a -> IO (a,b)) -> IO b  -}
---------


-- * IS

data IsInfo = IsInfo { isInfoMpiComm :: Comm,
                       isIndexSet :: ![Int]} deriving (Eq, Show)

type ISI = (IS, IsInfo)
data PetscIs = PIs (MVar ISI)


withIsCreateGeneral :: IsInfo -> PetscCopyMode_ -> (IS -> IO a) -> IO a
withIsCreateGeneral iis mode =
  bracketChk (isCreateGeneral comm n idx mode) isDestroy where
    comm = isInfoMpiComm iis
    n = toCInt $ length idxi
    idx = map toCInt idxi
    idxi = isIndexSet iis

isColoringCreate ::
  Comm ->
  CInt ->
  CInt ->
  [CInt] ->
  PetscCopyMode_ ->
  IO ISColoring
isColoringCreate comm nc n cols cm =
  chk1 (isColoringCreate' comm nc n cols cm)

isColoringDestroy :: ISColoring -> IO ()
isColoringDestroy isc = chk0 (isColoringDestroy' isc)

dmCreateColoring :: DM -> ISColoringType_ -> IO ISColoring
dmCreateColoring dm coloringtype = chk1 $ dmCreateColoring' dm coloringtype


withDmIsColoring :: DM -> ISColoringType_ -> (ISColoring -> IO a) -> IO a
withDmIsColoring dm ct = bracket (dmCreateColoring dm ct) isColoringDestroy















-- * Vec


data VecInfo = VecInfo 
 {vecInfoMpiComm :: Comm ,
  vecInfoSizeLocal :: !Int ,
  vecInfoSizeGlobal :: !Int } deriving (Eq, Show)

data PVec = PVec { vec     :: !Vec,
                   vecInfo :: !VecInfo }
data PetscVec = PetscVec { unPetscVec :: MVar PVec }



makePetscVec v vi = do
  m <- newMVar (PVec v vi)
  return $ PetscVec m



usePetscVec x f = do
  let mv = unPetscVec x
  withMVar mv (`applyVec` f)
  return $ PetscVec mv

updatePetscVec x g = do
  let mv = unPetscVec x
  modifyMVar_ mv (`applyVec` g)
  return $ PetscVec mv



applyVec :: PVec -> (Vec -> IO Vec) -> IO PVec
applyVec vv fm = do
  out1 <- fm $ vec vv
  return $ PVec out1 (vecInfo vv)

applyVec' vv fm = do
  fm (vec vv)
  return (PVec (vec vv) (vecInfo vv))





vecCreate :: Comm -> IO Vec
vecCreate comm = chk1 (vecCreate' comm)

vecCreateMPI :: Comm -> Int -> Int -> IO Vec
vecCreateMPI comm nLocal nGlobal = chk1 (vecCreateMPI' comm nLocal nGlobal)

vecCreateMPIInfo :: VecInfo -> IO Vec
vecCreateMPIInfo vi = chk1 (vecCreateMPI' comm nl ng) where
  nl = vecInfoSizeLocal vi
  ng = vecInfoSizeGlobal vi
  comm = vecInfoMpiComm vi

vecDestroy :: Vec -> IO ()
vecDestroy v = chk0 (vecDestroy' v)

withVecCreate :: VecInfo -> (Vec -> IO a) -> IO a
withVecCreate vv = bracket (vecCreate comm) vecDestroy where
  comm = vecInfoMpiComm vv

withVecCreateMPI :: VecInfo -> (Vec -> IO a) -> IO a
withVecCreateMPI vv =
  bracket (vecCreateMPI comm nLoc nGlob) vecDestroy where
    nLoc = vecInfoSizeLocal vv
    nGlob = vecInfoSizeGlobal vv
    comm = vecInfoMpiComm vv


vecSetSizes :: Vec -> Int -> IO ()
vecSetSizes v n = chk0 $ vecSetSizes1 v (toCInt n)

withVecPipeline :: VecInfo -> (Vec -> IO a) -> (Vec -> IO b) -> IO b
withVecPipeline vv pre post = withVecCreate vv $ \v -> do
  vecSetSizes v nDim
  pre v
  vecAssemblyChk v
  post v
    where
      nDim = vecInfoSizeGlobal vv

withVecMPIPipeline :: VecInfo -> (Vec -> IO a) -> (Vec -> IO b) -> IO b
withVecMPIPipeline vv pre post = withVecCreateMPI vv $ \v -> do
  pre v
  vecAssemblyChk v
  post v

withVecMPIPipeline1 :: VecInfo -> (Vec -> IO Vec) -> (Vec -> IO a) -> IO a
withVecMPIPipeline1 vv pre post = withVecCreateMPI vv $ \v -> do
  v' <- pre v
  vecAssemblyChk v'
  post v'

vecAssemblyChk :: Vec -> IO ()
vecAssemblyChk v = chk0 (vecAssemblyBegin' v) >> chk0 (vecAssemblyEnd' v)

-- withVecAssemblyChk v f = chk0 (vecAssemblyBegin' v) >> f >> chk0 (vecAssemblyEnd' v)

-- | withVecAssemblyChk : perform a computation while vector assembly takes place
withVecAssemblyChk :: Vec -> IO a -> IO a
withVecAssemblyChk v = bracket_ (chk0 $ vecAssemblyBegin' v) (chk0 $ vecAssemblyEnd' v)

-- | vecEqual : compares two vectors. Returns true if the two vectors are either pointing to the same memory buffer, or if the two vectors have the same local and global layout as well as bitwise equality of all entries. Does NOT take round-off errors into account.
vecEqual :: Vec -> Vec -> IO Bool
vecEqual v1 v2 = chk1 $ vecEqual1 v1 v2

vecCopy_ vorig vcopy = chk0 $ vecCopy1 vorig vcopy
vecCopy vorig vcopy = do {vecCopy_ vorig vcopy ;  return vcopy}

vecDuplicate v = chk1 $ vecDuplicate1 v

-- | vecCopyDuplicate : duplicates Vec and copies content
vecCopyDuplicate :: Vec -> IO Vec
vecCopyDuplicate v = do
  v1 <- vecDuplicate v
  vecCopy v v1


vecSetValuesUnsafe :: Vec -> [CInt] -> [PetscScalar_] -> InsertMode_ -> IO ()
vecSetValuesUnsafe v ix y im =
  withArray ix $ \ixx ->
   withArray y $ \yy -> chk0 $ vecSetValues' v ni ixx yy im 
  where
  ni = toCInt $ length ix


vecSetValuesSafe :: Vec -> [Int] -> [PetscScalar_] -> InsertMode_ -> IO ()
vecSetValuesSafe v ix y im
  | safeFlag ix y sv = vecSetValuesUnsafe v ix' y im
  | otherwise = error "vecSetValuesSafe : "
      where
        sv = vecGetSizeUnsafe v
        ix' = map toCInt ix
        safeFlag ix_ y_ sv_ = c1 && c2 where
          c1 = length ix_ == length y_
          c2 = a >= 0 && b <= sv_
          ixs = qsort ix_
          (a, b) = (head ixs, last ixs)





vecView :: Vec -> PetscViewer -> IO ()
vecView v vi = chk0 $ vecView1 v vi

vecSetName v name = chk0 $ vecSetName1 v name

vecSet_ :: Vec -> PetscScalar_ -> IO ()
vecSet_ v n = chk0 $ vecSet1 v n

vecSet :: Vec -> PetscScalar_ -> IO Vec
vecSet v n = do {vecSet_ v n ; return v}

vecGetOwnershipRange :: Vec -> IO (Int, Int)
vecGetOwnershipRange v = 
  chk1 (vecGetOwnershipRange1 v) 


-- | Vec math. operations

vecDot :: Vec -> Vec -> IO PetscScalar_
vecDot v1 v2 = chk1 $ vecDot1 v1 v2

vecNorm :: Vec -> VecNorm_ -> IO PetscScalar_
vecNorm v nt = chk1 $ vecNorm1 nt v

vecSum :: Vec -> IO PetscScalar_
vecSum v = chk1 $ vecSum1 v

-- | Vec math (in-place, destructive) operations 
vecLog_, vecExp_, vecAbs_ :: Vec -> IO ()
vecLog, vecExp, vecAbs :: Vec -> IO Vec
vecLog_ v = chk0 $ vecLog' v
vecLog v = do {vecLog_ v; return v}
vecExp_ v = chk0 $ vecExp' v
vecExp v = do {vecExp_ v; return v}
vecAbs_ v = chk0 $ vecAbs' v
vecAbs v = do {vecAbs_ v ; return v}

vecScale_ :: Vec -> PetscScalar_ -> IO ()
vecScale_ v a = chk0 $ vecScale' v a
vecScale :: Vec -> PetscScalar_ -> IO Vec
vecScale v a = do {vecScale_ v a; return v}

-- | AXPY : y = a x + y
-- -- NB : x and y must be different vectors (i.e. distinct pointers)
vecAxpy :: PetscScalar_ -> Vec -> Vec -> IO Vec
vecAxpy a y x = do
  chk0 $ vecAxpy' y a x
  return y

-- | WAXPY : w = a x + y
-- -- NB : w cannot be either x or y, but x and y can be the same
vecWaxpy_ w a x y = chk0 $ vecWaxpy' w a x y
vecWaxpy w a x y = do {vecWaxpy_ w a x y; return w}

vecWaxpySafe a vx vy = withVecCreate vi $ \w ->
  vecWaxpy w a x y  -- NB: w is created on same Comm as x
   where
    vi = vecInfo vx
    x = vec vx
    y = vec vy

vecVecSum , (.+) :: Vec -> Vec -> IO Vec
vecVecSum = vecAxpy 1
(.+) = vecVecSum

vecVecSumSafe = vecWaxpySafe 1




vecGetSize :: Vec -> IO Int
vecGetSize v = liftM fi $ chk1 ( vecGetSize' v) 

vecGetSizeUnsafe :: Vec -> Int
vecGetSizeUnsafe = unsafePerformIO . vecGetSize

vecSize :: Vec -> Int
vecSize = vecGetSizeUnsafe



-- | print a Vec tor stdout 

vecViewStdout :: Vec -> IO ()
vecViewStdout v = chk0 $ vecViewStdout1 v




-- | getting/restoring a contiguous array from/to a Vec 

vecGetArray :: Vec -> Int -> IO [PetscScalar_]
vecGetArray v sz = chk1 $ vecGetArray' v sz

vecGetArraySafe :: Vec -> IO [PetscScalar_]
vecGetArraySafe v = do
  sz <- vecGetSize v
  vecGetArray v sz

-- vecGetVector v =
--   vecGetArray v >>= newForeignPtr_ >>= \l -> return $ V.unsafeFromForeignPtr0 l n
--    where n = vecSize v


-- PETSC_EXTERN PetscErrorCode VecRestoreArray(Vec,PetscScalar**);
vecRestoreArray v c = chk0 $ vecRestoreArray' v c

-- vecRestoreArrayV v p = go v 0 (vd - 1) where
--   go w n sz
--     | n == sz = V.unsafeWith w (\q -> pokeElemOff q n ())

-- vra0 v a idx
--   | idx > 0 = do
--       V.unsafeWith v (\q -> pokeElemOff q idx (a !! idx))
--       vra0 v a (idx - 1)
--   | otherwise = V.unsafeWith v (\q -> pokeElemOff q idx (a !! idx))

fvra v a i = V.unsafeWith v (\q -> pokeElemOff q i (a !! i))




withVecGetArray v = bracket (vecGetArraySafe v) (vecRestoreArray v)

vecGetArraySafeMVar v = do
  sz <- vecGetSize v
  a <- vecGetArray v sz
  newMVar a
vecRestoreArrayMVar v mc = withMVar mc (vecRestoreArray v)

withVecGetArrayMVar v =
  bracket (vecGetArraySafeMVar v) (vecRestoreArrayMVar v) 

withVecGetArrayMVarUse v f = withVecGetArrayMVar v $ \mv -> withMVar mv f
-- withVecGetArrayMVarModify v f = withVecGetArrayMVar v $ \mv -> modifyMVar_ mv f


vecRestoreArrayB v ar = alloca $ \ p -> do
  pokeArray p ar
  with p $ \pp -> chk0 $ vecRestoreArray0' v pp




bracket1 allocate release io = mask $ \restore -> do
  stuff <- allocate
  restore (io stuff) `finally` release stuff



-- withVecGetArray1d' x m ms =
--   bracketChk (vecGetArray1d' x m ms)

-- vecGetArray1d0 x m ms = do
--   p <- chk1 $ vecGetArray1d' x m ms
--   return p


















    
-- * Mat

withMat ::
  Comm -> (Mat -> IO a) -> IO a
withMat comm = bracketChk (matCreate' comm) matDestroy'

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

matDestroy ::
  Mat -> IO ()
matDestroy = chk0 . matDestroy'


-- withVecCreateMPI :: VecInfo -> (Vec -> IO a) -> IO a
-- withVecCreateMPI vv =
--   bracket (vecCreateMPI comm nLoc nGlob) vecDestroy where
--     nLoc = vecInfoSizeLocal vv
--     nGlob = vecInfoSizeGlobal vv
--     comm = vecInfoMpiComm vv

-- withMatCreateSeqAIJVarNZPR mi =
--   bracket (matCreateSeqAIJVarNZPR comm m n nnz) matDestroy
--    where
--      comm = 
     


-- | setting matrix values 

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



matSetup ::
  Mat -> IO ()
matSetup = chk0 . matSetup'

matAssemblyBegin, matAssemblyEnd :: 
  Mat -> IO ()
matAssemblyBegin = chk0 . matAssemblyBegin'
matAssemblyEnd = chk0 . matAssemblyEnd'

matAssembly ::
  Mat -> IO ()
matAssembly = matAssemblyBegin >> matAssemblyEnd

-- | withMatAssembly : we can perform some computation while data are in flight

withMatAssembly ::
  Mat -> IO a -> IO ()
withMatAssembly m f = do
  matAssemblyBegin m
  f 
  matAssemblyEnd m

matGetOwnershipRange ::
  Mat -> IO (Int, Int)
matGetOwnershipRange m = chk1 (matGetOwnershipRange' m)

matGetSizeCInt ::
  Mat -> IO (CInt, CInt)
matGetSizeCInt m = chk1 (matGetSize' m)

matGetSize ::
  Mat -> IO (Int, Int)
matGetSize mat = matGetSizeCInt mat >>= \(m,n) -> return (fi m, fi n)

matGetSizeUnsafe ::
  Mat -> (Int, Int)
matGetSizeUnsafe = unsafePerformIO . matGetSize

matGetSizeCIntUnsafe ::
  Mat -> (CInt, CInt)
matGetSizeCIntUnsafe = unsafePerformIO . matGetSizeCInt

-- data MatrixOrder = RowMajor | ColMajor deriving (Eq, Show)
-- transposeOrder RowMajor = ColMajor
-- transposeOrder ColMajor = RowMajor
-- -- matrixTranspose (Matrix r c d o)  = Matrix r c d (transposeOrder o)

data MatrixData a =
  MatrixData { matDataRowIdxs :: !(V.Vector Int),
               matDataColIdxs :: !(V.Vector Int),
               matDataEntries :: !(V.Vector a)} deriving (Eq, Show)

checkMatrixData :: MatrixData a -> Bool
checkMatrixData (MatrixData idxx idxy vals) = (lr == lc) && (lr == le) where
  (lr, lc, le) = (V.length idxx, V.length idxy, V.length vals)


identityMatrix :: Comm -> Int -> Mat -> PetscMatrix
identityMatrix comm n =
  PetscMatrix (MIConstNZPR (MatrixInfoBase comm n n) 1)


data MatrixInfoBase =
  MatrixInfoBase { matComm  :: Comm
                  ,matRows  :: !Int
                  ,matCols  :: !Int
                  -- ,matOrder :: !MatrixOrder
                 } deriving (Eq, Show)

mkMatrixInfoBase :: Comm -> MatrixData a -> MatrixInfoBase
mkMatrixInfoBase comm (MatrixData idxx idxy vals) =
  MatrixInfoBase comm (V.length idxx) (V.length idxy)


data MatrixInfo =
  MIConstNZPR MatrixInfoBase !Int
  | MIVarNZPR MatrixInfoBase !(V.Vector Int)


-- | a datatype encapsulating matrix information and the typed pointer
data PetscMatrix = PetscMatrix !MatrixInfo Mat

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



















-- * DM

dmCreate :: Comm -> IO DM
dmCreate comm = chk1 (dmCreate' comm)

dmDestroy :: DM -> IO ()
dmDestroy dm = chk0 (dmDestroy' dm)

dmCreateGlobalVector, dmCreateLocalVector :: DM -> IO Vec
dmCreateGlobalVector dm = chk1 (dmCreateGlobalVector' dm)
dmCreateLocalVector dm = chk1 (dmCreateLocalVector' dm)



-- -- * DMDA 

dmdaCreate :: Comm -> IO DM
dmdaCreate comm = chk1 (dmdaCreate' comm)

dmdaSetDim :: DM -> Int -> IO ()
dmdaSetDim dm d = chk0 (dmdaSetDim' dm d') where
  d' = toCInt d

-- dmdaSetSizes dm x y z = chk0 (dmdaSetSizes' dm x' y' z') where
--   (x',y',z') = (toCInt x, toCInt y, toCInt z)



dmdaCreate1d ::
  Comm ->             
  DMBoundaryType_ ->  -- b : type of boundary ghost cells
  PetscInt_ ->        -- mm : global array dimension 
  PetscInt_ ->        -- dof : # DOF / node
  PetscInt_ ->        -- sw : stencil width 
  [CInt] ->           -- # nodes in X dir / processor
  IO DM
dmdaCreate1d comm b mm dof sw lx =
  chk1 (dmdaCreate1d' comm b mm dof sw lx)

dmdaCreate2d ::
  Comm ->
  (DMBoundaryType_, DMBoundaryType_) -> -- (bx, by) : type of bdry ghost cells 
  DMDAStencilType ->                    -- sten : box or star stencil type
  (PetscInt_, PetscInt_) ->             -- (mm, nn) : global array dimensions
  PetscInt_ ->                          -- dof : # DOF / node
  PetscInt_ ->
  IO DM
dmdaCreate2d comm (bx, by) sten (mm, nn) dof s =
  chk1 (dmdaCreate2d' comm bx by sten mm nn dof s)



dmdaSetUniformCoordinates ::
  DM ->
  (PetscReal_, PetscReal_) ->
  (PetscReal_, PetscReal_) ->
  (PetscReal_, PetscReal_) ->
  IO ()
dmdaSetUniformCoordinates da (xmin, xmax) (ymin, ymax) (zmin, zmax) =
  chk0 (dmdaSetUniformCoordinates' da xmin xmax ymin ymax zmin zmax)

dmdaSetUniformCoordinates1d ::
  DM ->
  (PetscReal_, PetscReal_) ->
  IO ()
dmdaSetUniformCoordinates1d da (xmin, xmax) =
  dmdaSetUniformCoordinates da (xmin, xmax) (0,0) (0,0)

dmdaSetUniformCoordinates2d ::
  DM ->
  (PetscReal_, PetscReal_) ->
  (PetscReal_, PetscReal_) ->
  IO ()
dmdaSetUniformCoordinates2d da (xmin, xmax) (ymin, ymax)  =
  dmdaSetUniformCoordinates da (xmin, xmax) (ymin, ymax) (0,0)


-- | brackets for distributed arrays

withDmda1d ::
  Comm ->
  DMBoundaryType_ ->  -- b : type of boundary ghost cells
  PetscInt_ ->        -- mm : global array dimension 
  PetscInt_ ->        -- dof : # DOF / node
  PetscInt_ ->        -- sw : stencil width 
  [CInt] ->           -- # nodes in X dir / processor
  (DM -> IO a) ->
  IO a
withDmda1d comm b m dof sw lx =
  bracket (dmdaCreate1d comm b m dof sw lx) dmDestroy

withDmda2d0 ::
  Comm ->
  (DMBoundaryType_, DMBoundaryType_) ->
  DMDAStencilType ->
  (PetscInt_, PetscInt_) ->
  PetscInt_ ->
  PetscInt_ ->
  (DM -> IO a) ->
  IO a
withDmda2d0 comm (bx, by) sten (m, n) dof s =
  bracket (dmdaCreate2d comm (bx, by) sten (m, n) dof s) dmDestroy

withDmda2d :: Dmda2dInfo -> (DM ->  IO a) -> IO a
withDmda2d (Dmda2dInfo comm bdry sten szs dof sw _ _) =
  bracket (dmdaCreate2d comm bdry sten szs dof sw) dmDestroy 

-- | a datatype for Dmda2d + info

data Dmda2dP = Dmda2dP !Dmda2dInfo DM

data Dmda2dInfo =
  Dmda2dInfo {
    dmdaComm :: !Comm,
    dmdaBdryType :: !(DMBoundaryType_, DMBoundaryType_),
    dmdaStenType :: !DMDAStencilType,
    dmdaSizes :: !(PetscInt_, PetscInt_),
    dmdaNdofPN :: !PetscInt_,
    dmdaStenWidth :: !PetscInt_,
    dmdaBoundsX :: !(PetscReal_, PetscReal_),
    dmdaBoundsY :: !(PetscReal_, PetscReal_)
    }



-- | brackets for distributed arrays, uniform coordinates

withDmdaUniform1d ::
  Comm ->
  DMBoundaryType_ ->  -- b : type of boundary ghost cells
  PetscInt_ ->        -- mm : global array dimension 
  PetscInt_ ->        -- dof : # DOF / node
  PetscInt_ ->        -- sw : stencil width 
  [CInt] ->           -- # nodes in X dir / processor
  (PetscReal_, PetscReal_) ->  -- (xmin, xmax)
  (DM -> IO a) ->
  IO a
withDmdaUniform1d comm b m dof sw lx (x1,x2) f=
  withDmda1d comm b m dof sw lx $ \dm -> do
   dmdaSetUniformCoordinates1d dm (x1,x2)
   f dm 



withDmdaUniform2d ::
  Dmda2dInfo -> (DM -> IO a) -> IO a
withDmdaUniform2d (Dmda2dInfo comm bdryt sten szs dof sw bx by) f =
  withDmda2d0 comm bdryt sten szs dof sw $ \dm -> do
    dmdaSetUniformCoordinates2d dm bx by
    f dm

withDmdaUniform2d0 ::
  Comm ->
  (DMBoundaryType_, DMBoundaryType_) ->  -- b : type of boundary ghost cells
  DMDAStencilType ->
  (PetscInt_, PetscInt_) ->    -- (m, n) : global array dimensions 
  PetscInt_ ->                 -- dof : # DOF / node
  PetscInt_ ->                 -- sw : stencil width 
  (PetscReal_, PetscReal_) ->  -- (xmin, xmax)
  (PetscReal_, PetscReal_) ->  -- (ymin, ymax)
  (DM -> IO a) ->
  IO a
withDmdaUniform2d0 comm (bx,by) sten (m,n) dof sw (x1,x2) (y1,y2) f =
  withDmda2d0 comm (bx,by) sten (m,n) dof sw $ \dm -> do
    dmdaSetUniformCoordinates2d dm (x1,x2) (y1,y2)
    f dm




























-- * KSP

kspCreate :: Comm -> IO KSP
kspCreate comm = chk1 (kspCreate' comm)

kspDestroy :: KSP -> IO ()
kspDestroy ksp = chk0 (kspDestroy' ksp)

kspSetType :: KSP -> KspType_ -> IO ()
kspSetType ksp kt = chk0 (kspSetType' ksp kt)

withKsp :: Comm -> (KSP -> IO a) -> IO a
withKsp comm =
  bracket (kspCreate comm) kspDestroy

withKspSetup ::
  Comm ->
  KspType_ ->
  Mat ->            -- linear operator
  Mat ->            -- preconditioner
  Bool ->           -- set initial solution guess to nonzero vector
  (KSP -> IO a) ->  -- post-setup actions, i.e. solve with a r.h.s , etc.
  IO a
withKspSetup comm kt amat pmat ignz f = withKsp comm $ \ksp -> do
  kspSetOperators ksp amat pmat
  kspSetType ksp kt
  kspSetInitialGuessNonzero ksp ignz
  kspSetUp ksp
  f ksp

withKspSetupSolve ::
  Comm ->
  KspType_ ->
  Mat ->            -- linear operator
  Mat ->            -- preconditioner
  Bool ->           -- set initial solution guess to nonzero vector
  Vec ->            -- r.h.s
  Vec ->            -- solution (WILL BE OVERWRITTEN)
  (KSP -> IO a) ->  -- post-solve actions
  IO a
withKspSetupSolve comm kt amat pmat ignz rhsv solnv post =
  withKspSetup comm kt amat pmat ignz $ \ksp -> do
    kspSolve ksp rhsv solnv
    post ksp


kspSetOperators :: KSP -> Mat -> Mat -> IO ()
kspSetOperators ksp amat pmat = chk0 (kspSetOperators' ksp amat pmat)

kspSetInitialGuessNonzero :: KSP -> Bool -> IO ()
kspSetInitialGuessNonzero ksp ig = chk0 (kspSetInitialGuessNonzero' ksp ig)

kspSetUp :: KSP -> IO ()
kspSetUp ksp = chk0 (kspSetUp' ksp)

kspSolve, kspSolveTranspose :: 
  KSP -> Vec -> Vec -> IO ()
kspSolve ksp rhsv solnv =  chk0 (kspSolve' ksp rhsv solnv)
kspSolveTranspose ksp rhsv solnv = chk0 (kspSolve' ksp rhsv solnv)

kspSetReusePreconditioner ::
  KSP -> Bool -> IO ()
kspSetReusePreconditioner ksp b = chk0 (kspSetReusePreconditioner' ksp b)

kspGetRhs :: KSP -> IO Vec
kspGetRhs ksp = chk1 (kspGetRhs' ksp)

kspGetSolution :: KSP -> IO Vec
kspGetSolution ksp = chk1 (kspGetSolution' ksp)

kspGetResidualNorm :: KSP -> IO PetscReal_
kspGetResidualNorm ksp = chk1 (kspGetResidualNorm' ksp)

kspGetIterationNumber :: KSP -> IO CInt
kspGetIterationNumber ksp = chk1 (kspGetIterationNumber' ksp)










-- * PF

-- * SNES

snesCreate :: Comm -> IO SNES
snesCreate comm = chk1 (snesCreate' comm)

snesDestroy :: SNES -> IO ()
snesDestroy snes = chk0 (snesDestroy' snes)

snesSetType :: SNES -> SnesType_ -> IO ()
snesSetType snes st = chk0 $ snesSetType' snes st

withSnes :: Comm -> (SNES -> IO a) -> IO a
withSnes comm = bracket (snesCreate comm) snesDestroy

-- | snesSetFunction, snesSetJacobian : 
--   Newton-like methods typically solve linear systems of the form
--      f'(x) x = -f(x)
--   where f'(x) denotes the Jacobian matrix and f(x) is the function.

snesSetFunction ::
  SNES ->
  Vec ->        -- r : storage for function value
    (SNES ->       
     Vec ->        -- vector at which to compute residual
     IO CInt) -> 
  IO ()
snesSetFunction snes r f = chk0 $ snesSetFunction' snes r f

snesSetJacobian ::
  SNES ->
  Mat ->        -- amat : storage for approximate Jacobian
  Mat ->        -- pmat : storage for preconditioner (usually == amat)
    (SNES ->       
     Vec ->        -- vector at which to compute Jacobian
     Mat ->        
     Mat ->
     IO CInt) ->
  IO ()
snesSetJacobian snes amat pmat f = chk0 $ snesSetJacobian_' snes amat pmat f

snesSetUp :: SNES -> IO ()
snesSetUp snes = chk0 $ snesSetUp' snes

snesSolve ::
  SNES ->
  Vec ->   -- r.h.s
  Vec ->   -- solution (WILL BE OVERWRITTEN)
  IO ()
snesSolve snes rhsv solnv = chk0 $ snesSolve' snes rhsv solnv

snesGetSolution :: SNES -> IO Vec
snesGetSolution snes = chk1 $ snesGetSolution' snes

















-- * TS

tsCreate :: Comm -> IO TS
tsCreate comm = chk1 $ tsCreate' comm

tsDestroy :: TS -> IO ()
tsDestroy ts = chk0 $ tsDestroy' ts

withTs :: Comm -> (TS -> IO a) -> IO a
withTs comm = bracket (tsCreate comm) tsDestroy

tsSetProblemType :: TS -> TsProblemType -> IO ()
tsSetProblemType ts ty = chk0 $ tsSetProblemType' ts ty

tsSetInitialTimeStep ::
  TS ->
  PetscReal_ -> -- initial time
  PetscReal_ -> -- initial timestep
  IO ()
tsSetInitialTimeStep ts it dt = chk0 $ tsSetInitialTimeStep' ts it dt

tsSetDuration ::
  TS ->
  Int ->  -- max. # steps
  PetscReal_ -> -- max. time
  IO ()
tsSetDuration ts ms mt = chk0 $ tsSetDuration' ts ms mt

tsSetSolution ::
  TS ->
  Vec ->        -- initial condition
  IO ()
tsSetSolution ts isolnv = chk0 $ tsSetSolution' ts isolnv

tsSolve_ :: TS -> IO ()
tsSolve_ ts = chk0 $ tsSolve_' ts

tsSolveWithInitialCondition :: TS -> Vec -> IO ()
tsSolveWithInitialCondition ts isolnv = do
  tsSetSolution ts isolnv
  tsSolve_ ts







-- * Tao

taoCreate :: Comm -> IO Tao
taoCreate comm = chk1 $ taoCreate' comm

taoDestroy :: Tao -> IO ()
taoDestroy tao = chk0 $ taoDestroy' tao

withTao :: Comm -> (Tao -> IO a) -> IO a
withTao comm = bracket (taoCreate comm) taoDestroy

taoSetType :: Tao -> TaoType_ -> IO ()
taoSetType tao ty = chk0 $ taoSetType' tao ty

taoSetInitialVector :: Tao -> Vec -> IO ()
taoSetInitialVector tao x = chk0 $ taoSetInitialVector' tao x

taoSetVariableBounds ::
  Tao ->
  Vec ->  -- min
  Vec ->  -- max
  IO ()
taoSetVariableBounds tao xmin xmax = chk0 $ taoSetVariableBounds' tao xmin xmax

taoSolve :: Tao -> IO ()
taoSolve tao = chk0 $ taoSolve' tao

taoGetSolutionVector :: Tao -> IO Vec
taoGetSolutionVector tao = chk1 $ taoGetSolutionVector' tao

taoComputeObjective :: Tao -> Vec -> IO PetscReal_
taoComputeObjective tao v = chk1 $ taoComputeObjective' tao v

taoComputeGradient :: Tao -> Vec -> IO Vec
taoComputeGradient tao v = chk1 $ taoComputeGradient' tao v

taoIsObjectiveDefined, taoIsGradientDefined :: Tao -> IO PetscBool_
taoIsObjectiveDefined tao = chk1 $ taoIsObjectiveDefined' tao
taoIsGradientDefined tao = chk1 $ taoIsGradientDefined' tao













-- * PetscViewer

withPetscViewer :: Comm -> (PetscViewer -> IO a) -> IO a
withPetscViewer comm =
  bracketChk (petscViewerCreate' comm) petscViewerDestroy'

withPetscViewerSetup comm ty mode name f = withPetscViewer comm $ \v -> do
  chk0 $ petscViewerSetType' v ty
  chk0 $ petscViewerFileSetName' v name
  chk0 $ petscViewerFileSetMode' v mode
  f v


{- -- -- usage of HDF5 groups: 
 50:   VecView(x1, viewer);
 51:   PetscViewerHDF5PushGroup(viewer, "/testBlockSize");
 52:   VecView(x2, viewer);
 53:   PetscViewerHDF5PushGroup(viewer, "/testTimestep");
-}

withPetscViewerHDF5Group :: PetscViewer -> String -> (PetscViewer -> IO a) -> IO ()
withPetscViewerHDF5Group viewer name f = do
  chk0 $ petscViewerHDF5PushGroup1 viewer name
  f viewer
  chk0 $ petscViewerHDF5PopGroup1 viewer




  

-- * MPI

commWorld, commSelf :: Comm
commWorld = commWorld1
commSelf = commSelf1






-- * misc PETSc

-- -- NB : all PETSc functions must appear within a withPetsc* bracket

petscInit0 :: IO ()
petscInit0 = chk0 petscInit01

petscFin :: IO ()
petscFin = chk0 petscFin1

withPetsc0 :: IO a -> IO a
withPetsc0 = bracket_ petscInit0 petscFin

petscInit ::
  [String] ->   -- "argv" list of strings
  String ->     -- options string
  String ->     -- help string
  IO ()
petscInit args opts help = chk0 $ petscInitialize1 args opts help

withPetsc ::
  [String] -> String -> String -> IO a -> IO a
withPetsc a o h = bracket_ (petscInit a o h) petscFin
