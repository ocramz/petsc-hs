{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}

-- | Mid-level interface wrapping the inline-c calls
-- and 
--
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

import Control.Monad
import Control.Arrow
import Control.Concurrent
import Control.Exception

import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST) -- for HMatrix bits

import qualified Data.Vector as V
import qualified Data.Vector.Storable as V (unsafeFromForeignPtr, unsafeToForeignPtr)



withPtr f = alloca $ \e -> do
  err <- f e
  res <- peek e
  return (res, err)
  

--------

{- from Base :
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar :: MVar a -> (a -> IO (a,b)) -> IO b  -}
---------

-- f = V.unsafeFromForeignPtr



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







-- * Vec







-- newtype Vec1 a = Vec1 {unVec1 :: MVar (ForeignPtr PVec)}

-- -- type VFV = Vec -> IO ()

-- foreign import ccall "wrapper" -- wrapper stub 
--    mkVfv :: (Vec -> IO ()) -> IO (FunPtr (Vec -> IO ()))
            
-- vecDestroyWrapper = mkVfv $ chk0 . vecDestroy 

-- -- newV = do
-- --   fp <- mallocForeignPtrBytes (sizeOf (undefined :: PetscScalar_ ))
-- --   vd <- vecDestroyWrapper
-- --   addForeignPtrFinalizer vd fp
-- --   withForeignPtr fp (\p -> poke p (castPtr nullPtr))








-- --

-- -- HMatrix bits
-- -- -- (from Data.Packed.Internal.Vector and Data.Packed.ST)

-- data Vector t =
--     V { ioff :: {-# UNPACK #-} !Int              -- ^ offset of first element
--       , idim :: {-# UNPACK #-} !Int              -- ^ number of elements
--       , fptr :: {-# UNPACK #-} !(ForeignPtr t)   -- ^ foreign pointer to the memory block
--       }

-- unsafeToForeignPtr :: Storable a => Vector a -> (ForeignPtr a, Int, Int)
-- unsafeToForeignPtr v = (fptr v, ioff v, idim v)

-- -- | Same convention as in Roman Leshchinskiy's vector package.
-- unsafeFromForeignPtr :: Storable a => ForeignPtr a -> Int -> Int -> Vector a
-- unsafeFromForeignPtr fp i n | n > 0 = V {ioff = i, idim = n, fptr = fp}
--                             | otherwise = error "unsafeFromForeignPtr with dim < 1"

-- unsafeWith (V i _ fp) m = withForeignPtr fp $ \p -> m (p `advancePtr` i)

-- ioReadV v k = unsafeWith v $ \s -> peekElemOff s k
-- ioWriteV v k x = unsafeWith v $ \s -> pokeElemOff s k x

-- newtype STVector s t = STVector (Vector t)

-- unsafeReadVector (STVector v) = ioReadV v

-- unsafeReadVectorST (STVector v) = unsafeIOToST . ioReadV v
-- unsafeWriteVectorST (STVector v) k = unsafeIOToST . ioWriteV v k 

-- safeIndexV f (STVector v) k
--   | k < 0 || k > idim v = error "index out of range"
--   | otherwise = f (STVector v) k

-- readVector :: Storable t => STVector s t -> Int -> ST s t 
-- readVector = safeIndexV unsafeReadVectorST

-- writeVector :: Storable t => STVector s t -> Int -> t -> ST s ()
-- writeVector = safeIndexV unsafeWriteVectorST

-- -- -- -- from HMatrix Data.Packed.Internal.Common
-- (//) = flip ($)
-- type Adapt f t r = t -> ((f -> r) -> IO ()) -> IO ()
-- type Adapt1 f t = Adapt f t (IO CInt) -> t -> String -> IO ()
-- type Adapt2 f t1 r t2 = Adapt f t1 r -> t1 -> Adapt1 r t2

-- checkH msg f = do
--   err <- f
--   when (err /= 0 ) $ error ( msg ++ ": " )

-- app1 :: f -> Adapt1 f t
-- app1 f w o s = w o $ \a -> f // a // checkH s
-- app2 :: f -> Adapt2 f t1 r t2
-- app2 f w1 o1 w2 o2 s = ww2 w1 o1 w2 o2 $ \a b -> f // a // b // checkH s

-- ww2 u o v p f = u o $ v p . f

-- vecC2Hs x f = unsafeWith x $ \p -> do
--   let v g = g (idim x) p
--   f v

-- -- cloneVector :: Storable t => Vector t -> IO (Vector t)
-- cloneVector v = do
--         let n = idim v
--         r <- createVector n
--         let f _ s _ d =  copyArray d s n >> return 0
--         app2 f vecC2Hs v vecC2Hs r "cloneVector"
--         return r

-- createVector :: Storable a => Int -> IO (Vector a)
-- createVector n = do
--     when (n <= 0) $ error ("trying to createVector of dim "++show n)
--     fp <- doMalloc undefined
--     return $ unsafeFromForeignPtr fp 0 n
--   where
--     -- Use the much cheaper Haskell heap allocated storage
--     -- for foreign pointer space we control
--     doMalloc :: Storable b => b -> IO (ForeignPtr b)
--     doMalloc dummy = 
--         mallocForeignPtrBytes      (n * sizeOf dummy)

        


-- -- -- 













-- -- -

data VecInfo = VecInfo 
 {vecInfoMpiComm :: Comm ,
  vecInfoSizeLocal :: !Int ,
  vecInfoSizeGlobal :: !Int } deriving (Eq, Show)

data PVec = PVec { vec     :: !Vec,
                   vecInfo :: !VecInfo }
data PetscVec = PetscVec { unPetscVec :: MVar PVec }

-- data PVecF = PVecF {vecF :: !(ForeignPtr Vec),
--                     vecInfoF :: !VecInfo } deriving Eq
-- data PetscVecF = PetscVecF {unPetscVecF :: MVar PVecF}


makePetscVec v vi = do
  m <- newMVar (PVec v vi)
  return $ PetscVec m


-- class PetscStruct a b | a -> b where
--   withPS :: a -> (b -> IO b) -> IO a
--   modifyPS :: a -> (b -> IO b) -> IO a

-- instance PetscStruct PetscVec Vec where
--   withPS = usePetscVec
--   modifyPS = updatePetscVec

-- instance PetscStruct PetscMat Mat where
--   withPS = usePetscMat
--   modifyPS = updatePetscMat


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

-- applyVec_ :: PVec -> (Vec -> IO a) -> IO ()
-- applyVec_ vv fm = do
--   fm $ vec vv
--   return ()

{- withMVar :: MVar a -> (a -> IO b) -> IO b
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()    -}

-- class PetscStruct a where
--   withPS :: MVar a -> (a -> IO b) -> IO b
--   modifyPS :: MVar a -> (a -> IO a) -> IO ()




-- withPetscVecCreate vinfo f = withVecCreate vinfo $ \v -> do
--   a <- mkPetscVec vinfo v
--   modifyPetscVec a f

vecCreate :: Comm -> IO Vec
vecCreate comm = chk1 (vecCreate' comm)

vecCreateMPI :: Comm -> Int -> Int -> IO Vec
vecCreateMPI comm nLocal nGlobal = chk1 (vecCreateMPI' comm nLocal nGlobal)

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


-- withVecCreateMPIMutable (PVec (MVar (mv, vv))) f = withVecCreateMPI vv $ \v ->
--   modifyPVec v f

-- test = modifyMVar_

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







vecView v vi = chk0 $ vecView1 v vi
vecSetName v name = chk0 $ vecSetName1 v name

vecSet_ v n = chk0 $ vecSet1 v n
vecSet v n = do {vecSet_ v n ; return v}

vecGetOwnershipRange :: Vec -> IO (Int, Int)
vecGetOwnershipRange v = 
  chk1 (vecGetOwnershipRange1 v) 

vecDot v1 v2 = chk1 $ vecDot1 v1 v2
vecNorm v nt = chk1 $ vecNorm1 nt v
vecSum v = chk1 $ vecSum1 v

vecLog_ v = chk0 $ vecLog' v
vecLog v = do {vecLog_ v; return v}
vecExp_ v = chk0 $ vecExp' v
vecExp v = do {vecExp_ v; return v}
vecAbs_ v = chk0 $ vecAbs' v
vecAbs v = do {vecAbs_ v ; return v}

-- | AXPY : y = a x + y
-- -- NB : x and y must be different vectors
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

vecVecSum = vecAxpy 1
(.+) = vecVecSum

vecVecSumSafe = vecWaxpySafe 1

vecGetSize :: Vec -> IO Int
vecGetSize v = liftM fi $ chk1 ( vecGetSize1 v) 
-- vecSize v = unsafePerformIO (vecGetSize v)

vecViewStdout v = chk0 $ vecViewStdout1 v



-- vecGetArray1d v sz = chk1 (vecGetArray1d' v sz 0)



vecGetArray v sz = chk1 $ vecGetArray' v sz

vecGetArraySafe v = do
  sz <- vecGetSize v
  vecGetArray v sz



vecRestoreArray v c = chk0 $ vecRestoreArray' v c

-- vecRestoreArrayB v c = chk0 $ vecRestoreArrayB1 v c

-- vecRestoreArrayC v c = chk0 $ vecRestoreArrayC1 v c



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



withVecGetArray1d' x m ms =
  bracketChk (vecGetArray1d' x m ms)

-- vecGetArray1d0 x m ms = do
--   p <- chk1 $ vecGetArray1d' x m ms
--   return p





vecRestoreArrayB v ar = alloca $ \ p -> do
  pokeArray p ar
  with p $ \pp -> chk0 $ vecRestoreArray0' v pp




bracket1 allocate release io = mask $ \restore -> do
  stuff <- allocate
  restore (io stuff) `finally` release stuff







-- unsafeWith (Vector i _ fp) m = withForeignPtr fp $ \p -> m (p `advancePtr` i)



-- ioReadV v k = unsafeWith v $ \s -> peekElemOff s k
-- ioWriteV v k x = unsafeWith v $ \s -> pokeElemOff s k x

-- newtype STVector s t = STVector (Vector t)

-- unsafeReadVector (STVector v) = ioReadV v

-- unsafeReadVectorST (STVector v) = unsafeIOToST . ioReadV v
-- unsafeWriteVectorST (STVector v) k = unsafeIOToST . ioWriteV v k 

-- safeIndexV f (STVector v) k
--   | k < 0 || k > idim v = error "index out of range"
--   | otherwise = f (STVector v) k

-- readVector :: Storable t => STVector s t -> Int -> ST s t 
-- readVector = safeIndexV unsafeReadVectorST

-- writeVector :: Storable t => STVector s t -> Int -> t -> ST s ()
-- writeVector = safeIndexV unsafeWriteVectorST






















    
-- * Mat

withMat :: Comm -> (Mat -> IO a) -> IO a
withMat comm = bracketChk (matCreate' comm) matDestroy'

matCreate :: Comm -> IO Mat
matCreate comm = chk1 (matCreate' comm)

matDestroy :: Mat -> IO ()
matDestroy = chk0 . matDestroy'

matSetup :: Mat -> IO ()
matSetup = chk0 . matSetup'

matAssemblyBegin, matAssemblyEnd :: Mat -> IO ()
matAssemblyBegin = chk0 . matAssemblyBegin'
matAssemblyEnd = chk0 . matAssemblyEnd'

matAssembly :: Mat -> IO ()
matAssembly = matAssemblyBegin >> matAssemblyEnd

withMatAssembly m f = do
  matAssemblyBegin m
  f 
  matAssemblyEnd m

-- data MatrixOrder = RowMajor | ColMajor deriving (Eq, Show)
-- transposeOrder RowMajor = ColMajor
-- transposeOrder ColMajor = RowMajor
-- -- matrixTranspose (Matrix r c d o)  = Matrix r c d (transposeOrder o)

data MatrixData a =
  MatrixData { matDataRowIdxs :: !(V.Vector Int),
               matDataColIdxs :: !(V.Vector Int),
               matDataEntries :: !(V.Vector a)} deriving (Eq, Show)

checkMatrixData (MatrixData idxx idxy vals) = (lr == lc) && (lr == le) where
  (lr, lc, le) = (V.length idxx, V.length idxy, V.length vals)


identityMatrix comm n =
  PetscMatrix (MIConstNZPR (MatrixInfoBase comm n n) 1)

-- mkMatrixData idxx idxy vals
--   | checkMatrixData 

data MatrixInfoBase =
  MatrixInfoBase { matComm  :: Comm
                  ,matRows  :: !Int
                  ,matCols  :: !Int
                  -- ,matOrder :: !MatrixOrder
                 } deriving (Eq, Show)

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
  




















-- * DM
-- -- * DMDA 

-- * KSP

-- * PF

-- * SNES

-- * TS

-- * Tao


-- * PetscViewer

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

withPetscViewerHDF5Group viewer name f = do
  chk0 $ petscViewerHDF5PushGroup1 viewer name
  f viewer
  chk0 $ petscViewerHDF5PopGroup1 viewer




  

-- * MPI

commWorld = commWorld1
commSelf = commSelf1






-- * misc PETSc

-- commWorld = commWorld

petscInit0 = chk0 petscInit01
petscFin = chk0 petscFin1

withPetsc0 = bracket_ petscInit0 petscFin

petscInit args opts help = chk0 $ petscInitialize1 args opts help

withPetsc a o h = bracket_ (petscInit a o h) petscFin
