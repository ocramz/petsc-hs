{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.PutGet.DM
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | DM/DMDA Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.PutGet.DM where

import Numerical.PETSc.Internal.InlineC
import Numerical.PETSc.Internal.Types
import Numerical.PETSc.Internal.Exception
import Numerical.PETSc.Internal.Utils

import Numerical.PETSc.Internal.PutGet.Vec

import Foreign
import Foreign.C.Types

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Control.Arrow
import Control.Concurrent
import Control.Exception

-- import Control.Monad.ST (ST, runST)
-- import Control.Monad.ST.Unsafe (unsafeIOToST) -- for HMatrix bits

-- import qualified Data.Vector as V (Vector, freeze)
import qualified Data.Vector.Storable as V --  (unsafeWith, unsafeFromForeignPtr, unsafeToForeignPtr)
import qualified Data.Vector.Storable.Mutable as VM



-- | Dmda 1D + info

data PetscDmda1d = PetscDmda1d !Dmda2dInfo DM

data Dmda1dInfo =
  Dmda1dInfo {
    dmda1dComm :: !Comm,
    dmda1dBdryType :: !DMBoundaryType_,
    dmda1dSizes :: !Int,
    dmda1dNdofPN :: !Int,
    dmda1dStenWidth :: !Int,
    dmda1dBoundsX :: !(PetscReal_, PetscReal_)
    } deriving (Eq, Show)

               
-- | Dmda 2D + info

data PetscDmda2d = PetscDmda2d !Dmda2dInfo DM

data Dmda2dInfo =
  Dmda2dInfo {
    dmdaComm :: !Comm,
    dmdaBdryType :: !(DMBoundaryType_, DMBoundaryType_),
    dmdaStenType :: !DMDAStencilType,
    dmdaSizes :: !(Int, Int),
    dmdaNdofPN :: !Int,
    dmdaStenWidth :: !Int,
    dmdaBoundsX :: !(PetscReal_, PetscReal_),
    dmdaBoundsY :: !(PetscReal_, PetscReal_)
    } deriving (Eq, Show)







               




-- | create DM

dmCreate :: Comm -> IO DM
dmCreate comm = chk1 (dmCreate' comm)



-- | global and local vectors from/to DM

dmCreateGlobalVector, dmCreateLocalVector, dmGetGlobalVector, dmGetLocalVector :: 
  DM -> IO Vec
dmRestoreGlobalVector, dmRestoreLocalVector :: DM -> Vec -> IO ()

dmCreateGlobalVector dm = chk1 (dmCreateGlobalVector' dm)
dmCreateLocalVector dm = chk1 (dmCreateLocalVector' dm)

dmGetGlobalVector dm = chk1 (dmGetGlobalVector' dm)
dmRestoreGlobalVector dm v = chk0 (dmRestoreGlobalVector' dm v)

dmGetLocalVector dm = chk1 (dmGetLocalVector' dm)
dmRestoreLocalVector dm v = chk0 (dmRestoreLocalVector' dm v)





-- | fill DM local vector with global vector

dmGlobalToLocalBegin dm g mode l = chk0 (dmGlobalToLocalBegin' dm g mode l)
dmGlobalToLocalEnd dm g mode l = chk0 (dmGlobalToLocalEnd' dm g mode l)

withDmGlobalToLocal :: DM -> Vec -> InsertMode_ -> Vec -> IO a -> IO a
withDmGlobalToLocal dm g mode l =
  bracket_ (dmGlobalToLocalBegin dm g mode l) (dmGlobalToLocalEnd dm g mode l)

dmdaG2L :: DM -> Vec -> InsertMode_ -> Vec -> IO ()
dmdaG2L dm g mode l =
  (dmGlobalToLocalBegin dm g mode l) >> (dmGlobalToLocalEnd dm g mode l)




-- | destroy DM

dmDestroy :: DM -> IO ()
dmDestroy dm = chk0 (dmDestroy' dm)






-- | with DM brackets

withDm :: IO DM -> (DM -> IO a) -> IO a
withDm dc = bracket dc dmDestroy

withDmCreateGlobalVector :: DM -> (Vec -> IO a) -> IO a
withDmCreateGlobalVector dm = withVec (dmCreateGlobalVector dm)

withDmGetLocalVector :: DM -> (Vec -> IO a) -> IO a
withDmGetLocalVector dm =
  bracket (dmGetLocalVector dm) (dmRestoreLocalVector dm)

withDmGetGlobalVector :: DM -> (Vec -> IO a) -> IO a    
withDmGetGlobalVector dm =
  bracket (dmGetGlobalVector dm) (dmRestoreGlobalVector dm)


-- | get/restore a V.Vector rather than a Vec

withDmdaVecGetVector ::
  DM ->
  Vec ->
  Int ->                              -- length of vector to be copied
  (V.Vector PetscScalar_ -> IO a) ->
  IO a
withDmdaVecGetVector dm v len =
  bracket (dmdaVecGetVector dm v len) (dmdaVecRestoreVector dm v len) 










-- | overwrite local vector of values taken from Vec bound to DM

dmdaVecReplaceWVectorF ::
  DM ->
  Vec ->
  Int ->
  (V.Vector PetscScalar_ -> IO (V.Vector PetscScalar_)) ->
  IO ()
dmdaVecReplaceWVectorF dm v len f = do
  x <- dmdaVecGetVector dm v len
  y <- f x
  dmdaVecRestoreVector dm v len y

withDmdaCornersVecGetVector ::
  DM -> 
  Vec -> 
  (V.Vector PetscScalar_ -> IO (V.Vector PetscScalar_)) ->
  IO ()
withDmdaCornersVecGetVector dm v f = do
  (x0, len) <- dmdaGetCorners1d dm
  x <- dmdaVecGetVector dm v len
  y <- f x
  dmdaVecRestoreVector dm v len y








-- | composite DM -> Vec -> V.Vector brackets

withDmdaLocalVector ::
  DM ->
  Int ->
  (V.Vector PetscScalar_ -> IO a) ->
  IO a
withDmdaLocalVector dm len body =
  withDmGetLocalVector dm $ \v ->
   withDmdaVecGetVector dm v len body

-- withDmdaGlobalVector ::
--   DM ->
--   Int ->
--   (V.Vector PetscScalar_ -> IO a) ->
--   IO a
-- withDmdaGlobalVector dm len body =
--   withDmGetGlobalVector dm $ \v ->
--    withDmdaVecGetVector dm v len body









-- -- * DMDA 

-- | create DMDA 

dmdaCreate :: Comm -> IO DM
dmdaCreate comm = chk1 (dmdaCreate' comm)

-- dmdaCreate1d ::
--   Comm ->             
--   DMBoundaryType_ ->  -- b : type of boundary ghost cells
--   PetscInt_ ->        -- mm : global array dimension 
--   PetscInt_ ->        -- dof : # DOF / node
--   PetscInt_ ->        -- sw : stencil width 
--   [CInt] ->           -- # nodes in X dir / processor
--   IO DM
dmdaCreate1d comm b mm dof sw lx =
  chk1 (dmdaCreate1d' comm b mm' dof' sw' lx) where
    (mm', dof', sw') = (toCInt mm, toCInt dof, toCInt sw)

-- dmdaCreate2d ::
--   Comm ->
--   (DMBoundaryType_, DMBoundaryType_) -> -- (bx, by) : type of bdry ghost cells 
--   DMDAStencilType ->                    -- sten : box or star stencil type
--   (PetscInt_, PetscInt_) ->             -- (mm, nn) : global array dimensions
--   PetscInt_ ->                          -- dof : # DOF / node
--   PetscInt_ ->                          -- stencil width
--   IO DM
dmdaCreate2d comm (bx, by) sten (mm, nn) dof s =
  chk1 (dmdaCreate2d' comm bx by sten mm' nn' dof' s') where
    (mm', nn', dof', s') = (toCInt mm, toCInt nn, toCInt dof, toCInt s)













-- | get/set arrays from DMDA Vec's (NB : in gen. > 1 DOF/node !)

dmdaVecGetArrayPtr :: DM -> Vec -> IO (Ptr PetscScalar_)
dmdaVecGetArrayPtr dm v = chk1 (dmdaVecGetArray' dm v)

dmdaVecRestoreArrayPtr :: DM -> Vec -> Ptr PetscScalar_ -> IO ()
dmdaVecRestoreArrayPtr dm v vvp = chk0 (dmdaVecRestoreArray' dm v vvp)


dmdaVecGetVector :: DM -> Vec -> Int -> IO (V.Vector PetscScalar_)
dmdaVecGetVector dm v len = do
  p <- dmdaVecGetArrayPtr dm v
  pf <- newForeignPtr_ p
  V.freeze (VM.unsafeFromForeignPtr0 pf len)


dmdaVecRestoreVector :: DM -> Vec -> Int -> V.Vector PetscScalar_ -> IO ()
dmdaVecRestoreVector dm v len w = do
  p <- dmdaVecGetArrayPtr dm v
  pf <- newForeignPtr_ p
  V.copy (VM.unsafeFromForeignPtr0 pf len) w
  dmdaVecRestoreArrayPtr dm v p









-- | set DMDA properties 

dmdaSetDim :: DM -> Int -> IO ()
dmdaSetDim dm d = chk0 (dmdaSetDim' dm d') where
  d' = toCInt d

-- dmdaSetSizes dm x y z = chk0 (dmdaSetSizes' dm x' y' z') where
--   (x',y',z') = (toCInt x, toCInt y, toCInt z)

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











-- | DMDA brackets

withDmda1d ::
  Comm ->
  DMBoundaryType_ ->  -- b : type of boundary ghost cells
  Int ->        -- mm : global array dimension 
  Int ->        -- dof : # DOF / node
  Int ->        -- sw : stencil width 
  [Int] ->           -- # nodes in X dir / processor
  (DM -> IO a) ->
  IO a
withDmda1d comm b m dof sw lx =
  withDm (dmdaCreate1d comm b m dof sw lx')
   where lx' = map toCInt lx 

withDmda2d0 ::
  Comm ->
  (DMBoundaryType_, DMBoundaryType_) ->
  DMDAStencilType ->
  (Int, Int) ->
  Int ->
  Int ->
  (DM -> IO a) ->
  IO a
withDmda2d0 comm (bx, by) sten (m, n) dof s =
  withDm (dmdaCreate2d comm (bx, by) sten (m, n) dof s) 

withDmda2d1 ::
  Dmda2dInfo ->
  (DM ->  IO a) ->
  IO a
withDmda2d1 (Dmda2dInfo comm bdry sten szs dof sw _ _) =
  withDm (dmdaCreate2d comm bdry sten szs dof sw) 









-- | DMDA brackets, uniform coordinates

withDmdaUniform1d ::
  Comm ->
  DMBoundaryType_ ->  -- b : type of boundary ghost cells
  Int ->        -- mm : global array dimension 
  Int ->        -- dof : # DOF / node
  Int ->        -- sw : stencil width 
  [Int] ->           -- # nodes in X dir / processor
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
  (Int, Int) ->    -- (m, n) : global array dimensions 
  Int ->                 -- dof : # DOF / node
  Int ->                 -- sw : stencil width 
  (PetscReal_, PetscReal_) ->  -- (xmin, xmax)
  (PetscReal_, PetscReal_) ->  -- (ymin, ymax)
  (DM -> IO a) ->
  IO a
withDmdaUniform2d0 comm (bx,by) sten (m,n) dof sw (x1,x2) (y1,y2) f =
  withDmda2d0 comm (bx,by) sten (m,n) dof sw $ \dm -> do
    dmdaSetUniformCoordinates2d dm (x1,x2) (y1,y2)
    f dm
















-- | get DMDA info

dmdaGetInfoCInt da = chk1 (dmdaGetInfo__' da)

dmdaGetInfo3d da = do
  (d,(mm,nn,pp),(m,n,p),dof,s,(bx,by,bz),sten) <- dmdaGetInfoCInt da
  
  let
    dim = fi d
    dims = (fi mm,fi nn, fi pp)
    procsPerDim = (fi m, fi n, fi p)
    (ndof, ss) = (fi dof, fi s)
    bdries = (cIntToDmBoundaryType bx,
              cIntToDmBoundaryType by,
              cIntToDmBoundaryType bz)
    st = cIntToDmdaStencilType sten
  return (dim,dims,procsPerDim,ndof,ss,bdries,st)

dmdaGetInfo2d ::
  DM ->
  IO (Int,
      (Int,Int),
      (Int,Int),
      Int,
      Int,
      (DMBoundaryType_, DMBoundaryType_),
      DMDAStencilType)
dmdaGetInfo2d da = do
  (d,(mm,nn,_),(m,n,_),dof,s,(bx,by,_),st) <- dmdaGetInfo3d da
  return (d,(mm,nn),(m,n),dof,s,(bx,by),st)

-- dmdaGetInfo1d da = do       -- not sure good idea
--   (d,(mm,_,_),(m,_,_),dof,s,(bx,_,_),st) <- dmdaGetInfo3d da
--   return (d, mm , m ,dof,s, bx ,st)


dmdaGetCorners1d ::
  DM ->
  IO (Int,     -- index of first entry
      Int)     -- # entries
dmdaGetCorners1d dm = do
  t <- chk1 $ dmdaGetCorners1d' dm >>= \x -> return $ f1d x
  return $ fromIntegralTup t


dmdaGetCorners2d ::
  DM ->
  IO ((Int,    -- 1. dim, index of 1st entry
       Int),   -- ", # entries
      (Int,    -- 2. dim, index of 1st entry
       Int))   -- ", # entries
dmdaGetCorners2d dm = do
  x <- chk1 $ dmdaGetCorners2d' dm >>= \x -> return $ f2d x
  return $ fromIntegralTup2 x

dmdaGetCorners3d ::
  DM -> IO ((Int,Int,Int), (Int,Int,Int))
dmdaGetCorners3d dm = do
  x <- chk1 $ dmdaGetCorners3d' dm >>= \x -> return $ f3d x
  return $ fromIntegralTup3 x

