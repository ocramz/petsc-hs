{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, RankNTypes#-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Raw.PutGet.DM
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | DM/DMDA Mid-level interface
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Raw.PutGet.DM where

import Numerical.PETSc.Raw.InlineC
import Numerical.PETSc.Raw.Types
import Numerical.PETSc.Raw.Exception
import Numerical.PETSc.Raw.Utils
import Numerical.PETSc.Raw.Internal

import Foreign
import Foreign.C.Types

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Control.Arrow
import Control.Concurrent
import Control.Exception

import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST) -- for HMatrix bits

import qualified Data.Vector as V
import qualified Data.Vector.Storable as V (unsafeWith, unsafeFromForeignPtr, unsafeToForeignPtr)





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


-- | a datatype for Dmda1d + info

data PetscDmda1d = PetscDmda1d !Dmda2dInfo DM

data Dmda1dInfo =
  Dmda1dInfo {
    dmda1dComm :: !Comm,
    dmda1dBdryType :: !DMBoundaryType_,
    dmda1dSizes :: !PetscInt_,
    dmda1dNdofPN :: !PetscInt_,
    dmda1dStenWidth :: !PetscInt_,
    dmda1dBoundsX :: !(PetscReal_, PetscReal_)
    } deriving (Eq, Show)




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




-- | a datatype for Dmda2d + info

data PetscDmda2d = PetscDmda2d !Dmda2dInfo DM

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
    } deriving (Eq, Show)




withDmda2d1 ::
  Dmda2dInfo ->
  (DM ->  IO a) ->
  IO a
withDmda2d1 (Dmda2dInfo comm bdry sten szs dof sw _ _) =
  bracket (dmdaCreate2d comm bdry sten szs dof sw) dmDestroy 


-- withDmda2d Dmda2dInfo{..} pre post =
--   withDmda2d1 di $ \p -> do
--    pre dm 
--    post dm 





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

























