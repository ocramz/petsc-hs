{-# LANGUAGE ForeignFunctionInterface #-}
module Numerical.PETSc.Internal.TypesC2Hs
--        (
--   PetscInt,
--   PetscScalar,
--   PetscBool,
--   PetscReal,
-- --  ErrCodeA                      
--              )
       where

import Foreign
import Foreign.C.Types
import Foreign.Storable

-- | PETSc headers
#include <petscsnes.h>
#include <petsctao.h>
#include <petscdm.h>
#include <petscdmda.h>
#include <petscdmcomposite.h>
#include <petscts.h>
#include <petscviewer.h>
#include <petscviewerhdf5.h>
#include <petscsys.h>
#include <petscpctypes.h>

-- | SLEPc headers
#include <slepceps.h>
#include <slepcsvd.h>

type PetscInt_ = {# type PetscInt #}
type PetscScalar_ = {#type PetscScalar #}
type PetscReal_ = {#type PetscReal#}
-- /* typedef enum { PETSC_FALSE,PETSC_TRUE } PetscBool; */
-- {#enum PetscBool as PetscBool {underscoreToCase} deriving (Eq, Show)#}
type PetscBool_ = {#type PetscBool#}
-- deriving instance Storable PetscBool

-- * IS

-- * Vec

-- * Mat

-- * DM

{# enum DMBoundaryType as DmBoundaryType {underscoreToCase} deriving (Eq, Show) #}


-- -- * DMDA

-- * KSP

-- * PF

-- * SNES

-- * TS

-- * TAO

-- * Viewer

-- * PETSc misc

-- * MPI misc

-- CString




-- errors etc


-- /* typedef int PetscErrorCode; */
-- {#typedef PetscErrorCode CInt#}
-- {#default out `ErrCode' [PetscErrorCode] convErrCode#}
-- convErrCode :: CInt -> ErrCode
-- convErrCode = ErrCode . fromIntegral
-- newtype ErrCode = ErrCode { unErrCode :: Int } deriving (Eq, Show)




