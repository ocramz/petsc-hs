{-# LANGUAGE ForeignFunctionInterface #-}

-- | C2HS type inference macros
module Numerical.PETSc.Internal.TypesInternal
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

#include <petscksp.h>

type PetscInt_ = {# type PetscInt #}
type PetscScalar_ = {#type PetscScalar #}
type PetscReal_ = {#type PetscReal#}
-- /* typedef enum { PETSC_FALSE,PETSC_TRUE } PetscBool; */
-- {#enum PetscBool as PetscBool {underscoreToCase} deriving (Eq, Show)#}
type PetscBool_ = {#type PetscBool#}


-- * PETSc misc

-- * MPI misc

-- CString











