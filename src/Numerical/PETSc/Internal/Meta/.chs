{-# LANGUAGE ForeignFunctionInterface#-}

module Numerical.PETSc.Internal.TypesC2HsGen where
import Numerical.PETSc.Internal.Utils
import Foreign
import Foreign.C.Types
import Foreign.Storable

#include <petscsnes.h> 
#include <petsctao.h> 
#include <petscdm.h> 
#include <petscdmda.h> 
#include <petscdmcomposite.h> 
#include <petscts.h> 
#include <petscviewer.h> 
#include <petscviewerhdf5.h> 
#include <petscsys.h> 
#include <petsctypes.h> 
#include <slepceps.h> 
#include <slepcsvd.h> 

{# enum DMBoundaryType as DMBoundaryType_ { underscoreToCase }  deriving (Eq, Show) #}
type DMBoundaryType = {# type DMBoundaryType #}
dmBoundaryTypeToC x = (toCInt $ fromEnum   x  ::  DMBoundaryType_) :: DMBoundaryType
dmBoundaryTypeFromC c = (toEnum $ fromIntegral   c  ::  DMBoundaryType) :: DMBoundaryType_

