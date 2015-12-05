{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.Petsc.Internal.Internal
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | inline-c context (i.e. the C-Haskell type map)
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Internal where

import Numerical.PETSc.Internal.Types

import qualified Language.C.Inline         as C
import qualified Language.C.Types          as CT
import           Language.C.Inline.Context

import qualified Language.Haskell.TH       as TH

import           Data.Monoid               ((<>), mempty)
import qualified Data.Map                  as Map

-- * inline-c PETSc Context (type maps)
petscCtx :: Context
petscCtx = baseCtx <> funCtx <> vecCtx <> bsCtx <> pctx <> sctx where
  pctx = mempty {ctxTypesTable = petscTypesTable}
  sctx = mempty {ctxTypesTable = slepcTypesTable}

petscTypesTable :: Map.Map CT.TypeSpecifier TH.TypeQ  
petscTypesTable = Map.fromList
                  [
                    (typeNameId "PetscInt", [t| PetscInt_ |] )
                  , (typeNameId "PetscReal", [t| PetscReal_ |])  
                  , (typeNameId "PetscMPIInt", [t| PetscMPIInt_ |] )
                  , (typeNameId "PetscError", [t| PetscError_ |]) 
                  -- , (typeNameId "PetscBool", [t| PetscBool_ |])
                  , (typeNameId "PetscBool", [t| PetscBool |])                
                  , (typeNameId "PetscScalar", [t| PetscScalar_ |])
                  , (typeNameId "PetscLogStage", [t| PetscLogStage_ |])  

                  , (typeNameId "IS", [t| IS |] )
                  , (typeNameId "Vec", [t| Vec |] )
                  , (typeNameId "Mat", [t| Mat |] )
                  , (typeNameId "MatInfo", [t| MatInfo |])
                  , (typeNameId "DM", [t| DM |] )
                  , (typeNameId "DMDALocalInfo", [t| DMDALocalInfo |])
                  , (typeNameId "KSP", [t| KSP |])

                  , (typeNameId "SNES", [t| SNES |])
                  , (typeNameId "SNESLineSearch", [t| SNESLineSearch|])  

                  , (typeNameId "PF", [t| PF |])

                  , (typeNameId "PetscSpace", [t| PetscSpace |])
                  , (typeNameId "PetscDualSpace", [t| PetscDualSpace |])
                  , (typeNameId "PetscFE", [t| PetscFE|])
                  , (typeNameId "PetscQuadrature", [t| PetscQuadrature |])  
                    
                  , (typeNameId "TS", [t| TS |])
                  , (typeNameId "TSTrajectory", [t|TSTrajectory|])
                   
                  , (typeNameId "Tao", [t| Tao |])

                  , (typeNameId "PetscViewer", [t| PetscViewer |])

                  , (typeNameId "MatFDColoring", [t| MatFDColoring |])
                  , (typeNameId "ISColoring", [t| ISColoring |])

                  , (typeNameId "MatFactorInfo", [t| MatFactorInfo |])
                  , (typeNameId "PetscSection", [t| PetscSection |])
                  ]
                  

slepcTypesTable :: Map.Map CT.TypeSpecifier TH.TypeQ
slepcTypesTable = Map.fromList
                  [(typeNameId "IS", [t| IS |] )
                  , (typeNameId "Vec", [t| Vec |] )
                  , (typeNameId "Mat", [t| Mat |] )
                    
                  , (typeNameId "EPS", [t| EPS |])
                  , (typeNameId "SVD", [t| SVD |])
                  , (typeNameId "PEP", [t| PEP |])
                  , (typeNameId "NEP", [t| NEP |])
                  , (typeNameId "MFN", [t| MFN |])                    

                  , (typeNameId "ST", [t| ST |])
                  , (typeNameId "DS", [t| DS |])
                  , (typeNameId "BV", [t| BV |])
                  , (typeNameId "FN", [t| FN |])
                  , (typeNameId "RG", [t| RG |])  ]


typeNameId :: CT.CIdentifier -> CT.TypeSpecifier
typeNameId = CT.TypeName 






-- * Complex numbers :

--  storable-complex in Hackage :
-- {-# LANGUAGE ScopedTypeVariables #-}

-- module Foreign.Storable.Complex () where

-- import Data.Complex
-- import Foreign.Storable
-- import Foreign.Ptr

-- -- This Storable instance for Complex is binary compatible with C99, C++ and
-- -- Fortran complex data types.
-- instance Storable a => Storable (Complex a) where
--     sizeOf z        = 2 * sizeOf (undefined :: a)
--     alignment z     = alignment (undefined :: a)
--     peek p          = do let q = castPtr p
--                          r <- peek q
--                          i <- peekElemOff q 1
--                          return (r :+ i)
--     poke p (r :+ i) = do let q = (castPtr p)
--                          poke q r
--                          pokeElemOff q 1 i




