{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.Petsc.Raw.Internal
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | inline-c context (i.e. the C-Haskell type map), and the necessary newtypes
--   and type synonyms necessary to its definition
--
-----------------------------------------------------------------------------

module Numerical.PETSc.Raw.Internal where

import qualified Language.C.Inline as C
import qualified Language.C.Types as CT
import Language.C.Inline.Context

import Foreign.C.Types 
import Foreign.C.String
import Foreign

import qualified Language.Haskell.TH as TH

import Data.Monoid ((<>), mempty)
import qualified Data.Map as Map

-- import qualified Data.Vector.Storable.Mutable as V

-- * inline-c PETSc Context (type maps)
petscCtx :: Context
petscCtx = baseCtx <> funCtx <> vecCtx <> ctx where
  ctx = mempty {ctxTypesTable = petscTypesTable}

petscTypesTable :: Map.Map CT.TypeSpecifier TH.TypeQ  
petscTypesTable = Map.fromList
                  [
                    (typeNameId "PetscInt", [t| PetscInt_ |] )
                  , (typeNameId "PetscReal", [t| PetscReal_ |])  
                  , (typeNameId "PetscMPIInt", [t| PetscMPIInt_ |] )
                  , (typeNameId "PetscError", [t| PetscError_ |]) 
                  , (typeNameId "PetscBool", [t| PetscBool_ |])
                  , (typeNameId "PetscScalar", [t| PetscScalar_ |])
                  , (typeNameId "PetscLogStage", [t| PetscLogStage_ |])  

                  , (typeNameId "IS", [t| IS |] )
                  , (typeNameId "Vec", [t| Vec |] )
                  , (typeNameId "Mat", [t| Mat |] )
                  , (typeNameId "DM", [t| DM |] )
                  , (typeNameId "KSP", [t| KSP |])

                  , (typeNameId "SNES", [t| SNES |])
                  , (typeNameId "SNESLineSearch", [t| SNESLineSearch|])  

                  , (typeNameId "PF", [t| PF |])

                  , (typeNameId "PetscSpace", [t| PetscSpace |])
                  , (typeNameId "PetscDualSpace", [t| PetscDualSpace |])
                  , (typeNameId "PetscFE", [t| PetscFE|])
                  , (typeNameId "PetscQuadrature", [t| PetscQuadrature |])  
                    
                  , (typeNameId "TS", [t| TS |])
                   
                  , (typeNameId "Tao", [t| Tao |])

                  , (typeNameId "PetscViewer", [t| PetscViewer |])

                  , (typeNameId "MatFDColoring", [t| MatFDColoring |])
                  , (typeNameId "ISColoring", [t| ISColoring |])
                  ]

typeNameId :: CT.CIdentifier -> CT.TypeSpecifier
typeNameId = CT.TypeName 


-- * type synonyms

type PetscLogStage_ = CInt
type PetscError_ = CInt

type PetscInt_ = CInt
type PetscBool_ = Bool
type PetscScalar_ = CDouble
type PetscReal_ = CDouble

type MatConst = CInt


-- -- FIXME : robust approach would be to infer the Hs types with c2hs

-- type PetscInt_ = PetscInt
-- type PetscBool_ = PetscBool
-- type PetscScalar_ = PetscScalar
-- type PetscReal_ = PetscReal



-- * newtypes

newtype PetscMPIInt_ = PetscMPIInt_ (Ptr PetscMPIInt_ ) deriving (Show, Storable)

newtype PetscReal = PetscReal (Ptr PetscReal)
instance Storable PetscReal where
  sizeOf _ = sizeOf (undefined :: PetscReal_)
  alignment = sizeOf
  peek = peek
  poke = poke 


newtype IS = IS (Ptr IS) deriving Storable

newtype Vec = Vec (Ptr Vec) deriving Storable

newtype Mat = Mat (Ptr Mat) deriving Storable

newtype DM = DM (Ptr DM) deriving Storable

newtype KSP = KSP (Ptr KSP) deriving Storable

newtype KSPConvergedReason = KSPConvergedReason (Ptr KSPConvergedReason) deriving (Eq, Storable)

newtype SNES = SNES (Ptr SNES) deriving Storable
newtype SNESLineSearch = SNESLineSearch (Ptr SNESLineSearch) deriving Storable

newtype PF = PF (Ptr PF) deriving Storable

newtype TS = TS (Ptr TS) deriving Storable

newtype Tao = Tao (Ptr Tao) deriving Storable 

newtype PetscSpace = PetscSpace (Ptr PetscSpace) deriving Storable
newtype PetscDualSpace = PetscDualSpace (Ptr PetscDualSpace) deriving Storable
newtype PetscFE = PetscFE (Ptr PetscFE) deriving Storable
newtype PetscQuadrature = PetscQuadrature (Ptr PetscQuadrature) deriving Storable

newtype PetscViewer = PetscViewer (Ptr PetscViewer) deriving Storable

newtype MatFDColoring = MatFDColoring (Ptr MatFDColoring) deriving Storable

newtype ISColoring = ISColoring (Ptr ISColoring) deriving Storable





-- * Complex numbers :

{- storable-complex in Hackage :
{-# LANGUAGE ScopedTypeVariables #-}

module Foreign.Storable.Complex () where

import Data.Complex
import Foreign.Storable
import Foreign.Ptr

-- This Storable instance for Complex is binary compatible with C99, C++ and
-- Fortran complex data types.
instance Storable a => Storable (Complex a) where
    sizeOf z        = 2 * sizeOf (undefined :: a)
    alignment z     = alignment (undefined :: a)
    peek p          = do let q = castPtr p
                         r <- peek q
                         i <- peekElemOff q 1
                         return (r :+ i)
    poke p (r :+ i) = do let q = (castPtr p)
                         poke q r
                         pokeElemOff q 1 i
-}




