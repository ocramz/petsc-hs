{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}

-- | Internal3 : ForeignPtr around recursive newtypes
-- -- e.g. newtype A = A (Ptr A)
-- -- --   data DataA = DataA (ForeignPtr A, ...)

module Numerical.PETSc.Internal3
       where

-- import Numerical.PETSc.Types
  --(PetscInt, PetscBool, PetscReal, PetscScalar)

import Control.Exception as E
import Data.Typeable
import qualified Language.C.Inline as C
import qualified Language.C.Types as CT
import Foreign.C.Types
import Foreign.C.String
import Foreign
import qualified Language.Haskell.TH as TH
import Language.C.Inline.Context
import Data.Monoid ((<>), mempty)
import Control.Monad (unless, when, liftM)
import Control.Concurrent
import qualified Data.Map as Map
import Language.C.Inline

import qualified Data.Vector.Storable.Mutable as V

C.include "<stdio.h>"
C.include "<math.h>"


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
                  -- , (typeNameId "DMBoundaryType", [t| DMBoundaryType_ |]) 
                  -- , (typeNameId "DMDAStencilType", [t| DMDAStencilType |])
                  -- , (typeNameId "DMDAInterpolationType", [t| DMDAInterpolationType |])
                  -- , (typeNameId "DMDAElementType", [t|DMDAElementType|])  
                    
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
                  ]

typeNameId :: String -> CT.TypeSpecifier
typeNameId = CT.TypeName . CT.Identifier

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
 -- peek p = do i <- (# peek Foo, i) p
 --                j <- (# peek Foo, j) p
 --                k <- (# peek Foo, k) p
 --                return $ Foo i j k
 --    poke p foo = do (# poke Foo, i) p (i foo)
 --                    (# poke Foo, j) p (j foo)
 --                    (# poke Foo, k) p (k foo)


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
