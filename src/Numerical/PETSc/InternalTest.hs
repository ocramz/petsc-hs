{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Numerical.PETSc.InternalTest where

-- import Numerical.PETSc.Types
import qualified Language.C.Inline as C
import qualified Language.C.Types as CT
import Foreign.C.Types
import Foreign.C.String
import Foreign
import qualified Language.Haskell.TH as TH
import Language.C.Inline.Context
import Data.Monoid ((<>), mempty)
import Control.Monad (unless, when, liftM)
import qualified Data.Map as Map
import Language.C.Inline

import qualified Data.Vector.Storable.Mutable as V

C.include "<stdio.h>"
C.include "<math.h>"

-- C.include "<petscksp.h>"
-- C.include "<petscsys.h>"

-- * inline-c PETSc Context (type mappings)
petscCtx :: Context
petscCtx = baseCtx <> funCtx <> vecCtx <> ctx where
  ctx = mempty {ctxTypesTable = petscTypesTable}

petscTypesTable :: Map.Map CT.TypeSpecifier TH.TypeQ
petscTypesTable = Map.fromList
                  [
                    (typeNameId "PetscInt", [t| PetscInt_ |] )  
                  , (typeNameId "PetscError", [t| PetscError_ |]) 
                  , (typeNameId "PetscBool", [t| PetscBool_ |])
                  , (typeNameId "Vec", [t| Vec |] )
                  , (typeNameId "Mat", [t| Mat |] )
                  , (typeNameId "KSP", [t| KSP |])   
                  , (typeNameId "Tao", [t| Tao |])
                  -- , (typeNameId "MPI_Comm", [t| Comm |])
                  ]

typeNameId :: String -> CT.TypeSpecifier
typeNameId = CT.TypeName . CT.Identifier

-- * type synonyms

type PetscInt_ = CInt
type PetscError_ = CInt
type PetscBool_ = Bool

-- * newtypes

newtype Vec = Vec (Ptr Vec) deriving Storable

newtype Mat = Mat (Ptr Mat) deriving Storable 
  
newtype KSP = KSP (Ptr KSP) deriving Storable

newtype Tao = Tao (Ptr Tao) deriving Storable 


-- * MPI

data Comm = Comm {unComm :: CInt} deriving (Eq, Show)


