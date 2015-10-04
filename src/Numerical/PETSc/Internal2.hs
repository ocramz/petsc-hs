{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
module Numerical.PETSc.Internal2
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
                  -- , (typeNameId "KSPType", [t| KSPType |])
                  -- , (typeNameId "KSPConvergedReason", [t| KSPConvergedReason|])

                  , (typeNameId "SNES", [t| SNES |])
                  , (typeNameId "SNESLineSearch", [t| SNESLineSearch|])  

                  , (typeNameId "PF", [t| PF |])

                  -- , (typeNameId "PetscViewer", [t| PetscViewer |])  

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

-- * DM




-- * KSP





-- * MPI








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



-- * error

handleErrTup (r, _) = return r
handleErr _ = return ()

-- handleErrTup :: (a, CInt) -> IO a
-- handleErrTup (res, ie)
--   | ie /= 0 = throwIOErr ie
--   | otherwise = return res

-- handleErr :: CInt -> IO ()
-- handleErr 0 = return ()
-- handleErr n = throwIOErr n

-- throwIOErr :: CInt -> IO a
-- throwIOErr n = throwIO (ErrorCall $ "PETSc error " ++ show (n' :: Int)) where
--   n' = fromIntegral (n :: CInt)



convErrCode :: CInt -> ErrCode
convErrCode = ErrCode . fromIntegral
newtype ErrCode = ErrCode { unErrCode :: Int } deriving (Eq, Show)



-- data ErrState e = NoErr | Err e

-- chkErrCode e | c == 0 = NoErr
--              | otherwise = Err c
--   where c = unErrCode e

-- * utils

-- boolErr :: (Monad m , Num a, Eq a) => m a -> m Bool
-- boolErr = liftM toBool



-- * Exceptions


-- data PetscException = NotInitialized
--                     | Finalized
--                     | Other
--                     deriving (Show,Typeable)
-- instance Exception PetscException



-- * utils

sndM :: Monad m => m (a, b) -> m b
sndM = liftM snd

fstM :: Monad m => m (a, b) -> m a
fstM = liftM fst

fst2 x = ((y1, y2), t ) where
  y1 = fst x
  y2 = (fst . snd) x
  t = (snd . snd) x

fst2M :: Monad m => (a, (b, c)) -> m ((a, b), c)
fst2M = return . fst2


-- fi = fromIntegral
