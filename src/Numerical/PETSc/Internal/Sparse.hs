{-# LANGUAGE FlexibleContexts, TypeFamilies, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Sparse
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Sparse vector and matrix indexing
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Sparse where

import Numerical.PETSc.Internal.Types

import Data.Functor
import Control.Monad

import qualified Data.Vector as V

import Foreign.C.Types


infixl 0 ~!~
c ~!~ msg = when c (error msg)

-- type AssocMatrix = [((Int,Int),Double)]
type AssocMatrix a = V.Vector ((Int, Int), a)
type PetscAssocMatrix = AssocMatrix PetscScalar_

data CSR = CSR
        { csrVals  :: V.Vector Double
        , csrCols  :: V.Vector CInt
        , csrRows  :: V.Vector CInt
        , csrNRows :: Int
        , csrNCols :: Int
        } deriving Show

data CSC = CSC
        { cscVals  :: V.Vector Double
        , cscRows  :: V.Vector CInt
        , cscCols  :: V.Vector CInt
        , cscNRows :: Int
        , cscNCols :: Int
        } deriving Show
