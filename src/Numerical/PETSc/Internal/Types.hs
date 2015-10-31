{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Types
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | Types for inline-c based signatures and resp. helpers
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Types where

-- import Foreign
import Foreign.C.Types



data InsertMode_ = NotSetValues | InsertValues | AddValues | MaxValues | InsertAllValues | AddAllValues | InsertBCValues | AddBCValues deriving (Eq, Enum, Show)

insertModeToInt x = fromEnum (x :: InsertMode_) 



data PetscCopyMode_ =
  PetscCopyVals | PetscOwn | PetscUsePointer deriving (Eq, Show, Enum)
petscCopyModeToInt x = fromEnum (x :: PetscCopyMode_ )

-- * IS


-- ISColoringType  -- determines if the coloring is for the entire parallel grid/graph/matrix or for just the local ghosted portion
-- Synopsis :
-- typedef enum {IS_COLORING_GLOBAL,IS_COLORING_GHOSTED} ISColoringType;
--   IS_COLORING_GLOBAL - does not include the colors for ghost points, this is used when the function is called synchronously in parallel. This requires generating a "parallel coloring".
--   IS_COLORING_GHOSTED - includes colors for ghost points, this is used when the function can be called separately on individual processes with the ghost points already filled in. Does not require a "parallel coloring", rather each process colors its local + ghost part. Using this can result in much less parallel communication. In the paradigm of DMGetLocalVector() and DMGetGlobalVector() this could be called IS_COLORING_LOCAL


data ISColoringType_ = ISColoringGlobal | ISColoringGhosted deriving (Eq, Show, Enum)
isColoringTypeToInt x = fromEnum (x :: ISColoringType_)


-- * Vec

data VecNorm_ = VecNorm1 | VecNorm2 | VecNormFrobenius | VecNormInfty | VecNorm1and2 deriving (Eq, Enum, Show)
vecNormToInt x = fromEnum (x :: VecNorm_ )



-- * Mat

data MatType_ = MatSame | MatMaij | MatSeqMaij | MatMPIMaij | MatIs | MatAij
              | MatSeqAij | MatSeqAijPThread | MatAijPthread | MatMPIAij -- etc.
                                                               deriving (Eq, Show)
matTypeToStr MatSame = "same"
matTypeToStr MatIs = "is"
matTypeToStr MatAij = "aij"
matTypeToStr MatMPIAij = "mpiaij"
matTypeToStr _ = "mpiaij" -- default


data MatCompositeType_ = MatCompositeAdditive | MatCompositeMultiplicative deriving (Eq, Show, Enum)
matCompositeTypeToInt x = fromEnum (x :: MatCompositeType_ )



-- * DM

data DMBoundaryType_ = DmBNone | DmBGhosted | DmBMirror | DmBPeriodic | DmBTwist deriving (Eq, Show, Enum)
dmBoundaryTypeToInt x = fromEnum (x :: DMBoundaryType_)

cIntToDmBoundaryType c =
  case g of 0 -> DmBNone
            1 -> DmBGhosted
            2 -> DmBMirror
            3 -> DmBPeriodic
            4 -> DmBTwist
            _ -> DmBNone -- default
  where g = fromIntegral (c :: CInt)


-- -- * DMDA

data DMDAStencilType = DmSStar | DmSBox deriving (Eq, Show, Enum)
dmdaStencilTypeToInt x = fromEnum (x :: DMDAStencilType)

cIntToDmdaStencilType c =
  case g of 0 -> DmSStar
            1 -> DmSBox
            _ -> DmSStar -- default
  where g = fromIntegral (c :: CInt)



-- * KSP

data KspType_ = KspRichardson | KspChebyshev | KspCg | KspGroppCg | KspPipeCg
              | KspCgne | KspNash | KspStcg | KspGltr | KspGmres | KspFgmres
              | KspLgmres | KspDgmres | KspPgmres | KspTcqmr | KspBcgs | KspIbcgs
              | KspFbcgs | KspFbcgsr | KspBcgsl | KspCgs | KspTfqmr | KspCr
              | KspPipecr | KspLsqr | KspPreonly | KspQcg | KspBicg | KspMinres
              | KspSymmlq | KspLcd | KspPython | KspCgr | KspSpecest
              deriving (Eq, Show)

kspTypeToStr :: KspType_ -> String
kspTypeToStr KspRichardson = "richardson"
kspTypeToStr KspChebyshev = "chebyshev"
kspTypeToStr KspCg = "cg"
kspTypeToStr KspGmres = "gmres"
kspTypeToStr KspBicg = "bicg"
kspTypeToStr KspMinres = "minres"
kspTypeToStr _ = "cg" -- default, for no particular reason

data KspConvergedReason = KspConvergedRtolNormal | KspConvergedAtolNormal
                        | KspConvergedRtol | KspConvergedAtol
                        | KspConvergedIts | KspConvergedCgNegCurv
                        | KspConvergedCgConstrained | KspConvergedStepLength
                        | KspConvergedHappyBreakdown
                        | KspDivergedNull | KspDivergedIts | KspDivergedDtol
                        | KspDivergedBreak
                        | KspDivergedBreakBicg | KspDivergedNonsymm
                        | KspDivergedIndefPC
                        | KspDivergedNaNorInf | KspDivergedIndefMat
                        | KspCConvergedIterating
                        deriving (Eq, Show, Enum)

kspConvergedIntToReason :: Int -> KspConvergedReason
kspConvergedIntToReason r =
  case r of 1    -> KspConvergedRtolNormal
            9    -> KspConvergedAtolNormal
            2    -> KspConvergedRtol 
            3    -> KspConvergedAtol
            4    -> KspConvergedIts
            5    -> KspConvergedCgNegCurv
            6    -> KspConvergedCgConstrained
            7    -> KspConvergedStepLength
            8    -> KspConvergedHappyBreakdown
            (-2) -> KspDivergedNull
            (-3) -> KspDivergedIts





-- * PF

data PFType_ = PfConstant | PfMat | PfString | PfQuick | PfIdentity | PfMatlab deriving (Eq, Show)

pfTypeToStr PfConstant = "constant"
pfTypeToStr PfMat = "mat"
pfTypeToStr PfString = "string"
pfTypeToStr PfQuick = "quick"
pfTypeToStr PfIdentity = "identity"
pfTypeToStr PfMatlab = "matlab"




-- * SNES

data SnesType_ = SnesNewtonLs | SnesNewtonTr | SnesPython | SnesTest
               | SnesNRichardson | SnesKspOnly | SnesViNewtonRsLs
               | SnesViNewtonSsLs | SnesNgmres | SnesQn | SnesShell | SnesNgs
               | SnesNcg | SnesFas | SnesMs | SnesNasm | SnesAnderson | SnesAspin
               | SnesComposite deriving (Eq, Show)

snesTypeToStr :: SnesType_ -> String
snesTypeToStr SnesNewtonLs = "newtonls"
snesTypeToStr SnesNewtonTr = "newtontr"
snesTypeToStr SnesNgmres = "ngmres"
snesTypeToStr _ = "newtonls" -- default




-- * TS

data TsProblemType = TsLinear | TsNonlinear deriving (Eq, Show, Enum)
tsProblemTypeToInt x = fromEnum (x :: TsProblemType)




-- * TAO

data TaoType_ = TaoLmvm | TaoNls | TaoNtr | TaoNtl | TaoCg | TaoTron | TaoOwlqn
              | TaoBmrm | TaoBlmvm | TaoBqpip | TaoGpcg | TaoNm | TaoPounders
              | TaoLcl | TaoSsils | TaoSSfls | TaoAsils | TaoAsfls | TaoIpm | TaoTest
              deriving (Eq, Show)

taoTypeToStr TaoLmvm = "lmvm"
taoTypeToStr TaoNls = "nls"
taoTypeToStr TaoNtr = "ntr"
taoTypeToStr TaoNtl = "ntl"
taoTypeToStr TaoCg = "cg"
taoTypeToStr TaoSsils = "ssils"
taoTypeToStr TaoIpm = "ipm"
taoTypeToStr _ = "cg" -- default

data TaoConvergedReason_ = TaoConvFaTol | TaoConvFrTol | TaoConvGaTol
                         | TaoConvGrTol | TaoConvGtTol
                         | TaoConvStepTol | TaoConvMinF | TaoConvUser
                         | TaoDivMaxIts | TaoDivNan | TaoDivMaxFcn | TaoDivLsFail
                         | TaoDivTrReduct | TaoDivUser deriving (Eq,Show,Enum)

taoConvergedIntToReason x =
  case x of 1 -> TaoConvFaTol
            2 -> TaoConvFrTol
            3 -> TaoConvGaTol
            4 -> TaoConvGrTol
            5 -> TaoConvGtTol
            6 -> TaoConvStepTol
            7 -> TaoConvMinF
            8 -> TaoConvUser
            (-2) -> TaoDivMaxIts
            (-4) -> TaoDivNan
            (-5) -> TaoDivMaxFcn
            (-6) -> TaoDivLsFail
            (-7) -> TaoDivTrReduct
            (-8) -> TaoDivUser
            _ -> TaoDivUser




-- * Viewer

data PetscViewerType_ = ViewerSock | ViewerAscii | ViewerBinary | ViewerString | ViewerDraw | ViewerVu | ViewerMathematica | ViewerNetcdf | ViewerHdf5 | ViewerVtk | ViewerMatlab | ViewerSaws deriving (Eq, Enum, Show)

viewerTypeToInt x = fromEnum (x :: PetscViewerType_)

viewerTypeToStr ViewerSock = "socket"
viewerTypeToStr ViewerAscii = "ascii"
viewerTypeToStr ViewerHdf5 = "hdf5"
viewerTypeToStr ViewerNetcdf = "netcdf"

-- * FileMode
data PetscFileMode_ = FileModeRead | FileModeWrite | FileModeAppend | FileModeUpdate | FileModeAppendUpdate deriving (Eq, Enum, Show)
fileModeToInt x = fromEnum (x :: PetscFileMode_)

-- FILE_MODE_READ - open a file at its beginning for reading
-- FILE_MODE_WRITE - open a file at its beginning for writing (will create if the file does not exist)
-- FILE_MODE_APPEND - open a file at end for writing
-- FILE_MODE_UPDATE - open a file for updating, meaning for reading and writing
-- FILE_MODE_APPEND_UPDATE - open a file for updating, meaning for reading and writing, at the end




-- * MPI


-- -- Comm
data Comm = Comm {unComm :: CInt} deriving (Eq, Show)



-- -- Rank
newtype Rank = MkRank { rankId :: CInt -- ^ Extract numeric value of the 'Rank'
                      }
   deriving (Eq, Ord, Enum, Integral, Real, Show)

instance Num Rank where
  (MkRank x) + (MkRank y) = MkRank (x+y)
  (MkRank x) * (MkRank y) = MkRank (x*y)
  abs (MkRank x) = MkRank (abs x)
  signum (MkRank x) = MkRank (signum x)
  fromInteger x
    | x >  fromIntegral (maxBound :: CInt) = error "Rank value does not fit into 32 bits"
    | x < 0             = error "Negative Rank value"
    | otherwise         = MkRank (fromIntegral x)



