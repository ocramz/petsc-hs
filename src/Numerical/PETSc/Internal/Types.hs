{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, TypeSynonymInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Types
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Types for inline-c based signatures and resp. helpers
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Types where

import Numerical.PETSc.Internal.Utils
import Numerical.PETSc.Internal.Storable.StorableContainer

import Data.Char

import Data.Complex
import Foreign.Storable.Complex

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG


-- | helper for marshalling method strings

showDropN :: Show a => Int -> a -> String
showDropN n = drop n . map toLower . show




-- | elementary type synonyms

type PetscLogStage_ = CInt
type PetscError_ = CInt

type PetscInt_ = CInt
type PetscBool_ = Bool
type PetscScalar_ = CDouble  -- | FIXME : what abt complex-scalar PETSc builds?
type PetscReal_ = CDouble

type MatConst = CInt


-- -- FIXME : robust approach would be to infer the Hs types with c2hs

-- type PetscInt_ = PetscInt
-- type PetscBool_ = PetscBool
-- type PetscScalar_ = PetscScalar
-- type PetscReal_ = PetscReal






-- | shortcut synonyms

type Scalar = PetscScalar_







-- | newtypes

newtype PetscBool = PetscBool (Ptr PetscBool) deriving Show
instance Storable PetscBool where
  sizeOf _ = sizeOf (undefined :: CChar)
  alignment _ = alignment (undefined :: CChar)
  peek = peek
  poke = poke

-- | NB : ^ already exists in Foreign.Storable :
  
-- instance Storable Bool where
--    sizeOf _          = sizeOf (undefined::HTYPE_INT)
--    alignment _       = alignment (undefined::HTYPE_INT)
--    peekElemOff p i   = liftM (/= (0::HTYPE_INT)) $ peekElemOff (castPtr p) i
--    pokeElemOff p i x = pokeElemOff (castPtr p) i (if x then 1 else 0::HTYPE_INT)




newtype PetscMPIInt_ = PetscMPIInt_ (Ptr PetscMPIInt_ ) deriving (Show, Storable)

newtype PetscReal = PetscReal (Ptr PetscReal)
instance Storable PetscReal where
  sizeOf _ = sizeOf (undefined :: PetscReal_)
  alignment _ = alignment (undefined :: PetscReal_)
  peek = peek
  poke = poke 


newtype IS = IS (Ptr IS) deriving Storable

newtype Vec = Vec (Ptr Vec) deriving Storable

-- instance StorableContainer Vec IO PetscScalar_ where
  


newtype Mat = Mat (Ptr Mat) deriving Storable

newtype DM = DM (Ptr DM) deriving Storable
newtype DMDALocalInfo = DMDALocalInfo (Ptr DMDALocalInfo) deriving Storable

newtype KSP = KSP (Ptr KSP) deriving Storable

newtype PC = PC (Ptr PC) deriving Storable

newtype KSPConvergedReason = KSPConvergedReason (Ptr KSPConvergedReason)
                           deriving (Eq, Storable)

newtype SNES = SNES (Ptr SNES) deriving Storable
newtype SNESLineSearch = SNESLineSearch (Ptr SNESLineSearch) deriving Storable

newtype PF = PF (Ptr PF) deriving Storable

newtype TS = TS (Ptr TS) deriving Storable
newtype TSTrajectory = TSTrajectory (Ptr TSTrajectory) deriving Storable

newtype Tao = Tao (Ptr Tao) deriving Storable 

newtype PetscSpace = PetscSpace (Ptr PetscSpace) deriving Storable
newtype PetscDualSpace = PetscDualSpace (Ptr PetscDualSpace) deriving Storable
newtype PetscFE = PetscFE (Ptr PetscFE) deriving Storable
newtype PetscQuadrature = PetscQuadrature (Ptr PetscQuadrature) deriving Storable

newtype PetscViewer = PetscViewer (Ptr PetscViewer) deriving Storable

newtype MatFDColoring = MatFDColoring (Ptr MatFDColoring) deriving Storable

newtype ISColoring = ISColoring (Ptr ISColoring) deriving Storable

newtype MatFactorInfo = MatFactorInfo (Ptr MatFactorInfo) deriving Storable

newtype PetscSection = PetscSection (Ptr PetscSection) deriving Storable


-- | SLEPc newtypes

newtype EPS = EPS (Ptr EPS) deriving Storable

newtype SVD = SVD (Ptr SVD) deriving Storable

newtype PEP = PEP (Ptr PEP) deriving Storable

newtype NEP = NEP (Ptr NEP) deriving Storable

newtype MFN = MFN (Ptr MFN) deriving Storable


-- Spectral Transformation (ST) 
newtype ST = ST (Ptr ST) deriving Storable
-- Direct Solver (DS)
newtype DS = DS (Ptr DS) deriving Storable
-- Basis Vectors (BV)
newtype BV = BV (Ptr BV) deriving Storable
-- Mathematical Function (FN)
newtype FN = FN (Ptr FN) deriving Storable
-- spectral Region (RG)
newtype RG = RG (Ptr RG) deriving Storable








-- | end newtypes
















data InsertMode_ = NotSetValues | InsertValues | AddValues | MaxValues
                 | InsertAllValues | AddAllValues | InsertBCValues | AddBCValues
                 deriving (Eq, Enum, Show)

insertModeToInt :: InsertMode_ -> Int
insertModeToInt x = fromEnum (x :: InsertMode_) 



data PetscCopyMode_ =
  PetscCopyVals | PetscOwn | PetscUsePointer deriving (Eq, Show, Enum)
petscCopyModeToInt :: PetscCopyMode_ -> Int
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

data VecNorm_ = VecNorm1 | VecNorm2 | VecNormFrobenius | VecNormInfty
              | VecNorm1and2
              deriving (Eq, Enum, Show)

vecNormToInt :: VecNorm_ -> Int
vecNormToInt x = fromEnum (x :: VecNorm_ )




data VecInfo = VecInfo 
 {vecInfoMpiComm :: Comm ,
  vecInfoSizeLocal :: !Int ,
  vecInfoSizeGlobal :: !Int } deriving (Eq, Show)











-- * Mat

data MatType_ = MatSame | MatMaij | MatSeqMaij | MatMPIMaij | MatIs | MatAij
              | MatSeqAij | MatSeqAijPThread | MatAijPthread | MatMPIAij | MatMPIBaij -- etc.
                                                               deriving (Eq, Show)

matTypeToStr :: MatType_ -> String
matTypeToStr mty = showDropN 3 (mty :: MatType_)


-- typedef enum {NORM_1=0,NORM_2=1,NORM_FROBENIUS=2,NORM_INFINITY=3,NORM_1_AND_2=4} NormType;
data MatNorm_ = MatNorm1 | MatNorm2 | MatNormF | MatNormInfty | MatNorm1and2 deriving (Eq, Show, Enum)
matNormToInt :: MatNorm_ -> Int
matNormToInt x = fromEnum (x :: MatNorm_)


-- typedef enum {MAT_INITIAL_MATRIX,MAT_REUSE_MATRIX,MAT_IGNORE_MATRIX} MatReuse;
data MatReuse_ = MatInitialMtx | MatReuseMtx | MatIgnoreMtx deriving (Eq, Show, Enum)

matReuseToInt :: MatReuse_ -> Int
matReuseToInt x = fromEnum (x :: MatReuse_ )


-- | MatStencil 
data MatStencil = MatStencil CInt CInt CInt CInt deriving (Eq, Show)

instance Storable MatStencil where
  sizeOf _ = 4 * sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    k <- peekElemOff q 0
    j <- peekElemOff q 1
    i <- peekElemOff q 2
    c <- peekElemOff q 3
    return (MatStencil k j i c)
      where q = castPtr ptr
  poke p (MatStencil k j i c) = do
    pokeElemOff q 0 k
    pokeElemOff q 1 j
    pokeElemOff q 2 i
    pokeElemOff q 3 c
      where q = castPtr p

-- typedef struct {
--   PetscLogDouble block_size;                         /* block size */
--   PetscLogDouble nz_allocated,nz_used,nz_unneeded;   /* number of nonzeros */
--   PetscLogDouble memory;                             /* memory allocated */
--   PetscLogDouble assemblies;                         /* number of matrix assemblies called */
--   PetscLogDouble mallocs;                            /* number of mallocs during MatSetValues() */
--   PetscLogDouble fill_ratio_given,fill_ratio_needed; /* fill ratio for LU/ILU */
--   PetscLogDouble factor_mallocs;                     /* number of mallocs during factorization */
-- } MatInfo;
data MatInfo =
  MatInfo {matInfoBlockSize :: CDouble,
           matInfoNzAllocated, matInfoNzUsed, matInfoNzUnneeded :: CDouble,
           matInfoMemory :: CDouble,
           matInfoAssemblies :: CDouble,
           matInfoMallocs :: CDouble,
           matInfoFillRatioGiven, matInfoFillRatioNeeded :: CDouble,
           matInfoFactorMallocs :: CDouble } deriving (Eq, Show)

instance Storable MatInfo where
  sizeOf _ = 10 * sizeOf (undefined :: CDouble)
  alignment _ = alignment (undefined :: CDouble)
  peek ptr = do
    bs <- peekElemOff q 0
    nza <- peekElemOff q 1
    nzu <- peekElemOff q 2
    nzun <- peekElemOff q 3
    mem <- peekElemOff q 4
    asm <- peekElemOff q 5
    mall <- peekElemOff q 6
    frg <- peekElemOff q 7
    fri <- peekElemOff q 8
    factmall <- peekElemOff q 9
    return (MatInfo bs nza nzu nzun mem asm mall frg fri factmall)
      where q = castPtr ptr
  poke ptr (MatInfo bs nza nzu nzun mem asm mall frg fri factmall) = do
    pokeElemOff q 0 bs
    pokeElemOff q 1 nza
    pokeElemOff q 2 nzu
    pokeElemOff q 3 nzun
    pokeElemOff q 4 mem
    pokeElemOff q 5 asm
    pokeElemOff q 6 mall
    pokeElemOff q 7 frg
    pokeElemOff q 8 fri
    pokeElemOff q 9 factmall
     where q = castPtr ptr

-- typedef enum {MAT_LOCAL=1,MAT_GLOBAL_MAX=2,MAT_GLOBAL_SUM=3} MatInfoType;
data MatInfoType_ = MatInfoLocal | MatInfoGlobalMax | MatInfoGlobalSum deriving (Eq, Show, Enum)

matInfoTypeToInt :: MatInfoType_ -> Int
matInfoTypeToInt t = case t of
  MatInfoLocal -> 1
  MatInfoGlobalMax -> 2
  MatInfoGlobalSum -> 3

-- typedef enum {DIFFERENT_NONZERO_PATTERN,SUBSET_NONZERO_PATTERN,SAME_NONZERO_PATTERN} MatStructure;
data MatStructure_ = MatDifferentNZPattern | MatSubsetNZPattern | MatSameNZPattern
                   deriving (Eq, Show, Enum)
                            
matStructureToInt x = fromEnum (x :: MatStructure_ )


data MatCompositeType_ = MatCompositeAdditive | MatCompositeMultiplicative
                       deriving (Eq, Show, Enum)
                                
matCompositeTypeToInt x = fromEnum (x :: MatCompositeType_ )



-- * DM

-- data DMBoundaryType_ = DmBNone | DmBGhosted | DmBMirror | DmBPeriodic | DmBTwist
--                      deriving (Eq, Show, Enum)

-- dmBoundaryTypeToCInt :: DMBoundaryType_ -> CInt
-- dmBoundaryTypeToCInt i = toCInt $ dmBoundaryTypeToInt i 

-- dmBoundaryTypeToInt :: DMBoundaryType_ -> Int
-- dmBoundaryTypeToInt x = fromEnum (x :: DMBoundaryType_)

-- cIntToDmBoundaryType :: CInt -> DMBoundaryType_
-- cIntToDmBoundaryType c =
--   case g of 0 -> DmBNone
--             1 -> DmBGhosted
--             2 -> DmBMirror
--             3 -> DmBPeriodic
--             4 -> DmBTwist
--             _ -> DmBNone -- default
--   where
--     g :: Int
--     g = fromIntegral (c :: CInt)


-- -- * DMDA

data DMDAStencilType = DmSStar | DmSBox deriving (Eq, Show, Enum)
dmdaStencilTypeToInt x = fromEnum (x :: DMDAStencilType)

cIntToDmdaStencilType :: CInt -> DMDAStencilType
cIntToDmdaStencilType c =
  case g of 0 -> DmSStar
            1 -> DmSBox
            _ -> DmSStar -- default
  where
    g :: Int
    g = fromIntegral (c :: CInt)



-- * KSP

data KspType_ = KspRichardson | KspChebyshev | KspCg | KspGroppCg | KspPipeCg
              | KspCgne | KspNash | KspStcg | KspGltr | KspGmres | KspFgmres
              | KspLgmres | KspDgmres | KspPgmres | KspTcqmr | KspBcgs | KspIbcgs
              | KspFbcgs | KspFbcgsr | KspBcgsl | KspCgs | KspTfqmr | KspCr
              | KspPipecr | KspLsqr | KspPreonly | KspQcg | KspBicg | KspMinres
              | KspSymmlq | KspLcd | KspPython | KspCgr | KspSpecest
              deriving (Eq, Show)

kspTypeToStr :: KspType_ -> String
kspTypeToStr kt = showDropN 3 (kt :: KspType_)



data KspConvergedReason = KspConvergedRtolNormal | KspConvergedAtolNormal
                        | KspConvergedRtol | KspConvergedAtol
                        | KspConvergedIts | KspConvergedCgNegCurv
                        | KspConvergedCgConstrained | KspConvergedStepLength
                        | KspConvergedHappyBreakdown
                        | KspDivergedNull | KspDivergedIts | KspDivergedDtol
                        | KspDivergedBreakdown
                        | KspDivergedBreakdownBicg | KspDivergedNonsymm
                        | KspDivergedIndefPC
                        | KspDivergedNaNorInf | KspDivergedIndefMat
                        | KspDivergedPCSetupFailed
                        | KspConvergedIterating
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
            (-4) -> KspDivergedDtol
            (-5) -> KspDivergedBreakdown
            (-6) -> KspDivergedBreakdownBicg
            (-7) -> KspDivergedNonsymm
            (-8) -> KspDivergedIndefPC
            (-9) -> KspDivergedNaNorInf
            (-10)-> KspDivergedIndefMat
            (-11)-> KspDivergedPCSetupFailed
            0    -> KspConvergedIterating
            n    -> error $ "KSP returned an unknown code " ++ show n
            





-- 





-- * PC

data PCType_ = PcNone | PcJacobi | PcSOR | PcLU | PcShell | PcBJacobi | PcMG
             | PcEisenstat | PcILU | PcICC | PcASM | PcGASM | PcKSP | PcComposite
             | PcSPAI | PcNN | PcCholesky | PcPBJacobi | PcMat | PcHypre | PcParms
             | PcFieldSplit | PcTFS | PcML | PcGalerkin | PcExotic | PcCP | PcBFBT
             | PcLSC | PcPython | PcPFMG | PcSysPFMG | PcRedistribute | PcSVD
             | PcGAMG  -- plus the CUDA ones, see petscpctypes.h
             deriving (Eq, Show, Enum)

pcTypeToString :: PCType_ -> String
pcTypeToString t = showDropN 2 (t :: PCType_)

  











-- * PF

data PFType_ = PfConstant | PfMat | PfString | PfQuick | PfIdentity | PfMatlab deriving (Eq, Show)

pfTypeToStr :: PFType_ -> String
pfTypeToStr pt = showDropN 2 (pt :: PFType_)










-- * SNES

data SnesType_ = SnesNewtonLs | SnesNewtonTr | SnesPython | SnesTest
               | SnesNRichardson | SnesKspOnly | SnesViNewtonRsLs
               | SnesViNewtonSsLs | SnesNgmres | SnesQn | SnesShell | SnesNgs
               | SnesNcg | SnesFas | SnesMs | SnesNasm | SnesAnderson | SnesAspin
               | SnesComposite deriving (Eq, Show)

snesTypeToStr :: SnesType_ -> String 
snesTypeToStr st = showDropN 4 (st :: SnesType_)





data SnesConvergedReason = SnesConvergedFnormAbs
                           | SnesConvergedFnormRelative
                           | SnesConvergedSnormRelative  
                           | SnesConvergedIts   
                           | SnesConvergedTRDelta  
                           | SnesDivergedFunctionDomain  
                           | SnesDivergedFunctionCount   
                           | SnesDivergedLinearSolve  
                           | SnesDivergedFnormNaN  
                           | SnesDivergedMaxIts  
                           | SnesDivergedLineSearch  
                           | SnesDivergedInnerSolve  
                           | SnesDivergedLocalMin  
                           | SnesConvergedIterating
                              deriving (Eq, Show)  

snesConvergedIntToReason :: Int -> SnesConvergedReason
snesConvergedIntToReason x =
  case x of 
--               SNES_CONVERGED_FNORM_ABS         =  2, /* ||F|| < atol */
    2 -> SnesConvergedFnormAbs
--               SNES_CONVERGED_FNORM_RELATIVE    =  3, /* ||F|| < rtol*||F_initial|| */
    3 -> SnesConvergedFnormRelative
--               SNES_CONVERGED_SNORM_RELATIVE    =  4, /* Newton computed step size small; || delta x || < stol || x ||*/
    4 -> SnesConvergedSnormRelative
--               SNES_CONVERGED_ITS               =  5, /* maximum iterations reached */
    5 -> SnesConvergedIts
--               SNES_CONVERGED_TR_DELTA          =  7,
    7 -> SnesConvergedTRDelta
--               /* diverged */
--               SNES_DIVERGED_FUNCTION_DOMAIN     = -1, /* the new x location passed the function is not in the domain of F */
    (-1) -> SnesDivergedFunctionDomain
--               SNES_DIVERGED_FUNCTION_COUNT      = -2,
    (-2) -> SnesDivergedFunctionCount
--               SNES_DIVERGED_LINEAR_SOLVE        = -3, /* the linear solve failed */
    (-3) -> SnesDivergedLinearSolve
--               SNES_DIVERGED_FNORM_NAN           = -4,
    (-4) -> SnesDivergedFnormNaN
--               SNES_DIVERGED_MAX_IT              = -5,
    (-5) -> SnesDivergedMaxIts
--               SNES_DIVERGED_LINE_SEARCH         = -6, /* the line search failed */
    (-6) -> SnesDivergedLineSearch
--               SNES_DIVERGED_INNER               = -7, /* inner solve failed */
    (-7) -> SnesDivergedInnerSolve
--               SNES_DIVERGED_LOCAL_MIN           = -8, /* || J^T b || is small, implies converged to local minimum of F() */
    (-8) -> SnesDivergedLocalMin
--               SNES_CONVERGED_ITERATING          =  0} SNESConvergedReason;
    0 -> SnesConvergedIterating
    n -> error $ "SNES returned an unknown code " ++ show n






-- * TS

data TsProblemType = TsLinear | TsNonlinear deriving (Eq, Show, Enum)

tsProblemTypeToInt :: TsProblemType -> Int
tsProblemTypeToInt x = fromEnum (x :: TsProblemType)


data TsType_ = TsEuler | TsBEuler | TsPseudo | TsCn | TsSundials | TsRK | TsPython
             | TsTheta | TsAlpha | TsGl | TsSsp | TsArkimex | TsRosw | TsEimex
             | TsMimex deriving (Eq, Show)

tsTypeToString :: TsType_ -> String
tsTypeToString ts = showDropN 2 (ts :: TsType_)




data TsConvergedReason_ = TsConvergedIterating | TsConvergedTime | TsConvergedIts
                        | TsConvergedUser | TsConvergedEvent | TsDivergedNonlinSolve
                        | TsDivergedStepRejected
                        deriving (Eq, Enum, Show)

tsConvergedIntToReason :: CInt -> TsConvergedReason_
tsConvergedIntToReason c = case fi c of
  0 -> TsConvergedIterating
  1 -> TsConvergedTime
  2 -> TsConvergedIts
  3 -> TsConvergedUser
  4 -> TsConvergedEvent
  (-1) -> TsDivergedNonlinSolve
  (-2) -> TsDivergedStepRejected
  n -> error $ "TS returned an unknown code " ++ show n

-- typedef enum {TS_EXACTFINALTIME_STEPOVER=0,TS_EXACTFINALTIME_INTERPOLATE=1,TS_EXACTFINALTIME_MATCHSTEP=2} TSExactFinalTimeOption;
 -- TS_EXACTFINALTIME_STEPOVER    - Don't do anything if final time is exceeded
 -- TS_EXACTFINALTIME_INTERPOLATE - Interpolate back to final time
 -- TS_EXACTFINALTIME_MATCHSTEP - Adapt final time step to match the final time
data TsExactFinalTimeOption_ =
  TsEftStepover | TsEftInterpolate | TsEftMatchStep deriving (Eq, Enum, Show)

tsExactFinalTimeOptionToCInt :: TsExactFinalTimeOption_ -> CInt
tsExactFinalTimeOptionToCInt o = toCInt $ fromEnum (o :: TsExactFinalTimeOption_)







-- * TAO

data TaoType_ = TaoLmvm | TaoNls | TaoNtr | TaoNtl | TaoCg | TaoTron | TaoOwlqn
              | TaoBmrm | TaoBlmvm | TaoBqpip | TaoGpcg | TaoNm | TaoPounders
              | TaoLcl | TaoSsils | TaoSSfls | TaoAsils | TaoAsfls | TaoIpm | TaoTest
              deriving (Eq, Show)

taoTypeToStr :: TaoType_ -> String
taoTypeToStr tt = showDropN 3 (tt :: TaoType_)




data TaoConvergedReason_ = TaoConvFaTol | TaoConvFrTol | TaoConvGaTol
                         | TaoConvGrTol | TaoConvGtTol
                         | TaoConvStepTol | TaoConvMinF | TaoConvUser
                         | TaoDivMaxIts | TaoDivNan | TaoDivMaxFcn | TaoDivLsFail
                         | TaoDivTrReduct | TaoDivUser
                         deriving (Eq,Show,Enum)

taoConvergedIntToReason :: Int -> TaoConvergedReason_
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
            n -> error $ "TAO returned an unknown code " ++ show n










-- * Viewer

data PetscViewerType_ = ViewerSock | ViewerAscii | ViewerBinary | ViewerString
                      | ViewerDraw | ViewerVu | ViewerMathematica | ViewerNetCDF
                      | ViewerHDF5 | ViewerVtk | ViewerMatlab | ViewerSaws
                      deriving (Eq, Enum, Show)

viewerTypeToInt x = fromEnum (x :: PetscViewerType_)

viewerTypeToStr :: PetscViewerType_ -> String
viewerTypeToStr vt = showDropN 6 (vt :: PetscViewerType_)




data PetscViewerFormat_ =
    ViewFmtDefault | ViewFmtAsciiMatlab | ViewFmtAsciiMathematica
    | ViewFmtAsciiImpl | ViewFmtAsciiInfo | ViewFmtAsciiInfoDetail
    | ViewFmtAsciiCommon | ViewFmtAsciiSymmodu | ViewFmtAsciiIndex
    | ViewFmtAsciiDense | ViewFmtAsciiMatrixMarket | ViewFmtAsciiVtk
    | ViewFmtAsciiVtkCell | ViewFmtAsciiVtkCoords | ViewFmtAsciiPcice
    | ViewFmtAsciiPython | ViewFmtAsciiFactorInfo | ViewFmtAsciiLatex
    | ViewFmtDrawBasci | ViewFmtDrawLg | ViewFmtDrawContour | ViewFmtDrawPorts
    | ViewFmtVtkVts | ViewFmtVtkVtr | ViewFmtVtkVtu | ViewFmtBinaryMatlab
    | ViewFmtViewerNative | ViewFmtHdf5Viz | ViewFmtNoFormat
    deriving (Eq, Show, Enum)

petscViewerFormatToCInt :: PetscViewerFormat_ -> CInt
petscViewerFormatToCInt x = toCInt $ fromEnum (x :: PetscViewerFormat_)









-- * FileMode
data PetscFileMode_ = FileModeRead | FileModeWrite | FileModeAppend
                    | FileModeUpdate | FileModeAppendUpdate
                    deriving (Eq, Enum, Show)
                             
fileModeToInt x = fromEnum (x :: PetscFileMode_)

-- FILE_MODE_READ - open a file at its beginning for reading
-- FILE_MODE_WRITE - open a file at its beginning for writing (will create if the file does not exist)
-- FILE_MODE_APPEND - open a file at end for writing
-- FILE_MODE_UPDATE - open a file for updating, meaning for reading and writing
-- FILE_MODE_APPEND_UPDATE - open a file for updating, meaning for reading and writing, at the end






-- * MPI


-- -- Comm
data Comm = Comm {unComm :: CInt} deriving (Eq, Show)


newtype MpiCommSize = MkMpiCommSz {unCommSize' :: Int} deriving (Eq, Show, Ord, Enum)
newtype MpiCommRank = MkMpiCommRk {unCommRank' :: Int} deriving (Eq, Show, Ord, Enum)
data MPIComm = MPIComm { comm :: Comm, commSize :: MpiCommSize, commRank :: MpiCommRank} deriving (Eq, Show)



-- -- -- Rank
-- newtype Rank = MkRank { rankId :: CInt -- ^ Extract numeric value of the 'Rank'
--                       }
--    deriving (Eq, Ord, Enum, Integral, Real, Show)

-- instance Num Rank where
--   (MkRank x) + (MkRank y) = MkRank (x+y)
--   (MkRank x) * (MkRank y) = MkRank (x*y)
--   abs (MkRank x) = MkRank (abs x)
--   signum (MkRank x) = MkRank (signum x)
--   fromInteger x
--     | x >  fromIntegral (maxBound :: CInt) = error "Rank value does not fit into 32 bits"
--     | x < 0             = error "Negative Rank value"
--     | otherwise         = MkRank (fromIntegral x)









-- | SLEPc datatypes and enum conversion




-- * EPS

data EpsType_ =
  EpsPower | EpsSubspace | EpsArnoldi | EpsLanczos | EpsKrylovSchur | EpsGd | EpsJd
  | EpsRqcg | EpsLobpcg | EpsCiss | EpsLapack | EpsArpack | EpsBlzpack | EpsTrlan
  | EpsBlopex | EpsPrimme | EpsFeast deriving (Eq, Show, Enum)

epsTypeToStr :: EpsType_ -> String
epsTypeToStr et = showDropN 3 (et :: EpsType_)








-- typedef enum { EPS_HEP=1, EPS_GHEP, EPS_NHEP, EPS_GNHEP, EPS_PGNHEP, EPS_GHIEP } EPSProblemType;

data EpsProblemType_ = EpsHep | EpsGHep | EpsNHep | EpsGNHep | EpsPGNHep | EpsGHIep
                     deriving (Eq, Show, Enum)

epsProblemTypeToInt EpsHep = 1
epsProblemTypeToInt x = fromEnum (x :: EpsProblemType_ )

data EpsErrorType_ = EpsErrorAbsolute | EpsErrorRelative | EpsErrorBackward deriving (Eq, Show, Enum)

epsErrorTypeToInt x = fromEnum (x :: EpsErrorType_)


-- EpsConv_ : convergence test 

data EpsConv_ = EpsConvAbs | EpsConvEig | EpsConvNorm | EpsConvUser deriving (Eq, Show, Enum)

epsConvToInt x = fromEnum (x :: EpsConv_)



-- EPS balancing for non-Hermitian problems
data EpsBalance_ = EpsBalanceNone | EpsBalanceOneSide | EpsBalanceTwoSide | EpsBalanceUser deriving (Eq, Show, Enum)

epsBalanceToInt x = fromEnum (x :: EpsBalance_)


-- typedef enum { EPS_LARGEST_MAGNITUDE=1, EPS_SMALLEST_MAGNITUDE, EPS_LARGEST_REAL, EPS_SMALLEST_REAL, EPS_LARGEST_IMAGINARY, EPS_SMALLEST_IMAGINARY, EPS_TARGET_MAGNITUDE, EPS_TARGET_REAL, EPS_TARGET_IMAGINARY, EPS_ALL, EPS_WHICH_USER } EPSWhich;

data EpsWhich_ =
  LargestMag | SmallestMag | LargestReal | SmallestReal | LargestImag | SmallestImag
  | TargetMag | TargetReal | TargetImag | EpsWhichAll | EpsWhichUser deriving (Eq, Show, Enum)

epsWhichToInt :: EpsWhich_ -> Int
epsWhichToInt x = 1 + fromEnum (x :: EpsWhich_)







-- * SVD

data SvdType_ =
  SvdCross | SvdCyclic | SvdLanczos | SvdTRLanczos | SvdLapack
   deriving (Eq, Show, Ord)







-- * ST -- spectral transformations
data StType_ =
  StShell | StShift | StSInvert | StCayley | StPrecond deriving (Eq, Ord, Show)

stTypeToStr :: StType_ -> String
stTypeToStr st = showDropN 2 (st :: StType_)

                                             




