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

import Data.Complex
import Foreign.Storable.Complex

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG



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








-- end newtypes
















data InsertMode_ = NotSetValues | InsertValues | AddValues | MaxValues
                 | InsertAllValues | AddAllValues | InsertBCValues | AddBCValues
                 deriving (Eq, Enum, Show)

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

data VecNorm_ = VecNorm1 | VecNorm2 | VecNormFrobenius | VecNormInfty
              | VecNorm1and2
              deriving (Eq, Enum, Show)

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
matTypeToStr MatSame = "same"
matTypeToStr MatIs = "is"
matTypeToStr MatAij = "aij"
matTypeToStr MatMPIAij = "mpiaij"
matTypeToStr MatMPIBaij = "mpibaij"
matTypeToStr _ = "mpiaij" -- default



-- typedef enum {NORM_1=0,NORM_2=1,NORM_FROBENIUS=2,NORM_INFINITY=3,NORM_1_AND_2=4} NormType;
data MatNorm_ = MatNorm1 | MatNorm2 | MatNormF | MatNormInfty | MatNorm1and2 deriving (Eq, Show, Enum)
matNormToInt x = fromEnum (x :: MatNorm_)


-- typedef enum {MAT_INITIAL_MATRIX,MAT_REUSE_MATRIX,MAT_IGNORE_MATRIX} MatReuse;
data MatReuse_ = MatInitialMtx | MatReuseMtx | MatIgnoreMtx deriving (Eq, Show, Enum)

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

data DMBoundaryType_ = DmBNone | DmBGhosted | DmBMirror | DmBPeriodic | DmBTwist
                     deriving (Eq, Show, Enum)

dmBoundaryTypeToInt :: DMBoundaryType_ -> Int
dmBoundaryTypeToInt x = fromEnum (x :: DMBoundaryType_)

cIntToDmBoundaryType :: CInt -> DMBoundaryType_
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

cIntToDmdaStencilType :: CInt -> DMDAStencilType
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
kspTypeToStr x = case x of
  KspRichardson -> "richardson"
  KspChebyshev -> "chebyshev"
  KspCg -> "cg"
  KspGroppCg -> "groppcg"
  KspPipeCg -> "pipecg"
  KspCgne -> "cgne"
  KspNash -> "nash"
  KspStcg -> "stcg"
  KspGltr -> "gltr"
  KspGmres -> "gmres"
  KspFgmres -> "fgmres"
  KspLgmres -> "lgmres"
  KspDgmres -> "dgmres"
  KspPgmres -> "pgmres"
  KspTcqmr -> "tcqmr"
  KspBcgs -> "bcgs"
  KspIbcgs -> "ibcgs"
  KspFbcgs -> "fbcgs"
  KspFbcgsr -> "fbcgsr"
  KspBcgsl -> "bcgsl"
  KspCgs -> "cgs"
  KspTfqmr -> "tfqmr"
  KspCr -> "cr"
  KspPipecr -> "pipecr"
  KspLsqr -> "lsqr"
  KspPreonly -> "preonly"
  KspQcg -> "qcg"
  KspBicg -> "bicg"
  KspMinres -> "minres"
  KspSymmlq -> "symmlq"
  KspLcd -> "lcd"
  KspPython -> "python"
  KspCgr -> "cgr"
  KspSpecest -> "specest"

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
snesTypeToStr t = case t of
  SnesNewtonLs -> "newtonls"
  SnesNewtonTr -> "newtontr"
  SnesPython -> "python"
  SnesTest -> "test"
  SnesNRichardson -> "nrichardson"
  SnesKspOnly -> "ksponly"
  SnesViNewtonRsLs -> "vinewtonrsls"
  SnesViNewtonSsLs -> "vinewtonssls"
  SnesNgmres -> "ngmres"
  SnesQn -> "qn"
  SnesShell -> "shell"
  SnesNgs -> "ngs"
  SnesNcg -> "ncg"
  SnesFas -> "fas"
  SnesMs -> "ms"
  SnesNasm -> "nasm"
  SnesAnderson -> "anderson"
  SnesAspin -> "aspin"
  SnesComposite -> "composite"



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
                           | SnesOtherReason Int
                              deriving (Eq, Show)  

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
    n -> SnesOtherReason n

-- * TS

data TsProblemType = TsLinear | TsNonlinear deriving (Eq, Show, Enum)

tsProblemTypeToInt :: TsProblemType -> Int
tsProblemTypeToInt x = fromEnum (x :: TsProblemType)


data TsType_ = TsEuler | TsBEuler | TsPseudo | TsCn | TsSundials | TsRK | TsPython
             | TsTheta | TsAlpha | TsGl | TsSsp | TsArkimex | TsRosw | TsEimex
             | TsMimex deriving (Eq, Show) 

tsTypeToString :: TsType_ -> String
tsTypeToString t = case t of TsEuler -> "euler"
                             TsBEuler -> "beuler"
                             TsPseudo -> "pseudo"
                             TsCn -> "cn"
                             TsSundials -> "sundials"
                             TsRK -> "rk"
                             TsPython -> "python"
                             TsTheta -> "theta"
                             TsAlpha -> "alpha"
                             TsGl -> "gl"
                             TsSsp -> "ssp"
                             TsArkimex -> "arkimex"
                             TsRosw -> "rosw"
                             TsEimex -> "eimex"
                             TsMimex -> "mimex"

data TsConvergedReason_ = TsConvergedIterating | TsConvergedTime | TsConvergedIts
                        | TsConvergedUser | TsConvergedEvent | TsDivergedNonlinSolve
                        | TsDivergedStepRejected | TsUndefinedConvergenceCase
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
  _ -> TsUndefinedConvergenceCase




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

data PetscViewerType_ = ViewerSock | ViewerAscii | ViewerBinary | ViewerString
                      | ViewerDraw | ViewerVu | ViewerMathematica | ViewerNetCDF
                      | ViewerHDF5 | ViewerVtk | ViewerMatlab | ViewerSaws
                      deriving (Eq, Enum, Show)

viewerTypeToInt x = fromEnum (x :: PetscViewerType_)

viewerTypeToStr :: PetscViewerType_ -> String
viewerTypeToStr v = case v of
  ViewerSock -> "socket"
  ViewerAscii -> "ascii"
  ViewerBinary -> "binary"
  ViewerString -> "string"
  ViewerDraw -> "draw"
  ViewerVu -> "vu"
  ViewerMathematica -> "mathematica"
  ViewerNetCDF -> "netcdf"
  ViewerHDF5 -> "hdf5"
  ViewerVtk -> "vtk"
  ViewerMatlab -> "matlab"
  ViewerSaws -> "saws"








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





-- | SLEPc datatypes and enum conversion hacks




-- * EPS

data EpsType_ =
  EpsPower | EpsSubspace | EpsArnoldi | EpsLanczos | EpsKrylovSchur | EpsGd | EpsJd
  | EpsRqcg | EpsLobpcg | EpsCiss | EpsLapack | EpsArpack | EpsBlzpack | EpsTrlan
  | EpsBlopex | EpsPrimme | EpsFeast deriving (Eq, Show, Enum)

epsTypeToString t =
  case t of EpsPower -> "power"
            EpsSubspace -> "subspace"
            EpsArnoldi -> "arnoldi"
            EpsLanczos -> "lanczos"
            EpsKrylovSchur -> "krylovschur"
            EpsGd -> "gd"
            _ -> epsTypeToString EpsKrylovSchur -- default

-- typedef const char* EPSType;
-- #define EPSPOWER       "power"
-- #define EPSSUBSPACE    "subspace"
-- #define EPSARNOLDI     "arnoldi"
-- #define EPSLANCZOS     "lanczos"
-- #define EPSKRYLOVSCHUR "krylovschur"
-- #define EPSGD          "gd"
-- #define EPSJD          "jd"
-- #define EPSRQCG        "rqcg"
-- #define EPSLOBPCG      "lobpcg"
-- #define EPSCISS        "ciss"
-- #define EPSLAPACK      "lapack"
-- #define EPSARPACK      "arpack"
-- #define EPSBLZPACK     "blzpack"
-- #define EPSTRLAN       "trlan"
-- #define EPSBLOPEX      "blopex"
-- #define EPSPRIMME      "primme"
-- #define EPSFEAST       "feast"



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

stTypeToString t =
  case t of StShell -> "shell"
            StShift -> "shift"
            StSInvert -> "sinvert"
            StCayley -> "cayley"
            StPrecond -> "precond"
                                             
