-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.Petsc.Internal.C2HsGen.GenerateC2Hs
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | c2hs generation
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.C2HsGen.GenerateC2Hs where

import Data.Char
import Data.List

-- | SETTINGS

-- | output destination
outFormat :: OutputFormat  -- `FileFmt` or `StdoutFmt`
outFormat = StdoutFmt       

-- | `.chs` file name
outModuleName = "TypesC2HsGen"

-- | list of C types to map to Haskell
outTypes :: [CType]
outTypes = [
  dm "BoundaryType"
  , dmda "StencilType"
  -- , snes "ConvergedReason"
           ]

-- | type synonyms
outTypeSynonyms = [
  petsc "Scalar"
  , petsc "Real"
                  ]

-- | module paths
internalPath = dottedPath ["Numerical", "PETSc", "Internal"]
c2hsGenPath = dottedPath ["Numerical", "PETSc", "Internal", "C2HsGen"]





-- | 

main :: IO ()
main | outFormat == StdoutFmt = emitModule modulename modImports headers types typeds
     | otherwise = writeModuleToFile fname modulename modImports headers types
 where
  fname = modulename ++ ".chs"
  modulename = outModuleName 
  modImports = [(internalPath ++ "Utils", Nothing),
                ("Foreign", Nothing),
                ("Foreign.C.Types", Just "CT"),
                ("Foreign.Storable", Nothing)]
  petscHeaders =
    chImports "petsc" ["snes",
                       "tao",
                       "dm", "dmda", "dmcomposite",
                       "ts",
                       "viewer", "viewerhdf5",
                       "sys"]
  slepcHeaders =
    chImports "slepc" ["eps", "svd"]
  headers = petscHeaders ++ slepcHeaders
  types = outTypes
  typeds = outTypeSynonyms

dm :: CTypeSuffix -> CType
dm = CType "DM"

dmda = CType "DMDA"

snes = CType "SNES"

petsc = CType "Petsc"






-- | c2hs stuff

-- | outer brackets, i.e.  `{# ... #}`
-- c2hsMacro1 :: C2HsTypes -> [String] -> String
c2hsMacro1 t body = c2hsMacro [show t, spaces body] where
  c2hsMacro c = concat ["{# ", spaces c, " #}"]


-- | e.g. "{underscoreToCase}"
typeOptions :: TypeOptions -> String
typeOptions to = concat ["{ ", show (to :: TypeOptions) , " }"]






-- | Haskell + c2hs  declarations e.g.
-- `type Ty = {# type Ty #}`
typeDecl :: CType -> String
typeDecl t = spaces ["type", show t, "=", typeType [ show t ]]

-- `type Ty_ = {# type Ty #}`
typeDecl' t = spaces ["type", typeNameHs t, "=", typeType [show t] ]





-- | Haskell - C marshalling helpers



-- dmbToC = (CInt . fromIntegral . fromEnum 
marshalToC :: CType -> String
marshalToC ty =
  cr [funSig, funDef]
  where
   funDef = spaces [funName, "=", "CT.CInt", ".", "fromIntegral", ".", "fromEnum"]
   funName = concat [funCType ty, "ToC"]
   funSig = spaces [funName, "::", typeNameHs ty, "->", show ty]

-- dmbFromC = (toEnum . fromIntegral
marshalFromC :: CType -> String
marshalFromC ty =
  cr [funSig, funDef]
  where
   funDef = spaces [funName, "=", "toEnum", ".", "fromIntegral"]
   funName = concat [funCType ty, "FromC"]
   funSig = spaces [funName, "::", show ty, "->", typeNameHs ty]



-- | C2HS enum declaration

-- enumType :: [String] -> String
enumType t = c2hsMacro1 Enum t

enumDefineType t = c2hsMacro1 EnumDefine t

-- `{# enum .. as .. {underscoreToCase} deriving (Eq, Show) #}`
enumTypeDecl :: CType -> String
enumTypeDecl ty =
  enumType ([show ty ++" as "++ typeNameHs ty,
             typeOptions UnderscoreToCase,
             " deriving (Eq, Show)"])


-- | entry for a single type (declarations and conversion functions)
-- enumTypeEntry "" = error "enumTypeEntry : type cannot be empty"
enumTypeEntry :: CType -> String
enumTypeEntry ty =
  entry [enumTypeDecl ty,
         typeDecl ty,
         marshalToC ty,
         marshalFromC ty]
  where
      entry :: [String] -> String
      entry ll = cr ll ++ "\n"

enumEntries :: [CType] -> String
enumEntries tt = cr (map enumTypeEntry tt)



-- | separate type declarations

typeDecls tt = cr (map typeDecl' tt)
   



-- | PREAMBLE sections

preamble :: ModuleName -> [(ModuleName, Qual)] -> [HeaderName] -> String
preamble m ii hh = cr [
  extFFI,
  moduleStr m,                 -- module declaration
  concat (hsImports ii),       -- Haskell module imports
  concat hh                    -- PETSC C header imports
                      ]    

-- extensions
extFFI = langExt "ForeignFunctionInterface"
langExt c = "{-# LANGUAGE " ++ c ++ "#-}\n"

-- current module name
moduleStr :: ModuleName -> String
moduleStr [] = error "moduleStr : module name cannot be empty"
moduleStr m@(mi:_)
  | isUpper mi = spaces ["module", c2hsGenPath ++ m, "where"]
  | otherwise = "moduleStr : module name initial must be uppercase"


-- Haskell module import
type Qual = Maybe String -- "qualified" = Just <QualifiedModuleName>

importStr :: (ModuleName, Qual) -> String
importStr ([], _ )= error "importStr : module name cannot be empty"
importStr (i@(ii:_), q)
  | isUpper ii = spaces (qualStr q i)
  | otherwise = error "importStr : module name initial must be uppercase"

qualStr :: Qual -> ModuleName -> [String]
qualStr (Just qs) m = importbracket "qualified" m (spaces ["as",qs])
qualStr Nothing m = importbracket "" m ""
importbracket qq mm tt = ["import",qq,mm,tt,"\n"]

hsImports :: [(ModuleName, Qual)] -> [String]
hsImports = map importStr


-- C header import

chImportStr :: String -> HeaderName -> String
chImportStr _ [] = error "hImportStr : file name cannot be empty"
chImportStr hprefix m = concat ["#include ", "<", hfname, "> \n"] where
  hfname = hprefix ++ m ++ ".h"

chImports :: String -> [HeaderName] -> [String]
chImports hprefix = map (chImportStr hprefix)





-- | putting it all together

-- | module to stdout
-- emitModule :: ModuleName -> [ModuleName] -> [HeaderName] -> [CType] -> IO ()
emitModule m ii hh ee tt = do
  putStrLn $ preamble m ii hh
  putStrLn "-- | Type synonyms \n"
  putStrLn $ typeDecls tt
  putStrLn "-- | Enumeration types \n"
  putStrLn $ enumEntries ee

-- | module to file
-- writeModuleToFile :: FilePath -> ModuleName -> [ModuleName] -> [HeaderName] -> [CType] -> IO ()
writeModuleToFile fp m ii hh ee  =
  writeFile fp (cr ([preamble m ii hh,
                     enumEntries ee]))



-- |===================

data OutputFormat = StdoutFmt | FileFmt deriving (Eq, Show)

type CTypePrefix = String
type CTypeSuffix = String
data CType = CType { ctypePre :: CTypePrefix, ctypeSuff :: CTypeSuffix} deriving (Eq)
instance Show CType where
  show = showCType

showCType :: CType -> String
showCType (CType a o) = a ++ o 
funCType :: CType -> String
funCType (CType a o) = (map toLower a) ++ o


-- | types

type ModuleName = String
type HeaderName = String

--  c2hs types
data C2HsTypes = Enum
               | Type
               | EnumDefine
               deriving Eq
instance Show C2HsTypes where
  show Enum = "enum"
  show Type = "type"
  show EnumDefine = "enum define"


typeType t = c2hsMacro1 Type t

data TypeOptions = UnderscoreToCase | NoTO deriving (Eq)
instance Show TypeOptions where
  show UnderscoreToCase = "underscoreToCase"
  show NoTO = ""

  

-- | conversion functions

typeNameHs :: CType -> String
typeNameHs t = (show t) ++ "_"


-- | helpers

dottedPath :: [String] -> String
dottedPath pp = concat $ map (\p -> p ++ ".") pp

spaces :: [String] -> String
spaces = intercalate " "

inParens :: [String] -> String
inParens c = "(" ++ spaces c ++ ")"

cr :: [String] -> String
cr = intercalate "\n"





