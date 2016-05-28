-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.Petsc.Internal.Meta.GenerateC2Hs
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | c2hs generation
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Meta.GenerateC2Hs where

import Data.Char
import Data.List

-- | SETTINGS

-- | output destination
outFormat :: OutputFormat  -- `FileFmt` or `StdoutFmt`
outFormat = StdoutFmt       

-- | `.chs` file name
outModuleName = "TypesC2HsGen"

outTypes :: [CType]
outTypes = [dm "BoundaryType"]

-- | module paths
-- genFilePath = dottedPath ["Numerical", "PETSc", "Internal", "Meta"]

internalPath = dottedPath ["Numerical", "PETSc", "Internal"]


-- | 

main :: IO ()
main | outFormat == StdoutFmt = emitModule modulename modImports headers types
     | otherwise = writeModuleToFile fname modulename modImports headers types
 where
  fname = modulename ++ ".chs"
  modulename = outModuleName 
  modImports = [internalPath ++ "Utils",
                "Foreign",
                "Foreign.C.Types",
                "Foreign.Storable"]
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

dm :: CTypeSuffix -> CType
dm = CType "DM"


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
typeDecl t = spaces ["type", show t, "=", typeType [ show t]]



-- | Haskell - C marshalling helpers

-- dmbToC x = toCInt $ fromEnum (x :: Dmb_) :: Dmb
marshalToC :: CType -> String
marshalToC ty =
  concat [funCType ty, "ToC", " ", freeVarName] ++
  " = " ++
  (inParens [toCTyF, "$", fromHsTyF, " ", freeVarName, " :: ", typeNameHs ty ]) ++ " :: " ++ show ty where
   toCTyF = "toCInt"
   fromHsTyF = "fromEnum"
   freeVarName = "x"

-- dmbFromC c = (toEnum $ fromIntegral (c :: Dmb)) :: Dmb_
marshalFromC :: CType -> String
marshalFromC ty =
    concat [funCType ty, "FromC", " ", freeVarName] ++
  " = " ++
  (inParens [toCTyF, "$", fromHsTyF, " ", freeVarName, " :: ", show ty ]) ++ " :: " ++ typeNameHs ty where
   toCTyF = "toEnum"
   fromHsTyF = "fromIntegral"
   freeVarName = "c"



-- | C2HS enum declaration

-- enumType :: [String] -> String
enumType t = c2hsMacro1 Enum t

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

--

   



-- | PREAMBLE sections

preamble :: ModuleName -> [ModuleName] -> [HeaderName] -> String
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
  | isUpper mi = spaces ["module", internalPath ++ m, "where"]
  | otherwise = "moduleStr : module name initial must be uppercase"


-- Haskell module import
importStr :: ModuleName -> String
importStr [] = error "importStr : module name cannot be empty"
importStr i@(ii:_)
  | isUpper ii = "import " ++ i ++ "\n"
  | otherwise = error "importStr : module name initial must be uppercase"

hsImports :: [ModuleName] -> [String]
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
emitModule :: ModuleName -> [ModuleName] -> [HeaderName] -> [CType] -> IO ()
emitModule m ii hh ee = do
  putStrLn $ preamble m ii hh
  putStrLn $ enumEntries ee

-- | module to file
writeModuleToFile :: FilePath -> ModuleName -> [ModuleName] -> [HeaderName] -> [CType] -> IO ()
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
showCType (CType a o) = (map toUpper a) ++ o
funCType :: CType -> String
funCType (CType a o) = (map toLower a) ++ o

-- | types
type ModuleName = String
type HeaderName = String

--  c2hs types
data C2HsTypes = Enum | Type deriving Eq
instance Show C2HsTypes where
  show Enum = "enum"
  show Type = "type"


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

lowercaseInitial :: String -> String 
lowercaseInitial (c:cs) = toLower c : cs
lowercaseInitial [] = []






