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



  

-- | constants
moduleRoot :: String
moduleRoot = "Numerical.PETSc.Internal."


-- main = emitModule modulename modImports headers types -- writeModuleToFile fname modulename modImports headers types
--  where
--   fname = modulename ++ ".chs"
--   modulename = "TypesC2HsGen"
--   modImports = [moduleRoot ++ "Utils",
--                 "Control.Monad",
--                 "Foreign",
--                 "Foreign.C.Types",
--                 "Foreign.Storable"]
--   petscHeaders =
--     chImports "petsc" ["snes", "tao", "dm", "dmda", "dmcomposite", "ts", "viewer", "viewerhdf5", "sys", "types"]
--   slepcHeaders =
--     chImports "slepc" ["eps", "svd"]
--   headers = petscHeaders ++ slepcHeaders
--   types = ["DMBoundaryType"]

dmTypes :: CTypeSuffix -> CType
dmTypes = CType "DM"


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
-- typeDecl :: String -> String
typeDecl t = spaces ["type", show t, "=", typeType [ show t]]



-- | Haskell - C marshalling helpers

-- dmbToC x = toCInt $ fromEnum (x :: Dmb_) :: Dmb
convertToC ty =
  concat [funCType ty, "ToC", " ", freeVarName] ++
  " = " ++
  (inParens [toCTyF, "$", fromHsTyF, " ", freeVarName, " :: ", typeNameHs (show ty) ]) ++ " :: " ++ show ty where
   toCTyF = "toCInt"
   fromHsTyF = "fromEnum"
   freeVarName = "x"

-- dmbFromC c = (toEnum $ fromIntegral (c :: Dmb)) :: Dmb_
convertFromC ty =
    concat [funCType ty, "FromC", " ", freeVarName] ++
  " = " ++
  (inParens [toCTyF, "$", fromHsTyF, " ", freeVarName, " :: ", show ty ]) ++ " :: " ++ typeNameHs (show ty) where
   toCTyF = "toEnum"
   fromHsTyF = "fromIntegral"
   freeVarName = "c"



-- | C2HS enum declaration

-- enumType :: [String] -> String
enumType t = c2hsMacro1 Enum t

-- `{# enum .. as .. {underscoreToCase} deriving (Eq, Show) #}`
enumTypeDecl ty =
  enumType ([show ty ++" as "++ typeNameHs (show ty),
             typeOptions UnderscoreToCase,
             " deriving (Eq, Show)"])


-- | entry for a single type (declarations and conversion functions)
-- enumTypeEntry "" = error "enumTypeEntry : type cannot be empty"
enumTypeEntry ty@(CType tys o) =
  entry [enumTypeDecl ty,
         typeDecl ty,
         convertToC ty,
         convertFromC ty]
  where
      entry :: [String] -> String
      entry ll = cr ll ++ "\n"


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
  | isUpper mi = spaces ["module", moduleRoot ++ m, "where"]
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
-- emitModule :: ModuleName -> [ModuleName] -> [HeaderName] -> [String] -> IO ()
emitModule m ii hh ee = do
  putStrLn $ preamble m ii hh
  putStrLn $ enumEntries ee

-- | module to file
-- writeModuleToFile :: FilePath -> ModuleName -> [ModuleName] -> [HeaderName] -> [String] -> IO ()
writeModuleToFile fp m ii hh ee  =
  writeFile fp (cr ([preamble m ii hh,
                     enumEntries ee]))



-- |===================

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

-- typeNameHs :: String -> String
typeNameHs t = (show t) ++ "_"


-- | helpers

spaces :: [String] -> String
spaces = intercalate " "

inParens :: [String] -> String
inParens c = "(" ++ spaces c ++ ")"

cr :: [String] -> String
cr = intercalate "\n"

lowercaseInitial :: String -> String 
lowercaseInitial (c:cs) = toLower c : cs
lowercaseInitial [] = []






