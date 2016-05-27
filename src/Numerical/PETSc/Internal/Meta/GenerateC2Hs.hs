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


-- | outer brackets
c2hsMacro1 :: C2HsTypes -> [String] -> String
c2hsMacro1 t body = c2hsMacro [show t, spaces body] where
  c2hsMacro c = concat ["{# ", spaces c, " #}"]

-- | c2hs types
data C2HsTypes = Enum | Type deriving Eq
instance Show C2HsTypes where
  show Enum = "enum"
  show Type = "type"
enumType t = c2hsMacro1 Enum t
typeType t = c2hsMacro1 Type t

-- | "`type` as `type`"
typeAs t t2 = concat [t, " as ", t2]
typeAsUnderscore t = typeAs t (t ++ "_")


-- | e.g. "{underscoreToCase}"
typeOptions :: TypeOptions -> String
typeOptions to = concat ["{ ", show (to :: TypeOptions) , " }"]

data TypeOptions = UnderscoreToCase | NoTO deriving (Eq)
instance Show TypeOptions where
  show UnderscoreToCase = "underscoreToCase"
  show NoTO = ""


-- | Haskell + c2hs  declarations e.g.
-- `type Ty = {# type Ty #}`
typeDecl t = spaces ["type", t, "=", typeType [t]]



-- | Haskell - C marshalling helpers

-- dmbToC x = toCInt $ fromEnum (x :: Dmb_) :: Dmb
convertToC ty =
  concat [lowercaseInitial ty, "ToC", " ", freeVarName] ++
  " = " ++
  (inParens [toCTyF, "$", fromHsTyF, " ", freeVarName, " :: ", typeNameHs ty ]) ++ " :: " ++ ty where
   toCTyF = "toCInt"
   fromHsTyF = "fromEnum"
   freeVarName = "x"

-- dmbFromC c = (toEnum $ fromIntegral (c :: Dmb)) :: Dmb_
convertFromC ty =
    concat [lowercaseInitial ty, "FromC", " ", freeVarName] ++
  " = " ++
  (inParens [toCTyF, "$", fromHsTyF, " ", freeVarName, " :: ", ty ]) ++ " :: " ++ typeNameHs ty where
   toCTyF = "toEnum"
   fromHsTyF = "fromIntegral"
   freeVarName = "c"


-- `{# enum .. #}`
enumTypeDecl ty =
  enumType ([ty ++" as "++ typeNameHs ty,
             typeOptions UnderscoreToCase,
             " deriving (Eq, Show)"])


-- | entry for one type (declarations and conversion functions)
enumTypeEntry "" = error "enumTypeEntry : type cannot be empty"
enumTypeEntry ty@(tys:_)
  | isUpper tys = entry [enumTypeDecl ty, typeDecl ty, convertToC ty, convertFromC ty]
  | otherwise = error "enumTypeEntry : type name must be uppercase"

enumEntries tt = cr (map enumTypeEntry tt)

--

preamble m ii = cr [moduleStr m, concat (map importStr ii)]




-- module
moduleStr m@(mi:_)
  | isUpper mi = spaces ["module", moduleRoot ++ m, "where"]
  | otherwise = "moduleStr : module name initial must be uppercase"
moduleStr [] = error "moduleStr : module name cannot be empty"

-- import
importStr i@(ii:_)
  | isUpper ii = "import " ++ i ++ "\n"
  | otherwise = error "importStr : module name initial must be uppercase"
importStr [] = error "importStr : module name cannot be empty"
    



type ModuleName = String


-- | putting it all together

emitModule :: ModuleName -> [ModuleName] -> [String] -> IO ()
emitModule m ii ee = do
  putStrLn $ preamble m ii
  putStrLn $ enumEntries ee

writeModuleToFile :: FilePath -> ModuleName -> [ModuleName] -> [String] -> IO ()
writeModuleToFile fp m ii ee  = writeFile fp (cr ([preamble m ii, enumEntries ee]))



-- |===================

-- | conversion functions

typeNameHs :: String -> String
typeNameHs t = t ++ "_"


-- | helpers
entry :: [String] -> String
entry ll = cr ll ++ "\n"

spaces :: [String] -> String
spaces = intercalate " "

inParens :: [String] -> String
inParens c = "(" ++ spaces c ++ ")"

cr :: [String] -> String
cr = intercalate "\n"

lowercaseInitial :: String -> String 
lowercaseInitial (c:cs) = toLower c : cs
lowercaseInitial [] = []






