module Numerical.PETSc.Internal.Meta.GenerateC2Hs where

import Data.Char
import Data.List


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


-- Haskell declarations
typeDecl t = spaces ["type", t, "=", typeType [t]]


-- | conversion functions

typeNameHs :: String -> String
typeNameHs t = t ++ "_"

-- dmbToC x = toCInt $ fromEnum (x :: DMBoundaryType_) :: DMBoundaryType
convertToC ty =
  concat [ty, "ToC", " ", freeVarName] ++
  " = " ++
  (inParens [toCTyF, "$", fromHsTyF, " ", freeVarName, " :: ", typeNameHs ty ]) ++ " :: " ++ ty where
   toCTyF = "toCInt"
   fromHsTyF = "fromEnum"
   freeVarName = "x"

-- dmbFromC c = (toEnum $ fromIntegral (c :: DMBoundaryType)) :: DMBoundaryType_

convertFromC ty =
    concat [ty, "FromC", " ", freeVarName] ++
  " = " ++
  (inParens [toCTyF, "$", fromHsTyF, " ", freeVarName, " :: ", ty ]) ++ " :: " ++ typeNameHs ty where
   toCTyF = "toEnum"
   fromHsTyF = "fromIntegral"
   freeVarName = "c"


--
enumTypeDecl ty =
  enumType ([ty ++" as "++ typeNameHs ty,
             typeOptions UnderscoreToCase,
             " deriving (Eq, Show)"])


-- | entry for one type (declarations and conversion functions)
enumTypeEntry ty = cr [enumTypeDecl ty, typeDecl ty, convertToC ty, convertFromC ty]



-- | helpers
spaces :: [String] -> String
spaces = intercalate " "

inParens :: [String] -> String
inParens c = "(" ++ spaces c ++ ")"

cr :: [String] -> String
cr = intercalate "\n"

lowerCaseInitial :: String -> String 
lowerCaseInitial (c:cs) = toLower c : cs
lowerCaseInitial [] = []


-- 
preamble m ii = cr [moduleStr m, concat (map importStr ii)]
-- module
moduleStr m = spaces ["module", m, "where"]

-- import
importStr i = "import " ++ i


-- examples
ex1 t = enumType ([typeAsUnderscore t, typeOptions UnderscoreToCase, " deriving Eq"])
ex2 = typeDecl 
