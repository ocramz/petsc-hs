import Data.Maybe (fromJust)

import Distribution.Simple
-- import Distribution.Simple.UserHooks (buildHook)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
-- import Distribution.Simple.BuildPaths
import Distribution.Verbosity (Verbosity, normal)
import Distribution.PackageDescription (PackageDescription, BuildInfo, library, libBuildInfo, includeDirs, extraLibDirs, extraLibs)

import Control.Monad

-- import Data.String
import Data.Char (toLower)

-- import System.Cmd
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath -- ((</>), splitPath)
import System.Exit ( ExitCode(..) )
import System.Process



-- | currently supported

petsc, slepc :: Dep 
petsc = mkDep Petsc 3 7 2
slepc = mkDep Slepc 3 7 1



-- | BUILD SEQUENCE :

-- | 1) dependencies :

-- | check if either already installed, if not see 1.1) and/or 1.2), otherwise skip to 2)

-- | 1.1) PETSc
-- -- configure
-- -- make

-- | 1.2) SLEPc
-- -- configure
-- -- make


-- | 2) inline-c pass










main = defaultMainWithHooks simpleUserHooks
                                            {
                                              buildHook = myBuildHook
                                            , cleanHook = myCleanHook
                                            }

myBuildHook pkgDescr localBuildInfo userHooks buildFlags = return ()

myCleanHook pd _ uh cf = return ()







-- | dependencies model

data VerN =
  VerN { _verMajor :: Int,
         _verMinor :: Int,
         _verPatch :: Int} deriving Eq
instance Show VerN where
  show = showVerN 

showVerN :: VerN -> String
showVerN v = show v1 ++ ", " ++ show v2 ++ ", " ++ show v3
  where (v1,v2,v3) = (_verMajor v, _verMinor v, _verPatch v)

data DepName = Petsc | Slepc deriving (Eq)
instance Show DepName where
  show Petsc = "petsc"
  show Slepc = "slepc"

data Dep = Dep DepName VerN deriving Eq
mkDep d nmm nm np  = Dep d (VerN nmm nm np)

instance Show Dep where
  show (Dep s n) = show s ++ "-" ++ show n














-- | utils

isDirectory :: FilePath -> Bool
isDirectory = hasTrailingPathSeparator

addIncludes, addExtraLibs, addLibs :: 
  PackageDescription -> [FilePath] -> PackageDescription
addIncludes pd = addDirWrapper pd appendIncludeDirs
addExtraLibs pd = addDirWrapper pd appendExtraLibs
addLibs pd = addDirWrapper pd appendLibs
  



addDirWrapper :: PackageDescription ->
                 (BuildInfo -> t -> BuildInfo) ->
                 t ->
                 PackageDescription
addDirWrapper pd m x = pd { library = Just lib' } where
  lib = (fromJust . library) pd
  bi = libBuildInfo lib
  lib' = lib { libBuildInfo = m bi x}


appendIncludeDirs, appendExtraLibs, appendLibs :: 
   BuildInfo -> 
   [FilePath] -> 
   BuildInfo
appendIncludeDirs bi x = bi { includeDirs = ids ++ x } where
  ids = includeDirs bi
appendExtraLibs bi x = bi { extraLibDirs = lds ++ x } where
  lds = extraLibDirs bi
appendLibs bi x = bi { extraLibs = lbs ++ x } where
  lbs = extraLibs bi








-- | testing testing

t0 = liftM splitPath getCurrentDirectory




-- | from the deprecated testing syntax : 

-- testing _ _ _ _ = do
--      err <- system "make"
--      -- system "make -s -C tests clean"
--      when (err /= ExitSuccess) $ ioError $ userError "failed"




