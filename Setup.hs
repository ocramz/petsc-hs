module Main where

import Data.Maybe (fromJust)

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
-- import Distribution.Simple.BuildPaths
import Distribution.PackageDescription (PackageDescription, BuildInfo, library, libBuildInfo, includeDirs, extraLibDirs, extraLibs)

import Control.Monad

import Data.String
import qualified Data.ByteString.Lazy.Char8 as BS

import System.Cmd
import System.Directory
import System.FilePath ((</>), splitPath)
import System.Exit ( ExitCode(..) )














addIncludes, addExtraLibs, addLibs :: 
  PackageDescription -> [FilePath] -> PackageDescription
addIncludes pd = addDirWrapper pd appendIncludeDirs
addExtraLibs pd = addDirWrapper pd appendExtraLibs
addLibs pd = addDirWrapper pd appendLibs
  






main = defaultMainWithHooks simpleUserHooks
                                            {
                                              buildHook = myBuildHook
                                            , cleanHook = myCleanHook
                                            }

myBuildHook pkgDescr localBuildInfo userHooks buildFlags= return ()

myCleanHook pd _ uh cf = return ()







-- | utils




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




