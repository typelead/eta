module GHCVM.Packages
  (module Packages,
   packageHsLibs,
   packageHsLibJars,
   getPackageLibJars)
where

import Module
import DynFlags
import Packages hiding (packageHsLibs)
import System.FilePath
import System.Directory
import Data.List (nub)
import Control.Monad (forM, filterM)

packageHsLibs :: DynFlags -> PackageConfig -> [String]
packageHsLibs dflags = hsLibraries

packageHsLibJars :: DynFlags -> PackageConfig -> [String]
packageHsLibJars dflags p = map (<.> "jar") $ packageHsLibs dflags p

getPackageLibJars :: DynFlags -> [PackageKey] -> IO [String]
getPackageLibJars dflags pkgs = do
  packages <- getPreloadPackagesAnd dflags pkgs
  -- TODO: Include extra libraries too
  jarPaths <- forM packages $ \pc -> do
    let hsLibs = packageHsLibJars dflags pc
        libDirs = nub $ libraryDirs pc
    filterM doesFileExist [ dir </> jar | jar <- hsLibs, dir <- libDirs ]
  return $ concat jarPaths
