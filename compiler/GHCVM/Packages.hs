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

packageHsLibs :: DynFlags -> PackageConfig -> [String]
packageHsLibs dflags p = hsLibraries p

packageHsLibJars :: DynFlags -> PackageConfig -> [String]
packageHsLibJars dflags p = map (<.> "jar") $ packageHsLibs dflags p

getPackageLibJars :: DynFlags -> [PackageKey] -> IO [String]
getPackageLibJars dflags pkgs =
  -- TODO: Include extra libraries too
  concatMap (packageHsLibJars dflags) `fmap` getPreloadPackagesAnd dflags pkgs
