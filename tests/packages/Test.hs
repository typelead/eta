#!/usr/bin/env stack
{- stack
     --resolver lts-6.6
     --install-ghc
     runghc
     --package turtle
-}

{-#LANGUAGE OverloadedStrings#-}

module Test where

import GHC.IO.Exception (ExitCode(..))
import System.Exit (die)
import Data.Monoid ((<>))
import Data.Text (pack)
import Turtle.Prelude (shell)

-- TODO: Automatize this to fetch from ghcvm-hackage
patchedPackages
    :: [String]
patchedPackages =
    [ "array"
    , "bytestring"
    , "base-orphans"
    , "binary"
    , "containers"
    , "filepath"
    , "primitive"
    , "random"
    , "time"]

-- TODO: Have a special file in ghcvm-package with this list to automatize it
-- Packages that are working without any patching
unPatchedPackages
    :: [String]
unPatchedPackages =
    [ "Adaptive"
    , "agum"
    , "array-utils"
    , "base-prelude"
    , "basic-lens"]

buildPackage :: String -> IO ()
buildPackage pkg = do
    let buildCmd = "cabalvm install " <> (pack pkg)
    exitCode <- shell buildCmd ""
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure x -> die ("error in building " <> pkg)
    return ()

main :: IO ()
main = do
    let packages = patchedPackages <> unPatchedPackages
    shell "cabalvm update" ""
    mapM_ buildPackage packages
