#!/usr/bin/env stack
{- stack
     --resolver lts-6.6
     --install-ghc
     runghc
     --package turtle
     --package aeson
-}

{-#LANGUAGE OverloadedStrings#-}

module Test where

import GHC.IO.Exception (ExitCode(..))
import System.Exit (die)
import Data.Monoid ((<>))
import Data.Text (pack)
import Data.Aeson
import Turtle.Prelude (shell)
import qualified Data.Text.IO as TIO

data Packages = Packages {
      patched :: [Text],
      vanilla :: [Text]
    } deriving (Show, Eq, Ord)

instance FromJSON Packages where
    parseJSON (Object v) = Person <$> v.: "patched" <*> v.: "vanilla"
    parseJSON _ = empty

parsePackagesFile :: FilePath -> IO Packages
parsePackagesFile fname = do
  contents <- TIO.readFile fname
  packages <- decode contents
  return packages

packagesFilePath :: FilePath
packagesFilePath = "~/.cabalvm/patches"

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
  pkgs <- parsePackagesFile packagesFilePath
  print pkgs



-- main :: IO ()
-- main = do
--     let packages = patchedPackages <> unPatchedPackages
--     shell "cabalvm update" ""
--     mapM_ buildPackage packages
