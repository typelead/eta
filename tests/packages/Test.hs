#!/usr/bin/env stack
{- stack
     --resolver lts-6.6
     --install-ghc
     runghc
     --package turtle
     --package text
     --package aeson
     --package bytestring
     --package directory
-}

{-#LANGUAGE OverloadedStrings#-}

module Test where

import GHC.IO.Exception (ExitCode(..))
import System.Exit (die)
import Data.Monoid ((<>))
import Data.Text (unpack, Text)
import Control.Applicative (empty)
import Data.Aeson
import Turtle.Prelude (shell)
import qualified Data.ByteString.Lazy as BS
import System.Directory (getHomeDirectory)

data Packages = Packages {
      patched :: [Text],
      vanilla :: [Text]
    } deriving (Show, Eq, Ord)

instance FromJSON Packages where
    parseJSON (Object v) = Packages <$> v.: "patched" <*> v.: "vanilla"
    parseJSON _ = empty

parsePackagesFile :: FilePath -> IO (Maybe Packages)
parsePackagesFile fname = do
  contents <- BS.readFile fname
  let packages = decode contents
  return packages

packagesFilePath :: IO FilePath
packagesFilePath = (<> "/.cabalvm/packages.json") <$> getHomeDirectory

buildPackage :: Text -> IO ()
buildPackage pkg = do
    let buildCmd = "cabalvm install " <> pkg
    exitCode <- shell buildCmd ""
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure x -> die ("error in building " <> unpack pkg)
    return ()

main :: IO ()
main = do
  cabalvmPkgs <- packagesFilePath
  pkg <- parsePackagesFile cabalvmPkgs
  case pkg of
    Nothing -> die "Problem parsing your packages.json file"
    Just pkg' -> 
        let packages = (patched pkg') <> (vanilla pkg')
        in mapM_ buildPackage packages
