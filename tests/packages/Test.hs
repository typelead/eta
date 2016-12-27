{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.IO.Exception (ExitCode(..))
import System.Exit (die)
import Data.Monoid ((<>))
import Control.Applicative (empty)
import Data.Aeson
import Turtle.Prelude (procStrictWithErr, shell)
import qualified Data.ByteString.Lazy as BS
import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>))
import Data.Text (unpack, Text)
import qualified Data.Text.IO as T

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
packagesFilePath = (</> "patches" </> "packages.json") <$> getAppUserDataDirectory "epm"

buildPackage :: Text -> IO ()
buildPackage pkg = do
    let args = ["install", pkg]
    (exitCode, out, err) <- procStrictWithErr "epm" args empty
    case exitCode of
        ExitSuccess -> T.putStr out
        ExitFailure x -> T.putStr err >> die ("error in building " <> unpack pkg)
    return ()

main :: IO ()
main = do
  let vmUpdateCmd = "epm update"
  shell vmUpdateCmd ""
  epmPkgs <- packagesFilePath
  pkg <- parsePackagesFile epmPkgs
  case pkg of
    Nothing -> die "Problem parsing your packages.json file"
    Just pkg' ->
        let packages = (patched pkg') <> (vanilla pkg')
        in mapM_ buildPackage packages
