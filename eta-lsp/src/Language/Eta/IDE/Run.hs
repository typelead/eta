{-# LANGUAGE RecordWildCards #-}
module Language.Eta.IDE.Run where

import System.Directory
import System.Log.Logger

import qualified Language.Haskell.LSP.Core as Core

import Language.Eta.IDE.Types
import Language.Eta.LSP.Run

run :: IDEOptions -> IO ()
run IDEOptions {..} = do

  let logLevel
        | ideOptsDebug = DEBUG
        | otherwise    = INFO

  Core.setupLogger ideOptsLogFile ["eta-ide"] logLevel

  realWorkingDirectory <- getCurrentDirectory

  maybe (pure ()) setCurrentDirectory ideOptsProjectDir

  case ideOptsIdeMode of
    LSPMode  -> runLSP realWorkingDirectory
    JSONMode -> putStrLn "JSON mode not supported yet."