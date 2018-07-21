module Language.Eta.IDE.Options
  (ideOptionsParser)
where

import Data.Monoid

import Options.Applicative

import Language.Eta.IDE.Types

ideOptionsParser :: Parser IDEOptions
ideOptionsParser = IDEOptions
  <$> optional (strOption
       ( long "log-file"
      <> short 'l'
      <> metavar "LOG-FILE"
      <> help "Log file to dump server diagnostics, defaults to stdout."
       ))
  <*> ideModeOption
  <*> optional (strOption
       ( long "project-dir"
      <> short 'r'
      <> metavar "PROJECT-DIR"
      <> help "Project directory, defaults to the current directory"))
  <*> switch 
       ( long "debug"
      <> short 'd'
      <> help "Enable additional logging"
       )

ideModeOption :: Parser IDEMode
ideModeOption =
      flag' LSPMode
           (long "lsp"
         <> help "Enable the Language Server Protocol on stdin/stdout.")
  <|> flag' JSONMode
           (long "json"
         <> help "Enable the ad-hoc JSON protocol on stdin/stdout.")
