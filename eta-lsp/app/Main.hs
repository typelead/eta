module Main where

import Data.Monoid

import Options.Applicative

import Language.Eta.IDE.Options
import Language.Eta.IDE.Run

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (ideOptionsParser <**> helper)
      ( fullDesc
     <> progDesc "Provide IDE services for the Eta programming language."
     <> header "eta-ide - An LSP and JSON server for Eta" )
