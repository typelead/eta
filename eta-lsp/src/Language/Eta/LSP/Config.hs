{-# LANGUAGE OverloadedStrings #-}
module Language.Eta.LSP.Config
  ( Config(..)
  , readConfig )
where

import Control.Lens
import Data.Aeson
import qualified Data.Text as T
import Language.Haskell.LSP.Types

data Config = Config { maxNumberOfProblems :: Int }
  deriving (Show)

readConfig :: DidChangeConfigurationNotification -> Either T.Text Config
readConfig notification =
  case fromJSON (notification ^. (params . settings)) of
    Success c -> Right c
    Error err -> Left $ T.pack err

instance FromJSON Config where
  parseJSON = withObject "Config" $ \config -> do
    etaConfig <- config .: "languageServerEta"
    flip (withObject "Config.settings") etaConfig $ \s ->
      Config <$> s .: "maxNumberOfProblems"
