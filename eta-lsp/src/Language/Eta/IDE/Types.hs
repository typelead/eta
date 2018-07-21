module Language.Eta.IDE.Types
 ( IDEOptions(..), IDEMode(..) )
where

data IDEMode = LSPMode | JSONMode
  deriving Show

data IDEOptions = IDEOptions
  { ideOptsLogFile    :: Maybe String
  , ideOptsIdeMode    :: IDEMode
  , ideOptsProjectDir :: Maybe String
  , ideOptsDebug      :: Bool
  } deriving (Show)
