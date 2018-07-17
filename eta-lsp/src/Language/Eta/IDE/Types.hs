module Language.Eta.IDE.Types
 ( IDEOptions(..), IDEMode(..) )
where

data IDEMode = LSPMode | JSONMode
  deriving Show

data IDEOptions = IDEOptions
  { goptsLogFile    :: Maybe String
  , goptsIdeMode    :: IDEMode
  , goptsProjectDir :: Maybe String
  } deriving (Show)
