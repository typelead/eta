module Codec.JVM.Cond where

-- | Condition

data Cond
  = EQ
  | NE
  deriving (Eq, Ord, Show)
  {--
  | LT
  | LE
  | GT
  | GE
  --}

