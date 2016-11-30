{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
module GHC.JArray (
    jobjectArrayAt#,
    jobjectArraySet#
  ) where

import GHC.Prim
import GHC.Types
