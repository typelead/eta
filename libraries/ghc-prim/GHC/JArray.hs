{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
module GHC.JArray (
    jobjectArrayAt#,
    objectArraySet#
  ) where

import GHC.Prim
import GHC.Types
