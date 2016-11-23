{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
module GHC.JArray (
    objectArrayAt#,
    objectArraySet#
  ) where

import GHC.Prim
import GHC.Types
