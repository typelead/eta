{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude, UnliftedFFITypes,
             GHCForeignImportPrim #-}
{-# OPTIONS_HADDOCK hide #-}
module GHC.JArray
  ( ObjectArray#,
    JByteArray#,
    IntArray#,
    objectArrayAt#,
    objectArraySet#
  ) where

import GHC.Prim
import GHC.Types

type IntArray# = Object# (JArray# Int#)
type JByteArray# = Object# (JArray# JByte#)
type ObjectArray# a = Object# (JArray# (Object# a))
