{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude, UnliftedFFITypes,
             GHCForeignImportPrim #-}
{-# OPTIONS_HADDOCK hide #-}
module GHC.JArray
  ( ObjectArray#,
    IntArray#,
    objectArrayAt#,
    objectArraySet#
  ) where

import GHC.Prim
import GHC.Classes
import GHC.Types

type IntArray# = Object# (JArray# Int#)
type ObjectArray# a = Object# (JArray# (Object# a))

foreign import prim "@inline aarrayAt"  objectArrayAt#
  :: ObjectArray# a -> Int# -> a
foreign import prim "@inline aarraySet" objectArraySet#
  :: ObjectArray# a -> Int# -> a -> ObjectArray# a
