{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
module GHC.JArray
  ( JByteArray#,
    JShortArray#,
    JCharArray#,
    IntArray#,
    JLongArray#,
    FloatArray#,
    DoubleArray#,
    ObjectArray#,
    objectArrayAt#,
    objectArraySet#
  ) where

import GHC.Prim
import GHC.Types

type JByteArray# = Object# (JArray# JByte#)
type JShortArray# = Object# (JArray# JShort#)
type JCharArray# = Object# (JArray# JChar#)
type IntArray# = Object# (JArray# Int#)
type JLongArray# = Object# (JArray# Int64#)
type FloatArray# = Object# (JArray# Float#)
type DoubleArray# = Object# (JArray# Double#)
type ObjectArray# a = Object# (JArray# (Object# a))
