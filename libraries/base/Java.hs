{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
module Java
  ( JClass(..),
    getClass,
    JByteArray(..),
    JShortArray(..),
    JCharArray(..),
    IntArray(..),
    JLongArray(..),
    FloatArray(..),
    DoubleArray(..),
    ObjectArray(..)
  )
where

import GHC.Base
import GHC.Pack
import GHC.Types
import GHC.JArray
import GHCVM.JString

data {-# CLASS "java.lang.Class" #-} JClass = JClass (Object# JClass)

data JByteArray = JBA# JByteArray#
data JShortArray = JSA# JShortArray#
data JCharArray = JCA# JCharArray#
data IntArray = IA# IntArray#
data JLongArray = JLA# JLongArray#
data FloatArray = FA# FloatArray#
data DoubleArray = DA# DoubleArray#
data ObjectArray a = OA# (ObjectArray# a)

foreign import java unsafe "java.lang.Class.forName" getClass_ :: JString -> Java a JClass

getClass :: String -> Java a JClass
getClass = getClass_ . mkJString
