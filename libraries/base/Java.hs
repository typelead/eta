{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
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
    ObjectArray(..),
    StringArray(..),

    -- * Java related
    java, io, (<.>), (>-)
  )
where

import GHC.Base
import GHC.Pack
import GHC.Types
import GHC.JArray
import ETA.JString
import System.IO.Unsafe

data {-# CLASS "java.lang.Class" #-} JClass = JClass (Object# JClass)

data JByteArray = JBA# JByteArray#
data JShortArray = JSA# JShortArray#
data JCharArray = JCA# JCharArray#
data IntArray = IA# IntArray#
data JLongArray = JLA# JLongArray#
data FloatArray = FA# FloatArray#
data DoubleArray = DA# DoubleArray#
data StringArray = SA# (ObjectArray# JString)
data ObjectArray a = OA# (ObjectArray# a)

foreign import java unsafe "@static java.lang.Class.forName" getClass' :: JString -> JClass

getClass :: String -> JClass
getClass = getClass' . mkJString

java :: (Class c) => c -> Java c a -> IO a
java c (Java m) = IO $ \s -> case m (unobj c) of (# _, a #) -> (# s, a #)

(<.>) :: (Class c) => c -> Java c a -> Java b a
(<.>) cls (Java m) = Java $ \o -> case m (unobj cls) of (# _, a #) -> (# o, a #)

io :: IO a -> Java c a
io (IO m) = Java $ \o -> case m realWorld# of (# _, a #) -> (# o, a #)

(>-) :: (Class b) => Java a b -> Java b c -> Java a c
(>-) (Java m) (Java n) =
  Java $ \a ->
           case m a of
             (# a', b #) ->
               case n (unobj b) of
                 (# _, c #) -> (# a', c #)
