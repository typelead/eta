{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples,
             FunctionalDependencies #-}
module Java
  ( JClass(..),
    getClass,
    JByteArray(..),
    JShortArray(..),
    JCharArray(..),
    JIntArray(..),
    JLongArray(..),
    JFloatArray(..),
    JDoubleArray(..),
    JStringArray(..),

    -- * Java related
    java, pureJava, io,
    (<.>), (>-)
  )
where

import GHC.Base
import GHC.Pack
import GHC.Types
import GHC.JArray
import ETA.JString
import System.IO.Unsafe

data {-# CLASS "java.lang.Class" #-} JClass = JClass (Object# JClass)

data {-# CLASS "boolean[]" #-} JBooleanArray = JBooleanArray (Object# JBooleanArray)
data {-# CLASS "byte[]"    #-} JByteArray    = JByteArray    (Object# JByteArray)
data {-# CLASS "char[]"    #-} JCharArray    = JCharArray    (Object# JCharArray)
data {-# CLASS "short[]"   #-} JShortArray   = JShortArray   (Object# JShortArray)
data {-# CLASS "int[]"     #-} JIntArray     = JIntArray     (Object# JIntArray)
data {-# CLASS "long[]"    #-} JLongArray    = JLongArray    (Object# JLongArray)
data {-# CLASS "float[]"   #-} JFloatArray   = JFloatArray   (Object# JFloatArray)
data {-# CLASS "double[]"  #-} JDoubleArray  = JDoubleArray  (Object# JDoubleArray)

data {-# CLASS "java.lang.String[]" #-} JStringArray = JStringArray (Object# JStringArray)

instance Class JBooleanArray where
  unobj (JBooleanArray o) = o
  obj = JBooleanArray

instance Class JByteArray where
  unobj (JByteArray o) = o
  obj = JByteArray

instance Class JCharArray where
  unobj (JCharArray o) = o
  obj = JCharArray

instance Class JShortArray where
  unobj (JShortArray o) = o
  obj = JShortArray

instance Class JIntArray where
  unobj (JIntArray o) = o
  obj = JIntArray

instance Class JLongArray where
  unobj (JLongArray o) = o
  obj = JLongArray

instance Class JFloatArray where
  unobj (JFloatArray o) = o
  obj = JFloatArray

instance Class JDoubleArray where
  unobj (JDoubleArray o) = o
  obj = JDoubleArray

instance Class JStringArray where
  unobj (JStringArray o) = o
  obj = JStringArray

class (Class c) => JArray e c | c -> e where
  jarrayNew :: Int -> Java a c
  jarrayAt  :: Int -> Java c e
  jarraySet :: Int -> e -> Java c ()

foreign import java unsafe "@static java.lang.Class.forName" getClass'
  :: JString -> JClass

getClass :: String -> JClass
getClass = getClass' . mkJString

java :: (Class c) => c -> Java c a -> IO a
java c (Java m) = IO $ \s -> case m (unobj c) of (# _, a #) -> (# s, a #)

pureJava :: (Class c) => c -> Java c a -> a
pureJava c (Java m) = case m (unobj c) of (# _, a #) -> a

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
