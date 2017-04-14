{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FunctionalDependencies,
 DefaultSignatures, UnboxedTuples, MagicHash #-}

module Java.Array where

import GHC.Base
import Java.Core

class (Class c) => JArray e c | c -> e, e -> c where
  anew :: Int -> Java a c
  default anew :: (Class e) => Int -> Java a c
  {-# INLINE anew #-}
  anew (I# n#) = Java $ \o ->
    case jobjectArrayNew# n# realWorld# of
      (# _, o' #) -> case obj o' of
        c -> (# o, c #)

  aget :: Int -> Java c e
  default aget :: (Class e) => Int -> Java c e
  {-# INLINE aget #-}
  aget (I# n#) = Java $ \o ->
    case jobjectArrayAt# o n# realWorld# of
      (# _, o' #) -> case obj o' of
        o'' -> (# o, o'' #)

  aset :: Int -> e -> Java c ()
  default aset :: (Class e) => Int -> e -> Java c ()
  {-# INLINE aset #-}
  aset (I# n#) e = Java $ \o ->
    case jobjectArraySet# o n# (unobj e) realWorld# of
      _ -> (# o, () #)

data {-# CLASS "java.lang.String[]" #-} JStringArray = JStringArray (Object# JStringArray)

arrayFromList :: JArray e c => [e] -> Java a c

instance JArray JString JStringArray
