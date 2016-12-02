{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples,
             FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Core
-- Copyright   :  (c) Rahul Muttineni 2016
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  rahulmutt@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The core facilities to help with Java FFI usage.
--
-----------------------------------------------------------------------------

module Java.Core
  ( java, javaWith, pureJava, io,
    (<.>), (>-),
    JClass(..),
    getClass
  )
where

import GHC.Base

foreign import java unsafe "@new" globalObject :: Object

java :: Java c a -> IO a
java (Java m) = IO $ \s ->
  case m (unsafeCoerce# (unobj globalObject)) of
    (# _, a #) -> (# s, a #)

javaWith :: (Class c) => c -> Java c a -> IO a
javaWith c (Java m) = IO $ \s -> case m (unobj c) of (# _, a #) -> (# s, a #)

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

data {-# CLASS "java.lang.Class" #-} JClass = JClass (Object# JClass)

foreign import java unsafe "@static java.lang.Class.forName" getClass
  :: JString -> JClass
