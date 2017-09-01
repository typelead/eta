{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.StringUtils
-- Copyright   :  (c) Jyothsna Srinivas 2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  jyothsnasrinivas17@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Bindings for Java String utilities
--
-----------------------------------------------------------------------------

module Java.StringUtils
  (module Java.StringUtils, CharSequence)
where

import GHC.Base
import GHC.Int
import Java.Array
import Java.Core
import Java.NIO
import Java.Text
import Java.Primitive
import Java.String

-- Start java.lang.String

foreign import java unsafe charAt :: JString -> Int -> JChar

foreign import java unsafe codePointAt :: JString -> Int -> Int

foreign import java unsafe codePointBefore :: JString -> Int -> Int

foreign import java unsafe codePointCount :: JString -> Int -> Int -> Int

foreign import java unsafe compareTo :: JString -> JString -> Int

foreign import java unsafe compareToIgnoreCase :: JString -> JString -> Int

foreign import java unsafe concat :: JString -> JString -> JString

foreign import java unsafe contains :: JString -> CharSequence -> Bool

foreign import java unsafe contentEquals :: JString -> CharSequence -> Bool

foreign import java unsafe "contentEquals" contentEqualsString :: JString -> StringBuffer -> Bool

foreign import java unsafe endsWith :: JString -> JString -> Bool

foreign import java unsafe equals :: JString -> Object -> Bool

foreign import java unsafe equalsIgnoreCase :: JString -> JString -> Bool

foreign import java unsafe getBytes :: JString -> JByteArray

foreign import java unsafe "getBytes" getBytesCharset :: JString -> Charset -> JByteArray

foreign import java unsafe getChars :: JString -> Int -> Int -> JCharArray -> Int -> ()

foreign import java unsafe indexOf :: JString -> Int -> Int

foreign import java unsafe "indexOf" indexOf2 :: JString -> Int -> Int -> Int

foreign import java unsafe "indexOf" indexOf3 :: JString -> JString -> Int

foreign import java unsafe "indexOf" indexOf4 :: JString -> JString -> Int -> Int

foreign import java unsafe intern :: JString -> JString

foreign import java unsafe isEmpty :: JString -> Bool

foreign import java unsafe lastIndexOf :: JString -> Int -> Int

foreign import java unsafe "lastIndexOf" lastIndexOf2 :: JString -> Int -> Int -> Int

foreign import java unsafe "lastIndexOf" lastIndexOf3 :: JString -> JString -> Int

foreign import java unsafe "lastIndexOf" lastIndexOf4 :: JString -> JString -> Int -> Int

foreign import java unsafe length :: JString -> Int

foreign import java unsafe matches :: JString -> JString -> Bool

foreign import java unsafe offsetByCodePoints :: JString -> Int -> Int -> Int

foreign import java unsafe regionMatches :: JString -> Bool -> Int -> JString -> Int -> Int -> Bool

foreign import java unsafe "regionMatches" regionMatches2 :: JString -> Int -> JString -> Int -> Bool

foreign import java unsafe replace :: JString -> JChar -> JChar -> JString

foreign import java unsafe "replace"
  replaceCharSequence :: JString -> CharSequence -> CharSequence -> JString

foreign import java unsafe replaceAll :: JString -> JString -> JString -> JString

foreign import java unsafe replaceFirst :: JString -> JString -> JString -> JString

foreign import java unsafe split :: JString -> JString -> JStringArray

foreign import java unsafe "split" split2 :: JString -> JString -> Int -> JStringArray

foreign import java unsafe startsWith :: JString -> JString -> Bool

foreign import java unsafe "startsWith" startsWith2 :: JString -> JString -> Int -> Bool

foreign import java unsafe subSequence :: JString -> Int -> Int -> CharSequence

foreign import java unsafe substring :: JString -> Int -> JString

foreign import java unsafe "substring" substring2 :: JString -> Int -> Int -> JString

foreign import java unsafe toCharArray :: JString -> JCharArray

foreign import java unsafe toLowerCase :: JString -> JCharArray

foreign import java unsafe "toLowerCase" toLowerCaseLocale :: JString -> Locale -> JCharArray

foreign import java unsafe toString :: JString -> JCharArray

foreign import java unsafe toUpperCase :: JString -> JCharArray

foreign import java unsafe "toUpperCase" toUpperCaseLocale :: JString -> Locale -> JCharArray

foreign import java unsafe trim :: JString -> JString

-- Start java.lang.StringBuffer

data {-# CLASS "java.lang.StringBuffer" #-} StringBuffer = StringBuffer (Object# StringBuffer)
  deriving Class

-- End java.lang.StringBuffer
