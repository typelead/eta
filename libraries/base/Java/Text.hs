{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.IO
-- Copyright   :  (c) Jyothsna Srinivas 2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  jyothsnasrinivas17@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Bindings for Java Text utilities
--
-----------------------------------------------------------------------------

module Java.Text where

import GHC.Base
import GHC.Int
import Java.Array
import Java.Collections
import Java.Primitive
import Java.Utils
import Java.Wrappers

-- Start java.util.Calendar

data {-# CLASS "java.util.Locale" #-} Locale = Locale (Object# Locale)
  deriving Class

foreign import java unsafe clone :: Java Locale Object

foreign import java unsafe getCountry :: Java Locale String

foreign import java unsafe getDisplayCountry :: Java Locale String

foreign import java unsafe "getDisplayCountry" getDisplayCountry2 :: Locale -> Java Locale String

foreign import java unsafe getDisplayName :: Java Locale String

foreign import java unsafe "getDisplayName" getDisplayName2 :: Locale -> Java Locale String

foreign import java unsafe getDisplayScript :: Java Locale String

foreign import java unsafe "getDisplayScript" getDisplayScript2 :: Locale -> Java Locale String

foreign import java unsafe getDisplayVariant :: Java Locale String

foreign import java unsafe "getDisplayVariant" getDisplayVariant2 :: Locale -> Java Locale String

foreign import java unsafe getExtension :: JChar -> Java Locale String

foreign import java unsafe getExtensionKeys :: Java Locale (Set JCharacter)

foreign import java unsafe getISO3Country :: Java Locale String

foreign import java unsafe getISO3Language :: Java Locale String

foreign import java unsafe getISOCountries :: Java Locale String

foreign import java unsafe getISOLanguages :: Java Locale String

foreign import java unsafe getLanguage :: Java Locale String

foreign import java unsafe getScript :: Java Locale String

foreign import java unsafe getUnicodeLocaleAttributes :: Java Locale (Set JString)

foreign import java unsafe getUnicodeLocaleKeys :: Java Locale (Set JString)

foreign import java unsafe getUnicodeLocaleType :: String -> Java Locale String

foreign import java unsafe getVariant :: Java Locale String

foreign import java unsafe toLanguageTag :: Java Locale String

-- End java.util.Locale
