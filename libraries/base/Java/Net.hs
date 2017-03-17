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
-- Bindings for Java Net utilities
--
-----------------------------------------------------------------------------

module Java.Net where

import GHC.Base
import GHC.Int
import Java.Array
import Java.IO
import Java.Primitive

-- Start java.net.URL

data {-# CLASS "java.net.URL" #-} URL = URL (Object# URL)
  deriving Class

foreign import java unsafe getAuthority :: Java URL String

foreign import java unsafe getContent :: Java URL Object

-- foreign import java unsafe "getContent" getContent2 :: JClassArray -> Java URL Object

foreign import java unsafe getDefaultPort :: Java URL Int

foreign import java unsafe getFile :: Java URL String

foreign import java unsafe getHost :: Java URL String

foreign import java unsafe getPath :: Java URL String

foreign import java unsafe getPort :: Java URL Int

foreign import java unsafe getProtocol :: Java URL String

foreign import java unsafe getQuery :: Java URL String

foreign import java unsafe getRef :: Java URL String

foreign import java unsafe getUserInfo :: Java URL String

-- foreign import java unsafe getConnection :: Java URL URLConnection

-- foreign import java unsafe "getConnection" getConnection2 :: Proxy -> Java URL URLConnection

foreign import java unsafe openStream :: Java URL InputStream

foreign import java unsafe sameFile :: URL -> Java URL Bool

foreign import java unsafe set :: String -> String -> Int -> String -> String -> Java URL ()

foreign import java unsafe "set"
  set2 :: String -> String -> Int -> String -> String -> String -> String -> String -> Java URL ()

foreign import java unsafe toExternalForm :: Java URL String

-- foreign import java unsafe toURI :: Java URL URI
-- TODO

-- End java.net.URL
