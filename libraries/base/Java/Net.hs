{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Net
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
import Java.Collections
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

foreign import java unsafe toURI :: Java URL URI

foreign import java unsafe "toURI" toURIFile :: Java File URI

foreign import java unsafe "toURL" toURLFile :: Java File URL
-- TODO

-- End java.net.URL

-- Start java.net.URI

data {-# CLASS "java.net.URI" #-} URI = URI (Object# URI)
  deriving Class

-- End java.net.URI

-- Start java.net.InetAddress

data {-# CLASS "java.net.InetAddress" #-} InetAddress = InetAddress (Object# InetAddress)
  deriving Class

foreign import java unsafe getAddress :: (b <: InetAddress) => Java b JByteArray

foreign import java unsafe getCanonicalHostName :: (b <: InetAddress) => Java b String

foreign import java unsafe getHostAddress :: (b <: InetAddress) => Java b String

foreign import java unsafe getHostName :: (b <: InetAddress) => Java b String

foreign import java unsafe isAnyLocalAddress :: (b <: InetAddress) => Java b Bool

foreign import java unsafe isLinkLocalAddress :: (b <: InetAddress) => Java b Bool

foreign import java unsafe isLoopbackAddress :: (b <: InetAddress) => Java b Bool

foreign import java unsafe isMCGlobal :: (b <: InetAddress) => Java b Bool

foreign import java unsafe isMCLinkLocal :: (b <: InetAddress) => Java b Bool

foreign import java unsafe isMCNodeLocal :: (b <: InetAddress) => Java b Bool

foreign import java unsafe isMCOrgLocal :: (b <: InetAddress) => Java b Bool

foreign import java unsafe isMCSiteLocal :: (b <: InetAddress) => Java b Bool

foreign import java unsafe isMulticastAddress :: (b <: InetAddress) => Java b Bool

foreign import java unsafe isReachable :: (b <: InetAddress) => Int -> Java b Bool

foreign import java unsafe "isReachable"
  isReachableNI :: (b <: InetAddress) => NetworkInterface -> Int -> Int -> Java b Bool

foreign import java unsafe isSiteLocalAddress :: (b <: InetAddress) => Java b Bool

-- End java.net.InetAddress

-- Start java.net.NetworkInterface

data {-# CLASS "java.net.NetworkInterface" #-} NetworkInterface = NetworkInterface (Object# NetworkInterface)
  deriving Class

foreign import java unsafe getDisplayName :: Java NetworkInterface String

foreign import java unsafe getHardwareAddress :: Java NetworkInterface JByteArray

foreign import java unsafe getIndex :: Java NetworkInterface Int

foreign import java unsafe getInetAddresses :: Java NetworkInterface (Enumeration InetAddress)

foreign import java unsafe getInterfaceAddresses :: Java NetworkInterface (List InterfaceAddress)

foreign import java unsafe getMTU :: Java NetworkInterface Int

foreign import java unsafe getName :: Java NetworkInterface String

foreign import java unsafe getParent :: Java NetworkInterface NetworkInterface

foreign import java unsafe getSubInterfaces :: Java NetworkInterface (Enumeration NetworkInterface)

foreign import java unsafe isLoopback :: Java NetworkInterface Bool

foreign import java unsafe isPointToPoint :: Java NetworkInterface Bool

foreign import java unsafe isUp :: Java NetworkInterface Bool

foreign import java unsafe isVirtual :: Java NetworkInterface Bool

foreign import java unsafe supportsMulticast :: Java NetworkInterface Bool

-- End java.net.NetworkInterface

-- Start java.net.InterfaceAddress

data {-# CLASS "java.net.InterfaceAddress" #-} InterfaceAddress = InterfaceAddress (Object# InterfaceAddress)
  deriving Class

foreign import java unsafe "getAddress" getAddressIA :: Java InterfaceAddress Bool

foreign import java unsafe getBroadcast :: Java InterfaceAddress Bool

foreign import java unsafe getNetworkPrefixLength :: Java InterfaceAddress Short

-- End java.net.InterfaceAddress
