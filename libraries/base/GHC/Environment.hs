{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module GHC.Environment (getFullArgs) where

import GHC.Base
import GHC.Pack
import Java

getFullArgs :: IO [String]
getFullArgs = do
  jargs <- getFullArgs'
  return (map unpackCString (fromJava jargs))

foreign import java unsafe "@static @field eta.runtime.RuntimeOptions.fullProgArgs"
  getFullArgs' :: IO JStringArray

