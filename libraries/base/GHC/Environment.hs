{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module GHC.Environment (getFullArgs) where

import GHC.Base
import GHC.Pack
import Java
import Java.StringBase

getFullArgs :: IO [String]
getFullArgs = do
  jargs <- getFullArgs'
  return (map fromJString (fromJava jargs))

foreign import java unsafe "@static eta.runtime.Runtime.getLocalProgramArguments"
  getFullArgs' :: IO JStringArray

