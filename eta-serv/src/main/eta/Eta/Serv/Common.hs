{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Eta.Serv.Common
 (toJStringArray
 ,exceptionToString
 ,handleExceptionAsString)
where

import Java
import Java.Exception

import Control.Exception

toJStringArray :: [String] -> JStringArray
toJStringArray strs = toJava jstrings
  where !jstrings = map toJava strs :: [JString]

handleExceptionAsString :: SomeException -> String
handleExceptionAsString ex = case fromException ex of
  Just (e :: JException) -> fromJava (exceptionToString e)
  Nothing -> show ex

foreign import java unsafe "@static eta.serv.Utils.exceptionToString"
  exceptionToString :: JException -> JString
