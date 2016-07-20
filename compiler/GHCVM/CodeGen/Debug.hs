module GHCVM.CodeGen.Debug
  (module Outputable,
   str)
where

import Outputable hiding ((<>))
import FastString

str :: String -> SDoc
str = ptext . sLit
