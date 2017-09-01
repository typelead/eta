{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Java.String where

import GHC.Base
import {-# SOURCE #-} Java.Array (JArray)

fromJString :: JString -> String

toJString :: String -> JString

data {-# CLASS "java.lang.String[]" #-} JStringArray = JStringArray (Object# JStringArray)

instance JArray JString JStringArray

