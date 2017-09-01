{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
module Java.Utils where

import GHC.Base

eqObject# :: Object# a -> Object# b -> Bool

toString# :: Object# a -> String

