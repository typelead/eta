{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleContexts, MagicHash #-}
module Java.Primitive where

import GHC.Base
import GHC.Real

data JChar = JC# JChar#

instance Integral JChar