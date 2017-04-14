{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleContexts, MagicHash #-}
module Java.Collections where

import Java.Core

data {-# CLASS "java.util.Set" #-} Set a = Set (Object# (Set a))


instance Extends a Object => JavaConverter [a] (Set a) where
