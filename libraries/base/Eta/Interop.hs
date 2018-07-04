{-# LANGUAGE PolyKinds, DataKinds, MagicHash, ScopedTypeVariables, MultiParamTypeClasses #-}
module Eta.Interop (SObject(..), Overloadable(..)) where

import GHC.Base
import GHC.TypeLits

data SObject (s :: Symbol) (xs :: [*]) = SObject (Object# Object)

class Overloadable (a :: k) (s :: Symbol) r where
  overloaded :: Proxy# a -> Proxy# s -> r
