{-# LANGUAGE PolyKinds, DataKinds, MagicHash, ScopedTypeVariables, MultiParamTypeClasses #-}
module Eta.Interop (SObject(..)) where

import Data.Proxy
import GHC.Base
import GHC.TypeLits

data {-# CLASS "java.lang.Object" #-} SObject (s :: Symbol) = SObject (Object# (SObject s))

instance (KnownSymbol s) => Class (SObject s) where
  unobj (SObject s) = s
  obj s = SObject s
  classIdentifier _ = symbolVal (Proxy :: Proxy s)

class Overloadable (a :: k) (s :: Symbol) r where
  overloaded :: Proxy# a -> Proxy# s -> r

class Constructor r where
  new :: r
