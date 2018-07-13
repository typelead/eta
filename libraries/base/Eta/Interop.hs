{-# LANGUAGE PolyKinds, DataKinds, MagicHash, ScopedTypeVariables, MultiParamTypeClasses #-}
module Eta.Interop (SObject(..)) where

import Data.Proxy
import GHC.Base
import GHC.TypeLits

data {-# CLASS "java.lang.Object" #-} SObject (s :: Symbol) (xs :: [*]) =
  SObject (Object# (SObject s xs))

instance (KnownSymbol s) => Class (SObject s xs) where
  unobj (SObject s) = s
  obj s = SObject s
  classIdentifier _ = symbolVal (Proxy :: Proxy s)
