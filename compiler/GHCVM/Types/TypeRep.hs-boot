module GHCVM.Types.TypeRep where

import GHCVM.Utils.Outputable (Outputable)

data Type
data TyThing

type PredType = Type
type Kind = Type
type SuperKind = Type

instance Outputable Type
