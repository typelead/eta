module Eta.Types.Type where
import {-# SOURCE #-} Eta.Types.TypeRep( Type, Kind )
import Eta.BasicTypes.Var

isPredTy :: Type -> Bool

typeKind :: Type -> Kind
substKiWith :: [KindVar] -> [Kind] -> Kind -> Kind
eqKind :: Kind -> Kind -> Bool
