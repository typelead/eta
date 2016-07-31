module GHCVM.Types.Type where
import {-# SOURCE #-} GHCVM.Types.TypeRep( Type, Kind )
import GHCVM.BasicTypes.Var

isPredTy :: Type -> Bool

typeKind :: Type -> Kind
substKiWith :: [KindVar] -> [Kind] -> Kind -> Kind
eqKind :: Kind -> Kind -> Bool
