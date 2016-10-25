module ETA.Types.Type where
import {-# SOURCE #-} ETA.Types.TypeRep( Type, Kind )
import ETA.BasicTypes.Var

isPredTy :: Type -> Bool

typeKind :: Type -> Kind
substKiWith :: [KindVar] -> [Kind] -> Kind -> Kind
eqKind :: Kind -> Kind -> Bool
