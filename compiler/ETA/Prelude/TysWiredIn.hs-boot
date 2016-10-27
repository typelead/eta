module ETA.Prelude.TysWiredIn where

import {-# SOURCE #-} ETA.Types.TyCon      (TyCon)
import {-# SOURCE #-} ETA.Types.TypeRep    (Type)


eqTyCon, coercibleTyCon :: TyCon
typeNatKind, typeSymbolKind :: Type
mkBoxedTupleTy :: [Type] -> Type
