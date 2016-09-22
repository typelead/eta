module GHCVM.Prelude.TysWiredIn where

import {-# SOURCE #-} GHCVM.Types.TyCon      (TyCon)
import {-# SOURCE #-} GHCVM.Types.TypeRep    (Type)


eqTyCon, coercibleTyCon :: TyCon
typeNatKind, typeSymbolKind :: Type
mkBoxedTupleTy :: [Type] -> Type
