module Eta.Prelude.TysWiredIn where

import {-# SOURCE #-} Eta.Types.TyCon      (TyCon)
import {-# SOURCE #-} Eta.Types.TypeRep    (Type)


eqTyCon, listTyCon, coercibleTyCon :: TyCon
typeNatKind, typeSymbolKind :: Type
mkBoxedTupleTy :: [Type] -> Type
