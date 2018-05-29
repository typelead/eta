module Eta.Prelude.TysWiredIn where

import {-# SOURCE #-} Eta.Types.TyCon      (TyCon)
import {-# SOURCE #-} Eta.Types.TypeRep    (Type)


eqTyCon, coercibleTyCon :: TyCon
typeNatKind, typeSymbolKind :: Type
mkBoxedTupleTy :: [Type] -> Type
