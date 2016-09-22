module GHCVM.Types.TyCon where

import GHCVM.BasicTypes.Name (Name)
import GHCVM.BasicTypes.Unique (Unique)

data TyCon

tyConName           :: TyCon -> Name
tyConUnique         :: TyCon -> Unique
isTupleTyCon        :: TyCon -> Bool
isUnboxedTupleTyCon :: TyCon -> Bool
isFunTyCon          :: TyCon -> Bool
