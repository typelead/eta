module ETA.Types.TyCon where

import ETA.BasicTypes.Name (Name)
import ETA.BasicTypes.Unique (Unique)

data TyCon

tyConName           :: TyCon -> Name
tyConUnique         :: TyCon -> Unique
isTupleTyCon        :: TyCon -> Bool
isUnboxedTupleTyCon :: TyCon -> Bool
isFunTyCon          :: TyCon -> Bool
