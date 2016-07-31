module GHCVM.BasicTypes.DataCon where
import GHCVM.BasicTypes.Name( Name, NamedThing )
import {-# SOURCE #-} GHCVM.Types.TyCon( TyCon )
import GHCVM.BasicTypes.Unique ( Uniquable )
import GHCVM.Utils.Outputable ( Outputable, OutputableBndr )

data DataCon
data DataConRep
dataConName      :: DataCon -> Name
dataConTyCon     :: DataCon -> TyCon
isVanillaDataCon :: DataCon -> Bool

instance Eq DataCon
instance Ord DataCon
instance Uniquable DataCon
instance NamedThing DataCon
instance Outputable DataCon
instance OutputableBndr DataCon
