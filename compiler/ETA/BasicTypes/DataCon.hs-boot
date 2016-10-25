module ETA.BasicTypes.DataCon where
import ETA.BasicTypes.Name( Name, NamedThing )
import {-# SOURCE #-} ETA.Types.TyCon( TyCon )
import ETA.BasicTypes.Unique ( Uniquable )
import ETA.Utils.Outputable ( Outputable, OutputableBndr )

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
