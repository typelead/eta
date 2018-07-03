module Eta.BasicTypes.DataCon where
import Eta.BasicTypes.Name( Name, NamedThing )
import {-# SOURCE #-} Eta.Types.TyCon( TyCon )
import Eta.BasicTypes.Unique ( Uniquable )
import Eta.Utils.Outputable ( Outputable, OutputableBndr )

data DataCon
data DataConRep
dataConName      :: DataCon -> Name
dataConTyCon     :: DataCon -> TyCon
isVanillaDataCon :: DataCon -> Bool
dataConIsInfix :: DataCon -> Bool

instance Eq DataCon
instance Ord DataCon
instance Uniquable DataCon
instance NamedThing DataCon
instance Outputable DataCon
instance OutputableBndr DataCon
