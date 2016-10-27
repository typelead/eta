module ETA.BasicTypes.PatSyn where
import ETA.BasicTypes.Name( NamedThing )
import Data.Typeable ( Typeable )
import Data.Data ( Data )
import ETA.Utils.Outputable ( Outputable, OutputableBndr )
import ETA.BasicTypes.Unique ( Uniquable )

data PatSyn

instance Eq PatSyn
instance Ord PatSyn
instance NamedThing PatSyn
instance Outputable PatSyn
instance OutputableBndr PatSyn
instance Uniquable PatSyn
instance Typeable PatSyn
instance Data PatSyn
