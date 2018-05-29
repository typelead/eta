module Eta.BasicTypes.PatSyn where
import Eta.BasicTypes.Name( NamedThing )
import Data.Typeable ( Typeable )
import Data.Data ( Data )
import Eta.Utils.Outputable ( Outputable, OutputableBndr )
import Eta.BasicTypes.Unique ( Uniquable )

data PatSyn

instance Eq PatSyn
instance Ord PatSyn
instance NamedThing PatSyn
instance Outputable PatSyn
instance OutputableBndr PatSyn
instance Uniquable PatSyn
instance Typeable PatSyn
instance Data PatSyn
