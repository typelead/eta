module GHCVM.BasicTypes.PatSyn where
import GHCVM.BasicTypes.Name( NamedThing )
import Data.Typeable ( Typeable )
import Data.Data ( Data )
import GHCVM.Utils.Outputable ( Outputable, OutputableBndr )
import GHCVM.BasicTypes.Unique ( Uniquable )

data PatSyn

instance Eq PatSyn
instance Ord PatSyn
instance NamedThing PatSyn
instance Outputable PatSyn
instance OutputableBndr PatSyn
instance Uniquable PatSyn
instance Typeable PatSyn
instance Data PatSyn
