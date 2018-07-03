{-# LANGUAGE CPP #-}
module Eta.BasicTypes.PatSyn where
import Eta.BasicTypes.Name( NamedThing )
#if __GLASGOW_HASKELL__ < 800
import Data.Typeable ( Typeable )
#endif
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
#if __GLASGOW_HASKELL__ < 800
instance Typeable PatSyn
#endif
instance Data PatSyn
patSynIsInfix :: PatSyn -> Bool
