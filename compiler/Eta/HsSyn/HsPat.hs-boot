{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
#if __GLASGOW_HASKELL__ > 706
{-# LANGUAGE RoleAnnotations #-}
#endif

module Eta.HsSyn.HsPat where
import Eta.BasicTypes.SrcLoc( Located )

import Data.Data hiding (Fixity)
import Eta.Utils.Outputable
import Eta.HsSyn.PlaceHolder      ( DataId )

#if __GLASGOW_HASKELL__ > 706
type role Pat nominal
#endif
data Pat (i :: *)
type LPat i = Located (Pat i)

#if __GLASGOW_HASKELL__ > 706 && __GLASGOW_HASKELL__ < 800
instance Typeable Pat
#elif __GLASGOW_HASKELL__ <= 706
instance Typeable1 Pat
#endif

instance (DataId id) => Data (Pat id)
instance (OutputableBndr name) => Outputable (Pat name)
