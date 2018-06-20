module Eta.Iface.IfaceType where

import {-# SOURCE #-} Eta.Types.TypeRep (Type)
import Eta.Utils.Outputable

data IfaceType

toIfaceType :: Type -> IfaceType

pprIfaceContext :: Outputable a => [a] -> SDoc
