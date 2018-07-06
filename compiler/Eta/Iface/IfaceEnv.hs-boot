module Eta.Iface.IfaceEnv where

import Eta.BasicTypes.Module
import Eta.BasicTypes.OccName
import Eta.TypeCheck.TcRnMonad
import Eta.BasicTypes.Name
import Eta.BasicTypes.SrcLoc

newGlobalBinder :: Module -> OccName -> SrcSpan -> TcRnIf a b Name
