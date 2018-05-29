module Eta.CodeGen.Bind where

import Eta.CodeGen.Monad( CodeGen )
import Eta.StgSyn.StgSyn( StgBinding )

cgBind :: StgBinding -> CodeGen ()