module ETA.CodeGen.Bind where

import ETA.CodeGen.Monad( CodeGen )
import ETA.StgSyn.StgSyn( StgBinding )

cgBind :: StgBinding -> CodeGen ()