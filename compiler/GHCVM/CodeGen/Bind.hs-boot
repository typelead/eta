module GHCVM.CodeGen.Bind where

import GHCVM.CodeGen.Monad( CodeGen )
import GHCVM.StgSyn.StgSyn( StgBinding )

cgBind :: StgBinding -> CodeGen ()