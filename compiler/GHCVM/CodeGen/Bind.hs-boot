module GHCVM.CodeGen.Bind where

import GHCVM.CodeGen.Monad( CodeGen )
import StgSyn( StgBinding )

cgBind :: StgBinding -> CodeGen ()