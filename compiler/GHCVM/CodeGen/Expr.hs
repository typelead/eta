module GHCVM.CodeGen.Expr where

import StgSyn
import GHCVM.CodeGen.Monad

cgExpr :: StgExpr -> CodeGen ()
cgExpr _ = return ()
