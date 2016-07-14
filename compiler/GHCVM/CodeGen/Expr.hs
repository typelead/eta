module GHCVM.CodeGen.Expr where

import StgSyn
import GHCVM.CodeGen.Monad
import Codec.JVM

cgExpr :: StgExpr -> CodeGen ()
cgExpr _ = return ()
