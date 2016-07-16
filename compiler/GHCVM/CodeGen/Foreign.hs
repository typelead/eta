module GHCVM.CodeGen.Foreign where

import Type
import StgSyn
import ForeignCall

import GHCVM.CodeGen.Monad

cgForeignCall :: ForeignCall -> [StgArg] -> Type -> CodeGen ()
cgForeignCall _ _ _ = unimplemented "cgForeignCall"

