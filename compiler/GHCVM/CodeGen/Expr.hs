module GHCVM.CodeGen.Expr where

import Id
import StgSyn
import GHCVM.Primitive
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Layout
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Closure
import GHCVM.CodeGen.Env
import GHCVM.CodeGen.Rts
import Codec.JVM

import Data.Monoid((<>))

cgExpr :: StgExpr -> CodeGen ()
cgExpr (StgApp fun args) = cgIdApp fun args
cgExpr _ = unimplemented "cgExpr"

cgIdApp :: Id -> [StgArg] -> CodeGen ()
cgIdApp funId [] | isVoidJTy (idType funId) = emitReturn []
cgIdApp funId args = do
  dflags <- getDynFlags
  funInfo <- getCgIdInfo funId
  selfLoopInfo <- getSelfLoop
  let cgFunId = cgId funInfo
      funArg = StgVarArg cgFunId
      funName = idName cgFunId
      fun = idInfoLoadCode funInfo
      lfInfo = cgLambdaForm funInfo
      funLoc = cgLocation funInfo
  case getCallMethod dflags funName cgFunId lfInfo (length args) funLoc
                     selfLoopInfo of
    ReturnIt -> emitReturn [funLoc]
    EnterIt -> emitEnter funLoc
    SlowCall -> slowCall funLoc args
    DirectEntry entryCode arity -> directCall False entryCode arity args
    JumpToIt -> unimplemented "cgIdApp: JumpToIt"

emitEnter :: CgLoc -> CodeGen ()
emitEnter thunk = do
  sequel <- getSequel
  case sequel of
    Return ->
      emit $ loadContext
          <> enterMethod thunk
    AssignTo cgLocs -> unimplemented "emitEnter: case AssignTo"
