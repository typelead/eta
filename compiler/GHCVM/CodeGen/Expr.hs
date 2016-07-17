module GHCVM.CodeGen.Expr where

import Id
import PrimOp
import StgSyn
import DataCon
import GHCVM.Primitive
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Layout
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Closure
import GHCVM.CodeGen.Env
import GHCVM.CodeGen.Rts
import GHCVM.CodeGen.Con
import GHCVM.CodeGen.Prim
import {-# SOURCE #-} GHCVM.CodeGen.Bind (cgBind)
import Codec.JVM

import Data.Monoid((<>))

cgExpr :: StgExpr -> CodeGen ()
cgExpr (StgApp fun args) = cgIdApp fun args
cgExpr (StgOpApp (StgPrimOp SeqOp) [StgVarArg a, _] _) = cgIdApp a []
cgExpr (StgOpApp op args ty) = cgOpApp op args ty
cgExpr (StgConApp con args) = cgConApp con args
cgExpr (StgTick t e) = cgExpr e
cgExpr (StgLit lit) = unimplemented "cgExpr: StgLit"
cgExpr (StgLet binds expr) = cgBind binds >> cgExpr expr
cgExpr (StgLetNoEscape _ _ binds expr) = unimplemented "cgExpr: StgLetNoEscape"
cgExpr (StgCase expr _ _ binder _ altType alts) =
  cgCase expr binder altType alts
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

cgConApp :: DataCon -> [StgArg] -> CodeGen ()
cgConApp con args
  | isUnboxedTupleCon con = do
      ftCodes <- getNonVoidFtCodes args
      emitReturn $ toCgLocs ftCodes
  | otherwise = do
      (idInfo, genInitCode) <- buildDynCon con args
      initCode <- genInitCode
      emit initCode
      emitReturn [cgLocation idInfo]

cgCase :: StgExpr -> Id -> AltType -> [StgAlt] -> CodeGen ()
cgCase _ _ _ _ = unimplemented "cgCase"
