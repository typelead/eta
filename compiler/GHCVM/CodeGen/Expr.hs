module GHCVM.CodeGen.Expr where

import CoreSyn
import Type
import TyCon
import Id
import PrimOp
import StgSyn
import DataCon
import Panic
import Util (unzipWith)
import GHCVM.Util
import GHCVM.Primitive
import GHCVM.CodeGen.Utils
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Name
import GHCVM.CodeGen.Layout
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Closure
import GHCVM.CodeGen.Env
import GHCVM.CodeGen.Rts
import GHCVM.CodeGen.Con
import GHCVM.CodeGen.Prim
import GHCVM.CodeGen.ArgRep
import {-# SOURCE #-} GHCVM.CodeGen.Bind (cgBind)
import Codec.JVM

import Data.Monoid((<>))
import Control.Monad(when, forM_, unless)
import Data.Maybe(fromJust)

cgExpr :: StgExpr -> CodeGen ()
cgExpr (StgApp fun args) = cgIdApp fun args
cgExpr (StgOpApp (StgPrimOp SeqOp) [StgVarArg a, _] _) = cgIdApp a []
cgExpr (StgOpApp op args ty) = cgOpApp op args ty
cgExpr (StgConApp con args) = cgConApp con args
cgExpr (StgTick t e) = cgExpr e
cgExpr (StgLit lit) = emitReturn [mkLocDirect $ cgLit lit]
cgExpr (StgLet binds expr) = do
  cgBind binds
  cgExpr expr
cgExpr (StgLetNoEscape _ _ binds expr) = unimplemented "cgExpr: StgLetNoEscape"
  -- joinPoint <- newLabel
  -- cgLneBinds joinPoint binds
  -- cgExpr expr
  -- emit $ startLabel joinPoint

cgExpr (StgCase expr _ _ binder _ altType alts) =
  cgCase expr binder altType alts
cgExpr _ = unimplemented "cgExpr"

cgLneBinds :: Label -> StgBinding -> CodeGen ()
cgLneBinds joinLabel (StgNonRec binder rhs) = do
  (info, code) <- cgLetNoEscapeRhs joinLabel binder rhs
  code
  addBinding info
cgLneBinds joinLabel (StgRec pairs) = do
  result <- sequence $ unzipWith (cgLetNoEscapeRhs joinLabel) pairs
  let (infos, codes) = unzip result
  addBindings infos
  sequence_ codes

cgLetNoEscapeRhs :: Label -> Id -> StgRhs -> CodeGen (CgIdInfo, CodeGen ())
cgLetNoEscapeRhs joinLabel binder rhs = do
  (info, rhsCode) <- cgLetNoEscapeRhsBody binder rhs
  let (bindLabel, _) = fromJust . maybeLetNoEscape $ info
      code = do (_, body) <- getCodeWithResult rhsCode
                -- TODO: Finish up
                emit $ body
                -- emitWithBranch body
  return (info, code)

cgLetNoEscapeRhsBody :: Id -> StgRhs -> CodeGen (CgIdInfo, CodeGen())
cgLetNoEscapeRhsBody binder (StgRhsClosure _ _ _ _ _ args body)
  = cgLetNoEscapeClosure binder (nonVoidIds args) body
cgLetNoEscapeRhsBody binder (StgRhsCon _ con args)
  = cgLetNoEscapeClosure binder [] (StgConApp con args)

cgLetNoEscapeClosure
  :: Id -> [NonVoid Id] -> StgExpr -> CodeGen (CgIdInfo, CodeGen ())
cgLetNoEscapeClosure binder args body = do
  label <- newLabel
  argLocs <- mapM newIdLoc args
  return (lneIdInfo label binder argLocs, code)
  where code = forkLneBody $ do
          argLocs <- mapM newIdLoc args
          bindArgs $ zip args argLocs
          cgExpr body

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
    JumpToIt label cgLocs -> do
      codes <- getNonVoidArgLoadCodes args
      emit $ multiAssign cgLocs codes
      emit $ goto label

emitEnter :: CgLoc -> CodeGen ()
emitEnter thunk = do
  sequel <- getSequel
  case sequel of
    Return ->
      emit $ loadContext
          <> enterMethod thunk
    AssignTo cgLocs ->
      emit $ loadContext
          <> evaluateMethod thunk
          <> mkReturnEntry cgLocs


cgConApp :: DataCon -> [StgArg] -> CodeGen ()
cgConApp con args
  | isUnboxedTupleCon con = do
      ftCodes <- getNonVoidFtCodes args
      emitReturn $ map mkLocDirect ftCodes
  | otherwise = do
      -- TODO: Is dataConWorId the right thing to pass?
      (idInfo, genInitCode) <- buildDynCon (dataConWorkId con) con args
      initCode <- genInitCode
      emit initCode
      emitReturn [cgLocation idInfo]

cgCase :: StgExpr -> Id -> AltType -> [StgAlt] -> CodeGen ()
cgCase (StgOpApp (StgPrimOp op) args _) binder (AlgAlt tyCon) alts
  | isEnumerationTyCon tyCon = do
      tagExpr <- doEnumPrimop op args
      unless (isDeadBinder binder) $ do
        bindLoc <- newIdLoc (NonVoid binder)
        bindArg (NonVoid binder) bindLoc
        emitAssign bindLoc $ snd (tagToClosure tyCon tagExpr)
      (maybeDefault, branches) <- cgAlgAltRhss (NonVoid binder) alts
      emit $ intSwitch tagExpr branches maybeDefault
  where doEnumPrimop :: PrimOp -> [StgArg] -> CodeGen Code
        doEnumPrimop TagToEnumOp [arg] = getArgLoadCode (NonVoid arg)
        doEnumPrimop primop args = do
          tmp <- newTemp jint
          cgPrimOp [tmp] primop args
          return (loadLoc tmp)

cgCase (StgApp v []) _ (PrimAlt _) alts
  | isVoidJRep (idJPrimRep v)
  , [(DEFAULT, _, _, rhs)] <- alts
  = cgExpr rhs

cgCase (StgApp v []) binder altType@(PrimAlt _) alts
  | isUnLiftedType (idType v)
  || repsCompatible
  = do
      when (not repsCompatible) $
        panic "cgCase: reps do not match, perhaps a dodgy unsafeCoerce?"
      vInfo <- getCgIdInfo v
      cgLoc <- newIdLoc nvBinder
      emitAssign cgLoc (idInfoLoadCode vInfo)
      bindArgs [(nvBinder, cgLoc)]
      cgAlts nvBinder altType alts
  where repsCompatible = vRep == idJPrimRep binder
        -- TODO: Allow integer conversions?
        nvBinder = NonVoid binder
        vRep = idJPrimRep v

cgCase scrut@(StgApp v []) _ (PrimAlt _) _ = do
  cgLoc <- newIdLoc (NonVoid v)
  withSequel (AssignTo [cgLoc]) $ cgExpr scrut
  panic "cgCase: bad unsafeCoerce!"
  -- TODO: Generate infinite loop here?

cgCase (StgOpApp (StgPrimOp SeqOp) [StgVarArg a, _] _) binder altType alts
  = cgCase (StgApp a []) binder altType alts

cgCase scrut binder altType alts = do
  altLocs <- mapM newIdLoc retBinders
  withSequel (AssignTo altLocs) $ cgExpr scrut
  bindArgs $ zip retBinders altLocs
  cgAlts (NonVoid binder) altType alts
  where retBinders = chooseReturnBinders binder altType alts

chooseReturnBinders :: Id -> AltType -> [StgAlt] -> [NonVoid Id]
chooseReturnBinders binder (PrimAlt _) _ = nonVoidIds [binder]
chooseReturnBinders _ (UbxTupAlt _) [(_, ids, _, _)] = nonVoidIds ids
chooseReturnBinders binder (AlgAlt _) _ = nonVoidIds [binder]
chooseReturnBinders binder PolyAlt _ = nonVoidIds [binder]
chooseReturnBinders _ _ _ = panic "chooseReturnBinders"

cgAlts :: NonVoid Id -> AltType -> [StgAlt] -> CodeGen ()
cgAlts _ PolyAlt [(_, _, _, rhs)] = cgExpr rhs
cgAlts _ (UbxTupAlt _) [(_, _, _, rhs)] = cgExpr rhs
cgAlts binder (PrimAlt _) alts = do
  taggedBranches <- cgAltRhss binder alts
  binderLoc <- getCgLoc binder
  let (DEFAULT, deflt) = head taggedBranches
      taggedBranches' = [(lit, code) | (LitAlt lit, code) <- taggedBranches]
  emit $ litSwitch (locFt binderLoc) (loadLoc binderLoc) taggedBranches' deflt
cgAlts binder (AlgAlt tyCon) alts = do
  (maybeDefault, branches) <- cgAlgAltRhss binder alts
  binderLoc <- getCgLoc binder
  emit $ intSwitch (getTagMethod $ loadLoc binderLoc) branches maybeDefault
cgAlts _ _ _ = panic "cgAlts"

cgAltRhss :: NonVoid Id -> [StgAlt] -> CodeGen [(AltCon, Code)]
cgAltRhss binder alts = do
  baseLoc <- getCgLoc binder
  forkAlts $ map (cgAlt baseLoc) alts
  where cgAlt :: CgLoc -> StgAlt -> CodeGen (AltCon, Code)
        cgAlt baseLoc (con, binders, _, rhs) =
          getCodeWithResult $ do
            bindConArgs con baseLoc binders
            cgExpr rhs
            return con

bindConArgs :: AltCon -> CgLoc -> [Id] -> CodeGen ()
bindConArgs (DataAlt con) base args =
  forM_ indexedFields $ \(i, (ft, arg)) -> do
    cgLoc <- newIdLoc arg
    emitAssign cgLoc $ loadLoc base
                    <> getfield (mkFieldRef conClass (constrField i) ft)
    bindArg arg cgLoc
  where indexedFields = indexList . getNonVoidFts $ zip maybeFields args
        conClass = dataConClass con
        maybeFields = map repFieldType $ dataConRepArgTys con
bindConArgs _ _ _ = return ()

cgAlgAltRhss :: NonVoid Id -> [StgAlt] -> CodeGen (Maybe Code, [(Int, Code)])
cgAlgAltRhss binder alts = do
  taggedBranches <- cgAltRhss binder alts
  let maybeDefault = case taggedBranches of
                       ((DEFAULT, rhs) : _) -> Just rhs
                       _ -> Nothing
      branches = [ (getDataConTag con, code)
                 | (DataAlt con, code) <- taggedBranches ]
  return (maybeDefault, branches)



