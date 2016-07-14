module GHCVM.CodeGen.Env where

import Id
import StgSyn

import Codec.JVM

import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Utils

import Control.Monad

loadArgCode :: NonVoid StgArg -> CodeGen Code
loadArgCode (NonVoid (StgVarArg var)) = liftM idInfoLoadCode $ getCgIdInfo var
loadArgCode (NonVoid (StgLitArg literal)) = return $ cgLit literal

idInfoLoadCode :: CgIdInfo -> Code
idInfoLoadCode CgIdInfo { cgLocation } = loadLoc cgLocation


rebindId :: NonVoid Id -> CgLoc -> CodeGen ()
rebindId nvId@(NonVoid id) cgLoc = do
  info <- getCgIdInfo id
  bindId nvId (cgLambdaForm info) cgLoc

bindId :: NonVoid Id -> LambdaFormInfo -> CgLoc -> CodeGen ()
bindId nvId@(NonVoid id) lfInfo cgLoc =
  addBinding (mkCgIdInfoWithLoc id lfInfo cgLoc)

