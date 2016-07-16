module GHCVM.CodeGen.Env where

import Id
import StgSyn

import Codec.JVM

import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Closure
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Utils

import Control.Monad (liftM)

getArgLoadCode :: NonVoid StgArg -> CodeGen Code
getArgLoadCode (NonVoid (StgVarArg var)) = liftM idInfoLoadCode $ getCgIdInfo var
getArgLoadCode (NonVoid (StgLitArg literal)) = return $ cgLit literal

idInfoLoadCode :: CgIdInfo -> Code
idInfoLoadCode CgIdInfo { cgLocation } = loadLoc cgLocation

rebindId :: NonVoid Id -> CgLoc -> CodeGen ()
rebindId nvId@(NonVoid id) cgLoc = do
  info <- getCgIdInfo id
  bindId nvId (cgLambdaForm info) cgLoc

bindId :: NonVoid Id -> LambdaFormInfo -> CgLoc -> CodeGen ()
bindId nvId@(NonVoid id) lfInfo cgLoc =
  addBinding (mkCgIdInfoWithLoc id lfInfo cgLoc)

bindArg :: NonVoid Id -> CgLoc -> CodeGen ()
bindArg nvid@(NonVoid id) = bindId nvid (mkLFArgument id)

bindArgs :: [(NonVoid Id, CgLoc)] -> CodeGen ()
bindArgs = mapM_ (\(nvId, cgLoc) -> bindArg nvId cgLoc)

  
