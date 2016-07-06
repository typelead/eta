module GHCVM.CodeGen.Env where

import StgSyn

import Codec.JVM

import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Utils

loadArgCode :: NonVoid StgArg -> CodeGen Code
loadArgCode (NonVoid (StgVarArg var)) = do
  CgIdInfo {..} <- getCgIdInfo var
  let ft = lfFieldType cgLambdaForm
  return . getstatic $ mkFieldRef cgModuleClass cgClosureName ft
loadArgCode (NonVoid (StgLitArg literal)) = return $ cgLit literal
