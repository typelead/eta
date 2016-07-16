module GHCVM.CodeGen.Env where

import Id
import StgSyn

import Codec.JVM

import GHCVM.Primitive
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Closure
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Utils
import GHCVM.CodeGen.ArgRep

import Control.Monad (liftM)
import Data.Maybe (fromJust)

getArgLoadCode :: NonVoid StgArg -> CodeGen Code
getArgLoadCode (NonVoid (StgVarArg var)) = liftM idInfoLoadCode $ getCgIdInfo var
getArgLoadCode (NonVoid (StgLitArg literal)) = return $ cgLit literal

getNonVoidArgLoadCodes :: [StgArg] -> CodeGen [Code]
getNonVoidArgLoadCodes [] = return []
getNonVoidArgLoadCodes (arg:args)
  | isVoidJRep (argJPrimRep arg) = getNonVoidArgLoadCodes args
  | otherwise = do
      code <- getArgLoadCode (NonVoid arg)
      codes <- getNonVoidArgLoadCodes args
      return (code:codes)

getNonVoidFtCodes :: [StgArg] -> CodeGen [(FieldType, Code)]
getNonVoidFtCodes [] = return []
getNonVoidFtCodes (arg:args)
  | isVoidJRep (argJPrimRep arg) = getNonVoidFtCodes args
  | otherwise = do
      code <- getArgLoadCode (NonVoid arg)
      ftCodes <- getNonVoidFtCodes args
      return ((ft, code) : ftCodes)
  where primRep = argJPrimRep arg
        ft = fromJust . primRepFieldType $ primRep

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

rhsIdInfo :: Id -> LambdaFormInfo -> CodeGen (CgIdInfo, CgLoc)
rhsIdInfo id lfInfo = do
  cgLoc <- newTemp (lfFieldType lfInfo) -- TODO: Maybe make this more precise?
  return (mkCgIdInfoWithLoc id lfInfo cgLoc, cgLoc)

mkRhsInit :: CgLoc -> Code -> Code
mkRhsInit = storeLoc
