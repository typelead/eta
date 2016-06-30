{-# LANGUAGE MultiParamTypeClasses #-}
module GHCVM.CodeGen.Monad
  (CgEnv(..),
   CgState(..),
   CodeGen(..),
   addBinding,
   addBindings,
   setBindings,
   getCgIdInfo
  ) where

import Module
import VarEnv
import Id
import Name

import Data.Text hiding (foldl)

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import Codec.JVM
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Closure

data CgEnv = CgEnv {
  cgClassName :: Text,
  cgQClassName :: Text,
  cgModule :: Module
  }

data CgState = CgState {
  cgBindings :: CgBindings,
  cgMethodDefs :: [MethodDef],
  cgFieldDefs :: [FieldDef],
  cgClassInitCode :: [Code],
  cgCompiledClosures :: [ClassFile],
  cgCurrentClassName :: Text,
  cgSuperClassName :: Maybe Text }

newtype CodeGen a = CG { unCG :: CgEnv -> CgState -> IO (CgState, a) }

instance Functor CodeGen where
  fmap = liftM

instance Applicative CodeGen where
  pure = return
  (<*>) = ap

instance Monad CodeGen where
  return x = CG $ \_ s -> return (s, x)
  m >>= f = CG $ \e s -> do
      (s0, x) <- unCG m e s
      unCG (f x) e s0

instance MonadState CgState CodeGen where
  state action = CG $ \_ s -> do
    let (a, s') = action s
    return (s', a)

instance MonadReader CgEnv CodeGen where
  ask = CG $ \env s -> return (s, env)
  local f action = CG $ \env s -> unCG action (f env) s

instance HasModule CodeGen where
  getModule = asks cgModule


setClass :: Text -> CodeGen ()
setClass className = modify $ \s -> s { cgCurrentClassName = className }

getModule :: CodeGen Module
getModule = asks cgModule

getBindings :: CodeGen CgBindings
getBindings = gets cgBindings

setBindings :: CgBindings -> CodeGen ()
setBindings bindings = modify $ \s -> s { cgBindings = bindings }

getCgIdInfo :: Id -> CodeGen CgIdInfo
getCgIdInfo id = do
  localBindings <- getBindings
  case lookupVarEnv localBindings id of
    Just info -> return info
    Nothing -> do
      let name = idName id
      if isExternalName name then
        return . mkCgIdInfo id
               $ mkLFImported id
      else error $ "getCgIdInfo: Not external name"

addBinding :: CgIdInfo -> CodeGen ()
addBinding cgIdInfo = do
  bindings <- getBindings
  setBindings $ extendVarEnv bindings (cgId cgIdInfo) cgIdInfo

addBindings :: [CgIdInfo] -> CodeGen ()
addBindings newCgIdInfos = do
        bindings <- getBindings
        let newBindings = foldl
              (\binds info -> extendVarEnv binds (cgId info) info)
              bindings
              newCgIdInfos
        setBindings newBindings
