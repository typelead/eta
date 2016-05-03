{-# LANGUAGE MultiParamTypeClasses #-}
module GHCVM.CodeGen.Monad
  (CgEnv(..),
   CgState(..),
   CodeGen(..),
   code,
   addBinding,
   addBindings,
   setBindings,
   getCgIdInfo
  ) where

import Module
import VarEnv
import Id
import Name

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import JVM.Builder
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Closure

data CgEnv = CgEnv {
  cgPackagePrefix :: B.ByteString,
  cgFileName :: B.ByteString,
  cgClassName :: B.ByteString,
  cgModule :: Module }

data CgState = CgState {
  cgBindings :: CgBindings }

newtype CodeGen e a = CG {
  unCG :: CgEnv -> CgState -> GenerateIO e (CgState, a) }

instance Functor (CodeGen e) where
  fmap = liftM

instance Applicative (CodeGen e) where
  pure = return
  (<*>) = ap

instance Monad (CodeGen e) where
  return x = CG $ \_ s -> return (s, x)
  m >>= f = CG $ \e s -> do
      (s0, x) <- unCG m e s
      unCG (f x) e s0

instance MonadState CgState (CodeGen e) where
  state action = CG $ \_ s -> do
    let (a, s') = action s
    return (s', a)

instance MonadReader CgEnv (CodeGen e) where
  ask = CG $ \env s -> return (s, env)
  local f action = CG $ \env s -> unCG action (f env) s

instance HasModule (CodeGen e) where
  getModule = asks cgModule

code :: GenerateIO e a -> CodeGen e a
code fc = CG $ \_ s -> do
                a <- fc
                return (s, a)

getModule :: CodeGen e Module
getModule = asks cgModule

getBindings :: CodeGen e CgBindings
getBindings = gets cgBindings

setBindings :: CgBindings -> CodeGen e ()
setBindings bindings = modify $ \s -> s { cgBindings = bindings }

getCgIdInfo :: Id -> CodeGen e CgIdInfo
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

addBinding :: CgIdInfo -> CodeGen e ()
addBinding cgIdInfo = do
  bindings <- getBindings
  setBindings $ extendVarEnv bindings (cgId cgIdInfo) cgIdInfo

addBindings :: [CgIdInfo] -> CodeGen e ()
addBindings newCgIdInfos = do
        bindings <- getBindings
        let newBindings = foldl
              (\binds info -> extendVarEnv binds (cgId info) info)
              bindings
              newCgIdInfos
        setBindings newBindings
