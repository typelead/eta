module GHCVM.CodeGen.Monad
  (CgEnv(..),
   CgState(..),
   CodeGen(..),
   addBinding,
   addBindings,
   setBindings,
   getCgIdInfo,
   newTypeClosure,
   newExportedClosure,
   newHiddenClosure,
   newClosure
  ) where

import DynFlags
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
  cgQClassName :: !Text,
  cgModule :: !Module,
  cgDynFlags :: !DynFlags }

data CgState = CgState {
  cgBindings :: !CgBindings,
  cgCompiledClosures :: ![ClassFile],
  cgClassInitCode :: ![Code],
  -- Related to class file generation
  cgMethodDefs :: ![MethodDef],
  cgFieldDefs :: ![FieldDef],
  cgClassName :: !Text,
  cgSuperClassName :: !(Maybe Text) }

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
  state action = CG $ \_ s -> case action s of
    (a, s') -> return (s', a)

instance MonadReader CgEnv CodeGen where
  ask = CG $ \env s -> return (s, env)
  local f action = CG $ \env s -> unCG action (f env) s

instance HasModule CodeGen where
  getModule = asks cgModule

instance HasDynFlags CodeGen where
  getDynFlags = asks cgDynFlags

getModClass :: CodeGen Text
getModClass = asks cgQClassName

addCompiledClosure :: ClassFile -> CodeGen ()
addCompiledClosure classFile = modify $ \s@CgState{..} ->
  s { cgCompiledClosures = classFile : cgCompiledClosures }

setClass :: Text -> CodeGen ()
setClass className = modify $ \s -> s { cgClassName = className }

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

defineMethod :: MethodDef -> CodeGen ()
defineMethod md = modify $ \s@CgState{..} ->
  s { cgMethodDefs = md : cgMethodDefs }

defineField :: FieldDef -> CodeGen ()
defineField md = modify $ \s@CgState{..} ->
  s { cgFieldDefs = md : cgFieldDefs }

newExportedClosure, newHiddenClosure :: Text
                 -> Text
                 -> CodeGen ()
                 -> CodeGen Text
newExportedClosure = newClosure [Public]
newHiddenClosure = newClosure [Private]

newTypeClosure :: Text
               -> Text
               -> CodeGen Text
newTypeClosure thisClass superClass =
  newClosure [Public, Abstract] thisClass superClass $
    defineMethod . defaultConstructor $ superClass

newClosure :: [AccessFlag]
           -> Text
           -> Text
           -> CodeGen ()
           -> CodeGen Text
newClosure accessFlags className superClassName genCode = do
  state0 <- get
  modClass <- getModClass
  let qclassName = append modClass . cons '$' $ className
  modify $ \s -> s { cgMethodDefs = [],
                     cgFieldDefs = [],
                     cgClassName = qclassName,
                     cgSuperClassName = Just superClassName }
  genCode
  CgState {..} <- get
  let compiledClosure = mkClassFile java7 accessFlags cgClassName cgSuperClassName cgFieldDefs cgMethodDefs
  put state0
  addCompiledClosure compiledClosure
  return qclassName
