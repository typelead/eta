{-# LANGUAGE BangPatterns #-}
module GHCVM.CodeGen.Monad
  (CgEnv(..),
   CgState(..),
   CodeGen(..),
   getClass,
   addBinding,
   addBindings,
   setBindings,
   defineMethod,
   defineMethods,
   defineField,
   defineFields,
   getCgIdInfo,
   newTypeClosure,
   newExportedClosure,
   newHiddenClosure,
   newClosure,
   classFromCgState,
   runCodeGen
  ) where

import DynFlags
import Module
import VarEnv
import Id
import Name

import Data.List
import Data.Text hiding (foldl, length, concatMap, map, intercalate)

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

instance Show CgState where
  show CgState {..} = "cgClassName: "         ++ show cgClassName      ++ "\n"
                   ++ "cgClassInitCode: "     ++ show cgClassInitCode  ++ "\n"
                   ++ "cgMethodDefs: "        ++ show cgMethodDefs     ++ "\n"
                   ++ "cgFieldDefs: "         ++ show cgFieldDefs      ++ "\n"
                   ++ "cgSuperClassName: "    ++ show cgSuperClassName ++ "\n"
                   ++ "cgCompiledClosures: \n"  ++ (intercalate "\n" . map show $ cgCompiledClosures)

newtype CodeGen a = CG { unCG :: CgEnv -> CgState -> IO (CgState, a) }

instance Functor CodeGen where
  fmap = liftM

instance Applicative CodeGen where
  pure = return
  (<*>) = ap

instance Monad CodeGen where
  return x = CG $ \_ s -> return (s, x)
  m >>= f = CG $ \e s -> do
      (!s0, !x) <- unCG m e s
      unCG (f x) e s0

instance MonadState CgState CodeGen where
  state action = CG $ \_ s -> case action s of
    (!a, !s') -> return (s', a)

instance MonadReader CgEnv CodeGen where
  ask = CG $ \env s -> return (s, env)
  local f action = CG $ \env s -> unCG action (f env) s

instance HasModule CodeGen where
  getModule = asks cgModule

instance HasDynFlags CodeGen where
  getDynFlags = asks cgDynFlags

instance MonadIO CodeGen where
  liftIO io = CG $ \_ s -> io >>= (\a -> return (s, a))

getModClass :: CodeGen Text
getModClass = asks cgQClassName

mergeCompiledClosures :: [ClassFile] -> CodeGen ()
mergeCompiledClosures classFiles = modify $ \s@CgState{..} ->
  s { cgCompiledClosures = classFiles ++ cgCompiledClosures }

addCompiledClosure :: ClassFile -> CodeGen ()
addCompiledClosure classFile = modify $ \s@CgState{..} ->
  s { cgCompiledClosures = classFile : cgCompiledClosures }

setClass :: Text -> CodeGen ()
setClass className = modify $ \s -> s { cgClassName = className }

getClass :: CodeGen Text
getClass = gets cgClassName

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

defineMethods :: [MethodDef] -> CodeGen ()
defineMethods mds = modify $ \s@CgState{..} ->
  s { cgMethodDefs = mds ++ cgMethodDefs }

defineField :: FieldDef -> CodeGen ()
defineField md = modify $ \s@CgState{..} ->
  s { cgFieldDefs = md : cgFieldDefs }

defineFields :: [FieldDef] -> CodeGen ()
defineFields md = modify $ \s@CgState{..} ->
  s { cgFieldDefs = md ++ cgFieldDefs }

newExportedClosure, newHiddenClosure
  :: Text
  -> Text
  -> CodeGen ()
  -> CodeGen Text
newExportedClosure = newClosure [Public]
newHiddenClosure = newClosure [Private]

newTypeClosure
  :: Text
  -> Text
  -> CodeGen Text
newTypeClosure thisClass superClass =
  newClosure [Public, Abstract] thisClass superClass $
    defineMethod $ mkDefaultConstructor thisClass superClass

newClosure
  :: [AccessFlag]
  -> Text
  -> Text
  -> CodeGen ()
  -> CodeGen Text
newClosure accessFlags className superClassName genCode = do
  state0 <- get
  modClass <- getModClass
  let qclassName = append modClass . cons '$' $ className
  -- TODO: Address how to deal with cgClassInitCode and cgBindings
  modify $ \s -> s { cgMethodDefs = [],
                     cgFieldDefs = [],
                     cgClassName = qclassName,
                     cgSuperClassName = Just superClassName,
                     cgCompiledClosures = []}
  genCode
  state1@CgState {..} <- get
  let compiledClosure = classFromCgState accessFlags state1
  -- TODO: Ensure the state is restored properly
  put state0
  mergeCompiledClosures (compiledClosure : cgCompiledClosures)
  return qclassName

classFromCgState :: [AccessFlag] -> CgState -> ClassFile
classFromCgState accessFlags CgState {..} = mkClassFile java7 accessFlags cgClassName cgSuperClassName cgFieldDefs cgMethodDefs

runCodeGen :: CgEnv -> CgState -> CodeGen a -> IO [ClassFile]
runCodeGen env state m = do
  (state', _) <- unCG m env state
  let compiledModuleClass = classFromCgState [Public, Super] state'
  return (compiledModuleClass : cgCompiledClosures state')
