module GHCVM.CodeGen.Monad
  (CgEnv(..),
   CgState(..),
   CodeGen(..),
   emit,
   initCg,
   setSuperClass,
   getSuperClass,
   setClosureClass,
   withMethod,
   getModClass,
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
   runCodeGen,
   addInitStep,
   forkClosureBody)
where

import DynFlags
import Module
import VarEnv
import Id
import Name

import Data.Monoid((<>))
import Data.List
import Data.Maybe (fromJust)
import Data.Text hiding (foldl, length, concatMap, map, intercalate)

import Control.Monad (liftM, ap)
import Control.Monad.State (MonadState(..), get, gets, modify)
import Control.Monad.Reader (MonadReader(..), ask, asks, local)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import Codec.JVM
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Closure
import GHCVM.CodeGen.Name

data CgEnv =
  CgEnv { cgQClassName :: !Text
        , cgModule     :: !Module
        , cgDynFlags   :: !DynFlags
        , cgSequel     :: !Sequel
        , cgSelfLoop   :: !(Maybe SelfLoopInfo) }

data CgState =
  CgState { cgBindings         :: !CgBindings
          -- Accumulating
          , cgCompiledClosures :: ![ClassFile]
          , cgClassInitCode    :: !Code
          -- Top-level definitions
          , cgAccessFlags    :: [AccessFlag]
          , cgMethodDefs     :: ![MethodDef]
          , cgFieldDefs      :: ![FieldDef]
          , cgClassName      :: !Text
          , cgSuperClassName :: !(Maybe Text)
          -- Current method
          , cgCode           :: !Code }

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
  {-# INLINE return #-}
  return x = CG $ \_ s -> return (s, x)
  {-# INLINE (>>=) #-}
  m >>= f = CG $ \e s -> do
      (!s0, !x) <- unCG m e s
      unCG (f x) e s0

instance MonadState CgState CodeGen where
  {-# INLINE state #-}
  state action = CG $ \_ s -> case action s of
    (!a, !s') -> return (s', a)

instance MonadReader CgEnv CodeGen where
  {-# INLINE ask #-}
  ask = CG $ \env s -> return (s, env)
  {-# INLINE local #-}
  local f action = CG $ \env s -> unCG action (f env) s

instance HasModule CodeGen where
  getModule = asks cgModule

instance HasDynFlags CodeGen where
  getDynFlags = asks cgDynFlags

instance MonadIO CodeGen where
  {-# INLINE liftIO #-}
  liftIO io = CG $ \_ s -> io >>= (\a -> return (s, a))

initCg :: DynFlags -> Module -> (CgEnv, CgState)
initCg dflags mod =
  (CgEnv   { cgModule           = mod
           , cgQClassName       = className
           , cgDynFlags         = dflags
           , cgSequel           = Return
           , cgSelfLoop         = Nothing },
   CgState { cgBindings         = emptyVarEnv
           , cgCode             = mempty
           , cgAccessFlags      = [Public, Super]
           , cgMethodDefs       = []
           , cgFieldDefs        = []
           , cgClassInitCode    = mempty
           , cgClassName        = className
           , cgCompiledClosures = []
           , cgSuperClassName   = Nothing })
  where className = moduleJavaClass mod

emit :: Code -> CodeGen ()
emit code = modify $ \s@CgState { cgCode } -> s { cgCode = cgCode <> code }

getMethodCode :: CodeGen Code
getMethodCode = gets cgCode

setMethodCode :: Code -> CodeGen ()
setMethodCode code = modify $ \s -> s { cgCode = code }

getClass :: CodeGen Text
getClass = gets cgClassName

getModClass :: CodeGen Text
getModClass = asks cgQClassName

getInitCode :: CodeGen Code
getInitCode = gets cgClassInitCode

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
      let mod = nameModule name
      curMod <- getModule
      if mod /= curMod then
        return . mkCgIdInfo id $ mkLFImported id
      else
        error "getCgIdInfo: Not external name"

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

mergeCompiledClosures :: [ClassFile] -> CodeGen ()
mergeCompiledClosures classFiles = modify $ \s@CgState{..} ->
  s { cgCompiledClosures = classFiles ++ cgCompiledClosures }

addCompiledClosure :: ClassFile -> CodeGen ()
addCompiledClosure classFile = modify $ \s@CgState{..} ->
  s { cgCompiledClosures = classFile : cgCompiledClosures }

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
  -> CodeGen CgState
newExportedClosure = newClosure [Public, Super, Final]
newHiddenClosure = newClosure [Private, Super, Final]

newTypeClosure
  :: Text
  -> Text
  -> CodeGen CgState
newTypeClosure thisClass superClass =
  newClosure [Public, Abstract, Super] thisClass superClass $
    defineMethod $ mkDefaultConstructor thisClass superClass

newClosure
  :: [AccessFlag]
  -> Text
  -> Text
  -> CodeGen ()
  -> CodeGen CgState
newClosure accessFlags clName superClassName genCode =
  newClosureGeneric $ do
    setAccessFlags accessFlags
    setClosureClass clName
    setSuperClass superClassName
    genCode

setAccessFlags :: [AccessFlag] -> CodeGen ()
setAccessFlags accessFlags = modify $ \s -> s { cgAccessFlags = accessFlags }

setSuperClass :: Text -> CodeGen ()
setSuperClass superClassName =
  modify $ \s -> s { cgSuperClassName = Just superClassName }

-- NOTE: We make an assumption that we never directly derive from
--       java.lang.Object
getSuperClass :: CodeGen Text
getSuperClass = fmap fromJust . gets $ cgSuperClassName

setClosureClass :: Text -> CodeGen ()
setClosureClass clName = do
  modClass <- getModClass
  let qClName = qualifiedName modClass clName
  modify $ \s -> s { cgClassName = qClName }

-- NOTE: Changes made to class generation state are forgotten after
--       the body is executed
newClosureGeneric :: CodeGen () -> CodeGen CgState
newClosureGeneric genCode = do
  state0@CgState
    { cgAccessFlags = a
    , cgMethodDefs = b
    , cgFieldDefs = c
    , cgClassName = d
    , cgSuperClassName = e } <- get
  -- TODO: Ensure the proper state is reset.
  modify $ \s -> s { cgAccessFlags = [Public, Super, Final]
                   , cgMethodDefs = []
                   , cgFieldDefs = []
                   , cgClassName = mempty
                   , cgSuperClassName = Nothing }
  genCode
  state1 <- get
  let compiledClosure = classFromCgState state1
  -- TODO: Ensure the state is restored properly
  modify $ \s -> s { cgAccessFlags = a
                   , cgMethodDefs = b
                   , cgFieldDefs = c
                   , cgClassName = d
                   , cgSuperClassName = e }
  addCompiledClosure compiledClosure
  return state1

classFromCgState :: CgState -> ClassFile
classFromCgState CgState {..} =
  mkClassFile java7 cgAccessFlags cgClassName cgSuperClassName
    cgFieldDefs cgMethodDefs

runCodeGen :: CgEnv -> CgState -> CodeGen a -> IO [ClassFile]
runCodeGen env state codeGenAction = do
  let codeGenActionPlus = do
        codeGenAction
        modClass <- getModClass
        initCode <- getInitCode
        defineMethod $ mkMethodDef modClass
          [Public, Static] "<clinit>" [] void (initCode <> vreturn)

  (state'@CgState {..}, _) <- unCG codeGenActionPlus env state

  -- NOTE: addInnerClasses is to ensure that any unused data types/closures
  --       are added to the constant pool
  let compiledModuleClass =
        addInnerClasses cgCompiledClosures $
          classFromCgState state'

  return (compiledModuleClass : cgCompiledClosures)

addInitStep :: Code -> CodeGen ()
addInitStep code = modify $ \s@CgState{..} ->
  s { cgClassInitCode = cgClassInitCode <> code }

-- NOTE: New bindings generated by the body are forgotten after
--       the body is executed
forkClosureBody :: CodeGen () -> CodeGen CgState
forkClosureBody body =
  local (\env -> env { cgSequel = Return
                     , cgSelfLoop = Nothing })
       . newClosureGeneric $ do
    oldBindings <- getBindings
    body
    setBindings oldBindings

withMethod :: [AccessFlag] -> Text -> [FieldType] -> ReturnType -> CodeGen () -> CodeGen MethodDef
withMethod accessFlags name fts rt body = do
  oldCode <- getMethodCode
  setMethodCode mempty
  body
  emit $ vreturn
  clsName <- getClass
  newCode <- getMethodCode
  let methodDef = mkMethodDef clsName accessFlags name fts rt newCode
  defineMethod methodDef
  setMethodCode oldCode
  return methodDef

