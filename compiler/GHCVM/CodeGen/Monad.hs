{-# LANGUAGE OverloadedStrings #-}
module GHCVM.CodeGen.Monad
  (CgEnv(..),
   CgState(..),
   CodeGen(..),
   crashDoc,
   debugDoc,
   debugState,
   debug,
   withSequel,
   emit,
   initCg,
   getCgLoc,
   getCodeWithResult,
   newTemp,
   newIdLoc,
   peekNextLocal,
   setNextLocal,
   newLocal,
   newLabel,
   setNextLabel,
   getSequel,
   getSelfLoop,
   setSuperClass,
   getSuperClass,
   setClosureClass,
   withSelfLoop,
   withMethod,
   getModClass,
   getClass,
   addBinding,
   addBindings,
   setBindings,
   printBindings,
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
   forkClosureBody,
   forkLneBody,
   forkAlts,
   unimplemented,
   getDynFlags)
where

import GHCVM.Main.DynFlags
import GHCVM.BasicTypes.Module
import GHCVM.BasicTypes.VarEnv
import GHCVM.BasicTypes.Id
import GHCVM.BasicTypes.Name
import GHCVM.Utils.Outputable hiding ((<>))
import GHCVM.Utils.FastString
import GHCVM.Types.TyCon

import Data.Monoid((<>))
import Data.List
import Data.Maybe (fromMaybe)
import Data.Text hiding (foldl, length, concatMap, map, intercalate)

import Control.Monad (liftM, ap, when, forM)
import Control.Monad.State (MonadState(..), get, gets, modify)
import Control.Monad.Reader (MonadReader(..), ask, asks, local)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import Codec.JVM

import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Closure
import GHCVM.CodeGen.Name
import GHCVM.CodeGen.ArgRep
import GHCVM.Debug
import GHCVM.Util
import GHCVM.Utils.Digraph

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
          , cgClassInitCode    :: ![Node FieldRef Code]
          -- Top-level definitions
          , cgAccessFlags    :: [AccessFlag]
          , cgMethodDefs     :: ![MethodDef]
          , cgFieldDefs      :: ![FieldDef]
          , cgClassName      :: !Text
          , cgSuperClassName :: !(Maybe Text)
          -- Current method
          , cgCode           :: !Code
          , cgNextLocal      :: Int
          , cgNextLabel      :: Int }

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
           , cgSuperClassName   = Nothing
           , cgNextLocal        = 0
           , cgNextLabel        = 0 })
  where className = moduleJavaClass mod

emit :: Code -> CodeGen ()
emit code = modify $ \s@CgState { cgCode } -> s { cgCode = cgCode <> code }

peekNextLocal :: CodeGen Int
peekNextLocal = gets cgNextLocal

peekNextLabel :: CodeGen Int
peekNextLabel = gets cgNextLabel

newLabel :: CodeGen Label
newLabel = do
  next <- peekNextLabel
  modify $ \s@CgState { cgNextLabel } ->
             s { cgNextLabel = cgNextLabel + 1}
  return $ mkLabel next

newLocal :: FieldType -> CodeGen Int
newLocal ft = do
  next <- peekNextLocal
  modify $ \s@CgState { cgNextLocal } ->
             s { cgNextLocal = cgNextLocal + fieldSz}
  return next
  where fieldSz = fieldSize ft

setNextLocal :: Int -> CodeGen ()
setNextLocal n = modify $ \s -> s { cgNextLocal = n }

setNextLabel :: Int -> CodeGen ()
setNextLabel n = modify $ \s -> s { cgNextLabel = n }

getMethodCode :: CodeGen Code
getMethodCode = gets cgCode

setMethodCode :: Code -> CodeGen ()
setMethodCode code = modify $ \s -> s { cgCode = code }

getClass :: CodeGen Text
getClass = gets cgClassName

getModClass :: CodeGen Text
getModClass = asks cgQClassName

getInitCode :: CodeGen Code
getInitCode = gets (foldMap (\(a, _, _) -> a) . flattenSCCs . stronglyConnCompG . graphFromEdgedVertices . cgClassInitCode)

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
      curMod <- getModule
      let name = idName id
      -- TODO: Change this back.
      let mod = fromMaybe (pprPanic "getCgIdInfo: no module" (ppr id)) $ nameModule_maybe name
      --let mod = fromMaybe curMod $ nameModule_maybe name
      dflags <- getDynFlags
      if mod /= curMod then return . mkCgIdInfo dflags id $ mkLFImported id
      else return . mkCgIdInfo dflags id $ mkLFImported id
      -- TODO: Change this back.
      -- crashDoc $ str "getCgIdInfo[not external name]:" <+> ppr id

printBindings :: CodeGen ()
printBindings = do
  bindings <- getBindings
  debugDoc $ str "printBindings" <+> ppr bindings

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
  -> CodeGen a
  -> CodeGen (a, CgState)
newExportedClosure = newClosure [Public, Super, Final]
newHiddenClosure = newClosure [Private, Super, Final]

newTypeClosure
  :: Text
  -> Text
  -> CodeGen ((), CgState)
newTypeClosure thisClass superClass =
  newClosure [Public, Abstract, Super] thisClass superClass $
    defineMethod $ mkDefaultConstructor thisClass superClass

newClosure
  :: [AccessFlag]
  -> Text
  -> Text
  -> CodeGen a
  -> CodeGen (a, CgState)
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
getSuperClass = fmap (expectJust "getSuperClass") . gets $ cgSuperClassName

setClosureClass :: Text -> CodeGen ()
setClosureClass clName = do
  modClass <- getModClass
  let qClName = qualifiedName modClass clName
  modify $ \s -> s { cgClassName = qClName }

-- NOTE: Changes made to class generation state are forgotten after
--       the body is executed
newClosureGeneric :: CodeGen a -> CodeGen (a, CgState)
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
  result <- genCode
  state1 <- get
  let compiledClosure = classFromCgState state1
  -- TODO: Ensure the state is restored properly
  modify $ \s -> s { cgAccessFlags = a
                   , cgMethodDefs = b
                   , cgFieldDefs = c
                   , cgClassName = d
                   , cgSuperClassName = e }
  addCompiledClosure compiledClosure
  return (result, state1)

classFromCgState :: CgState -> ClassFile
classFromCgState CgState {..} =
  mkClassFile java7 cgAccessFlags cgClassName cgSuperClassName []
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

addInitStep :: Node FieldRef Code -> CodeGen ()
addInitStep code = modify $ \s@CgState{..} ->
  s { cgClassInitCode = cgClassInitCode <> [ code ] }

-- NOTE: New bindings generated by the body are forgotten after
--       the body is executed
forkClosureBody :: CodeGen a -> CodeGen (a, CgState)
forkClosureBody body =
  local (\env -> env { cgSequel = Return
                     , cgSelfLoop = Nothing })
       . newClosureGeneric $ do
    oldBindings <- getBindings
    result <- body
    setBindings oldBindings
    return result

withMethod :: [AccessFlag] -> Text -> [FieldType] -> ReturnType -> CodeGen () -> CodeGen MethodDef
withMethod accessFlags name fts rt body = do
  oldCode <- getMethodCode
  oldNextLocal <- peekNextLocal
  oldNextLabel <- peekNextLabel
  setMethodCode mempty
  setNextLocal 2
  setNextLabel 0
  body
  emit vreturn
  clsName <- getClass
  newCode <- getMethodCode
  let methodDef = mkMethodDef clsName accessFlags name fts rt newCode
  defineMethod methodDef
  setMethodCode oldCode
  setNextLocal oldNextLocal
  setNextLabel oldNextLabel
  return methodDef

withSelfLoop :: SelfLoopInfo -> CodeGen a -> CodeGen a
withSelfLoop selfLoopInfo =
  local (\env -> env { cgSelfLoop = Just selfLoopInfo })

unimplemented :: String -> CodeGen a
unimplemented msg = do
  liftIO . putStrLn $ "Not implemented: " ++ msg
  return undefined

getSequel :: CodeGen Sequel
getSequel = asks cgSequel

getSelfLoop :: CodeGen (Maybe SelfLoopInfo)
getSelfLoop = asks cgSelfLoop

newTemp :: Bool -> FieldType -> CodeGen CgLoc
newTemp isClosure ft = do
  n <- newLocal ft
  return $ LocLocal isClosure ft n

-- TODO: Verify that this does as intended
getCodeWithResult :: CodeGen a -> CodeGen (a, Code)
getCodeWithResult gen = do
  state1 <- get
  modify $ \s -> s { cgCode = mempty }
  a <- gen
  state2 <- get
  put $ state2 { cgCode = cgCode state1 }
  return (a, cgCode state2)

newIdLoc :: NonVoid Id -> CodeGen CgLoc
newIdLoc (NonVoid id) = newTemp (isGcPtrRep rep) (primRepFieldType rep)
  where rep = idPrimRep id

getCgLoc :: NonVoid Id -> CodeGen CgLoc
getCgLoc (NonVoid id) = do
  info <- getCgIdInfo id
  return $ cgLocation info

forkAlts :: [(a, CodeGen ())] -> CodeGen [(a, Code)]
forkAlts alts =
  forM alts $ \(val, altCode) -> do
    code <- forkLneBody altCode
    return (val, code)

withSequel :: Sequel -> CodeGen a -> CodeGen a
withSequel sequel = local (\env -> env { cgSequel = sequel })

forkLneBody :: CodeGen () -> CodeGen Code
forkLneBody body = do
  oldBindings <- getBindings
  oldCode <- getMethodCode
  oldNextLocal <- peekNextLocal
  setMethodCode mempty
  body
  newCode <- getMethodCode
  setMethodCode oldCode
  setNextLocal oldNextLocal
  setBindings oldBindings
  return newCode

debug :: String -> CodeGen ()
debug msg = do
  dflags <- getDynFlags
  when (verbosity dflags > 1) $
    liftIO $ putStrLn msg

debugDoc :: SDoc -> CodeGen ()
debugDoc sdoc = do
  dflags <- getDynFlags
  when (verbosity dflags > 1) $
    liftIO . putStrLn $ showSDocDump dflags sdoc

debugState :: CodeGen ()
debugState = do
  dflags <- getDynFlags
  bindings <- getBindings
  when (verbosity dflags > 1) $
    debugDoc $ str "cgBindings: " <+> ppr bindings

crashDoc :: SDoc -> CodeGen a
crashDoc sdoc = do
  debugDoc sdoc
  error "crash"
