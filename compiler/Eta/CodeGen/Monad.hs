{-# LANGUAGE OverloadedStrings #-}
module Eta.CodeGen.Monad
  (CgEnv(..),
   CgState(..),
   CodeGen(..),
   crashDoc,
   traceCg,
   printDoc,
   debugState,
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
   newRecursiveInitNumber,
   getSequel,
   getSelfLoop,
   setClass,
   withModClass,
   setSuperClass,
   getSuperClass,
   setClosureClass,
   withSelfLoop,
   withMethod,
   getModClass,
   getClass,
   getContextLoc,
   setContextLoc,
   extendNameEnv,
   newDedupedId,
   addBinding,
   addBindings,
   getBindings,
   setBindings,
   forbidScoping,
   getScopedBindings,
   addScopedBinding,
   getAllowScoping,
   getPreserveCaseOfCase,
   preserveCaseOfCase,
   setPreserveCaseOfCase,
   printBindings,
   defineMethod,
   defineStaticMethod,
   defineField,
   defineFields,
   getCgIdInfo,
   newTypeClosure,
   newDataClosure,
   newClosure,
   defineSingletonInstance,
   classFromCgState,
   runCodeGen,
   forkClosureBody,
   forkLneBody,
   forkLneBodyNoLocals,
   forkAlts,
   unimplemented,
   getDynFlags,
   getHscEnv,
   getSourceFilePath,
   liftIO,
   dumpDedupedIds)
where

import Eta.Main.DynFlags
import Eta.Main.ErrUtils
import Eta.BasicTypes.Module
import Eta.BasicTypes.VarEnv
import Eta.BasicTypes.Id
import Eta.BasicTypes.Name
import Eta.BasicTypes.Unique
import Eta.Utils.Outputable hiding ((<>))
import Eta.Types.TyCon

import Data.Monoid((<>))
import Data.List
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text hiding (foldl, length, concatMap, map, intercalate, findIndex, toLower, zip)

import System.FilePath (takeFileName)

import Control.Monad (liftM, ap, when, forM)
import Control.Monad.State (MonadState(..), get, gets, modify)
import Control.Monad.Reader (MonadReader(..), ask, asks, local)
import Control.Monad.IO.Class
import Codec.JVM

import Eta.REPL.Linker
import Eta.Main.HscTypes
import Eta.CodeGen.Types
import Eta.CodeGen.Closure
import Eta.CodeGen.Name
import Eta.CodeGen.ArgRep
import Eta.CodeGen.Rts
import Eta.Debug
import Eta.Utils.Util

data CgEnv =
  CgEnv { cgQClassName :: !Text
        , cgModule     :: !Module
        , cgHscEnv     :: !HscEnv
        , cgDynFlags   :: !DynFlags
        , cgSequel     :: !Sequel
        , cgSelfLoop   :: !(Maybe SelfLoopInfo) }

data CgState =
  CgState { cgBindings            :: !CgBindings
          , cgNameEnvironment     :: FastStringEnv Int
          , cgIdDocs              :: [SDoc]
          -- Accumulating
          , cgCompiledClosures    :: [ClassFile]
          , cgRecursiveInitNumber :: Int
          , cgStaticMethodDefs    :: [MethodDef]
          -- Top-level definitions
          , cgAccessFlags         :: ![AccessFlag]
          , cgMethodDefs          :: ![MethodDef]
          , cgFieldDefs           :: ![FieldDef]
          , cgClassName           :: !Text
          , cgSuperClassName      :: !(Maybe Text)
          , cgSourceFilePath      :: !(Maybe FilePath)
          -- Current method
          , cgCode                :: !Code
          , cgContextLoc          :: Code
          , cgScopedBindings      :: CgBindings
          , cgAllowScoping        :: Bool
          , cgPreserveCaseOfCase  :: Bool
          , cgNextLocal           :: Int
          , cgNextLabel           :: Int }

instance Show CgState where
  show CgState {..} = "cgClassName: "         ++ show cgClassName      ++ "\n"
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

getHscEnv :: CodeGen HscEnv
getHscEnv = asks cgHscEnv

initCg :: HscEnv -> Module -> ModLocation -> (CgEnv, CgState)
initCg hsc_env mod modLoc =
  (CgEnv   { cgModule              = mod
           , cgQClassName          = className
           , cgDynFlags            = dflags
           , cgHscEnv              = hsc_env
           , cgSequel              = Return
           , cgSelfLoop            = Nothing },
   CgState { cgBindings            = emptyVarEnv
           , cgNameEnvironment     = emptyFsEnv
           , cgIdDocs              = []
           , cgCode                = mempty
           , cgContextLoc          = mempty
           , cgAccessFlags         = [Public, Super]
           , cgMethodDefs          = []
           , cgFieldDefs           = []
           , cgClassName           = className
           , cgSourceFilePath      = srcFilePath
           , cgCompiledClosures    = []
           , cgRecursiveInitNumber = 0
           , cgSuperClassName      = Nothing
           , cgScopedBindings      = emptyVarEnv
           , cgStaticMethodDefs    = []
           , cgAllowScoping        = True
           , cgPreserveCaseOfCase  = False
           , cgNextLocal           = 0
           , cgNextLabel           = 0 })
  where className = moduleJavaClass mod
        srcFilePath = ml_hs_file modLoc
        dflags = hsc_dflags hsc_env

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
             s { cgNextLabel = cgNextLabel + 1 }
  return $ mkLabel next

newLocal :: FieldType -> CodeGen Int
newLocal ft = do
  next <- peekNextLocal
  modify $ \s@CgState { cgNextLocal } ->
             s { cgNextLocal = cgNextLocal + fieldSz }
  return next
  where fieldSz = fieldSize ft

setNextLocal :: Int -> CodeGen ()
setNextLocal n = modify $ \s -> s { cgNextLocal = n }

setNextLabel :: Int -> CodeGen ()
setNextLabel n = modify $ \s -> s { cgNextLabel = n }

newRecursiveInitNumber :: CodeGen Int
newRecursiveInitNumber = do
  recInitNo <- gets cgRecursiveInitNumber
  modify $ \s -> s { cgRecursiveInitNumber = recInitNo + 1 }
  return recInitNo

getMethodCode :: CodeGen Code
getMethodCode = gets cgCode

setMethodCode :: Code -> CodeGen ()
setMethodCode code = modify $ \s -> s { cgCode = code }

getContextLoc :: CodeGen Code
getContextLoc = gets cgContextLoc

setContextLoc :: Code -> CodeGen ()
setContextLoc code = modify $ \s -> s { cgContextLoc = code }

getClass :: CodeGen Text
getClass = gets cgClassName

getModClass :: CodeGen Text
getModClass = asks cgQClassName

getBindings :: CodeGen CgBindings
getBindings = gets cgBindings

setBindings :: CgBindings -> CodeGen ()
setBindings bindings = modify $ \s -> s { cgBindings = bindings }

getScopedBindings :: CodeGen CgBindings
getScopedBindings = gets cgScopedBindings

addScopedBinding :: CgIdInfo -> CodeGen ()
addScopedBinding cgIdInfo = do
  bindings <- getScopedBindings
  setScopedBindings $ extendVarEnv bindings (cgId cgIdInfo) cgIdInfo

setScopedBindings :: CgBindings -> CodeGen ()
setScopedBindings bindings = modify $ \s -> s { cgScopedBindings = bindings }

getAllowScoping :: CodeGen Bool
getAllowScoping = gets cgAllowScoping

forbidScoping :: CodeGen a -> CodeGen a
forbidScoping cg = do
  oldScoping <- getAllowScoping
  setAllowScoping False
  r <- cg
  setAllowScoping oldScoping
  return r

setAllowScoping :: Bool -> CodeGen ()
setAllowScoping bool = modify $ \s -> s { cgAllowScoping = bool }

getPreserveCaseOfCase :: CodeGen Bool
getPreserveCaseOfCase = gets cgPreserveCaseOfCase

preserveCaseOfCase :: CodeGen a -> CodeGen a
preserveCaseOfCase cg = do
  oldPreserve <- getPreserveCaseOfCase
  setPreserveCaseOfCase True
  r <- cg
  setPreserveCaseOfCase oldPreserve
  return r

setPreserveCaseOfCase :: Bool -> CodeGen ()
setPreserveCaseOfCase bool = modify $ \s -> s { cgPreserveCaseOfCase = bool }

getCgIdInfo :: Id -> CodeGen CgIdInfo
getCgIdInfo id = do
  hsc_env <- getHscEnv
  let dflags = hsc_dflags hsc_env
  localBindings <- getBindings
  case lookupVarEnv localBindings id of
    Just info -> return info
    Nothing
      | HscInterpreted <- hscTarget dflags -> do
          href <- liftIO $ getHValueAsInt hsc_env (idName id)
          case href of
            Just href' -> do
              traceCg $ "getCgIdInfo[StablePtr]:" <+> ppr id <+> ppr href'
              return $ mkCgIdInfoWithLoc id (mkLFImported id) $ mkStablePtrLoc href'
            Nothing -> defaultResult dflags
      | otherwise -> defaultResult dflags
  where defaultResult dflags = do
          curMod <- getModule
          let name = idName id
          let mod  = fromMaybe (pprPanic "getCgIdInfo: no module" (ppr id))
                  $ nameModule_maybe name
          if mod /= curMod then return . mkCgIdInfo dflags id Nothing $ mkLFImported id
          else return . mkCgIdInfo dflags id Nothing $ mkLFImported id

printBindings :: CodeGen ()
printBindings = do
  bindings <- getBindings
  traceCg $ str "printBindings" <+> ppr bindings

extendNameEnv :: [Id] -> CodeGen ()
extendNameEnv ids = modify $ \s@(CgState { cgNameEnvironment = nameEnv }) ->
                              s { cgNameEnvironment = extendFsEnvWith nameEnv fss }
  where fss = zip (map idFastString ids) (repeat 1)

-- Used for deterministic class naming
newDedupedId :: Id -> CodeGen Id
newDedupedId id = do
  mod <- getModule
  nameEnv <- gets cgNameEnvironment
  let (!id', !i')
        | Just i <- lookupFsEnv nameEnv fs = (transformedId mod i, i + 1)
        | otherwise = (id, 1)
  modify $ \s -> s { cgNameEnvironment = extendFsEnv nameEnv fs i' }
  addIdDoc id id'
  return id'
  where fs = idFastString id
        transformedId mod i = id'
          where id' = setIdName id $ if isInternalName name
                                     then mkInternalName uniq occ' loc
                                     else mkExternalName uniq mod occ' loc
                occ' = mkOccName ns (occNameString occ ++ "$" ++ show i)
                name = idName id
                uniq = nameUnique name
                occ  = nameOccName name
                loc  = nameSrcSpan name
                ns   = occNameSpace occ

addIdDoc :: Id -> Id -> CodeGen ()
addIdDoc id0 id1 = modify $ \s@CgState{..} ->
  s { cgIdDocs = (hcat [ppr (idOcc id0), underscore, pprUniqueAlways (idUnique id0)]
              <+> arrow <+> ppr (idOcc id1)) : cgIdDocs }
  where idOcc = nameOccName . idName

dumpDedupedIds :: CodeGen ()
dumpDedupedIds = do
  dflags <- getDynFlags
  CgState { cgIdDocs } <- get
  liftIO $ dumpIfSet_dyn dflags Opt_D_dump_stg "STG Simplified Ids:"
             (vcat (Data.List.reverse cgIdDocs))

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

addCompiledClosure :: ClassFile -> CodeGen ()
addCompiledClosure classFile = modify $ \s@CgState{..} ->
  s { cgCompiledClosures = classFile : cgCompiledClosures }

defineMethod :: MethodDef -> CodeGen ()
defineMethod md = modify $ \s@CgState{..} ->
  s { cgMethodDefs = md : cgMethodDefs }

defineStaticMethod :: MethodDef -> CodeGen ()
defineStaticMethod md = modify $ \s@CgState{..} ->
  s { cgStaticMethodDefs = md : cgStaticMethodDefs }

defineField :: FieldDef -> CodeGen ()
defineField md = modify $ \s@CgState{..} ->
  s { cgFieldDefs = md : cgFieldDefs }

defineFields :: [FieldDef] -> CodeGen ()
defineFields md = modify $ \s@CgState{..} ->
  s { cgFieldDefs = md ++ cgFieldDefs }

newDataClosure
  :: Text
  -> Text
  -> CodeGen a
  -> CodeGen (a, CgState)
newDataClosure = newClosure [Public, Super, Final]

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
    setClass clName
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
  setClass (qualifiedName modClass clName)

setClass :: Text -> CodeGen ()
setClass clName = modify $ \s -> s { cgClassName = clName }

withModClass :: CodeGen a -> CodeGen a
withModClass codeGen = do
  cls' <- getClass
  getModClass >>= setClass
  r <- codeGen
  setClass cls'
  return r

-- NOTE: Changes made to class generation state are forgotten after
--       the body is executed
newClosureGeneric :: CodeGen a -> CodeGen (a, CgState)
newClosureGeneric genCode = do
  CgState
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
  let compiledClosure = classFromCgState [] [] state1
  -- TODO: Ensure the state is restored properly
  modify $ \s -> s { cgAccessFlags = a
                   , cgMethodDefs = b
                   , cgFieldDefs = c
                   , cgClassName = d
                   , cgSuperClassName = e }
  addCompiledClosure compiledClosure
  return (result, state1)

classFromCgState :: [MethodDef] -> [FieldDef] -> CgState
                 -> ClassFile
classFromCgState mds fds CgState {..} =
  mkClassFileWithAttrs java7 cgAccessFlags cgClassName cgSuperClassName []
    (cgFieldDefs ++ fds) srcFile (cgMethodDefs ++ mds) filterDataTyCons
  where srcFile = maybeToList $ sourceFileAttr cgSourceFilePath
        sourceFileAttr = fmap (mkSourceFileAttr . pack . takeFileName)

runCodeGen :: Maybe ([MethodDef], [FieldDef])
           -> CgEnv -> CgState -> CodeGen a -> IO [ClassFile]
runCodeGen mMFs env state codeGenAction = do
  (state'@CgState {..}, _) <- unCG codeGenAction env state

  -- NOTE: addInnerClasses is to ensure that any unused data types/closures
  --       are added to the constant pool
  let compiledModuleClass =
        addInnerClasses filterDataTyCons cgCompiledClosures $ do
          let (mds, fds) = case mMFs of
                Just (mds, fds) -> (mds, fds)
                Nothing -> ([], [])
          classFromCgState (mds ++ cgStaticMethodDefs) fds state'

  return (compiledModuleClass : cgCompiledClosures)

forkClosureBody :: CodeGen a -> CodeGen (a, CgState)
forkClosureBody body =
  local (\env -> env { cgSequel = Return
                     , cgSelfLoop = Nothing })
       . newClosureGeneric $ do
    oldBindings <- getBindings
    result <- body
    setBindings oldBindings
    return result

withMethod :: [AccessFlag] -> Text -> [FieldType] -> ReturnType -> CodeGen () -> CodeGen ()
withMethod accessFlags name fts rt body = do
  oldCode        <- getMethodCode
  oldNextLocal   <- peekNextLocal
  oldNextLabel   <- peekNextLabel
  scopedBindings <- getScopedBindings
  scoping        <- getAllowScoping
  preserve       <- getPreserveCaseOfCase
  contextLoc'    <- getContextLoc
  setMethodCode mempty
  setNextLocal (staticOffset + sum (map fieldSize fts))
  setContextLoc contextLoc
  setNextLabel 0
  setScopedBindings emptyVarEnv
  setAllowScoping True
  setPreserveCaseOfCase False
  body
  modClass <- getModClass
  clsName  <- getClass
  newCode  <- getMethodCode
  let methodDef = mkMethodDef clsName accessFlags name fts rt newCode
      defineMeth
        | modClass == clsName = defineStaticMethod
        | otherwise           = defineMethod
  defineMeth methodDef
  setMethodCode oldCode
  setNextLocal oldNextLocal
  setNextLabel oldNextLabel
  setScopedBindings scopedBindings
  setAllowScoping scoping
  setPreserveCaseOfCase preserve
  setContextLoc contextLoc'
  where staticOffset
          | Static `elem` accessFlags = 0
          | otherwise                 = 1
        contextLoc = gload contextType contextLocIndex
        contextLocIndex
          | Just n <- findIndex (== contextType) fts
          = staticOffset + n
          | otherwise = 0

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
  return $ mkLocLocal isClosure ft n

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
forkAlts [(val, altCode)] = do
  preserve <- getPreserveCaseOfCase
  let fork
        | preserve  = forkLneBodyNoLocals
        | otherwise = forkLneBody
  code <- fork altCode
  return [(val, code)]
forkAlts alts = do
  preserve <- getPreserveCaseOfCase
  when preserve $
    setPreserveCaseOfCase False
  forM alts $ \(val, altCode) -> do
    code <- forkLneBody altCode
    return (val, code)

withSequel :: Sequel -> CodeGen a -> CodeGen a
withSequel sequel = local (\env -> env { cgSequel = sequel, cgSelfLoop = Nothing })

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

forkLneBodyNoLocals :: CodeGen () -> CodeGen Code
forkLneBodyNoLocals body = do
  oldBindings <- getBindings
  oldCode <- getMethodCode
  setMethodCode mempty
  body
  newCode <- getMethodCode
  setMethodCode oldCode
  setBindings oldBindings
  return newCode

getSourceFilePath :: CodeGen (Maybe FilePath)
getSourceFilePath = gets cgSourceFilePath

traceCg :: SDoc -> CodeGen ()
traceCg sdoc = do
  dflags   <- getDynFlags
  when (dopt Opt_D_dump_cg_trace dflags) $
    liftIO $ dumpSDoc dflags neverQualify Opt_D_dump_cg_trace "" sdoc

printDoc :: SDoc -> CodeGen ()
printDoc sdoc = do
  dflags <- getDynFlags
  liftIO . putStrLn $ showSDocDump dflags sdoc

debugState :: CodeGen ()
debugState = do
  dflags <- getDynFlags
  bindings <- getBindings
  when (verbosity dflags > 1) $
    traceCg $ str "cgBindings: " <+> ppr bindings

crashDoc :: SDoc -> CodeGen a
crashDoc sdoc = do
  traceCg sdoc
  error "crash"

defineSingletonInstance :: Text -> CodeGen ()
defineSingletonInstance thisClass = do
  defineField  $ mkFieldDef [Public, Static] singletonInstanceName thisFt
  defineMethod $ mkMethodDef thisClass [Static] "<clinit>" [] void $
       new thisFt
    <> dup thisFt
    <> invokespecial (mkMethodRef thisClass "<init>" [] void)
    <> putstatic (mkFieldRef thisClass singletonInstanceName thisFt)
    <> vreturn
  where thisFt = obj thisClass
