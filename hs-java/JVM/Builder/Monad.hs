{-# LANGUAGE FlexibleContexts, TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables, NamedFieldPuns, RecordWildCards #-}
-- | This module defines Generate[IO] monad, which helps generating JVM code and
-- creating Java class constants pool.
--
-- Code generation could be done using one of two monads: Generate and GenerateIO.
-- Generate monad is pure (simply State monad), while GenerateIO is IO-related.
-- In GenerateIO additional actions are available, such as setting up ClassPath
-- and loading classes (from .class files or JAR archives).
--
module JVM.Builder.Monad
  (GState (..),
   emptyGState,
   Generator (..),
   Generate, GenerateIO,
   addToPool,
   i0, i1, i8,
   newMethod,
   newField,
   setStackSize, setMaxLocals,
   setClass, setSuper,
   withClassPath,
   getClassField, getClassMethod,
   generate, generateIO,
   generateCodeLength
  ) where

import Control.Monad.State as St
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Data.Word
import Data.Binary
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

import JVM.Types
import JVM.Common
import JVM.ClassFile
import JVM.Assembler
import JVM.Exceptions
import Java.ClassPath
import JVM.Attributes

-- | NOTE: The use of Maps for gsFields and gsMethods prevents you from generating overloaded methods.
-- | Generator state
data GState = GState {
  gsClassName :: B.ByteString,
  gsSuperClassName :: B.ByteString,
  gsGenerated :: [Instruction],             -- ^ Already generated code (in current method)
  gsPool :: Pool Direct,             -- ^ Already generated constants pool
  gsPoolIndex :: Word16,                -- ^ Next index to be used in constants pool
  gsFields :: M.Map B.ByteString (Field Direct),
  gsInitializationCode :: M.Map B.ByteString [Instruction],
  gsMethods :: M.Map B.ByteString (Method Direct),         -- ^ Already generated class methods
  gsCurrentMethod :: Maybe (Method Direct), -- ^ Current method
  gsStackSize :: Word16,                    -- ^ Maximum stack size for current method
  gsLocals :: Word16,                       -- ^ Maximum number of local variables for current method
  gsClassPath :: [Tree CPEntry],
  gsClassAttributes :: Attributes Direct,
  gsAccessFlags :: AccessFlags Direct,
  gsInnerClasses :: M.Map B.ByteString GState }
  deriving (Eq,Show)

addAccessFlag :: (Generator e g) => AccessFlag -> g e ()
addAccessFlag flag =
  St.modify (\s -> s { gsAccessFlags = S.insert flag (gsAccessFlags s)})

-- | Empty generator state
emptyGState ::  GState
emptyGState = GState {
  gsClassName = B.empty,
  gsSuperClassName = "java/lang/Object",
  gsAccessFlags = S.fromList [ACC_PUBLIC, ACC_SYNCHRONIZED],
  gsGenerated = [],
  gsPool = M.empty,
  gsPoolIndex = 1,
  gsFields = M.empty,
  gsInitializationCode = M.empty,
  gsMethods = M.empty,
  gsCurrentMethod = Nothing,
  gsStackSize = 496,
  gsLocals = 0,
  gsClassPath = [],
  gsClassAttributes = M.empty }

class (Monad (g e), MonadState GState (g e)) => Generator e g where
  throwG :: (Exception x, Throws x e) => x -> g e a

-- | Generate monad
newtype Generate e a = Generate {
  runGenerate :: EMT e (State GState) a }
  deriving (Functor, Applicative, Monad, MonadState GState)

instance MonadState st (EMT e (StateT st IO)) where
  get = lift St.get
  put x = lift (St.put x)

instance MonadState st (EMT e (State st)) where
  get = lift St.get
  put x = lift (St.put x)

-- | IO version of Generate monad
newtype GenerateIO e a = GenerateIO {
  runGenerateIO :: EMT e (StateT GState IO) a }
  deriving (Functor, Applicative, Monad, MonadState GState, MonadIO)

instance MonadIO (EMT e (StateT GState IO)) where
  liftIO action = lift $ liftIO action

instance Generator e GenerateIO where
  throwG e = GenerateIO (throw e)

instance (MonadState GState (EMT e (State GState))) => Generator e Generate where
  throwG e = Generate (throw e)

execGenerateIO :: [Tree CPEntry]
               -> GenerateIO (Caught SomeException NoExceptions) a
               -> IO GState
execGenerateIO cp (GenerateIO emt) = do
    let caught = emt `catch` (\(e :: SomeException) -> fail $ show e)
    execStateT (runEMT caught) (emptyGState {gsClassPath = cp})

execGenerate :: [Tree CPEntry]
             -> Generate (Caught SomeException NoExceptions) a
             -> GState
execGenerate cp (Generate emt) = do
    let caught = emt `catch` (\(e :: SomeException) -> fail $ show e)
    execState (runEMT caught) (emptyGState {gsClassPath = cp})

-- | Update ClassPath
withClassPath :: ClassPath () -> GenerateIO e ()
withClassPath cp = do
  res <- liftIO $ execClassPath cp
  st <- St.get
  St.put $ st {gsClassPath = res}

-- | Add a constant to pool
addItem :: (Generator e g) => Constant Direct -> g e Word16
addItem c = do
  pool <- St.gets gsPool
  case lookupPool c pool of
    Just i -> return i
    Nothing -> do
      i <- St.gets gsPoolIndex
      let pool' = M.insert i c pool
          i' = if long c
                 then i+2
                 else i+1
      st <- St.get
      St.put $ st {gsPool = pool',
                   gsPoolIndex = i'}
      return i

-- | Lookup in a pool
lookupPool :: Constant Direct -> Pool Direct -> Maybe Word16
lookupPool c pool =
  fromIntegral `fmap` mapFindIndex (== c) pool

addNT :: (Generator e g, HasSignature a) => NameType a -> g e Word16
addNT (NameType name sig) = do
  let bsig = encode sig
  x <- addItem (CNameType name bsig)
  addItem (CUTF8 name)
  addItem (CUTF8 bsig)
  return x

addSig :: (Generator e g) => MethodSignature -> g e Word16
addSig c@(MethodSignature args ret) = do
  let bsig = encode c
  addItem (CUTF8 bsig)

-- | Add a constant into pool
addToPool :: (Generator e g) => Constant Direct -> g e Word16
addToPool c@(CClass str) = do
  addItem (CUTF8 str)
  addItem c
addToPool c@(CField cls name) = do
  addToPool (CClass cls)
  addNT name
  addItem c
addToPool c@(CMethod cls name) = do
  addToPool (CClass cls)
  addNT name
  addItem c
addToPool c@(CIfaceMethod cls name) = do
  addToPool (CClass cls)
  addNT name
  addItem c
addToPool c@(CString str) = do
  addToPool (CUTF8 str)
  addItem c
addToPool c@(CNameType name sig) = do
  addItem (CUTF8 name)
  addItem (CUTF8 sig)
  addItem c
addToPool c = addItem c

addAttributeToPool :: (Generator e g) => Attribute -> g e Word16
addAttributeToPool attribute = addUTF8 (attributeNameString attribute)

addUTF8 :: (Generator e g) => B.ByteString -> g e Word16
addUTF8 = addItem . CUTF8

putInstruction :: (Generator e g) => Instruction -> g e ()
putInstruction instr = do
  st <- St.get
  let code = gsGenerated st
  St.put $ st {gsGenerated = code ++ [instr]}

-- | Generate one (zero-arguments) instruction
i0 :: (Generator e g) => Instruction -> g e ()
i0 = putInstruction

-- | Generate one one-argument instruction
i1 :: (Generator e g) => (Word16 -> Instruction) -> Constant Direct -> g e ()
i1 fn c = do
  ix <- addToPool c
  i0 (fn ix)

-- | Generate one one-argument instruction
i8 :: (Generator e g) => (Word8 -> Instruction) -> Constant Direct -> g e ()
i8 fn c = do
  ix <- addToPool c
  i0 (fn $ fromIntegral ix)

-- | Set class name
setClass :: (Generator e g) => B.ByteString -> g e ()
setClass name = St.modify (\s -> s { gsClassName = name })

-- | Set the super class
setSuper :: (Generator e g) => B.ByteString -> g e ()
setSuper name = St.modify (\s -> s { gsSuperClassName = name })

-- | Set maximum stack size for current method
setStackSize :: (Generator e g) => Word16 -> g e ()
setStackSize n = do
  st <- St.get
  St.put $ st {gsStackSize = n}

-- | Set maximum number of local variables for current method
setMaxLocals :: (Generator e g) => Word16 -> g e ()
setMaxLocals n = do
  st <- St.get
  St.put $ st {gsLocals = n}

-- | Start generating new method
startMethod :: (Generator e g) => [AccessFlag] -> B.ByteString -> MethodSignature -> g e ()
startMethod flags name sig = do
  addToPool (CString name)
  addSig sig
  setStackSize 4096
  setMaxLocals 100
  st <- St.get
  let method = Method {
    methodAccessFlags = S.fromList flags,
    methodName = name,
    methodSignature = sig,
    methodAttributesCount = 0,
    methodAttributes = M.empty }
  St.put $ st {gsGenerated = [],
               gsCurrentMethod = Just method }

-- | End of method generation
endMethod :: (Generator e g, Throws UnexpectedEndMethod e) => g e ()
endMethod = do
  m <- St.gets gsCurrentMethod
  codeAttribute <- St.gets genCode
  case m of
    Nothing -> throwG UnexpectedEndMethod
    Just method@Method {..} -> do
      let method' = method {
            methodAttributes = insertAttribute codeAttribute methodAttributes,
            methodAttributesCount = methodAttributesCount + 1}
      St.modify (\s -> s {
                    gsGenerated = [],
                    gsCurrentMethod = Nothing,
                    gsMethods = M.insert methodName method' (gsMethods s)})

-- | Generate new method
newMethod :: (Generator e g, Throws UnexpectedEndMethod e)
          => [AccessFlag]        -- ^ Access flags for method (public, static etc)
          -> B.ByteString        -- ^ Method name
          -> [ArgumentSignature] -- ^ Signatures of method arguments
          -> ReturnSignature     -- ^ Method return signature
          -> g e ()                -- ^ Generator for method code
          -> g e (NameType (Method Direct))
newMethod flags name args ret gen = do
  let sig = MethodSignature args ret
  startMethod flags name sig
  gen
  endMethod
  return (NameType name sig)

-- | Generate new field
newField :: (Generator e g)
         => [AccessFlag]     -- ^ Access flags
         -> B.ByteString     -- ^ Field name
         -> FieldSignature   -- ^ Field signature
         -> Maybe [Instruction]    -- ^ Initialization code
         -> g e ()
newField flags name sig code = do
  st@GState { gsFields, gsInitializationCode } <- St.get
  let field = Field {
        fieldAccessFlags = S.fromList flags,
        fieldName = name,
        fieldSignature = sig,
        fieldAttributesCount = 0,
        fieldAttributes = def }
  St.put $ st { gsFields = M.insert name field gsFields,
                gsInitializationCode = maybe gsInitializationCode
                                             (\x -> M.insert name x gsInitializationCode)
                                             code }

addAttribute :: (Generator e g) => Attribute -> g e ()
addAttribute attribute@InnerClasses {..} = do
  mapM_ addInnerClass innerClasses
  let attributeName = (attributeNameString attribute)
  St.modify (\s@GState { gsClassAttributes } ->
               let attribute' = case M.lookup attributeName gsClassAttributes of
                     Nothing -> attribute
                     Just foundAttribute@InnerClasses { innerClasses = innerClasses'} ->
                       foundAttribute { innerClasses = innerClasses ++ innerClasses' }
                       in
               s { gsClassAttributes = M.insert attributeName attribute' gsClassAttributes })
  where addInnerClass InnerClass {..} = do
          addToPool (CClass innerClassName)
          addToPool (CClass innerClassOuterClassName)
          addToPool (CUTF8 innerClassInnerName)

addAttribute attribute = error $ "addAttribute: pattern match failure = " ++ show attribute

newInnerClass :: (Generator e g)
              => [AccessFlag]    -- ^ Access flags
              -> B.ByteString    -- ^ Inner Class inner name
              -> B.ByteString    -- ^ Super class
              -> g e ()          -- ^ Body of inner class
              -> g e B.ByteString -- ^ Generated inner class name
newInnerClass flags name super gen = do
  prevGState <- St.get
  let outerClassName = gsClassName prevGState
      fullInnerClassName = B.append (BC.snoc outerClassName '$') name
      innerClassAttribute =
        InnerClasses {
            innerClasses = [InnerClass {
                               innerClassName = fullInnerClassName,
                               innerClassOuterClassName = outerClassName,
                               innerClassInnerName = name,
                               innerClassAccessFlags = S.fromList flags }]}
  St.put $ emptyGState { gsClassName = fullInnerClassName,
                         gsSuperClassName = super}
  gen
  addAttribute innerClassAttribute
  innerGState <- St.get
  St.put $ prevGState { gsInnerClasses = M.insert name innerGState (gsInnerClasses prevGState) }
  addAttribute innerClassAttribute
  return fullInnerClassName

-- | Get a class from current ClassPath
getClass :: (Throws ENotLoaded e, Throws ENotFound e)
         => String -> GenerateIO e (Class Direct)
getClass name = do
  cp <- St.gets gsClassPath
  res <- liftIO $ getEntry cp name
  case res of
    Just (NotLoaded p) -> throwG (ClassFileNotLoaded p)
    Just (Loaded _ c) -> return c
    Just (NotLoadedJAR p c) -> throwG (JARNotLoaded p c)
    Just (LoadedJAR _ c) -> return c
    Nothing -> throwG (ClassNotFound name)

-- | Get class field signature from current ClassPath
getClassField :: (Throws ENotFound e, Throws ENotLoaded e)
              => String -> B.ByteString -> GenerateIO e (NameType (Field Direct))
getClassField clsName fldName = do
  cls <- getClass clsName
  case lookupField fldName cls of
    Just fld -> return (fieldNameType fld)
    Nothing  -> throwG (FieldNotFound clsName fldName)

-- | Get class method signature from current ClassPath
getClassMethod :: (Throws ENotFound e, Throws ENotLoaded e)
               => String -> B.ByteString -> GenerateIO e (NameType (Method Direct))
getClassMethod clsName mName = do
  cls <- getClass clsName
  case lookupMethod mName cls of
    Just m -> return (methodNameType m)
    Nothing  -> throwG (MethodNotFound clsName mName)

-- | Access the generated bytecode length
encodedCodeLength :: GState -> Word32
encodedCodeLength st = fromIntegral . B.length . encode $ gsGenerated st

generateCodeLength :: Generate (Caught SomeException NoExceptions) a -> Word32
generateCodeLength = encodedCodeLength . execGenerate []

-- | Convert Generator state to method Code.
genCode :: GState -> Attribute
genCode st = Code {
    codeStackSize = gsStackSize st,
    codeMaxLocals = gsLocals st,
    codeLength = encodedCodeLength st,
    codeInstructions = gsGenerated st,
    codeExceptionsN = 0,
    codeExceptions = [],
    codeAttrsN = 0,
    codeAttributes = M.empty }

generateClasses :: GState -> [Class Direct]
generateClasses GState {..} = defaultClass {
    constsPoolSize = fromIntegral $ M.size gsPool,
    constsPool = gsPool,
    accessFlags = gsAccessFlags,
    thisClass = gsClassName,
    superClass = gsSuperClassName,
    classMethodsCount = fromIntegral $ length gsMethods,
    classMethods = M.elems gsMethods,
    classFieldsCount = fromIntegral $ length gsFields,
    classFields = M.elems gsFields } : concatMap generateClasses (M.elems gsInnerClasses)

initClass :: (Generator e g)
          => g e ()
          -> g e ()
initClass gen = do
  gen
  GState { gsSuperClassName, gsClassName } <- St.get
  addToPool (CClass gsSuperClassName)
  addToPool (CClass gsClassName)
  return ()

-- | Generate a class
generateIO :: [Tree CPEntry]
           -> GenerateIO (Caught SomeException NoExceptions) ()
           -> IO [Class Direct]
generateIO cp gen = liftM generateClasses $ execGenerateIO cp (initClass gen)

-- | Generate a class
generate :: [Tree CPEntry]
         -> Generate (Caught SomeException NoExceptions) ()
         -> [Class Direct]
generate cp gen = generateClasses . execGenerate cp $ initClass gen
