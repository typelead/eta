{-# LANGUAGE FlexibleContexts, TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
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
   setStackSize, setMaxLocals,
   withClassPath,
   getClassField, getClassMethod,
   generate, generateIO,
   generateCodeLength
  ) where

import Prelude hiding (catch)
import Control.Monad.State as St
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Data.Word
import Data.Binary
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B

import JVM.Common
import JVM.ClassFile
import JVM.Assembler
import JVM.Exceptions
import Java.ClassPath

-- | Generator state
data GState = GState {
  generated :: [Instruction],             -- ^ Already generated code (in current method)
  currentPool :: Pool Direct,             -- ^ Already generated constants pool
  nextPoolIndex :: Word16,                -- ^ Next index to be used in constants pool
  doneMethods :: [Method Direct],         -- ^ Already generated class methods
  currentMethod :: Maybe (Method Direct), -- ^ Current method
  stackSize :: Word16,                    -- ^ Maximum stack size for current method
  locals :: Word16,                       -- ^ Maximum number of local variables for current method
  classPath :: [Tree CPEntry]
  }
  deriving (Eq,Show)

-- | Empty generator state
emptyGState ::  GState
emptyGState = GState {
  generated = [],
  currentPool = M.empty,
  nextPoolIndex = 1,
  doneMethods = [],
  currentMethod = Nothing,
  stackSize = 496,
  locals = 0,
  classPath = []}

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
    execStateT (runEMT caught) (emptyGState {classPath = cp})

execGenerate :: [Tree CPEntry]
             -> Generate (Caught SomeException NoExceptions) a
             -> GState
execGenerate cp (Generate emt) = do
    let caught = emt `catch` (\(e :: SomeException) -> fail $ show e)
    execState (runEMT caught) (emptyGState {classPath = cp})

-- | Update ClassPath
withClassPath :: ClassPath () -> GenerateIO e ()
withClassPath cp = do
  res <- liftIO $ execClassPath cp
  st <- St.get
  St.put $ st {classPath = res}

-- | Add a constant to pool
addItem :: (Generator e g) => Constant Direct -> g e Word16
addItem c = do
  pool <- St.gets currentPool
  case lookupPool c pool of
    Just i -> return i
    Nothing -> do
      i <- St.gets nextPoolIndex
      let pool' = M.insert i c pool
          i' = if long c
                 then i+2
                 else i+1
      st <- St.get
      St.put $ st {currentPool = pool',
                   nextPoolIndex = i'}
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

putInstruction :: (Generator e g) => Instruction -> g e ()
putInstruction instr = do
  st <- St.get
  let code = generated st
  St.put $ st {generated = code ++ [instr]}

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

-- | Set maximum stack size for current method
setStackSize :: (Generator e g) => Word16 -> g e ()
setStackSize n = do
  st <- St.get
  St.put $ st {stackSize = n}

-- | Set maximum number of local variables for current method
setMaxLocals :: (Generator e g) => Word16 -> g e ()
setMaxLocals n = do
  st <- St.get
  St.put $ st {locals = n}

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
    methodAttributes = AR M.empty }
  St.put $ st {generated = [],
               currentMethod = Just method }

-- | End of method generation
endMethod :: (Generator e g, Throws UnexpectedEndMethod e) => g e ()
endMethod = do
  m <- St.gets currentMethod
  code <- St.gets genCode
  case m of
    Nothing -> throwG UnexpectedEndMethod
    Just method -> do
      let method' = method {methodAttributes = AR $ M.fromList [("Code", encodeMethod code)],
                            methodAttributesCount = 1}
      st <- St.get
      St.put $ st {generated = [],
                   currentMethod = Nothing,
                   doneMethods = doneMethods st ++ [method']}

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

-- | Get a class from current ClassPath
getClass :: (Throws ENotLoaded e, Throws ENotFound e)
         => String -> GenerateIO e (Class Direct)
getClass name = do
  cp <- St.gets classPath
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
encodedCodeLength st = fromIntegral . B.length . encodeInstructions $ generated st

generateCodeLength :: Generate (Caught SomeException NoExceptions) a -> Word32
generateCodeLength = encodedCodeLength . execGenerate []

-- | Convert Generator state to method Code.
genCode :: GState -> Code
genCode st = Code {
    codeStackSize = stackSize st,
    codeMaxLocals = locals st,
    codeLength = encodedCodeLength st,
    codeInstructions = generated st,
    codeExceptionsN = 0,
    codeExceptions = [],
    codeAttrsN = 0,
    codeAttributes = AP [] }

-- | Start class generation.
initClass :: (Generator e g) => B.ByteString -> g e Word16
initClass name = do
  addToPool (CClass "java/lang/Object")
  addToPool (CClass name)
  addToPool (CString "Code")

-- | Generate a class
generateIO :: [Tree CPEntry]
           -> B.ByteString
           -> GenerateIO (Caught SomeException NoExceptions) ()
           -> IO (Class Direct)
generateIO cp name gen = do
  let generator = do
        initClass name
        gen
  res <- execGenerateIO cp generator
  let code = genCode res
      d = defaultClass :: Class Direct
  return $ d {
        constsPoolSize = fromIntegral $ M.size (currentPool res),
        constsPool = currentPool res,
        accessFlags = S.fromList [ACC_PUBLIC, ACC_STATIC],
        thisClass = name,
        superClass = "java/lang/Object",
        classMethodsCount = fromIntegral $ length (doneMethods res),
        classMethods = doneMethods res }

-- | Generate a class
generate :: [Tree CPEntry]
         -> B.ByteString
         -> Generate (Caught SomeException NoExceptions) ()
         -> Class Direct
generate cp name gen =
  let generator = do
        initClass name
        gen
      res = execGenerate cp generator
      code = genCode res
      d = defaultClass :: Class Direct
  in  d {
        constsPoolSize = fromIntegral $ M.size (currentPool res),
        constsPool = currentPool res,
        accessFlags = S.fromList [ACC_PUBLIC, ACC_STATIC],
        thisClass = name,
        superClass = "java/lang/Object",
        classMethodsCount = fromIntegral $ length (doneMethods res),
        classMethods = doneMethods res }

