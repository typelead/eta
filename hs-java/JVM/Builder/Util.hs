{-# LANGUAGE NamedFieldPuns, FlexibleContexts #-}
module JVM.Builder.Util where

import JVM.Builder.Monad
import JVM.Builder.Instructions
import JVM.Types
import JVM.DataTypes
import JVM.ClassFile
import JVM.Exceptions
import Java.ClassPath

import Control.Monad
import Control.Monad.Exception
import Control.Monad.Exception.Base
import qualified Control.Monad.State as St

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

-- | Generate new Object field that is automatically initialized with the
-- | default constructor
newObjectField :: (Generator e g)
               => [AccessFlag]     -- ^ Access flags
               -> B.ByteString     -- ^ Field name
               -> String           -- ^ Class of the object
               -> g e ()
newObjectField flags name qClass =
  newField flags name (ObjectType qClass) . Just $ do
    newDefault $ BC.pack qClass
    return ()

initClass :: (Generator e g, Throws UnexpectedEndMethod e)
          => g e ()
          -> g e ()
initClass gen = do
  gen
  GState { gsSuperClassName, gsClassName } <- St.get
  addToPool (CClass gsSuperClassName)
  addToPool (CClass gsClassName)
  genInitializers
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
