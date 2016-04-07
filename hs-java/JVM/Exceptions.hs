{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
module JVM.Exceptions where

import Control.Monad.Exception
import qualified Data.ByteString.Lazy as B
import Data.Typeable (Typeable)

import JVM.ClassFile

data NoItemInPool = forall a. Show a => NoItemInPool a
  deriving (Typeable)

instance Exception NoItemInPool

instance Show NoItemInPool where
  show (NoItemInPool s) = "Internal error: no such item in pool: <" ++ show s ++ ">"

data UnexpectedEndMethod = UnexpectedEndMethod
  deriving (Typeable)

instance Show UnexpectedEndMethod where
  show UnexpectedEndMethod = "endMethod without startMethod!"

instance Exception UnexpectedEndMethod

data ENotLoaded = ClassFileNotLoaded FilePath
                | JARNotLoaded FilePath String
  deriving (Typeable)

instance Show ENotLoaded where
  show (ClassFileNotLoaded p) = "Class file was not loaded: " ++ p
  show (JARNotLoaded p c) = "Class was not loaded from JAR: " ++ p ++ ": " ++ c

instance Exception ENotLoaded

data ENotFound = ClassNotFound String
               | FieldNotFound String B.ByteString
               | MethodNotFound String B.ByteString
  deriving (Typeable)

instance Show ENotFound where
  show (ClassNotFound p) = "No such class in ClassPath: " ++ p
  show (FieldNotFound c f) = "No such field in class " ++ c ++ ": " ++ toString f
  show (MethodNotFound c m) = "No such method in class " ++ c ++ ": " ++ toString m

instance Exception ENotFound

force :: String -> EM AnyException a -> a
force s x =
  case tryEM x of
    Right result -> result
    Left  exc    -> error $ "Exception at " ++ s ++ ": " ++ show exc
