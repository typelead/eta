{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
module JVM.InvokeDynamic where

import JVM.Types

data ReferenceKind =
  REF_getField
  | REF_getStatic
  | REF_putField
  | REF_putStatic
  | REF_invokeVirtual
  | REF_invokeStatic
  | REF_invokeSpecial
  | REF_newInvokeSpecial
  | REF_invokeInterface

data BootstrapConstant stage = BC
     deriving (Show, Eq)

data BootstrapReference stage = BR
     deriving (Show, Eq)

data MethodHandle stage = MethodHandle {
  methodHandleRef :: Link stage (BootstrapReference stage) -- Should be a reference type
}

deriving instance Eq (MethodHandle File)
deriving instance Eq (MethodHandle Direct)
deriving instance Show (MethodHandle File)
deriving instance Show (MethodHandle Direct)

data BootstrapMethod stage = BootstrapMethod {
  bootstrapMethod :: Link stage (MethodHandle stage),
  bootstrapArgs :: [Link stage (BootstrapConstant stage)] -- Restrict the type of constants
}

deriving instance Eq (BootstrapMethod File)
deriving instance Eq (BootstrapMethod Direct)
deriving instance Show (BootstrapMethod File)
deriving instance Show (BootstrapMethod Direct)

type BootstrapArg stage = BootstrapConstant stage
