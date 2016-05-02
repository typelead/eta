
module JVM.Builder
  (module JVM.Builder.Monad,
   module JVM.Builder.Instructions,
   module JVM.Builder.Util,
   arrayOf, sizedArray
  ) where

import JVM.ClassFile

import JVM.Builder.Monad
import JVM.Builder.Instructions
import JVM.Builder.Util

arrayOf :: FieldType -> FieldType
arrayOf = Array Nothing

sizedArray :: Int -> FieldType -> FieldType
sizedArray n = Array (Just n)

