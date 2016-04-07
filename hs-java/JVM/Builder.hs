
module JVM.Builder
  (module JVM.Builder.Monad,
   module JVM.Builder.Instructions,
   arrayOf, sizedArray
  ) where

import JVM.ClassFile

import JVM.Builder.Monad
import JVM.Builder.Instructions

arrayOf :: FieldType -> FieldType
arrayOf t = Array Nothing t

sizedArray :: Int -> FieldType -> FieldType
sizedArray n t = Array (Just n) t

