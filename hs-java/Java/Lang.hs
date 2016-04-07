{-# LANGUAGE OverloadedStrings #-}
-- | This module exports some definitions from standard Java java.lang package.
module Java.Lang where

import Data.String

import JVM.Common ()  -- import instances only
import JVM.ClassFile

objectClass ::  FieldType
objectClass = ObjectType object

stringClass ::  FieldType
stringClass = ObjectType string

integerClass ::  FieldType
integerClass = ObjectType integer

systemClass ::  FieldType
systemClass = ObjectType system

object :: IsString s => s
object = "java/lang/Object"

string :: IsString s => s
string = "java/lang/String"

integer :: IsString s => s
integer = "java/lang/Integer"

system :: IsString s => s
system = "java/lang/System"

-- | java.lang.Object.<init>() method
objectInit :: NameType (Method Direct)
objectInit = NameType "<init>" $ MethodSignature [] ReturnsVoid

-- | java.lang.Integer.valueOf() method
valueOfInteger :: NameType (Method Direct)
valueOfInteger = NameType "valueOf" $ MethodSignature [IntType] (Returns Java.Lang.integerClass)

