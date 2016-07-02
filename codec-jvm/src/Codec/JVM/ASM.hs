{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- | Usage:
--
-- You can assemble a java class:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- import Data.Binary.Put (runPut)
-- import Data.Foldable (fold)
-- import qualified Data.ByteString.Lazy as BS
--
-- import Codec.JVM.ASM (mkClassFile, mkMethodDef)
-- import Codec.JVM.ASM.Code
-- import Codec.JVM.Class (ClassFile, putClassFile)
-- import Codec.JVM.Method (AccessFlag(..))
-- import Codec.JVM.Types
--
-- mainClass :: ClassFile
-- mainClass = mkClassFile java8 [] "HelloWorld" Nothing
--   [ mkMethodDef [Public, Static] "main" [arr.obj $ "java/lang/String"] void $ fold
--     [ getstatic systemOut
--     , bipush jint 42
--     , invokevirtual printlnI
--     , vreturn ]
--   ]
--     where
--       systemOut   = mkFieldRef  "java/lang/System"    "out"     (obj "java/io/PrintStream")
--       printlnI    = mkMethodRef "java/io/PrintStream" "println" [prim JInt]                   void
--
-- main :: IO ()
-- main = BS.writeFile "HelloWorld.class" $ runPut . putClassFile $ mainClass
-- @
--
module Codec.JVM.ASM where

import Data.Binary.Put (runPut)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import qualified Data.Set as Set

import Codec.JVM.ASM.Code (Code, vreturn, invokespecial, aload)
import Codec.JVM.Class (ClassFile(..))
import Codec.JVM.Const (Const(..))
import Codec.JVM.ConstPool (mkConstPool)
import Codec.JVM.Method (MethodInfo(..), unpackMethodInfo)
import Codec.JVM.Field (FieldInfo(..), unpackFieldInfo)
import Codec.JVM.Types

import qualified Codec.JVM.ASM.Code as Code
import qualified Codec.JVM.Class as Class
import qualified Codec.JVM.ConstPool as CP

mkClassFile :: Version
            -> [AccessFlag]
            -> Text
            -> Maybe Text
            -> [FieldDef]
            -> [MethodDef]
            -> ClassFile
mkClassFile v afs tc' sc' fds mds = ClassFile cp v (Set.fromList afs) tc sc [] fis mis []
    where
      tc = IClassName tc'
      sc = IClassName <$> sc'
      cs = ccs ++ mdcs ++ mics ++ fdcs ++ fics where
        ccs = concat [CP.unpackClassName tc, CP.unpackClassName $ fromMaybe jlObject sc]
        mdcs = mds >>= unpackMethodDef
        mics = mis >>= unpackMethodInfo
        fdcs = fds >>= unpackFieldDef
        fics = fis >>= unpackFieldInfo
      cp = mkConstPool cs
      mis = f <$> mds where
        f (MethodDef afs' n' (MethodDesc d as) code) =
          MethodInfo (Set.fromList afs') n' (Desc d) $ Code.toAttrs as cp code

      fis = f <$> fds where
        f (FieldDef afs' n' (FieldDesc d)) =
          FieldInfo (Set.fromList afs') n' (Desc d) []

data MethodDef = MethodDef [AccessFlag] UName MethodDesc Code

mkMethodDef :: [AccessFlag] -> Text -> [FieldType] -> ReturnType -> [Code] -> MethodDef
mkMethodDef afs n fts rt cs = mkMethodDef' afs n (mkMethodDesc fts rt) $ fold cs

mkMethodDef' :: [AccessFlag] -> Text -> MethodDesc -> Code -> MethodDef
mkMethodDef' afs n md c = MethodDef afs (UName n) md c

unpackMethodDef :: MethodDef -> [Const]
unpackMethodDef (MethodDef _ (UName n') (MethodDesc d _) code) = CUTF8 n':CUTF8 d:Code.consts code

data FieldDef = FieldDef [AccessFlag] UName FieldDesc

mkFieldDef :: [AccessFlag] -> Text -> FieldType -> FieldDef
mkFieldDef afs n ft = mkFieldDef' afs n (mkFieldDesc ft)

mkFieldDef' :: [AccessFlag] -> Text -> FieldDesc -> FieldDef
mkFieldDef' afs n fd = FieldDef afs (UName n) fd

unpackFieldDef :: FieldDef -> [Const]
unpackFieldDef (FieldDef _ (UName n') (FieldDesc d)) = [CUTF8 n', CUTF8 d]

defaultConstructor :: FieldType -> Text -> MethodDef
defaultConstructor thisClass superClass = mkMethodDef [Public] "<init>" [] void $
  [ aload thisClass 0,
    invokespecial $ mkMethodRef superClass "<init>" [] void,
    vreturn ]
