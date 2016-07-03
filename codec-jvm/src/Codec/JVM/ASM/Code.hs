module Codec.JVM.ASM.Code where

import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Word (Word8, Word16)

import qualified Data.ByteString as BS

import Codec.JVM.ASM.Code.Instr (Instr, runInstr)
import Codec.JVM.ASM.Code.Types (Offset(..), StackMapTable(..))
import Codec.JVM.Cond (Cond)
import Codec.JVM.Const (Const(..), ConstVal, constValType)
import Codec.JVM.ConstPool (ConstPool)
import Codec.JVM.Internal (packWord16be)
import Codec.JVM.Opcode (Opcode)
import Codec.JVM.Types

import Codec.JVM.ASM.Code.CtrlFlow (VerifType(..))
import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import qualified Codec.JVM.Cond as CD
import qualified Codec.JVM.ASM.Code.Instr as IT
import qualified Codec.JVM.ConstPool as CP
import qualified Codec.JVM.Opcode as OP

import qualified Data.IntMap.Strict as IntMap

data Code = Code
  { consts  :: [Const]
  , instr   :: Instr }
  deriving Show

instance Monoid Code where
  mempty = Code mempty mempty
  mappend (Code cs0 i0) (Code cs1 i1) = Code (mappend cs0 cs1) (mappend i0 i1)

mkCode :: [Const] -> Instr -> Code
mkCode = Code

mkCode' :: Instr -> Code
mkCode' = mkCode []

modifyStack = IT.ctrlFlow . CF.mapStack

codeConst :: Opcode -> FieldType -> Const -> Code
codeConst oc ft c = mkCode cs $ fold
  [ IT.op oc
  , IT.ix c
  , modifyStack $ CF.push ft ]
    where cs = CP.unpack c

codeBytes :: ByteString -> Code
codeBytes bs = mkCode [] $ IT.bytes bs

op :: Opcode -> Code
op = mkCode' . IT.op

pushBytes :: Opcode -> FieldType -> ByteString -> Code
pushBytes oc ft bs = mkCode' $ fold
  [ IT.op oc
  , IT.bytes bs
  , modifyStack $ CF.push ft ]

--
-- Operations
--

bipush :: FieldType -> Word8 -> Code
bipush ft w = pushBytes OP.bipush ft $ BS.singleton w

sipush :: FieldType -> Word16 -> Code
sipush ft w = pushBytes OP.sipush ft $ packWord16be w

ldc :: ConstVal -> Code
ldc cv = codeConst OP.ldc_w ft $ CValue cv where ft = constValType cv

dup :: FieldType -> Code
dup ft = mkCode'
       $ IT.op dupOp
       <> (modifyStack $ CF.push ft)
  where fsz = fieldSize ft
        dupOp = if fsz == 1 then OP.dup else OP.dup2

invoke :: Opcode -> MethodRef -> Code
invoke oc mr@(MethodRef _ _ fts rt) = mkCode cs $ fold
  [ IT.op oc
  , IT.ix c
  , modifyStack
  $ maybePushReturn
  . popArgs ]
    where
      maybePushReturn = maybe id CF.push rt
      popArgs = CF.pop' (sum $ fieldSize <$> fts)
      c = CMethodRef mr
      cs = CP.unpack c

invokevirtual :: MethodRef -> Code
invokevirtual = invoke OP.invokevirtual

invokespecial :: MethodRef -> Code
invokespecial = invoke OP.invokespecial

invokestatic :: MethodRef -> Code
invokestatic = invoke OP.invokestatic

getfield :: FieldRef -> Code
getfield fr@(FieldRef _ _ ft) = mkCode cs $ fold
  [ IT.op OP.getfield
  , IT.ix c
  , modifyStack
  $ CF.push ft
  . CF.pop' 1 ] -- NOTE: Assumes that an object ref takes 1 stack slot
  where c = CFieldRef fr
        cs = CP.unpack c

putfield :: FieldRef -> Code
putfield fr@(FieldRef _ _ ft) = mkCode cs $ fold
  [ IT.op OP.putfield
  , IT.ix c
  , modifyStack
  $ CF.pop' 1  -- NOTE: Assumes that an object ref takes 1 stack slot
  . CF.pop  ft ]
  where c = CFieldRef fr
        cs = CP.unpack c

iadd :: Code
iadd = mkCode' $ IT.op OP.iadd <> cf where
  cf = modifyStack $ CF.push jint . CF.pop' 2

iif :: Cond -> Code -> Code -> Code
iif cond ok ko = mkCode cs ins where
  cs = [ok, ko] >>= consts
  ins = IT.iif cond (instr ok) (instr ko)

ifne :: Code -> Code -> Code
ifne = iif CD.NE

ifeq :: Code -> Code -> Code
ifeq = iif CD.EQ

iload :: Word8 -> Code
iload n = mkCode' $ f n <> cf where
  f 0 = IT.op OP.iload_0
  f 1 = IT.op OP.iload_1
  f 2 = IT.op OP.iload_2
  f 3 = IT.op OP.iload_3
  f _ = fold [IT.op OP.iload, IT.bytes $ BS.singleton n]
  cf = IT.ctrlFlow $ CF.load n jint

ireturn :: Code
ireturn = op OP.ireturn

istore :: Word8 -> Code
istore n = mkCode' $ f n <> cf where
  f 0 = IT.op OP.istore_0
  f 1 = IT.op OP.istore_1
  f 2 = IT.op OP.istore_2
  f 3 = IT.op OP.istore_3
  f _ = fold [IT.op OP.istore, IT.bytes $ BS.singleton n]
  cf = IT.ctrlFlow $ CF.store n jint


getstatic :: FieldRef -> Code
getstatic fr@(FieldRef _ _ ft) = codeConst OP.getstatic ft $ CFieldRef fr

anewarray :: IClassName -> Code
anewarray cn = codeConst OP.anewarray (ObjectType cn) $ CClass cn

aload :: FieldType -> Word8 -> Code
aload cls n = mkCode' $ f n <> cf where
  f 0 = IT.op OP.aload_0
  f 1 = IT.op OP.aload_1
  f 2 = IT.op OP.aload_2
  f 3 = IT.op OP.aload_3
  f n = fold [IT.op OP.aload, IT.bytes $ BS.singleton n]
  cf = IT.ctrlFlow $ CF.load n cls

-- Generic load instruction
gload :: FieldType -> Word8 -> Code
gload ft n = mkCode' $ fold
  [ loadOp
  , IT.ctrlFlow
  $ CF.load n ft ]
  where loadOp = case CF.fieldTypeFlatVerifType ft of
          VInteger -> case n of
            0 -> IT.op OP.iload_0
            1 -> IT.op OP.iload_1
            2 -> IT.op OP.iload_2
            3 -> IT.op OP.iload_3
            _ -> IT.op OP.iload
              <> (IT.bytes $ BS.singleton n)
          VLong -> case n of
            0 -> IT.op OP.lload_0
            1 -> IT.op OP.lload_1
            2 -> IT.op OP.lload_2
            3 -> IT.op OP.lload_3
            _ -> IT.op OP.lload
              <> (IT.bytes $ BS.singleton n)
          VFloat -> case n of
            0 -> IT.op OP.fload_0
            1 -> IT.op OP.fload_1
            2 -> IT.op OP.fload_2
            3 -> IT.op OP.fload_3
            _ -> IT.op OP.fload
              <> (IT.bytes $ BS.singleton n)
          VDouble -> case n of
            0 -> IT.op OP.dload_0
            1 -> IT.op OP.dload_1
            2 -> IT.op OP.dload_2
            3 -> IT.op OP.dload_3
            _ -> IT.op OP.dload
              <> (IT.bytes $ BS.singleton n)
          VObject _ -> case n of
            0 -> IT.op OP.aload_0
            1 -> IT.op OP.aload_1
            2 -> IT.op OP.aload_2
            3 -> IT.op OP.aload_3
            _ -> IT.op OP.aload
              <> (IT.bytes $ BS.singleton n)
          _ -> error $ "gload: Wrong type of load!"

-- Generic store instruction
gstore :: (Integral a) => FieldType -> a -> Code
gstore ft n' = mkCode' $ fold
  [ storeOp
  , IT.ctrlFlow
  $ CF.store n ft ]
  where n = fromIntegral n'
        storeOp = case CF.fieldTypeFlatVerifType ft of
          VInteger -> case n of
            0 -> IT.op OP.istore_0
            1 -> IT.op OP.istore_1
            2 -> IT.op OP.istore_2
            3 -> IT.op OP.istore_3
            _ -> IT.op OP.istore
              <> (IT.bytes $ BS.singleton n)
          VLong -> case n of
            0 -> IT.op OP.lstore_0
            1 -> IT.op OP.lstore_1
            2 -> IT.op OP.lstore_2
            3 -> IT.op OP.lstore_3
            _ -> IT.op OP.lstore
              <> (IT.bytes $ BS.singleton n)
          VFloat -> case n of
            0 -> IT.op OP.fstore_0
            1 -> IT.op OP.fstore_1
            2 -> IT.op OP.fstore_2
            3 -> IT.op OP.fstore_3
            _ -> IT.op OP.fstore
              <> (IT.bytes $ BS.singleton n)
          VDouble -> case n of
            0 -> IT.op OP.dstore_0
            1 -> IT.op OP.dstore_1
            2 -> IT.op OP.dstore_2
            3 -> IT.op OP.dstore_3
            _ -> IT.op OP.dstore
              <> (IT.bytes $ BS.singleton n)
          VObject _ -> case n of
            0 -> IT.op OP.astore_0
            1 -> IT.op OP.astore_1
            2 -> IT.op OP.astore_2
            3 -> IT.op OP.astore_3
            _ -> IT.op OP.astore
              <> (IT.bytes $ BS.singleton n)
          _ -> error $ "gstore: Wrong type of load!"

initCtrlFlow :: Bool -> [FieldType] -> Code
initCtrlFlow isStatic args@(this:args')
  = mkCode'
  . IT.initCtrl
  . CF.mapLocals
  . const
  . CF.localsFromList
  $ fts
  where fts = if isStatic then args' else args

-- Void return
vreturn :: Code
vreturn = op OP.vreturn

-- Generic, non-void return
greturn :: FieldType -> Code
greturn ft = mkCode' $ fold
  [ IT.op returnOp
  , IT.ctrlFlow
  . CF.mapStack
  $ CF.pop ft ]
  where returnOp = case CF.fieldTypeFlatVerifType ft of
          VInteger -> OP.ireturn
          VLong -> OP.lreturn
          VFloat -> OP.freturn
          VDouble -> OP.dreturn
          VObject _ -> OP.areturn
          _ -> error $ "greturn: Wrong type of return!"
