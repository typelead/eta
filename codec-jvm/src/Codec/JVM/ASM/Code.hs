module Codec.JVM.ASM.Code where

import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Word (Word8, Word16)

import qualified Data.ByteString as BS

import Codec.JVM.ASM.Code.Instr (Instr, runInstr)
import Codec.JVM.ASM.Code.Types (Offset(..), StackMapTable(..))
import Codec.JVM.Attr (Attr(ACode, AStackMapTable), StackMapFrame(..), VerifType(..))
import Codec.JVM.Cond (Cond)
import Codec.JVM.Const (Const(..), ConstVal, constValType)
import Codec.JVM.ConstPool (ConstPool)
import Codec.JVM.Internal (packWord16be)
import Codec.JVM.Opcode (Opcode)
import Codec.JVM.Types

import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import qualified Codec.JVM.Cond as CD
import qualified Codec.JVM.ASM.Code.Instr as IT
import qualified Codec.JVM.ConstPool as CP
import qualified Codec.JVM.Opcode as OP

import qualified Data.IntMap.Strict as IntMap

-- TODO Return `Either` with error (currently CF.pop is unsafe)
toAttrs :: Int -> ConstPool -> Code -> [Attr]
toAttrs as cp code = f $ runInstr (instr code) cp where
  f (xs, cf, smt) = [ACode maxStack' maxLocals' xs attrs] where
      maxLocals' = max as $ CF.maxLocals cf
      maxStack' = CF.maxStack cf
      attrs = if null frames then [] else [AStackMapTable frames]
      frames = toStackMapFrames smt

-- TODO Optimization: For now we only generate Same or Full frames, we could encode better intermediate cases.
toStackMapFrames :: StackMapTable -> [(Offset, StackMapFrame)]
toStackMapFrames (StackMapTable cfs) = reverse (fst $ foldl' f ([], CF.empty) $ IntMap.toAscList cfs) where
    f (xs, last) (off, cf) = ((Offset off, smf):xs, cf) where
      smf = if CF.equiv last cf then SameFrame else fullFrame cf
    fullFrame cf = FullFrame lvts svts where
      lvts = fmap f $ IntMap.toList $ CF.locals cf where f (_, ft) = VerifType ft
      svts = fmap VerifType $ CF.stackVal $ CF.stack cf

data Code = Code
  { consts  :: [Const]
  , instr   :: Instr }

instance Monoid Code where
  mempty = Code mempty mempty
  mappend (Code cs0 i0) (Code cs1 i1) = Code (mappend cs0 cs1) (mappend i0 i1)

mkCode :: [Const] -> Instr -> Code
mkCode cs i = Code cs i

mkCode' :: Instr -> Code
mkCode' = mkCode []

codeConst :: Opcode -> FieldType -> Const -> Code
codeConst oc ft c = mkCode cs $ fold
  [ IT.op oc
  , IT.ix c
  , IT.ctrlFlow $ CF.mapStack $ CF.push ft ]
    where cs = CP.unpack c

codeBytes :: ByteString -> Code
codeBytes bs = mkCode [] $ IT.bytes bs

op :: Opcode -> Code
op = mkCode' . IT.op

pushBytes :: Opcode -> FieldType -> ByteString -> Code
pushBytes oc ft bs = mkCode' $ fold
  [ IT.op oc
  , IT.bytes bs
  , IT.ctrlFlow $ CF.mapStack $ CF.push ft ]

--
-- Operations
--

bipush :: FieldType -> Word8 -> Code
bipush ft w = pushBytes OP.bipush ft $ BS.singleton w

sipush :: FieldType -> Word16 -> Code
sipush ft w = pushBytes OP.sipush ft $ packWord16be w

ldc :: ConstVal -> Code
ldc cv = codeConst OP.ldc_w ft $ CValue cv where ft = constValType cv

invoke :: Opcode -> MethodRef -> Code
invoke oc mr@(MethodRef _ _ fts rt) = mkCode cs $ fold
  [ IT.op oc
  , IT.ix c
  , IT.ctrlFlow $ CF.mapStack $ CF.pop' (sum $ fieldSize <$> fts) <> maybe mempty CF.push rt ]
    where
      c = CMethodRef mr
      cs = CP.unpack c

invokevirtual :: MethodRef -> Code
invokevirtual = invoke OP.invokevirtual

invokespecial :: MethodRef -> Code
invokespecial = invoke OP.invokespecial

invokestatic :: MethodRef -> Code
invokestatic = invoke OP.invokestatic

iadd :: Code
iadd = mkCode' $ IT.op OP.iadd <> i where
  i = IT.ctrlFlow $ CF.mapStack $ CF.pop jInt <> CF.push jInt

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
  cf = IT.ctrlFlow $ CF.load n jInt

ireturn :: Code
ireturn = op OP.ireturn

istore :: Word8 -> Code
istore n = mkCode' $ f n <> cf where
  f 0 = IT.op OP.istore_0
  f 1 = IT.op OP.istore_1
  f 2 = IT.op OP.istore_2
  f 3 = IT.op OP.istore_3
  f _ = fold [IT.op OP.istore, IT.bytes $ BS.singleton n]
  cf = IT.ctrlFlow $ CF.store n jInt

vreturn :: Code
vreturn = op OP.vreturn

getstatic :: FieldRef -> Code
getstatic fr@(FieldRef _ _ ft) = codeConst OP.getstatic ft $ CFieldRef fr

anewarray :: IClassName -> Code
anewarray cn = codeConst OP.anewarray (ObjectType cn) $ CClass cn
