module Codec.JVM.ASM.Code where

import Control.Monad.Trans.RWS (ask)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Word (Word8, Word16)
import Data.Int (Int32, Int64)

import qualified Data.ByteString as BS

import Codec.JVM.ASM.Code.Instr (Instr(..), runInstr)
import Codec.JVM.ASM.Code.Types (Offset(..), StackMapTable(..))
import Codec.JVM.Cond (Cond)
import Codec.JVM.Const
import Codec.JVM.ConstPool (ConstPool)
import Codec.JVM.Internal (packWord16be, packI16)
import Codec.JVM.Opcode (Opcode)
import Codec.JVM.Types

import Codec.JVM.ASM.Code.CtrlFlow (VerifType(..))
import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import qualified Codec.JVM.Cond as CD
import qualified Codec.JVM.ASM.Code.Instr as IT
import qualified Codec.JVM.ConstPool as CP
import qualified Codec.JVM.Opcode as OP

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Maybe(maybe)

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

modifyStack = IT.modifyStack

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
       <> modifyStack (CF.push ft)
  where fsz = fieldSize ft
        dupOp = if fsz == 1 then OP.dup else OP.dup2

pop :: FieldType -> Code
pop ft = mkCode'
       $ IT.op popOp
       <> modifyStack (CF.pop ft)
  where fsz = fieldSize ft
        popOp = if fsz == 1 then OP.pop else OP.pop2


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

getstatic :: FieldRef -> Code
getstatic fr@(FieldRef _ _ ft) = mkCode cs $ fold
  [ IT.op OP.getstatic
  , IT.ix c
  , modifyStack
  $ CF.push ft ]
  where c = CFieldRef fr
        cs = CP.unpack c

putstatic :: FieldRef -> Code
putstatic fr@(FieldRef _ _ ft) = mkCode cs $ fold
  [ IT.op OP.putstatic
  , IT.ix c
  , modifyStack
  $ CF.pop ft ]
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

-- iload :: Word8 -> Code
-- iload n = mkCode' $ f n <> cf where
--   f 0 = IT.op OP.iload_0
--   f 1 = IT.op OP.iload_1
--   f 2 = IT.op OP.iload_2
--   f 3 = IT.op OP.iload_3
--   f _ = fold [IT.op OP.iload, IT.bytes $ BS.singleton n]
--   cf = IT.ctrlFlow $ CF.load n jint

-- ireturn :: Code
-- ireturn = op OP.ireturn

-- istore :: Word8 -> Code
-- istore n = mkCode' $ f n <> cf where
--   f 0 = IT.op OP.istore_0
--   f 1 = IT.op OP.istore_1
--   f 2 = IT.op OP.istore_2
--   f 3 = IT.op OP.istore_3
--   f _ = fold [IT.op OP.istore, IT.bytes $ BS.singleton n]
--   cf = IT.ctrlFlow $ CF.store n jint


-- getstatic :: FieldRef -> Code
-- getstatic fr@(FieldRef _ _ ft) = codeConst OP.getstatic ft $ CFieldRef fr

anewarray :: IClassName -> Code
anewarray cn = codeConst OP.anewarray (ObjectType cn) $ CClass cn

-- aload :: FieldType -> Word8 -> Code
-- aload cls n = mkCode' $ f n <> cf where
--   f 0 = IT.op OP.aload_0
--   f 1 = IT.op OP.aload_1
--   f 2 = IT.op OP.aload_2
--   f 3 = IT.op OP.aload_3
--   f n = fold [IT.op OP.aload, IT.bytes $ BS.singleton n]
--   cf = IT.ctrlFlow $ CF.load n cls

-- Generic instruction which selects either
-- the original opcode or the modified opcode
-- based on size
gwide :: (Integral a) => Opcode -> a -> Instr
gwide opcode n = wideInstr
  where wideInstr
          | n <= 255  = IT.op opcode
                     <> IT.bytes (BS.singleton $ fromIntegral n)
          | otherwise = IT.op OP.wide
                     <> IT.op opcode
                     <> IT.bytes (packI16 $ fromIntegral n)

-- Generic load instruction
gload :: FieldType -> Int -> Code
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
            _ -> gwide OP.iload n
          VLong -> case n of
            0 -> IT.op OP.lload_0
            1 -> IT.op OP.lload_1
            2 -> IT.op OP.lload_2
            3 -> IT.op OP.lload_3
            _ -> gwide OP.lload n
          VFloat -> case n of
            0 -> IT.op OP.fload_0
            1 -> IT.op OP.fload_1
            2 -> IT.op OP.fload_2
            3 -> IT.op OP.fload_3
            _ -> gwide OP.fload n
          VDouble -> case n of
            0 -> IT.op OP.dload_0
            1 -> IT.op OP.dload_1
            2 -> IT.op OP.dload_2
            3 -> IT.op OP.dload_3
            _ -> gwide OP.dload n
          VObject _ -> case n of
            0 -> IT.op OP.aload_0
            1 -> IT.op OP.aload_1
            2 -> IT.op OP.aload_2
            3 -> IT.op OP.aload_3
            _ -> gwide OP.aload n
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
            _ -> gwide OP.istore n
          VLong -> case n of
            0 -> IT.op OP.lstore_0
            1 -> IT.op OP.lstore_1
            2 -> IT.op OP.lstore_2
            3 -> IT.op OP.lstore_3
            _ -> gwide OP.lstore n
          VFloat -> case n of
            0 -> IT.op OP.fstore_0
            1 -> IT.op OP.fstore_1
            2 -> IT.op OP.fstore_2
            3 -> IT.op OP.fstore_3
            _ -> gwide OP.fstore n
          VDouble -> case n of
            0 -> IT.op OP.dstore_0
            1 -> IT.op OP.dstore_1
            2 -> IT.op OP.dstore_2
            3 -> IT.op OP.dstore_3
            _ -> gwide OP.dstore n
          VObject _ -> case n of
            0 -> IT.op OP.astore_0
            1 -> IT.op OP.astore_1
            2 -> IT.op OP.astore_2
            3 -> IT.op OP.astore_3
            _ -> gwide OP.astore n
          _ -> error $ "gstore: Wrong type of load!"

initCtrlFlow :: Bool -> [FieldType] -> Code
initCtrlFlow isStatic args@(_:args')
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
          _ -> error "greturn: Wrong type of return!"

new :: FieldType -> Code
new (ObjectType (IClassName className)) = mkCode cs $
  IT.withOffset $ \offset -> fold
    [ IT.op OP.new
    , IT.ix c
    , modifyStack (CF.vpush (VUninitialized $ fromIntegral offset))]
  where c = CClass . IClassName $ className
        cs = CP.unpack c
new ft@(ArrayType (BaseType prim)) = mkCode' $ fold
  [ IT.op OP.newarray
  , IT.bytes . BS.singleton $ fromIntegral atype
  , modifyStack (CF.push ft . CF.pop jint) ]
  where atype = case prim of
          JBool   -> 4
          JChar   -> 5
          JFloat  -> 6
          JDouble -> 7
          JByte   -> 8
          JShort  -> 9
          JInt    -> 10
          JLong   -> 11
new ft@(ArrayType (ObjectType (IClassName className))) = mkCode cs $ fold
  [ IT.op OP.anewarray
  , IT.ix c
  , modifyStack $ CF.push ft . CF.pop jint ]
  where c = CClass . IClassName $ className
        cs = CP.unpack c
new (BaseType prim)
  = error $ "new: Cannot instantiate a primitive type: " ++ show prim
new ft = error $ "new: Type not supported" ++ show ft

aconst_null :: Code
aconst_null = mkCode' $ IT.op OP.aconst_null <> modifyStack (CF.vpush VNull)

iconst :: FieldType -> Int32 -> Code
iconst ft i
  | i >= -1 && i <= 5 = mkCode' . (<> modifyStack (CF.push ft)) $
    case i of
      -1 -> IT.op OP.iconst_m1
      0  -> IT.op OP.iconst_0
      1  -> IT.op OP.iconst_1
      2  -> IT.op OP.iconst_2
      3  -> IT.op OP.iconst_3
      4  -> IT.op OP.iconst_4
      5  -> IT.op OP.iconst_5
      _  -> error "iconst: -1 <= i <= 5 DEFAULT"
  | i >= -128 && i <= 127 = bipush ft $ fromIntegral i
  | i >= -32768 && i <= 32767 = sipush ft $ fromIntegral i
  | otherwise = gldc ft $ cint i

constCode :: FieldType -> Opcode -> Code
constCode ft op = mkCode' $ IT.op op <> modifyStack (CF.push ft)

lconst :: Int64 -> Code
lconst l
  | l == 0 = code OP.lconst_0
  | l == 1 = code OP.lconst_1
  | otherwise = gldc ft $ clong l
  where ft = jlong
        code = constCode ft

fconst :: Float -> Code
fconst f
  | f == 0.0 = code OP.fconst_0
  | f == 1.0 = code OP.fconst_1
  | f == 2.0 = code OP.fconst_2
  | otherwise = gldc ft $ cfloat f
  where ft = jfloat
        code = constCode ft

dconst :: Double -> Code
dconst d
  | d == 0.0 = code OP.dconst_0
  | d == 1.0 = code OP.dconst_1
  | otherwise = gldc ft $ cdouble d
  where ft = jdouble
        code = constCode ft

sconst :: ByteString -> Code
sconst = gldc jstring . cstring . decodeUtf8

gldc :: FieldType -> Const -> Code
gldc ft c = mkCode cs $ loadCode
                     <> modifyStack (CF.push ft)
  where cs = CP.unpack c
        category2 = isCategory2 ft
        loadCode
          | category2 = IT.op OP.ldc2_w
                     <> IT.ix c
          | otherwise = Instr $ do
              cp <- ask
              let index = CP.ix $ CP.unsafeIndex c cp
              if index <= 255 then
                do IT.op' OP.ldc
                   IT.writeBytes (BS.singleton $ fromIntegral index)
              else
                do IT.op' OP.ldc_w
                   IT.writeBytes (packI16 $ fromIntegral index)

gconv :: FieldType -> FieldType -> Code
gconv ft1 ft2 = mkCode' $ convOpcode (baseType ft1) (baseType ft2)
                       <> modifyStack ( CF.push ft2
                                      . CF.pop ft1)
  where convOpcode pt1 pt2 = case (pt1, pt2) of
          (JInt, JByte) -> IT.op OP.i2b
          (JInt, JShort) -> IT.op OP.i2s
          (JInt, JChar) -> IT.op OP.i2c
          (JInt, JBool) -> mempty
          (JInt, JInt) -> mempty
          other -> error $ "Implement the other JVM primitive conversions."
                         ++ show other

-- Heuristic taken from https://ghc.haskell.org/trac/ghc/ticket/9159
gswitch :: Code -> [(Int, Code)] -> Maybe Code -> Code
gswitch expr [] (Just deflt) = deflt
gswitch expr [(_, code)] Nothing = code
gswitch expr branches maybeDefault =
  if nlabels > 0 &&
     tableSpaceCost + 3 * tableTimeCost <=
     lookupSpaceCost + 3 * lookupTimeCost then
    tableswitch lo hi branchMap maybeDefault
  else
    lookupswitch branchMap maybeDefault
  where branchMap = IntMap.fromList branches
        nlabels = IntMap.size branchMap
        lo = fst . IntMap.findMin $ branchMap
        hi = fst . IntMap.findMax $ branchMap
        tableSpaceCost = 4 + (hi - lo + 1)
        tableTimeCost = 3
        lookupSpaceCost = 3 + 2 * nlabels
        lookupTimeCost = nlabels

tableswitch :: Int -> Int -> IntMap Code -> Maybe Code -> Code
tableswitch lo hi branchMap maybeDefault =
  mkCode cs $ IT.tableswitch lo hi (fmap instr branchMap) (fmap instr maybeDefault)
  where cs = maybe [] consts maybeDefault
          ++ concatMap consts (IntMap.elems branchMap)

lookupswitch :: IntMap Code -> Maybe Code -> Code
lookupswitch branchMap maybeDefault =
  mkCode cs $ IT.lookupswitch (fmap instr branchMap) (fmap instr maybeDefault)
  where cs = maybe [] consts maybeDefault
          ++ concatMap consts (IntMap.elems branchMap)

startLabel :: Label -> Code
startLabel = mkCode' . IT.putLabel

goto :: Label -> Code
goto = mkCode' . IT.gotoLabel

gaload :: FieldType -> Code
gaload ft = mkCode' $ fold
  [ IT.op loadOp
  , modifyStack ( CF.push ft
                . CF.pop (jarray ft)
                . CF.pop jint) ]
  where loadOp = case ft of
          BaseType prim ->
            case prim of
              JBool   -> OP.baload
              JChar   -> OP.caload
              JFloat  -> OP.faload
              JDouble -> OP.daload
              JByte   -> OP.baload
              JShort  -> OP.saload
              JInt    -> OP.iaload
              JLong   -> OP.laload
          _ -> OP.aaload

gastore :: FieldType -> Code
gastore ft = mkCode' $ fold
  [ IT.op storeOp
  , modifyStack ( CF.pop (jarray ft)
                . CF.pop jint
                . CF.pop ft) ]
  where storeOp = case ft of
          BaseType prim ->
            case prim of
              JBool   -> OP.bastore
              JChar   -> OP.castore
              JFloat  -> OP.fastore
              JDouble -> OP.dastore
              JByte   -> OP.bastore
              JShort  -> OP.sastore
              JInt    -> OP.iastore
              JLong   -> OP.lastore
          _ -> OP.aastore
