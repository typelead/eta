-- | This module exports shortcuts for some of JVM instructions (which are defined in JVM.Assembler).
-- These functions get Constants, put them into constants pool and generate instruction using index
-- of constant in the pool.
module JVM.Builder.Instructions where

import Data.Word
import qualified Data.ByteString.Lazy as B
import Codec.Binary.UTF8.String (encodeString)
import Data.String

import JVM.ClassFile
import JVM.Assembler
import JVM.Builder.Monad

nop :: Generator e g => g e ()
nop = i0 NOP
aconst_null :: Generator e g => g e ()
aconst_null = i0 ACONST_NULL
iconst_m1 :: Generator e g => g e ()
iconst_m1 = i0 ICONST_M1
iconst_0 :: Generator e g => g e ()
iconst_0 = i0 ICONST_0
iconst_1 :: Generator e g => g e ()
iconst_1 = i0 ICONST_1
iconst_2 :: Generator e g => g e ()
iconst_2 = i0 ICONST_2
iconst_3 :: Generator e g => g e ()
iconst_3 = i0 ICONST_3
iconst_4 :: Generator e g => g e ()
iconst_4 = i0 ICONST_4
iconst_5 :: Generator e g => g e ()
iconst_5 = i0 ICONST_5
lconst_0 :: Generator e g => g e ()
lconst_0 = i0 LCONST_0
lconst_1 :: Generator e g => g e ()
lconst_1 = i0 LCONST_1
fconst_0 :: Generator e g => g e ()
fconst_0 = i0 FCONST_0
fconst_1 :: Generator e g => g e ()
fconst_1 = i0 FCONST_1
fconst_2 :: Generator e g => g e ()
fconst_2 = i0 FCONST_2
dconst_0 :: Generator e g => g e ()
dconst_0 = i0 DCONST_0
dconst_1 :: Generator e g => g e ()
dconst_1 = i0 DCONST_1

bipush :: Generator e g => Word8 -> g e ()
bipush x = i0 (BIPUSH x)
sipush :: Generator e g => Word16 -> g e ()
sipush x = i0 (SIPUSH x)

ldc1 :: Generator e g => Constant Direct -> g e ()
ldc1 x = i8 LDC1 x
ldc2 :: Generator e g => Constant Direct -> g e ()
ldc2 x = i1 LDC2 x
ldc2w :: Generator e g => Constant Direct -> g e ()
ldc2w x = i1 LDC2W x
iload :: Generator e g => Constant Direct -> g e ()
iload x = i8 ILOAD x
lload :: Generator e g => Constant Direct -> g e ()
lload x = i8 LLOAD x
fload :: Generator e g => Constant Direct -> g e ()
fload x = i8 FLOAD x
dload :: Generator e g => Constant Direct -> g e ()
dload x = i8 DLOAD x
aload :: Generator e g => Constant Direct -> g e ()
aload x = i8 ALOAD x

iload_ :: Generator e g => IMM -> g e ()
iload_ x = i0 (ILOAD_ x)
lload_ :: Generator e g => IMM -> g e ()
lload_ x = i0 (LLOAD_ x)
fload_ :: Generator e g => IMM -> g e ()
fload_ x = i0 (FLOAD_ x)
dload_ :: Generator e g => IMM -> g e ()
dload_ x = i0 (DLOAD_ x)
aload_ :: Generator e g => IMM -> g e ()
aload_ x = i0 (ALOAD_ x)

iaload :: Generator e g => g e ()
iaload = i0 IALOAD
laload :: Generator e g => g e ()
laload = i0 LALOAD
faload :: Generator e g => g e ()
faload = i0 FALOAD
daload :: Generator e g => g e ()
daload = i0 DALOAD
aaload :: Generator e g => g e ()
aaload = i0 AALOAD
caload :: Generator e g => g e ()
caload = i0 CALOAD
saload :: Generator e g => g e ()
saload = i0 SALOAD

istore :: Generator e g => Constant Direct -> g e ()
istore x = i8 ISTORE x
lstore :: Generator e g => Constant Direct -> g e ()
lstore x = i8 LSTORE x
fstore :: Generator e g => Constant Direct -> g e ()
fstore x = i8 FSTORE x
dstore :: Generator e g => Constant Direct -> g e ()
dstore x = i8 DSTORE x
astore :: Generator e g => Constant Direct -> g e ()
astore x = i8 ASTORE x

istore_ :: Generator e g => Word8 -> g e ()
istore_ x = i0 (ISTORE x)
lstore_ :: Generator e g => Word8 -> g e ()
lstore_ x = i0 (LSTORE x)
fstore_ :: Generator e g => Word8 -> g e ()
fstore_ x = i0 (FSTORE x)
dstore_ :: Generator e g => Word8 -> g e ()
dstore_ x = i0 (DSTORE x)
astore_ :: Generator e g => Word8 -> g e ()
astore_ x = i0 (ASTORE x)

iastore :: Generator e g => g e ()
iastore = i0 IASTORE
lastore :: Generator e g => g e ()
lastore = i0 LASTORE
fastore :: Generator e g => g e ()
fastore = i0 FASTORE
dastore :: Generator e g => g e ()
dastore = i0 DASTORE
aastore :: Generator e g => g e ()
aastore = i0 AASTORE
bastore :: Generator e g => g e ()
bastore = i0 BASTORE
castore :: Generator e g => g e ()
castore = i0 CASTORE
sastore :: Generator e g => g e ()
sastore = i0 SASTORE

pop :: Generator e g => g e ()
pop     = i0 POP    
pop2 :: Generator e g => g e ()
pop2    = i0 POP2   
dup :: Generator e g => g e ()
dup     = i0 DUP    
dup_x1 :: Generator e g => g e ()
dup_x1  = i0 DUP_X1 
dup_x2 :: Generator e g => g e ()
dup_x2  = i0 DUP_X2 
dup2 :: Generator e g => g e ()
dup2    = i0 DUP2   
dup2_x1 :: Generator e g => g e ()
dup2_x1 = i0 DUP2_X1
dup2_x2 :: Generator e g => g e ()
dup2_x2 = i0 DUP2_X2
swap :: Generator e g => g e ()
swap    = i0 SWAP   
iadd :: Generator e g => g e ()
iadd    = i0 IADD   
ladd :: Generator e g => g e ()
ladd    = i0 LADD   
fadd :: Generator e g => g e ()
fadd    = i0 FADD   
dadd :: Generator e g => g e ()
dadd    = i0 DADD   
isub :: Generator e g => g e ()
isub    = i0 ISUB   
lsub :: Generator e g => g e ()
lsub    = i0 LSUB   
fsub :: Generator e g => g e ()
fsub    = i0 FSUB   
dsub :: Generator e g => g e ()
dsub    = i0 DSUB   
imul :: Generator e g => g e ()
imul    = i0 IMUL   
lmul :: Generator e g => g e ()
lmul    = i0 LMUL   
fmul :: Generator e g => g e ()
fmul    = i0 FMUL   
dmul :: Generator e g => g e ()
dmul    = i0 DMUL   
idiv :: Generator e g => g e ()
idiv    = i0 IDIV   
ldiv :: Generator e g => g e ()
ldiv    = i0 LDIV   
fdiv :: Generator e g => g e ()
fdiv    = i0 FDIV   
ddiv :: Generator e g => g e ()
ddiv    = i0 DDIV   
irem :: Generator e g => g e ()
irem    = i0 IREM   
lrem :: Generator e g => g e ()
lrem    = i0 LREM   
frem :: Generator e g => g e ()
frem    = i0 FREM   
drem :: Generator e g => g e ()
drem    = i0 DREM   
ineg :: Generator e g => g e ()
ineg    = i0 INEG   
lneg :: Generator e g => g e ()
lneg    = i0 LNEG   
fneg :: Generator e g => g e ()
fneg    = i0 FNEG   
dneg :: Generator e g => g e ()
dneg    = i0 DNEG   
ishl :: Generator e g => g e ()
ishl    = i0 ISHL   
lshl :: Generator e g => g e ()
lshl    = i0 LSHL   
ishr :: Generator e g => g e ()
ishr    = i0 ISHR   
lshr :: Generator e g => g e ()
lshr    = i0 LSHR   
iushr :: Generator e g => g e ()
iushr   = i0 IUSHR  
lushr :: Generator e g => g e ()
lushr   = i0 LUSHR  
iand :: Generator e g => g e ()
iand    = i0 IAND   
land :: Generator e g => g e ()
land    = i0 LAND   
ior :: Generator e g => g e ()
ior     = i0 IOR    
lor :: Generator e g => g e ()
lor     = i0 LOR    
ixor :: Generator e g => g e ()
ixor    = i0 IXOR   
lxor :: Generator e g => g e ()
lxor    = i0 LXOR   

iinc :: Generator e g => Word8 -> Word8 -> g e ()
iinc x y = i0 (IINC x y)

i2l :: Generator e g => g e ()
i2l  = i0 I2L 
i2f :: Generator e g => g e ()
i2f  = i0 I2F 
i2d :: Generator e g => g e ()
i2d  = i0 I2D 
l2i :: Generator e g => g e ()
l2i  = i0 L2I 
l2f :: Generator e g => g e ()
l2f  = i0 L2F 
l2d :: Generator e g => g e ()
l2d  = i0 L2D 
f2i :: Generator e g => g e ()
f2i  = i0 F2I 
f2l :: Generator e g => g e ()
f2l  = i0 F2L 
f2d :: Generator e g => g e ()
f2d  = i0 F2D 
d2i :: Generator e g => g e ()
d2i  = i0 D2I 
d2l :: Generator e g => g e ()
d2l  = i0 D2L 
d2f :: Generator e g => g e ()
d2f  = i0 D2F 
i2b :: Generator e g => g e ()
i2b  = i0 I2B 
i2c :: Generator e g => g e ()
i2c  = i0 I2C 
i2s :: Generator e g => g e ()
i2s  = i0 I2S 
lcmp :: Generator e g => g e ()
lcmp = i0 LCMP

-- | Wide instruction
wide :: Generator e g => (Word8 -> Instruction) -> Constant Direct -> g e ()
wide fn c = do
  ix <- addToPool c
  let ix0 = fromIntegral (ix `div` 0x100) :: Word8
      ix1 = fromIntegral (ix `mod` 0x100) :: Word8
  i0 (WIDE ix0 $ fn ix1)

new :: Generator e g => B.ByteString -> g e ()
new cls =
  i1 NEW (CClass cls)

newArray :: Generator e g => ArrayType -> g e ()
newArray t =
  i0 (NEWARRAY $ atype2byte t)

allocNewArray :: Generator e g => B.ByteString -> g e ()
allocNewArray cls =
  i1 ANEWARRAY (CClass cls)

invokeVirtual :: Generator e g => B.ByteString -> NameType (Method Direct) -> g e ()
invokeVirtual cls sig =
  i1 INVOKEVIRTUAL (CMethod cls sig)

invokeStatic :: Generator e g => B.ByteString -> NameType (Method Direct) -> g e ()
invokeStatic cls sig =
  i1 INVOKESTATIC (CMethod cls sig)

invokeSpecial :: Generator e g => B.ByteString -> NameType (Method Direct) -> g e ()
invokeSpecial cls sig =
  i1 INVOKESPECIAL (CMethod cls sig)

getStaticField :: Generator e g => B.ByteString -> NameType (Field Direct) -> g e ()
getStaticField cls sig =
  i1 GETSTATIC (CField cls sig)

loadString :: Generator e g => String -> g e ()
loadString str =
  i8 LDC1 (CString $ fromString $ encodeString $ str)

allocArray :: Generator e g => B.ByteString -> g e ()
allocArray cls =
  i1 ANEWARRAY (CClass cls)

