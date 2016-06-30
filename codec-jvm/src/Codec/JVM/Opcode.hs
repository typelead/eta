module Codec.JVM.Opcode where

import Data.Word (Word8)

newtype Opcode = Opcode Int

opcode :: Opcode -> Word8
opcode (Opcode i) = fromIntegral i

-- | https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html

nop :: Opcode
aconst_null :: Opcode
iconst_m1 :: Opcode
iconst_0 :: Opcode
iconst_1 :: Opcode
iconst_2 :: Opcode
iconst_3 :: Opcode
iconst_4 :: Opcode
iconst_5 :: Opcode
lconst_0 :: Opcode
lconst_1 :: Opcode
fconst_0 :: Opcode
fconst_1 :: Opcode
fconst_2 :: Opcode
dconst_0 :: Opcode
dconst_1 :: Opcode
bipush :: Opcode
sipush :: Opcode
ldc :: Opcode
ldc_w :: Opcode
ldc2_w :: Opcode
iload :: Opcode
lload :: Opcode
fload :: Opcode
dload :: Opcode
aload :: Opcode
iload_0 :: Opcode
iload_1 :: Opcode
iload_2 :: Opcode
iload_3 :: Opcode
lload_0 :: Opcode
lload_1 :: Opcode
lload_2 :: Opcode
lload_3 :: Opcode
fload_0 :: Opcode
fload_1 :: Opcode
fload_2 :: Opcode
fload_3 :: Opcode
dload_0 :: Opcode
dload_1 :: Opcode
dload_2 :: Opcode
dload_3 :: Opcode
aload_0 :: Opcode
aload_1 :: Opcode
aload_2 :: Opcode
aload_3 :: Opcode
iaload :: Opcode
laload :: Opcode
faload :: Opcode
daload :: Opcode
aaload :: Opcode
baload :: Opcode
caload :: Opcode
saload :: Opcode
istore :: Opcode
lstore :: Opcode
fstore :: Opcode
dstore :: Opcode
astore :: Opcode
istore_0 :: Opcode
istore_1 :: Opcode
istore_2 :: Opcode
istore_3 :: Opcode
lstore_0 :: Opcode
lstore_1 :: Opcode
lstore_2 :: Opcode
lstore_3 :: Opcode
fstore_0 :: Opcode
fstore_1 :: Opcode
fstore_2 :: Opcode
fstore_3 :: Opcode
dstore_0 :: Opcode
dstore_1 :: Opcode
dstore_2 :: Opcode
dstore_3 :: Opcode
astore_0 :: Opcode
astore_1 :: Opcode
astore_2 :: Opcode
astore_3 :: Opcode
iastore :: Opcode
lastore :: Opcode
fastore :: Opcode
dastore :: Opcode
aastore :: Opcode
bastore :: Opcode
castore :: Opcode
sastore :: Opcode
pop :: Opcode
pop2 :: Opcode
dup :: Opcode
dup_x1 :: Opcode
dup_x2 :: Opcode
dup2 :: Opcode
dup2_x1 :: Opcode
dup2_x2 :: Opcode
swap :: Opcode
iadd :: Opcode
ladd :: Opcode
fadd :: Opcode
dadd :: Opcode
isub :: Opcode
lsub :: Opcode
fsub :: Opcode
dsub :: Opcode
imul :: Opcode
lmul :: Opcode
fmul :: Opcode
dmul :: Opcode
idiv :: Opcode
ldiv :: Opcode
fdiv :: Opcode
ddiv :: Opcode
irem :: Opcode
lrem :: Opcode
frem :: Opcode
drem :: Opcode
ineg :: Opcode
lneg :: Opcode
fneg :: Opcode
dneg :: Opcode
ishl :: Opcode
lshl :: Opcode
ishr :: Opcode
lshr :: Opcode
iushr :: Opcode
lushr :: Opcode
iand :: Opcode
land :: Opcode
ior :: Opcode
lor :: Opcode
ixor :: Opcode
lxor :: Opcode
iinc :: Opcode
i2l :: Opcode
i2f :: Opcode
i2d :: Opcode
l2i :: Opcode
l2f :: Opcode
l2d :: Opcode
f2i :: Opcode
f2l :: Opcode
f2d :: Opcode
d2i :: Opcode
d2l :: Opcode
d2f :: Opcode
i2b :: Opcode
i2c :: Opcode
i2s :: Opcode
lcmp :: Opcode
fcmpl :: Opcode
fcmpg :: Opcode
dcmpl :: Opcode
dcmpg :: Opcode
ifeq :: Opcode
ifne :: Opcode
iflt :: Opcode
ifge :: Opcode
ifgt :: Opcode
ifle :: Opcode
if_icmpeq :: Opcode
if_icmpne :: Opcode
if_icmplt :: Opcode
if_icmpge :: Opcode
if_icmpgt :: Opcode
if_icmple :: Opcode
if_acmpeq :: Opcode
if_acmpne :: Opcode
goto :: Opcode
jsr :: Opcode
ret :: Opcode
tableswitch :: Opcode
lookupswitch :: Opcode
ireturn :: Opcode
lreturn :: Opcode
freturn :: Opcode
dreturn :: Opcode
areturn :: Opcode
vreturn :: Opcode -- return
getstatic :: Opcode
putstatic :: Opcode
getfield :: Opcode
putfield :: Opcode
invokevirtual :: Opcode
invokespecial :: Opcode
invokestatic :: Opcode
invokeinterface :: Opcode
invokedynamic :: Opcode
new :: Opcode
newarray :: Opcode
anewarray :: Opcode
arraylength :: Opcode
athrow :: Opcode
checkcast :: Opcode
instanceof :: Opcode
monitorenter :: Opcode
monitorexit :: Opcode
wide :: Opcode
multianewarray :: Opcode
ifnull :: Opcode
ifnonnull :: Opcode
goto_w :: Opcode
jsr_w :: Opcode

nop = Opcode 0x00
aconst_null = Opcode 0x01
iconst_m1 = Opcode 0x02
iconst_0 = Opcode 0x03
iconst_1 = Opcode 0x04
iconst_2 = Opcode 0x05
iconst_3 = Opcode 0x06
iconst_4 = Opcode 0x07
iconst_5 = Opcode 0x08
lconst_0 = Opcode 0x09
lconst_1 = Opcode 0x0a
fconst_0 = Opcode 0x0b
fconst_1 = Opcode 0x0c
fconst_2 = Opcode 0x0d
dconst_0 = Opcode 0x0e
dconst_1 = Opcode 0x0f
bipush = Opcode 0x10
sipush = Opcode 0x11
ldc = Opcode 0x12
ldc_w = Opcode 0x13
ldc2_w = Opcode 0x14
iload = Opcode 0x15
lload = Opcode 0x16
fload = Opcode 0x17
dload = Opcode 0x18
aload = Opcode 0x19
iload_0 = Opcode 0x1a
iload_1 = Opcode 0x1b
iload_2 = Opcode 0x1c
iload_3 = Opcode 0x1d
lload_0 = Opcode 0x1e
lload_1 = Opcode 0x1f
lload_2 = Opcode 0x20
lload_3 = Opcode 0x21
fload_0 = Opcode 0x22
fload_1 = Opcode 0x23
fload_2 = Opcode 0x24
fload_3 = Opcode 0x25
dload_0 = Opcode 0x26
dload_1 = Opcode 0x27
dload_2 = Opcode 0x28
dload_3 = Opcode 0x29
aload_0 = Opcode 0x2a
aload_1 = Opcode 0x2b
aload_2 = Opcode 0x2c
aload_3 = Opcode 0x2d
iaload = Opcode 0x2e
laload = Opcode 0x2f
faload = Opcode 0x30
daload = Opcode 0x31
aaload = Opcode 0x32
baload = Opcode 0x33
caload = Opcode 0x34
saload = Opcode 0x35
istore = Opcode 0x36
lstore = Opcode 0x37
fstore = Opcode 0x38
dstore = Opcode 0x39
astore = Opcode 0x3a
istore_0 = Opcode 0x3b
istore_1 = Opcode 0x3c
istore_2 = Opcode 0x3d
istore_3 = Opcode 0x3e
lstore_0 = Opcode 0x3f
lstore_1 = Opcode 0x40
lstore_2 = Opcode 0x41
lstore_3 = Opcode 0x42
fstore_0 = Opcode 0x43
fstore_1 = Opcode 0x44
fstore_2 = Opcode 0x45
fstore_3 = Opcode 0x46
dstore_0 = Opcode 0x47
dstore_1 = Opcode 0x48
dstore_2 = Opcode 0x49
dstore_3 = Opcode 0x4a
astore_0 = Opcode 0x4b
astore_1 = Opcode 0x4c
astore_2 = Opcode 0x4d
astore_3 = Opcode 0x4e
iastore = Opcode 0x4f
lastore = Opcode 0x50
fastore = Opcode 0x51
dastore = Opcode 0x52
aastore = Opcode 0x53
bastore = Opcode 0x54
castore = Opcode 0x55
sastore = Opcode 0x56
pop = Opcode 0x57
pop2 = Opcode 0x58
dup = Opcode 0x59
dup_x1 = Opcode 0x5a
dup_x2 = Opcode 0x5b
dup2 = Opcode 0x5c
dup2_x1 = Opcode 0x5d
dup2_x2 = Opcode 0x5e
swap = Opcode 0x5f
iadd = Opcode 0x60
ladd = Opcode 0x61
fadd = Opcode 0x62
dadd = Opcode 0x63
isub = Opcode 0x64
lsub = Opcode 0x65
fsub = Opcode 0x66
dsub = Opcode 0x67
imul = Opcode 0x68
lmul = Opcode 0x69
fmul = Opcode 0x6a
dmul = Opcode 0x6b
idiv = Opcode 0x6c
ldiv = Opcode 0x6d
fdiv = Opcode 0x6e
ddiv = Opcode 0x6f
irem = Opcode 0x70
lrem = Opcode 0x71
frem = Opcode 0x72
drem = Opcode 0x73
ineg = Opcode 0x74
lneg = Opcode 0x75
fneg = Opcode 0x76
dneg = Opcode 0x77
ishl = Opcode 0x78
lshl = Opcode 0x79
ishr = Opcode 0x7a
lshr = Opcode 0x7b
iushr = Opcode 0x7c
lushr = Opcode 0x7d
iand = Opcode 0x7e
land = Opcode 0x7f
ior = Opcode 0x80
lor = Opcode 0x81
ixor = Opcode 0x82
lxor = Opcode 0x83
iinc = Opcode 0x84
i2l = Opcode 0x85
i2f = Opcode 0x86
i2d = Opcode 0x87
l2i = Opcode 0x88
l2f = Opcode 0x89
l2d = Opcode 0x8a
f2i = Opcode 0x8b
f2l = Opcode 0x8c
f2d = Opcode 0x8d
d2i = Opcode 0x8e
d2l = Opcode 0x8f
d2f = Opcode 0x90
i2b = Opcode 0x91
i2c = Opcode 0x92
i2s = Opcode 0x93
lcmp = Opcode 0x94
fcmpl = Opcode 0x95
fcmpg = Opcode 0x96
dcmpl = Opcode 0x97
dcmpg = Opcode 0x98
ifeq = Opcode 0x99
ifne = Opcode 0x9a
iflt = Opcode 0x9b
ifge = Opcode 0x9c
ifgt = Opcode 0x9d
ifle = Opcode 0x9e
if_icmpeq = Opcode 0x9f
if_icmpne = Opcode 0xa0
if_icmplt = Opcode 0xa1
if_icmpge = Opcode 0xa2
if_icmpgt = Opcode 0xa3
if_icmple = Opcode 0xa4
if_acmpeq = Opcode 0xa5
if_acmpne = Opcode 0xa6
goto = Opcode 0xa7
jsr = Opcode 0xa8
ret = Opcode 0xa9
tableswitch = Opcode 0xaa
lookupswitch = Opcode 0xab
ireturn = Opcode 0xac
lreturn = Opcode 0xad
freturn = Opcode 0xae
dreturn = Opcode 0xaf
areturn = Opcode 0xb0
vreturn = Opcode 0xb1 -- return
getstatic = Opcode 0xb2
putstatic = Opcode 0xb3
getfield = Opcode 0xb4
putfield = Opcode 0xb5
invokevirtual = Opcode 0xb6
invokespecial = Opcode 0xb7
invokestatic = Opcode 0xb8
invokeinterface = Opcode 0xb9
invokedynamic = Opcode 0xba
new = Opcode 0xbb
newarray = Opcode 0xbc
anewarray = Opcode 0xbd
arraylength = Opcode 0xbe
athrow = Opcode 0xbf
checkcast = Opcode 0xc0
instanceof = Opcode 0xc1
monitorenter = Opcode 0xc2
monitorexit = Opcode 0xc3
wide = Opcode 0xc4
multianewarray = Opcode 0xc5
ifnull = Opcode 0xc6
ifnonnull = Opcode 0xc7
goto_w = Opcode 0xc8
jsr_w = Opcode 0xc9
