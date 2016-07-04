{-# LANGUAGE RecordWildCards #-}
module Codec.JVM.ASM.Code.CtrlFlow where

import Codec.JVM.Const
import Codec.JVM.ConstPool
import Codec.JVM.Types

import Data.Binary.Put(Put, putWord8, putWord16be)
import Data.IntMap.Strict (IntMap, Key)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word8, Word16)
import Data.List (foldl')

data CtrlFlow = CtrlFlow
  { stack  :: !Stack
  , locals :: !Locals }
  deriving (Eq, Show)

type Locals = IntMap VerifType

localsFromList :: [FieldType] -> Locals
localsFromList fts = IntMap.fromList kvs
  where vts = concatMap (reverse . fieldTypeToVerifType) fts
        kvs = zip [1..] vts

insert :: (Integral a) => a -> FieldType -> Locals -> Locals
insert n' ft = IntMap.union (IntMap.fromList vts)
  where n   = fromIntegral n'
        vts = zip [n, n+1] (fieldTypeToVerifType ft)

remove :: (Integral a) => a -> FieldType -> Locals -> Locals
remove n' ft locals = foldl' (flip ($)) locals vts
  where n   = fromIntegral n'
        vts = map IntMap.delete (take (fieldSize ft) [n, n+1])

areLocalsSame :: Locals -> Locals -> Bool
areLocalsSame locals1 locals2 = locals1 == locals2

localsSize :: Locals -> Int
localsSize locals =
  if IntMap.null locals
    then 0
    else fst . IntMap.findMax $ locals

empty :: CtrlFlow
empty = CtrlFlow (Stack [] 0 0) mempty

equiv :: CtrlFlow -> CtrlFlow -> Bool
equiv cf0 cf1 = (locals cf0 == locals cf1)
             && (stackVal $ stack cf0) == (stackVal $ stack cf1)

mapStack :: (Stack -> Stack) -> CtrlFlow -> CtrlFlow
mapStack f cf = cf { stack = f $ stack cf }

mapLocals :: (Locals -> Locals) -> CtrlFlow -> CtrlFlow
mapLocals f cf = cf { locals = f $ locals cf }

maxStack :: CtrlFlow -> Int
maxStack = stackMax . stack

maxLocals :: CtrlFlow -> Int
maxLocals = localsSize . locals

-- TODO: What to do with locals?
load :: Word8 -> FieldType -> CtrlFlow -> CtrlFlow
load n ft cf@CtrlFlow {..} =
  cf { locals = insert n ft locals
     , stack  = push ft stack }

store :: Word8 -> FieldType -> CtrlFlow -> CtrlFlow
store n ft cf@CtrlFlow {..} =
  cf { locals = insert n ft locals
     , stack  = pop ft stack }

data Stack = Stack
  { stackVal  :: ![VerifType]
  , stackMax  :: !Int
  , stackSize :: !Int }
  deriving (Eq, Show)

push :: FieldType -> Stack -> Stack
push ft (Stack xs m sz) = Stack (vts ++ xs) m' sz'
  where vts = fieldTypeToVerifType ft
        dsz = length vts
        sz' = dsz + sz
        m'  = max m sz'

pop :: FieldType -> Stack -> Stack
pop ft = pop' $ fieldSize ft

pop' :: Int -> Stack -> Stack
pop' s (Stack xs m sz) = Stack (drop s xs) m (sz - s)

data VerifType
 = VTop
 | VInteger
 | VFloat
 | VDouble
 | VLong
 | VNull
 | VUninitializedThis
 | VObject IClassName
 | VUninitialized Word16
 deriving (Eq, Show)

fieldTypeFlatVerifType :: FieldType -> VerifType
fieldTypeFlatVerifType ft = case ft of
  BaseType JBool              -> VInteger
  BaseType JByte              -> VInteger
  BaseType JChar              -> VInteger
  BaseType JShort             -> VInteger
  BaseType JInt               -> VInteger
  BaseType JLong              -> VLong
  BaseType JFloat             -> VFloat
  BaseType JDouble            ->  VDouble
  ObjectType cn               -> VObject cn
  ArrayType ft'               -> VObject . IClassName $ mkFieldDesc' ft'

fieldTypeToVerifType :: FieldType -> [VerifType]
fieldTypeToVerifType ft = case ft of
  BaseType JBool              -> [VInteger]
  BaseType JByte              -> [VInteger]
  BaseType JChar              -> [VInteger]
  BaseType JShort             -> [VInteger]
  BaseType JInt               -> [VInteger]
  BaseType JLong              -> [VTop, VLong]
  BaseType JFloat             -> [VFloat]
  BaseType JDouble            -> [VTop, VDouble]
  ObjectType cn               -> [VObject cn]
  ArrayType ft'               -> [VObject . IClassName $ mkFieldDesc' ft']

putVerifType :: ConstPool -> VerifType -> Put
putVerifType _ VTop                    = putWord8 0
putVerifType _ VInteger                = putWord8 1
putVerifType _ VFloat                  = putWord8 2
putVerifType _ VDouble                 = putWord8 3
putVerifType _ VLong                   = putWord8 4
putVerifType _ VNull                   = putWord8 5
putVerifType _ VUninitializedThis      = putWord8 6
putVerifType cp (VObject icn)          = do
  putWord8 7
  putIx cp $ CClass icn
putVerifType _ (VUninitialized offset) = do
  putWord8 8
  putWord16be offset

compressCtrlFlow :: CtrlFlow -> ([VerifType], [VerifType])
compressCtrlFlow CtrlFlow {..} = ( compress . IntMap.elems $ locals
                                 , compress . reverse . stackVal $ stack)

compress :: [VerifType] -> [VerifType]
compress (VLong:x:xs) = VLong : compress xs
compress (VDouble:x:xs) = VDouble : compress xs
compress (x:xs) = x : compress xs
