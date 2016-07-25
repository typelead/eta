{-# LANGUAGE RecordWildCards #-}
module Codec.JVM.ASM.Code.CtrlFlow where

import Codec.JVM.Const
import Codec.JVM.ConstPool
import Codec.JVM.Types
import Codec.JVM.Internal

import Data.IntMap.Strict (IntMap, Key)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word8, Word16)
import Data.List (foldl')

data CtrlFlow = CtrlFlow
  { stack  :: !Stack
  , locals :: !Locals }
  deriving (Eq, Show)

data Locals = Locals
  { localsMap :: !(IntMap VerifType)
  , localsSize :: !Int
  , localsMax :: !Int }
  deriving (Eq, Show)

localsFromList :: [FieldType] -> Locals
localsFromList fts = Locals mp sz sz
  where vts = concatMap (reverse . fieldTypeToVerifType) fts
        kvs = zip [0..] vts
        mp = IntMap.fromList kvs
        sz = length vts

-- Should only be used on non-empty
computeNumLocals :: IntMap VerifType -> Int
computeNumLocals = (+1) . fst . IntMap.findMax

localVts :: Locals -> [VerifType]
localVts (Locals mp _ _) = IntMap.elems mp

-- TODO: Check if verif types are in the right order
insert :: (Integral a) => a -> FieldType -> Locals -> Locals
insert n' ft (Locals mp _ mx)= Locals mp' sz' mx'
  where n   = fromIntegral n'
        vts = zip [n, n+1] (reverse . fieldTypeToVerifType $ ft)
        mp' = IntMap.union (IntMap.fromList vts) mp
        sz' = computeNumLocals mp'
        mx' = max mx sz'

remove :: (Integral a) => a -> FieldType -> Locals -> Locals
remove n' ft (Locals mp _ mx) = Locals mp' sz' mx
  where n   = fromIntegral n'
        delta = fieldSize ft
        deletes = map IntMap.delete (take delta [n, n+1])
        mp' = foldl' (flip ($)) mp deletes
        sz' = computeNumLocals mp'

areLocalsSame :: Locals -> Locals -> Bool
areLocalsSame locals1 locals2 = locals1 == locals2

empty :: CtrlFlow
empty = CtrlFlow (Stack mempty 0 0) (Locals mempty 0 0)

equiv :: CtrlFlow -> CtrlFlow -> Bool
equiv cf0 cf1 = (locals cf0 == locals cf1)
             && stackVal (stack cf0) == stackVal (stack cf1)

mapStack :: (Stack -> Stack) -> CtrlFlow -> CtrlFlow
mapStack f cf = cf { stack = f $ stack cf }

mapLocals :: (Locals -> Locals) -> CtrlFlow -> CtrlFlow
mapLocals f cf = cf { locals = f $ locals cf }

maxStack :: CtrlFlow -> Int
maxStack = stackMax . stack

maxLocals :: CtrlFlow -> Int
maxLocals = localsMax . locals

-- TODO: What to do with locals?
load :: (Integral a) => a -> FieldType -> CtrlFlow -> CtrlFlow
load n ft cf@CtrlFlow {..} =
  cf { locals = insert n ft locals
     , stack  = push ft stack }

store :: (Integral a) => a -> FieldType -> CtrlFlow -> CtrlFlow
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

vpush :: VerifType -> Stack -> Stack
vpush vt (Stack xs m sz) = Stack (vt:xs) m' sz'
  where sz' = 1 + sz
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
  BaseType JDouble            -> VDouble
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
compressCtrlFlow CtrlFlow {..} = ( compress . localVts $ locals
                                 , compress . reverse . stackVal $ stack)

compress :: [VerifType] -> [VerifType]
compress [] = []
compress (VLong:_:xs) = VLong : compress xs
compress (VDouble:_:xs) = VDouble : compress xs
compress (x:xs) = x : compress xs

merge :: CtrlFlow -> [CtrlFlow] -> CtrlFlow
merge cf cfs = CtrlFlow stack' locals'
  where (smx', lmx') = foldl' (\(smx, lmx) cf ->
                                 ( max smx (maxStack cf)
                                 , max lmx (maxLocals cf)))
                              ( maxStack cf
                              , maxLocals cf )
                              cfs
        mergedStack = case cfs of
          (s:_) -> stack s
          _     -> stack cf
        stack' = mergedStack { stackMax = smx' }
        locals' = (locals cf) { localsMax = lmx' }
