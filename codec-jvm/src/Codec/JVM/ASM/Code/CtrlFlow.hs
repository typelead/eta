module Codec.JVM.ASM.Code.CtrlFlow where

import Codec.JVM.Types (FieldType, fieldSize)
import Data.IntMap.Strict as IntMap
import Data.Word (Word8)

data CtrlFlow = CtrlFlow
  { stack  :: Stack
  , locals :: IntMap FieldType }
  deriving (Eq, Show)

empty :: CtrlFlow
empty = CtrlFlow mempty mempty

equiv :: CtrlFlow -> CtrlFlow -> Bool
equiv cf0 cf1 = (locals cf0 == locals cf1) && (stackVal $ stack cf0) == (stackVal $ stack cf1)

mapStack :: (Stack -> Stack) -> CtrlFlow -> CtrlFlow
mapStack f cf = cf { stack = f $ stack cf }

maxStack :: CtrlFlow -> Int
maxStack = stackMax . stack

maxLocals :: CtrlFlow -> Int
maxLocals = maybe 0 (succ . fst . fst) . IntMap.maxViewWithKey . locals

load :: Word8 -> FieldType -> CtrlFlow -> CtrlFlow
load n ft cf = cf { locals = IntMap.insert (fromIntegral n) ft $ locals cf, stack = push ft $ stack cf }

store :: Word8 -> FieldType -> CtrlFlow -> CtrlFlow
store n ft cf = cf { locals = IntMap.insert (fromIntegral n) ft $ locals cf, stack = pop ft $ stack cf }

data Stack = Stack
  { stackVal    :: [FieldType]
  , stackMax    :: Int }
  deriving (Eq, Show)

instance Monoid Stack where
  mempty = Stack [] 0
  mappend (Stack vs0 m0) (Stack vs1 m1) = Stack (vs1 ++ vs0) (max m0 m1)

push :: FieldType -> Stack -> Stack
push ft (Stack xs i) = Stack ys (max i $ sum $ fieldSize <$> ys) where ys = ft:xs

pop :: FieldType -> Stack -> Stack
pop ft = pop' $ fieldSize ft

pop' :: Int -> Stack -> Stack
pop' s (Stack xs i) = Stack (drop s xs) i
