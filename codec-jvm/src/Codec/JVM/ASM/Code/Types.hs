{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codec.JVM.ASM.Code.Types where

import Codec.JVM.ASM.Code.CtrlFlow (CtrlFlow)

import Data.IntMap.Strict (IntMap)

newtype Offset = Offset Int -- absolute
  deriving (Num, Show)

newtype StackMapTable = StackMapTable (IntMap CtrlFlow)
  deriving Monoid

