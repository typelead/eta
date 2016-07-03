{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Codec.JVM.Attr where

import Data.ByteString (ByteString)
import Data.Binary.Put (Put, putByteString, putWord8, runPut, putWord16be)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.List (foldl')
import Data.Word(Word8, Word16)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text

import Codec.JVM.ASM.Code.CtrlFlow
import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import Codec.JVM.ASM.Code (Code(..))
import Codec.JVM.ASM.Code.Instr (runInstr)
import Codec.JVM.ASM.Code.Types (Offset(..), StackMapTable(..))
import Codec.JVM.Const (Const(..))
import Codec.JVM.ConstPool (ConstPool, putIx)
import Codec.JVM.Internal (putI16, putI32)
import Codec.JVM.Types (PrimType(..), FieldType(..), IClassName(..),
                        mkFieldDesc')

data Attr
  = ACode
    { maxStack  :: Int
    , maxLocals :: Int
    , code      :: ByteString
    , codeAttrs :: [Attr] }
  | AStackMapTable [(Offset, StackMapFrame)]

instance Show Attr where
  show attr = "A" ++ (Text.unpack $ attrName attr)

attrName :: Attr -> Text
attrName (ACode _ _ _ _)      = "Code"
attrName (AStackMapTable _)   = "StackMapTable"

unpackAttr :: Attr -> [Const]
unpackAttr attr@(ACode _ _ _ xs) = (CUTF8 $ attrName attr):(unpackAttr =<< xs)
unpackAttr attr = return . CUTF8 . attrName $ attr

putAttr :: ConstPool -> Attr -> Put
putAttr cp attr = do
  putIx cp $ CUTF8 $ attrName attr
  let xs = runPut $ putAttrBody cp attr
  putI32 . fromIntegral $ LBS.length xs
  putByteString $ LBS.toStrict xs

putAttrBody :: ConstPool -> Attr -> Put
putAttrBody cp (ACode ms ls xs attrs) = do
  putI16 ms
  putI16 ls
  putI32 . fromIntegral $ BS.length xs
  putByteString xs
  putI16 0 -- TODO Exception table
  putI16 $ length attrs
  mapM_ (putAttr cp) attrs
putAttrBody cp (AStackMapTable xs) = do
  putI16 $ length xs
  putStackMapFrames cp xs

-- | http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.4
--
-- Offsets are absolute (the delta conversion happen during serialization)

data StackMapFrame
  = SameFrame -- Covers normal & extended
  | SameLocals1StackItem !VerifType -- Covers normal & extended
  | ChopFrame !Word8
  | AppendFrame !Word8 ![VerifType]
  | FullFrame ![VerifType] ![VerifType]
  deriving (Eq, Show)

putStackMapFrames :: ConstPool -> [(Offset, StackMapFrame)] -> Put
putStackMapFrames cp xs = snd $ foldl' f (0, return ()) xs
  where f (offset, put) (Offset frameOffset, frame)
          = (frameOffset, put *> putFrame frame)
          where delta = fromIntegral $ frameOffset -
                  (if offset == 0 then 0 else offset + 1)
                putVerifTy = putVerifType cp
                putFrame SameFrame =
                  if delta <= 63
                    then putWord8 delta
                    else do
                      putWord8 251
                      putWord16be $ fromIntegral delta
                putFrame (SameLocals1StackItem vt) = do
                  if delta <= 63
                    then putWord8 $ delta + 64
                    else do
                      putWord8 247
                      putWord16be $ fromIntegral delta
                  putVerifTy vt
                putFrame (ChopFrame k) = do
                  -- ASSERT (1 <= k <= 3)
                  putWord8 $ 251 - k
                  putWord16be $ fromIntegral delta
                putFrame (AppendFrame k vts) = do
                  -- ASSERT (1 <= k <= 3)
                  putWord8 $ 251 + k
                  putI16 $ fromIntegral delta
                  traverse_ putVerifTy vts
                putFrame (FullFrame locals stack) = do
                  putWord8 255
                  putI16 $ fromIntegral delta
                  putI16 $ length locals
                  traverse_ putVerifTy locals
                  putI16 $ length stack
                  traverse_ putVerifTy stack

-- TODO Return `Either` with error (currently CF.pop is unsafe)
toAttrs :: Int -> ConstPool -> Code -> [Attr]
toAttrs as cp code = f $ runInstr (instr code) cp where
  f (xs, cf, smt) = [ACode maxStack' maxLocals' xs attrs] where
      maxLocals' = max as $ CF.maxLocals cf
      maxStack' = CF.maxStack cf
      attrs = if null frames then [] else [AStackMapTable frames]
      frames = toStackMapFrames smt

-- TODO Verify that the conversion is correct
toStackMapFrames :: StackMapTable -> [(Offset, StackMapFrame)]
toStackMapFrames (StackMapTable smt)
  = reverse (fst $ foldl' f ([], c) cfs)
  where ((_,c):cfs) = IntMap.toAscList smt
        f (!xs, !cf') (!off, !cf) = ((Offset off, smf):xs, cf)
          where smf = generateStackMapFrame cf' cf

generateStackMapFrame :: CtrlFlow -> CtrlFlow -> StackMapFrame
generateStackMapFrame cf1@(CtrlFlow stack1 locals1)
                      cf2@(CtrlFlow stack2 locals2)
  | sameLocals && sz < 2
  = case sz of
      0 -> SameFrame
      1 -> SameLocals1StackItem (head . stackVal $ stack2)
      _ -> fullFrame
  | otherwise
  = if lszdiff <= 3
       -- TODO: The Append & ChopFrame logic needs to be checked
       then case IntMap.split lsz1 locals2 of
              (_, higherPart) -> AppendFrame (fromIntegral lszdiff) $
                compress . IntMap.elems $ higherPart
       else if lszdiff >= -3
               then ChopFrame $ fromIntegral (-lszdiff)
               else fullFrame
  where fullFrame = case compressCtrlFlow cf2 of
                      (locals, stack) -> FullFrame locals stack
        sameLocals = areLocalsSame locals1 locals2
        lsz1 = localsSize locals1
        lsz2 = localsSize locals2
        lszdiff = lsz2 - lsz1
        sz = stackSize stack2
