module Codec.JVM.ConstPool where

import Control.Monad (join)
import Data.Binary.Put (Put, putByteString, putWord8, putWord16be)
import Data.Map.Strict (Map)
import Data.Text.Encoding (encodeUtf8)

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Codec.JVM.Const
import Codec.JVM.Internal (putI16, putI32)
import Codec.JVM.Types

newtype CIx = CIx Int

newtype ConstPool = ConstPool (Map Const Int)
  deriving Show

mkConstPool :: [Const] -> ConstPool
mkConstPool defs = ConstPool . snd $ L.foldl' f (0, M.empty) defs where
  f acc c = L.foldl' f' acc $ unpack c where
    f' (i, xs) y = if M.member y xs then (i, xs) else (i + 1, M.insert y i xs)

run :: ConstPool -> [Const]
run (ConstPool xs) = fmap fst $ L.sortOn snd $ M.toList xs

size :: ConstPool -> Int
size (ConstPool xs) = M.size xs

index :: Const -> ConstPool -> Maybe CIx
index def (ConstPool xs) =  CIx . (+) 1 <$> M.lookup def xs

ix :: CIx -> Int
ix (CIx x) = x

unsafeIndex :: Const -> ConstPool -> CIx
unsafeIndex def cp = maybe (error $ join ["Constant '", show def, "'not found."]) id $ index def cp

unpack :: Const -> [Const]
unpack (CClass cn)                = unpackClassName cn
unpack c@(CValue (CString str))   = [c, CUTF8 str]
unpack (CFieldRef   ref)          = unpackFieldRef ref
unpack (CMethodRef  ref)          = unpackMethodRef ref
unpack (CNameAndType nd)          = unpackNameAndType nd
unpack c                          = [c]

unpackClassName :: IClassName -> [Const]
unpackClassName cn@(IClassName str) = [CClass cn, CUTF8 str]

unpackFieldDesc :: UName -> FieldDesc -> [Const]
unpackFieldDesc n (FieldDesc t) = unpackNameAndType (NameAndDesc n $ Desc t)

unpackFieldRef :: FieldRef -> [Const]
unpackFieldRef  ref@(FieldRef cn n ft) =
  CFieldRef  ref:unpackClassName cn ++ unpackFieldDesc n (mkFieldDesc ft)

unpackMethodRef :: MethodRef -> [Const]
unpackMethodRef ref@(MethodRef cn n fts rt) =
  CMethodRef ref:unpackClassName cn ++ unpackNameAndType (NameAndDesc n $ Desc (mkMethodDesc' fts rt))

unpackNameAndType :: NameAndDesc -> [Const]
unpackNameAndType nd@(NameAndDesc (UName str0) (Desc str1)) = [CNameAndType nd, CUTF8 str0, CUTF8 str1]

putIx :: ConstPool -> Const -> Put
putIx cp c = putWord16be . fromIntegral . ix $ unsafeIndex c cp

putConstPool :: ConstPool -> Put
putConstPool cp = mapM_ putConst $ run cp where
  putConst c = do
    putWord8 . constTag $ c
    case c of
      (CUTF8 str) -> do
        putI16 (T.length str)
        putByteString $ encodeUtf8 str
      (CValue (CInteger i)) ->
        putI32 i
      (CValue (CString str)) ->
        putIx' $ CUTF8 str
      (CClass (IClassName str)) ->
        putIx' $ CUTF8 str
      (CFieldRef (FieldRef cn n ft)) -> do
        putRef cn n $ mkFieldDesc' ft
      (CMethodRef (MethodRef cn n fts rt)) ->
        putRef cn n $ mkMethodDesc' fts rt
      (CNameAndType (NameAndDesc (UName n) (Desc d))) -> do
        putIx' $ CUTF8 n
        putIx' $ CUTF8 d
    where
      putRef cn n d = do
        putIx' $ CClass cn
        putIx' . CNameAndType $ NameAndDesc n (Desc d)
      putIx' = putIx cp


