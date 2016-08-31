module Codec.JVM.ConstPool where

import Control.Arrow (second)
import Control.Monad (join, replicateM, forM)
import Data.IntMap.Lazy ((!))
import qualified Data.IntMap.Lazy as LazyMap
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import Data.Function (fix)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Codec.JVM.Const
import Codec.JVM.Internal
import Codec.JVM.Types

newtype CIx = CIx Int

newtype ConstPool = ConstPool (Map Const Int)
  deriving Show

type IxConstPool = LazyMap.IntMap Const

mkConstPool :: [Const] -> ConstPool
mkConstPool defs = ConstPool . snd $ L.foldl' f (0, M.empty) defs
  where f acc c = L.foldl' f' acc $ unpack c
          where f' (i, xs) y = if M.member y xs
                               then (i, xs)
                               else (i + constPoolSpace y, M.insert y i xs)

constPoolSpace :: Const -> Int
constPoolSpace (CValue (CLong _)) = 2
constPoolSpace (CValue (CDouble _)) = 2
constPoolSpace _ = 1

run :: ConstPool -> [Const]
run (ConstPool xs) = fmap fst $ L.sortOn snd $ M.toList xs

size :: ConstPool -> Int
size (ConstPool xs) = (M.foldl' max 0 xs) + 1

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
unpack (CInterfaceMethodRef ref)  = unpackInterfaceMethodRef ref
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

unpackInterfaceMethodRef :: MethodRef -> [Const]
unpackInterfaceMethodRef ref@(MethodRef cn n fts rt) =
  CInterfaceMethodRef ref:unpackClassName cn ++ unpackNameAndType (NameAndDesc n $ Desc (mkMethodDesc' fts rt))

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
        -- TODO: This should be encoded to modified UTF-8
        --       Works for code points below 0xFFFFFF
        let encoded = encodeUtf8 str
        putI16 $ BS.length encoded
        putByteString encoded
      (CValue (CInteger i)) ->
        putWord32be $ fromIntegral i  -- TODO: Change to putInt32be
      (CValue (CString s)) ->
        putIx' $ CUTF8 s
      (CValue (CLong l)) ->
        putWord64be $ fromIntegral l -- TODO: Change to putInt64be
      (CValue (CFloat f)) ->
        putFloatbe f
      (CValue (CDouble d)) ->
        putDoublebe d
      (CClass (IClassName str)) ->
        putIx' $ CUTF8 str
      (CFieldRef (FieldRef cn n ft)) ->
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

getConstPool :: Int -> Get IxConstPool
getConstPool n = do
  poolPairs <- decodeConsts 1
  -- Knot-tying for single-pass build of the constant pool
  return $ fix (\cp -> LazyMap.fromList $ map (second ($ cp)) poolPairs)
  where decodeConsts i
          | i > n = return []
          | otherwise = do (f, di) <- getConst
                           cs <- decodeConsts (i + di)
                           return $ (i, f) : cs

getConstAt :: (Integral a) => a -> IxConstPool -> Const
getConstAt i cp = (!) cp $ fromIntegral i

putConstAt :: (Integral a) => IxConstPool -> a -> Const -> IxConstPool
putConstAt pool i c = LazyMap.insert (fromIntegral i) c pool

getConst :: Get (IxConstPool -> Const, Int)
getConst = do
  tag <- getWord8
  case tag of
    1 -> do
      len <- getWord16be
      let len' = fromIntegral len
      bytes <- getByteString len'
      {- TODO: This fails for Unicode codepoints beyond U+FFFF
                because Modified-UTF8 doesn't support 4-byte
                representations and instead uses two-times-three-byte
                format. -}
      return (const $ CUTF8 $ decodeUtf8 bytes, 1)
    3 -> do
      word <- getWord32be
      return (const $ CValue (CInteger (fromIntegral word)), 1)
    4 -> do
      word <- getWord32be
      return (const $ CValue (CFloat (wordToFloat word)), 1)
    5 -> do
      word <- getWord64be
      return (const $ CValue (CLong (fromIntegral word)), 2)
    6 -> do
      word <- getWord64be
      return (const $ CValue (CDouble (wordToDouble word)), 2)
    7 -> do
      textIx <- getWord16be
      return ( \cp -> let CUTF8 t = getConstAt textIx cp
                      in CClass (IClassName t)
             , 1)
    8 -> do
      textIx <- getWord16be
      return ( \cp -> let CUTF8 t = getConstAt textIx cp
                      in CValue (CString t)
             , 1)
    9 -> do
      classIx <- getWord16be
      nameAndTypeIx <- getWord16be
      return ( \cp -> let iclassName = case getConstAt classIx cp of
                            CClass i -> i
                            c -> error $ "Type 9 (FieldRef): " ++ show (classIx, nameAndTypeIx, c)
                          CNameAndType (NameAndDesc uname (Desc desc)) = getConstAt nameAndTypeIx cp
                          Just ft = decodeFieldDesc desc
                      in CFieldRef $ FieldRef iclassName uname ft
             , 1)
    10 -> do
      classIx <- getWord16be
      nameAndTypeIx <- getWord16be
      return ( \cp -> let CClass iclassName = getConstAt classIx cp
                          CNameAndType (NameAndDesc uname (Desc desc)) = getConstAt nameAndTypeIx cp
                          Just (fts, rft) = decodeMethodDesc desc
                      in CMethodRef $ MethodRef iclassName uname fts rft
             , 1)
    11 -> do
      classIx <- getWord16be
      nameAndTypeIx <- getWord16be
      return ( \cp -> let CClass iclassName = getConstAt classIx cp
                          CNameAndType (NameAndDesc uname (Desc desc)) = getConstAt nameAndTypeIx cp
                          Just (fts, rft) = decodeMethodDesc desc
                      in CInterfaceMethodRef $ MethodRef iclassName uname fts rft
             , 1)
    12 -> do
      nameIx <- getWord16be
      descriptorIx <- getWord16be
      return ( \cp -> let CUTF8 name' = getConstAt nameIx cp
                          CUTF8 desc' = getConstAt descriptorIx cp
                      in CNameAndType (NameAndDesc (UName name') (Desc desc'))
             , 1)
    val -> error $ "getConst: " ++ show val
