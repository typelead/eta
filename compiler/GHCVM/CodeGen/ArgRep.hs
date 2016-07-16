module GHCVM.CodeGen.ArgRep
  (JArgRep(..),
   toArgRep,
   isNonV,
   idJArgRep,
   primRepFieldType,
   repFieldTypes,
   repFieldType,
   fieldTypeArgRep,
   contextLoad,
   contextStore,
   slowCallPattern
  ) where

import Id
import Type
import TyCon            ( PrimRep(..), primElemRepSizeB )
import BasicTypes       ( RepArity )
import DynFlags
import Data.Maybe
import GHCVM.Primitive
import GHCVM.CodeGen.Rts
import Codec.JVM
import Data.Monoid ((<>))
import Data.Text (Text)

data JArgRep = P   -- StgClosure
             | N   -- int-sized non-ptr
             | V   -- Void
             | L   -- long
             | F   -- float
             | D   -- double
             | O   -- Java object pointer

toJArgRep :: JPrimRep -> JArgRep
toJArgRep (HPrimRep primRep) = toArgRep primRep
toJArgRep JRepBool        = N
toJArgRep JRepChar           = N
toJArgRep JRepByte           = N
toJArgRep JRepShort          = N
toJArgRep (JRepObject _)     = O

toArgRep :: PrimRep -> JArgRep
toArgRep VoidRep           = V
toArgRep PtrRep            = P
toArgRep IntRep            = N
toArgRep WordRep           = N
toArgRep AddrRep           = N
toArgRep Int64Rep          = L
toArgRep Word64Rep         = L
toArgRep FloatRep          = F
toArgRep DoubleRep         = D
toArgRep (VecRep len elem) = error $ "Unsupported PrimRep: VecRep " ++ show len ++ " " ++ show elem

isNonV :: JArgRep -> Bool
isNonV V = False
isNonV _ = True

idJArgRep :: Id -> JArgRep
idJArgRep = toJArgRep . idJPrimRep

primRepFieldType :: JPrimRep -> Maybe FieldType
primRepFieldType (HPrimRep primRep) =
  case primRep of
    VoidRep           -> Nothing
    PtrRep            -> Just closureType
    IntRep            -> Just jint
    WordRep           -> Just jint
    AddrRep           -> Just jlong -- TODO: When implementing ByteArray#,
                                     --       revisit this.
    Int64Rep          -> Just jlong
    Word64Rep         -> Just jlong
    FloatRep          -> Just jfloat
    DoubleRep         -> Just jdouble
    (VecRep len elem) -> error $ "Unsupported PrimRep: VecRep " ++ show len ++ " " ++ show elem
primRepFieldType JRepBool               = Just jbool
primRepFieldType JRepChar               = Just jchar
primRepFieldType JRepByte               = Just jbyte
primRepFieldType JRepShort              = Just jshort
primRepFieldType (JRepObject className) = Just $ obj className

-- NOTE: We assume that unboxed tuples won't occur
repFieldType :: Type -> Maybe FieldType
repFieldType = primRepFieldType . typeJPrimRep . jrepType

repFieldTypes :: [Type] -> [FieldType]
repFieldTypes = mapMaybe repFieldType

fieldTypeArgRep :: FieldType -> JArgRep
fieldTypeArgRep ft
  | ft == closureType = P
  | otherwise =
    case ft of
      BaseType JDouble            -> D
      BaseType JFloat             -> F
      BaseType JLong              -> L
      ObjectType _                -> O
      ArrayType  _                -> O
      _                           -> N

-- NOTE: Assumes StgContext is in local variable slot 1
contextLoad :: FieldType -> JArgRep -> Int -> Code
contextLoad ft argRep n =
     loadContext
  <> iconst ft (fromIntegral n)
  <> loadMethod
  where loadMethod = case argRep of
          P -> loadR
          N -> loadI
          L -> loadL
          F -> loadF
          D -> loadD
          O -> loadO
          _ -> error "contextLoad: V"

contextStore :: FieldType -> JArgRep -> Code -> Int -> Code
contextStore ft argRep storeCode n =
     loadContext
  <> iconst ft (fromIntegral n)
  <> storeCode
  <> storeMethod
  where storeMethod = case argRep of
          P -> storeR
          N -> storeI
          L -> storeL
          F -> storeF
          D -> storeD
          O -> storeO
          _ -> error "contextStore: V"

slowCallPattern :: [JArgRep] -> (Text, RepArity, [FieldType])
slowCallPattern (P: P: P: P: P: P: _) =
  ("ap_pppppp", 6, replicate 6 closureType)
slowCallPattern (P: P: P: P: P: _)    =
  ("ap_ppppp", 5, replicate 5 closureType)
slowCallPattern (P: P: P: V: O: _)    =
  ("ap_pppvo", 5, replicate 3 closureType ++ [jobject])
slowCallPattern (P: P: P: P: _)       = ("ap_pppp", 4, replicate 4 closureType)
slowCallPattern (P: P: P: V: _)       = ("ap_pppv", 4, replicate 3 closureType)
slowCallPattern (P: P: V: O: _)       =
  ("ap_ppvo", 4, replicate 2 closureType ++ [jobject])
slowCallPattern (P: P: P: _)          = ("ap_ppp", 3, replicate 3 closureType)
slowCallPattern (P: P: V: _)          = ("ap_ppv", 3, replicate 2 closureType)
slowCallPattern (P: V: O: _)          = ("ap_pvo", 3, [closureType, jobject])
slowCallPattern (P: P: _)             = ("ap_pp", 2, replicate 2 closureType)
slowCallPattern (P: V: _)             = ("ap_pv", 2, [closureType])
slowCallPattern (P: _)                = ("ap_p", 1, [closureType])
slowCallPattern (O: _)                = ("ap_o", 1, [jobject])
slowCallPattern (N: _)                = ("ap_n", 1, [jint])
slowCallPattern (L: _)                = ("ap_l", 1, [jlong])
slowCallPattern (F: _)                = ("ap_f", 1, [jfloat])
slowCallPattern (D: _)                = ("ap_d", 1, [jdouble])
slowCallPattern (V: _)                = ("ap_v", 1, [])
slowCallPattern []                    = ("ap_0", 0, [])
