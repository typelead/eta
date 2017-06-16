{-# LANGUAGE OverloadedStrings #-}
module ETA.CodeGen.ArgRep
  (ArgRep(..),
   toArgRep,
   jrepType,
   isNonV,
   idArgRep,
   idPrimRep,
   primRepFieldType_maybe,
   primRepFieldType,
   ftArgRep,
   typeArgRep,
   repFieldTypes,
   repFieldType_maybe,
   contextLoad,
   contextStore,
   slowCallPattern,
   argRepFt
  ) where

import ETA.BasicTypes.Id
import ETA.Types.Type
import ETA.Types.TyCon            ( PrimRep(..) )
import ETA.BasicTypes.BasicTypes       ( RepArity )
-- import ETA.Main.DynFlags
import ETA.Debug
import Data.Maybe

import ETA.CodeGen.Rts
import ETA.Util
import Codec.JVM
import Data.Monoid ((<>))
import Data.Text (Text)

data ArgRep = P   -- Closure
             | N   -- int-sized non-ptr
             | V   -- Void
             | L   -- long
             | F   -- float
             | D   -- double
             | O   -- Java object pointer
             deriving (Eq, Show)

instance Outputable ArgRep where
  ppr = str . show

toArgRep :: PrimRep -> ArgRep
toArgRep VoidRep           = V
toArgRep PtrRep            = P
toArgRep IntRep            = N
toArgRep WordRep           = N
toArgRep AddrRep           = O
toArgRep Int64Rep          = L
toArgRep Word64Rep         = L
toArgRep FloatRep          = F
toArgRep DoubleRep         = D
toArgRep BoolRep           = N
toArgRep CharRep           = N
toArgRep ByteRep           = N
toArgRep ShortRep          = N
toArgRep (ObjectRep _)     = O
toArgRep (ArrayRep  _)     = O
--toArgRep (VecRep len elem) = error $ "Unsupported PrimRep: VecRep " ++ show len ++ " " ++ show elem

isNonV :: ArgRep -> Bool
isNonV V = False
isNonV _ = True

idArgRep :: Id -> ArgRep
idArgRep = toArgRep . idPrimRep

typeArgRep :: Type -> ArgRep
typeArgRep = toArgRep . typePrimRep

ftArgRep :: FieldType -> ArgRep
ftArgRep ft
  | ft == closureType = P
  | otherwise = case ft of
      BaseType JDouble            -> D
      BaseType JFloat             -> F
      BaseType JLong              -> L
      ObjectType _                -> O
      ArrayType  _                -> O
      _                           -> N

primRepFieldType_maybe :: PrimRep -> Maybe FieldType
primRepFieldType_maybe VoidRep = Nothing
primRepFieldType_maybe rep = Just $
  case rep of
    PtrRep              -> closureType
    IntRep              -> jint
    WordRep             -> jint
    AddrRep             -> jlong
    Int64Rep            -> jlong
    Word64Rep           -> jlong
    FloatRep            -> jfloat
    DoubleRep           -> jdouble
    BoolRep             -> jbool
    CharRep             -> jchar
    ByteRep             -> jbyte
    ShortRep            -> jshort
    ObjectRep className -> obj $ className
    ArrayRep  rep       -> ArrayType . fromJust $ primRepFieldType_maybe rep
    VoidRep             -> undefined

primRepFieldType :: PrimRep -> FieldType
primRepFieldType = expectJust "primRepFieldType" . primRepFieldType_maybe

-- NOTE: We assume that unboxed tuples won't occur
repFieldType_maybe :: Type -> Maybe FieldType
repFieldType_maybe = primRepFieldType_maybe . typePrimRep . jrepType

repFieldTypes :: [Type] -> [FieldType]
repFieldTypes = mapMaybe repFieldType_maybe

-- NOTE: Assumes StgContext is in local variable slot 1
contextLoad :: FieldType -> ArgRep -> Int -> Code
contextLoad _ argRep n =
     loadContext
  <> iconst jint (fromIntegral n)
  <> loadMethod
  where loadMethod = case argRep of
          P -> loadR
          N -> loadI
          L -> loadL
          F -> loadF
          D -> loadD
          O -> loadO
          _ -> error "contextLoad: V"

contextStore :: FieldType -> ArgRep -> Code -> Int -> Code
contextStore _ argRep storeCode n =
     loadContext
  <> iconst jint (fromIntegral n)
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

slowCallPattern :: [ArgRep] -> (Text, RepArity, [FieldType])
slowCallPattern (P: P: P: P: P: P: _) =
  ("ap_pppppp", 6, replicate 6 closureType)
slowCallPattern (P: P: P: P: P: _)    =
  ("ap_ppppp", 5, replicate 5 closureType)
slowCallPattern (P: P: P: P: _)       = ("ap_pppp", 4, replicate 4 closureType)
slowCallPattern (P: P: P: V: _)       = ("ap_pppv", 4, replicate 3 closureType)
slowCallPattern (P: P: P: _)          = ("ap_ppp", 3, replicate 3 closureType)
slowCallPattern (P: P: V: _)          = ("ap_ppv", 3, replicate 2 closureType)
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

idPrimRep :: Id -> PrimRep
idPrimRep = typePrimRep . idType

jrepType :: Type -> UnaryType
jrepType = head . flattenRepType . repType

argRepFt :: ArgRep -> FieldType
argRepFt P = closureType
argRepFt O = jobject
argRepFt N = jint
argRepFt F = jfloat
argRepFt L = jlong
argRepFt D = jdouble
argRepFt _ = panic "argRepFt: V argrep!"
