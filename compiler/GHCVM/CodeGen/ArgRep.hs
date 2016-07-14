module GHCVM.CodeGen.ArgRep
  (JArgRep(..),
   toArgRep,
   isNonV,
   idJArgRep,
   primRepFieldType,
   repFieldTypes,
   repFieldType,
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

-- slowCallPattern :: [ArgRep] -> (FastString, RepArity)
-- slowCallPattern (P: P: P: P: P: P: _) = (fsLit "stg_ap_pppppp", 6)
-- slowCallPattern (P: P: P: P: P: _)    = (fsLit "stg_ap_ppppp", 5)
-- slowCallPattern (P: P: P: P: _)       = (fsLit "stg_ap_pppp", 4)
-- slowCallPattern (P: P: P: V: _)       = (fsLit "stg_ap_pppv", 4)
-- slowCallPattern (P: P: P: _)          = (fsLit "stg_ap_ppp", 3)
-- slowCallPattern (P: P: V: _)          = (fsLit "stg_ap_ppv", 3)
-- slowCallPattern (P: P: _)             = (fsLit "stg_ap_pp", 2)
-- slowCallPattern (P: V: _)             = (fsLit "stg_ap_pv", 2)
-- slowCallPattern (P: _)                = (fsLit "stg_ap_p", 1)
-- slowCallPattern (V: _)                = (fsLit "stg_ap_v", 1)
-- slowCallPattern (N: _)                = (fsLit "stg_ap_n", 1)
-- slowCallPattern (F: _)                = (fsLit "stg_ap_f", 1)
-- slowCallPattern (D: _)                = (fsLit "stg_ap_d", 1)
-- slowCallPattern (L: _)                = (fsLit "stg_ap_l", 1)
-- slowCallPattern (V16: _)              = (fsLit "stg_ap_v16", 1)
-- slowCallPattern (V32: _)              = (fsLit "stg_ap_v32", 1)
-- slowCallPattern (V64: _)              = (fsLit "stg_ap_v64", 1)
-- slowCallPattern []                    = (fsLit "stg_ap_0", 0)

-- NOTE: We assume that unboxed tuples won't occur
repFieldType :: Type -> Maybe FieldType
repFieldType ty = primRepFieldType . typeJPrimRep $ head flattened
  where flattened = flattenRepType (repType ty)

repFieldTypes :: [Type] -> [FieldType]
repFieldTypes = mapMaybe repFieldType
