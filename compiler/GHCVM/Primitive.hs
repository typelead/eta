module GHCVM.Primitive where

import GHCVM.Prelude.ForeignCall(CType(..))
import GHCVM.Utils.Outputable
import GHCVM.Types.Type
import GHCVM.Types.TypeRep
import GHCVM.BasicTypes.Unique
import GHCVM.Utils.FastString
import GHCVM.Types.TyCon
import GHCVM.Prelude.PrelInfo
import GHCVM.BasicTypes.BasicTypes
import GHCVM.BasicTypes.Name
import GHCVM.BasicTypes.Id
import GHCVM.BasicTypes.Avail
import GHCVM.TypeCheck.TcType (tcSplitTyConApp_maybe)
import GHCVM.Prelude.PrelNames
import GHCVM.Prelude.TysPrim
import Data.Maybe
import GHCVM.Main.HscTypes
import GHCVM.Prelude.PrimOp
import GHCVM.Prelude.TysPrim
import GHCVM.BasicTypes.MkId

import qualified Data.Text as Text
import Data.Text (Text)
import GHCVM.CodeGen.Name
import Codec.JVM

mkJObjectTy :: Type -> Type
mkJObjectTy ty = TyConApp jobjectPrimTyCon [ty]

data JPrimRep = HPrimRep PrimRep
              | JRepBool
              | JRepChar
              | JRepByte
              | JRepShort
              | JRepObject Text
              deriving (Eq, Show)

intRep :: JPrimRep
intRep = HPrimRep IntRep

typeJPrimRep :: UnaryType -> JPrimRep
typeJPrimRep ty =
  case repType ty of
    UbxTupleRep _ -> pprPanic "typeJPrimRep: isUnboxedTypeTyCon" (ppr ty)
    UnaryRep rep -> case rep of
      TyConApp tc tys -> fromMaybe (HPrimRep $ tyConPrimRep tc)
                       $ maybeJRep tc tys
      FunTy _ _     -> HPrimRep PtrRep
      AppTy _ _     -> HPrimRep PtrRep
      TyVarTy _     -> HPrimRep PtrRep
      _             -> pprPanic "typeJPrimRep: UnaryRep" (ppr ty)

  -- case splitTyConApp_maybe ty of
  -- Just (tyCon, tys) -> if isUnboxedTupleTyCon tyCon
  --                         then pprPanic "typeJPrimRep: isUnboxedTypeTyCon" (ppr ty)
  --                         else case maybeJRep tyCon tys of
  --                                Just primRep -> primRep
  --                                Nothing -> HPrimRep $ tyConPrimRep tyCon
  -- Nothing -> pprPanic "typeJPrimRep: Unknown " (ppr ty)

maybeJRep :: TyCon -> [Type] -> Maybe JPrimRep
maybeJRep tyCon tys
  | tcUnique == jboolPrimTyConKey = Just JRepBool
  | tcUnique == jcharPrimTyConKey    = Just JRepChar
  | tcUnique == jbytePrimTyConKey    = Just JRepByte
  | tcUnique == jshortPrimTyConKey   = Just JRepShort
-- NOTE: A tag for a object MUST have an associated CType!
  | tcUnique == jobjectPrimTyConKey  =
    case splitTyConApp_maybe (head tys) of
      Just (tyCon1, _) ->
        case tyConCType_maybe tyCon1 of
          Just (CType _ _ fs) -> Just . JRepObject . Text.map (\c -> if c == '.' then '/' else c) . fastStringToText $ fs
          Nothing -> pprPanic "You should annotate " $ ppr tyCon <> ppr tys
      Nothing -> Just $ JRepObject jobjectC
  | otherwise                        = Nothing
  where tcUnique = tyConUnique tyCon

isVoidJRep :: JPrimRep -> Bool
isVoidJRep (HPrimRep VoidRep) = True
isVoidJRep _  = False

isPtrJRep :: JPrimRep -> Bool
isPtrJRep (HPrimRep PtrRep) = True
isPtrJRep _  = False

-- TODO: Is this right?
isVoidJTy :: Type -> Bool
isVoidJTy = isVoidJRep . typeJPrimRep

idJPrimRep :: Id -> JPrimRep
idJPrimRep = typeJPrimRep . idType

jrepType :: Type -> UnaryType
jrepType = head . flattenRepType . repType

mkJPrimRep :: PrimRep -> JPrimRep
mkJPrimRep = HPrimRep

objRep :: Text -> JPrimRep
objRep = JRepObject

tcSplitIOType_maybe :: Type -> Maybe (TyCon, Type)
tcSplitIOType_maybe ty
  = case tcSplitTyConApp_maybe ty of
        Just (ioTyCon, [ioResType])
         | ioTyCon `hasKey` ioTyConKey ->
            Just (ioTyCon, ioResType)
        _ ->
            Nothing

tcSplitJavaType_maybe :: Type -> Maybe (TyCon, Type, Type)
tcSplitJavaType_maybe ty
  = case tcSplitTyConApp_maybe ty of
        Just (javaTyCon, [javaTagType, javaResType])
         | javaTyCon `hasKey` javaTyConKey  ->
            Just (javaTyCon, javaTagType, javaResType)
        _ ->
            Nothing
