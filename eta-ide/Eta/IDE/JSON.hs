{-# LANGUAGE OverloadedStrings #-}
module Eta.IDE.JSON where

import Eta.BasicTypes.ConLike (ConLike(RealDataCon, PatSynCon))
import Eta.BasicTypes.PatSyn (patSynType)
import Eta.BasicTypes.DataCon (dataConUserType)
import Eta.BasicTypes.Var (varType)
import Eta.BasicTypes.Name (getOccString)
import Eta.Main.DynFlags (DynFlags)
import qualified Eta.Main.GHC as G
import Eta.Types.TyCon (isAlgTyCon)
import Eta.Types.Type (dropForAlls, splitFunTy_maybe, isPredTy, mkFunTy)
import Eta.Types.TypeRep
import Eta.Utils.Outputable
import qualified Eta.Utils.Pretty as Pretty

import Data.Aeson
import Data.Char
import Data.Maybe

-- | Encode a TyThing to a JSON Value; used by the :idebrowse command.
-- Much of this code was adapted from ghc-mod, see:
--  * https://github.com/DanielG/ghc-mod/blob/master/GhcMod/Exe/Browse.hs
--  * https://github.com/DanielG/ghc-mod/blob/master/core/GhcMod/Gap.hs
--  * https://github.com/DanielG/ghc-mod/blob/master/core/GhcMod/Doc.hs
thingJSON :: DynFlags -> TyThing -> Value
thingJSON dflags tyThing =
  object [ "isOp" .= isOp , "name" .= name , "type" .= typ ]
  where
  name = case tyThing of
    AnId x     -> showPpr dflags x
    AConLike x -> showPpr dflags x
    ATyCon x   -> showPpr dflags x
    ACoAxiom x -> showPpr dflags x

  typ = case tyThing of
    AnId x                   -> Just $ formatType $ varType x
    AConLike (RealDataCon x) -> Just $ formatType $ dataConUserType x
    AConLike (PatSynCon x)   -> Just $ formatType $ patSynType x
    ATyCon x                 -> unwords . (tyList x) <$> tyType x
    ACoAxiom x               -> Nothing
    where
    tyList x t = t : getOccString x : map getOccString (G.tyConTyVars x)

  tyType t
    | isDataTyCon t          = Just "data"
    | G.isNewTyCon t         = Just "newtype"
    | G.isClassTyCon t       = Just "class"
    | G.isTypeSynonymTyCon t = Just "type"
    | otherwise              = Nothing

  isDataTyCon t = isAlgTyCon t && not (G.isNewTyCon t) && not (G.isClassTyCon t)

  isOp = any (\c -> not $ c == '_' || isAlphaNum c) name

  formatType :: Type -> String
  formatType a = showOutputable $ removeForAlls a

  removeForAlls :: Type -> Type
  removeForAlls ty = removeForAlls' ty' tty'
    where
      ty'  = dropForAlls ty
      tty' = splitFunTy_maybe ty'

  removeForAlls' :: Type -> Maybe (Type, Type) -> Type
  removeForAlls' ty Nothing = ty
  removeForAlls' ty (Just (pre, ftype))
      | isPredTy pre        = mkFunTy pre (dropForAlls ftype)
      | otherwise           = ty

  showOutputable :: Outputable a => a -> String
  showOutputable = unwords . lines . showPage styleUnqualified . ppr

  showPage :: PprStyle -> SDoc -> String
  showPage style = showDocWith Pretty.PageMode . withPprStyleDoc dflags style

  styleUnqualified = mkUserStyle neverQualify AllTheWay

  showDocWith mode = Pretty.showDoc mode (G.pprCols dflags)
