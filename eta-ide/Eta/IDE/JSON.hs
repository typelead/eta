{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char
import Data.Maybe
import System.Console.Haskeline

class IDEJSON a where
  ideJSON :: DynFlags -> a -> Value

instance IDEJSON a => IDEJSON [a] where
  ideJSON dflags xs = toJSON $ map (ideJSON dflags) xs

data IDEError = IDEError String

instance ToJSON IDEError where
  toJSON (IDEError msg) = object [ "error" .= msg ]

data IDEResponse a = IDEResponse
  { ideResponseCommand :: String
  , ideResponseArgs    :: [String]
  , ideResponseResult  :: a
  }

instance ToJSON a => ToJSON (IDEResponse a) where
  toJSON IDEResponse {..} =
    object
      [ "command" .= ideResponseCommand
      , "args"    .= ideResponseArgs
      , "result"  .= ideResponseResult
      ]

instance IDEJSON a => IDEJSON (IDEResponse a) where
  ideJSON dflags r = toJSON $ r { ideResponseResult = ideJSON dflags $ ideResponseResult r }

browseResponse :: G.Module -> [TyThing] -> IDEResponse [TyThing]
browseResponse m = IDEResponse "idebrowse" [G.moduleNameString $ G.moduleName m]

instance IDEJSON TyThing where
  ideJSON = thingJSON

outputJSONLn :: (ToJSON a, MonadIO io) => a -> InputT io ()
outputJSONLn x = liftIO $ LBS.putStrLn $ encode $ x

-- | Encode a TyThing to a JSON Value; used by the :idebrowse command.
-- Note that the structure of the resulting object is something like -
--
-- data ThingJSON = ThingJSON { isOp :: Bool, name :: String, type :: Maybe String }
--
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

  isOp = not $ any (\c -> c == '_' || isAlphaNum c) name

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

  styleUnqualified = mkUserStyle dflags neverQualify AllTheWay

  showDocWith mode = Pretty.showDoc mode (G.pprCols dflags)
