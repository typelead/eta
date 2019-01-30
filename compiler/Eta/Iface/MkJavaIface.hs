{-
(c) Rahul Muttineni 2019
-}

-- | Module for constructing @ModIface@ values for direct java imports.
module Eta.Iface.MkJavaIface ( mkJavaIface ) where

import Eta.Iface.MkIface
import Eta.TypeCheck.TcRnMonad
import Eta.Main.HscTypes
import Eta.Main.DynFlags
import Eta.BasicTypes.Module
import Eta.BasicTypes.Interop
import Eta.BasicTypes.RdrName
import Eta.BasicTypes.NameEnv
import Eta.BasicTypes.Var
import Eta.Types.TyCon
import Eta.Types.InstEnv
import Eta.Utils.Maybes

import Eta.REPL.ClassInfo

import Control.Arrow ((&&&))
import Data.Char
import Data.Monoid
import qualified Data.Map as Map

mkJavaIface :: String -> TcRnIf glbl lcl ModIface
mkJavaIface clsNameHash = do
  hsc_env <- getTopEnv
  classIndex <- liftIO $ getClassIndex hsc_env

  let (clsName, rest) = break (== ':') clsNameHash
      importSpec = decodeJavaImportSpec $ drop 1 rest
      clsInfo = expectJust ("mkJavaIface: Missing class " ++ clsName)
              $ lookupClassIndex clsName classIndex

  (ids, tycons, clsInsts) <- generateBindings clsInfo importSpec

  let mod = mkModule javaUnitId (mkModuleName clsNameHash)
      typeEnv = typeEnvFromEntities ids tycons []
      mod_details = emptyModDetails { md_exports = map tyThingAvailInfo $ typeEnvElts typeEnv
                                    , md_types   = typeEnv
                                    , md_insts   = clsInsts }
  (_, res) <- liftIO $ mkIface_ hsc_env Nothing mod HsSrcFile False noDependencies
                emptyGlobalRdrEnv emptyNameEnv NoWarnings (emptyHpcInfo False) False
                Sf_None [] mod_details
  case res of
    Just (iface, _) -> return iface
    Nothing -> return (error "mkJavaIface: Orphans")

generateBindings :: CachedClassInfo -> JavaImportSpec
                 -> TcRnIf glbl lcl ([Id], [TyCon], [ClsInst])
generateBindings clsInfo (JavaImportSpec { jisMethods, jisFields }) = do
  let methods  = unCachedMap $ ciMethods clsInfo
      fields   = unCachedMap $ ciFields  clsInfo

      genIndexes specs entities =
        ( Map.fromList $ zip (Map.keys entities) [0..]
        , Map.fromList $ map (isIndex &&& id) specs)
      (indexedMethods, indexedMethodSpecs) = genIndexes jisMethods methods
      (indexedFields,  indexedFieldSpecs)  = genIndexes jisFields fields

      genMethodSpecs indexedEntities indexedSpecs mkInfo entities =
        Map.mapWithKey f entities
        where f name infos
                | Just GenImportSpec { isEffect, isNullable } <- lookup
                = map (\i -> mkMethodGenSpec (mkInfo i) isEffect isNullable) infos
                | otherwise
                = map (\i -> mkMethodGenSpec (mkInfo i) defaultEffect defaultNullable) infos
                where lookup = do
                         i <- Map.lookup name indexedEntities
                         Map.lookup i indexedSpecs
      fullGenSpecs = Map.unionsWith (++)
        [ Map.mapKeysWith (++) firstLower $
            genMethodSpecs indexedMethods indexedMethodSpecs Method methods
        , Map.mapKeysWith (++) (("get" <>) . firstUpper) $ fieldSpecs GetField
        , Map.mapKeysWith (++) (("set" <>) . firstUpper) $ fieldSpecs SetField
        ]
        where fieldSpecs mkInfo = genMethodSpecs indexedFields indexedFieldSpecs mkInfo fields

  (idss, clsInstss) <- fmap unzip $ mapM (uncurry generateSingleSpec) (Map.assocs fullGenSpecs)

  let ids      = concat idss
      clsInsts = concat clsInstss

  return (ids, [], clsInsts)

generateSingleSpec :: String -> [MethodGenSpec] -> TcRnIf glbl lcl ([Id], [ClsInst])
generateSingleSpec _name _spec = return ([], [])

data MethodOrFieldInfo =
   Method MethodInfo
 | GetField FieldInfo
 | SetField FieldInfo

data MethodGenSpec =
  MethodGenSpec { mgsInfo     :: MethodOrFieldInfo
                , mgsEffect   :: Effect
                , mgsNullable :: Bool }

mkMethodGenSpec :: MethodOrFieldInfo -> Effect -> Bool -> MethodGenSpec
mkMethodGenSpec mfinfo e b =
  MethodGenSpec { mgsInfo     = mfinfo
                , mgsEffect   = e
                , mgsNullable = b }

defaultEffect :: Effect
defaultEffect = JavaEffect

defaultNullable :: Bool
defaultNullable = False

firstLower :: String -> String
firstLower (x:xs) = toLower x : xs
firstLower []     = []

firstUpper :: String -> String
firstUpper (x:xs) = toUpper x : xs
firstUpper []     = []
