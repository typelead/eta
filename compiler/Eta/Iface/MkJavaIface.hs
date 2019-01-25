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

mkJavaIface :: ClassIndex -> String -> TcRnIf glbl lcl ModIface
mkJavaIface _classsIdx clsNameHash = do
  let (_clsName, rest) = break (== ':') clsNameHash
      _importSpec = decodeJavaImportSpec $ drop 1 rest
      ids = []
      tycons = []
      faminsts = []
      mod = mkModule javaUnitId (mkModuleName clsNameHash)
      typeEnv = typeEnvFromEntities ids tycons faminsts
      mod_details = emptyModDetails { md_exports = map tyThingAvailInfo $ typeEnvElts typeEnv
                                    , md_types   = typeEnv }
  hsc_env <- getTopEnv
  (_, res) <- liftIO $ mkIface_ hsc_env Nothing mod HsSrcFile False noDependencies emptyGlobalRdrEnv
                emptyNameEnv NoWarnings (emptyHpcInfo False) False Sf_None [] mod_details
  case res of
    Just (iface, _) -> return iface
    Nothing -> return (error "mkJavaIface: Orphans")
