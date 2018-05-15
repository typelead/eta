-- | This module provides an interface for typechecker plugins to
-- access select functions of the 'TcM', principally those to do with
-- reading parts of the state.
{-# LANGUAGE CPP #-}

module ETA.TypeCheck.TcPluginM (
#ifdef ETA_REPL
        -- * Basic TcPluginM functionality
        TcPluginM,
        tcPluginIO,
        tcPluginTrace,
        unsafeTcPluginTcM,

        -- * Finding Modules and Names
        FindResult(..),
        findImportedModule,
        lookupOrig,

        -- * Looking up Names in the typechecking environment
        tcLookupGlobal,
        tcLookupTyCon,
        tcLookupDataCon,
        tcLookupClass,
        tcLookup,
        tcLookupId,

        -- * Getting the TcM state
        getTopEnv,
        getEnvs,
        getInstEnvs,
        getFamInstEnvs,
        matchFam,

        -- * Type variables
        newFlexiTyVar,
        isTouchableTcPluginM,

        -- * Zonking
        zonkTcType,
        zonkCt
#endif
    ) where

#ifdef ETA_REPL
import qualified ETA.TypeCheck.TcRnMonad as TcRnMonad
import qualified ETA.TypeCheck.TcSMonad as TcSMonad
import qualified ETA.TypeCheck.TcEnv as TcEnv
import qualified ETA.TypeCheck.TcMType as TcMType
import qualified ETA.TypeCheck.Inst as Inst
import qualified ETA.TypeCheck.FamInst as FamInst
import qualified ETA.Iface.IfaceEnv as IfaceEnv
import qualified ETA.Main.Finder as Finder

import ETA.Types.FamInstEnv ( FamInstEnv )
import ETA.TypeCheck.TcRnMonad  ( TcGblEnv, TcLclEnv, Ct, TcPluginM
                  , unsafeTcPluginTcM, liftIO, traceTc )
import ETA.TypeCheck.TcMType    ( TcTyVar, TcType )
import ETA.TypeCheck.TcEnv      ( TcTyThing )
import ETA.TypeCheck.TcEvidence ( TcCoercion )

import ETA.BasicTypes.Module
import ETA.BasicTypes.Name
import ETA.Types.TyCon
import ETA.BasicTypes.DataCon
import ETA.Types.Class
import ETA.Main.HscTypes
import ETA.Utils.Outputable
import ETA.Types.Type
import ETA.BasicTypes.Id
import ETA.Types.InstEnv
import ETA.Utils.FastString


-- | Perform some IO, typically to interact with an external tool.
tcPluginIO :: IO a -> TcPluginM a
tcPluginIO a = unsafeTcPluginTcM (liftIO a)

-- | Output useful for debugging the compiler.
tcPluginTrace :: String -> SDoc -> TcPluginM ()
tcPluginTrace a b = unsafeTcPluginTcM (traceTc a b)


findImportedModule :: ModuleName -> Maybe FastString -> TcPluginM FindResult
findImportedModule mod_name mb_pkg = do
    hsc_env <- getTopEnv
    tcPluginIO $ Finder.findImportedModule hsc_env mod_name mb_pkg

lookupOrig :: Module -> OccName -> TcPluginM Name
lookupOrig mod = unsafeTcPluginTcM . IfaceEnv.lookupOrig mod


tcLookupGlobal :: Name -> TcPluginM TyThing
tcLookupGlobal = unsafeTcPluginTcM . TcEnv.tcLookupGlobal

tcLookupTyCon :: Name -> TcPluginM TyCon
tcLookupTyCon = unsafeTcPluginTcM . TcEnv.tcLookupTyCon

tcLookupDataCon :: Name -> TcPluginM DataCon
tcLookupDataCon = unsafeTcPluginTcM . TcEnv.tcLookupDataCon

tcLookupClass :: Name -> TcPluginM Class
tcLookupClass = unsafeTcPluginTcM . TcEnv.tcLookupClass

tcLookup :: Name -> TcPluginM TcTyThing
tcLookup = unsafeTcPluginTcM . TcEnv.tcLookup

tcLookupId :: Name -> TcPluginM Id
tcLookupId = unsafeTcPluginTcM . TcEnv.tcLookupId


getTopEnv :: TcPluginM HscEnv
getTopEnv = unsafeTcPluginTcM TcRnMonad.getTopEnv

getEnvs :: TcPluginM (TcGblEnv, TcLclEnv)
getEnvs = unsafeTcPluginTcM TcRnMonad.getEnvs

getInstEnvs :: TcPluginM InstEnvs
getInstEnvs = unsafeTcPluginTcM Inst.tcGetInstEnvs

getFamInstEnvs :: TcPluginM (FamInstEnv, FamInstEnv)
getFamInstEnvs = unsafeTcPluginTcM FamInst.tcGetFamInstEnvs

matchFam :: TyCon -> [Type] -> TcPluginM (Maybe (TcCoercion, TcType))
matchFam tycon args = unsafeTcPluginTcM $ TcSMonad.matchFamTcM tycon args


newFlexiTyVar :: Kind -> TcPluginM TcTyVar
newFlexiTyVar = unsafeTcPluginTcM . TcMType.newFlexiTyVar

isTouchableTcPluginM :: TcTyVar -> TcPluginM Bool
isTouchableTcPluginM = unsafeTcPluginTcM . TcRnMonad.isTouchableTcM


zonkTcType :: TcType -> TcPluginM TcType
zonkTcType = unsafeTcPluginTcM . TcMType.zonkTcType

zonkCt :: Ct -> TcPluginM Ct
zonkCt = unsafeTcPluginTcM . TcMType.zonkCt
#endif
