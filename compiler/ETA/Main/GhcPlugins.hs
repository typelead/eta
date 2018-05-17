{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module is not used by GHC itself.  Rather, it exports all of
-- the functions and types you are likely to need when writing a
-- plugin for GHC. So authors of plugins can probably get away simply
-- with saying "import GhcPlugins".
-- 
-- Particularly interesting modules for plugin writers include
-- "CoreSyn" and "CoreMonad".
module ETA.Main.GhcPlugins(
        module ETA.Main.Plugins,
        module ETA.BasicTypes.RdrName,
        module ETA.BasicTypes.OccName,
        module ETA.BasicTypes.Name,
        module ETA.BasicTypes.Var,
        module ETA.BasicTypes.Id,
        module ETA.BasicTypes.IdInfo,
        module ETA.SimplCore.CoreMonad,
        module ETA.Core.CoreSyn,
        module ETA.BasicTypes.Literal,
        module ETA.BasicTypes.DataCon,
        module ETA.Core.CoreUtils,
        module ETA.Core.MkCore,
        module ETA.Core.CoreFVs,
        module ETA.Core.CoreSubst,
        module ETA.Specialise.Rules,
        module ETA.Main.Annotations,
        module ETA.Main.DynFlags,
        module ETA.Main.Packages,
        module ETA.BasicTypes.Module,
        module ETA.Types.Type,
        module ETA.Types.TyCon,
        module ETA.Types.Coercion,
        module ETA.Prelude.TysWiredIn,
        module ETA.Main.HscTypes,
        module ETA.BasicTypes.BasicTypes,
        module ETA.BasicTypes.VarSet,
        module ETA.BasicTypes.VarEnv,
        module ETA.BasicTypes.NameSet,
        module ETA.BasicTypes.NameEnv,
        module ETA.Utils.UniqSet,
        module ETA.Utils.UniqFM,
        module ETA.Utils.FiniteMap,
        module ETA.Utils.Util,
        module Eta.Serialized,
        module ETA.BasicTypes.SrcLoc,
        module ETA.Utils.Outputable,
        module ETA.BasicTypes.UniqSupply,
        module ETA.BasicTypes.Unique,
        module ETA.Utils.FastString,
        module ETA.Utils.FastTypes
    ) where

-- Plugin stuff itself
import ETA.Main.Plugins

-- Variable naming
import ETA.BasicTypes.RdrName
import ETA.BasicTypes.OccName  hiding  ( varName {- conflicts with Var.varName -} )
import ETA.BasicTypes.Name     hiding  ( varName {- reexport from OccName, conflicts with Var.varName -} )
import ETA.BasicTypes.Var
import ETA.BasicTypes.Id       hiding  ( lazySetIdInfo, setIdExported, setIdNotExported {- all three conflict with Var -} )
import ETA.BasicTypes.IdInfo

-- Core
import ETA.SimplCore.CoreMonad
import ETA.Core.CoreSyn
import ETA.BasicTypes.Literal
import ETA.BasicTypes.DataCon
import ETA.Core.CoreUtils
import ETA.Core.MkCore
import ETA.Core.CoreFVs
import ETA.Core.CoreSubst

-- Core "extras"
import ETA.Specialise.Rules
import ETA.Main.Annotations

-- Pipeline-related stuff
import ETA.Main.DynFlags
import ETA.Main.Packages

-- Important GHC types
import ETA.BasicTypes.Module
import ETA.Types.Type     hiding {- conflict with CoreSubst -}
                ( substTy, extendTvSubst, extendTvSubstList, isInScope )
import ETA.Types.Coercion hiding {- conflict with CoreSubst -}
                ( substTy, extendTvSubst, substCo, substTyVarBndr, lookupTyVar )
import ETA.Types.TyCon
import ETA.Prelude.TysWiredIn
import ETA.Main.HscTypes
import ETA.BasicTypes.BasicTypes hiding ( Version {- conflicts with Packages.Version -} )

-- Collections and maps
import ETA.BasicTypes.VarSet
import ETA.BasicTypes.VarEnv
import ETA.BasicTypes.NameSet
import ETA.BasicTypes.NameEnv
import ETA.Utils.UniqSet
import ETA.Utils.UniqFM
-- Conflicts with UniqFM:
--import LazyUniqFM
import ETA.Utils.FiniteMap

-- Common utilities
import ETA.Utils.Util
import Eta.Serialized
import ETA.BasicTypes.SrcLoc
import ETA.Utils.Outputable
import ETA.BasicTypes.UniqSupply
import ETA.BasicTypes.Unique           ( Unique, Uniquable(..) )
import ETA.Utils.FastString
import ETA.Utils.FastTypes
