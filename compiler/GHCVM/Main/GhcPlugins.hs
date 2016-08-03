{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module is not used by GHC itself.  Rather, it exports all of
-- the functions and types you are likely to need when writing a
-- plugin for GHC. So authors of plugins can probably get away simply
-- with saying "import GhcPlugins".
-- 
-- Particularly interesting modules for plugin writers include
-- "CoreSyn" and "CoreMonad".
module GHCVM.Main.GhcPlugins(
        module GHCVM.Main.Plugins,
        module GHCVM.BasicTypes.RdrName,
        module GHCVM.BasicTypes.OccName,
        module GHCVM.BasicTypes.Name,
        module GHCVM.BasicTypes.Var,
        module GHCVM.BasicTypes.Id,
        module GHCVM.BasicTypes.IdInfo,
        module GHCVM.SimplCore.CoreMonad,
        module GHCVM.Core.CoreSyn,
        module GHCVM.BasicTypes.Literal,
        module GHCVM.BasicTypes.DataCon,
        module GHCVM.Core.CoreUtils,
        module GHCVM.Core.MkCore,
        module GHCVM.Core.CoreFVs,
        module GHCVM.Core.CoreSubst,
        module GHCVM.Specialise.Rules,
        module GHCVM.Main.Annotations,
        module GHCVM.Main.DynFlags,
        module GHCVM.Main.Packages,
        module GHCVM.BasicTypes.Module,
        module GHCVM.Types.Type,
        module GHCVM.Types.TyCon,
        module GHCVM.Types.Coercion,
        module GHCVM.Prelude.TysWiredIn,
        module GHCVM.Main.HscTypes,
        module GHCVM.BasicTypes.BasicTypes,
        module GHCVM.BasicTypes.VarSet,
        module GHCVM.BasicTypes.VarEnv,
        module GHCVM.BasicTypes.NameSet,
        module GHCVM.BasicTypes.NameEnv,
        module GHCVM.Utils.UniqSet,
        module GHCVM.Utils.UniqFM,
        module GHCVM.Utils.FiniteMap,
        module GHCVM.Utils.Util,
        module GHCVM.Utils.Serialized,
        module GHCVM.BasicTypes.SrcLoc,
        module GHCVM.Utils.Outputable,
        module GHCVM.BasicTypes.UniqSupply,
        module GHCVM.BasicTypes.Unique,
        module GHCVM.Utils.FastString,
        module GHCVM.Utils.FastTypes
    ) where

-- Plugin stuff itself
import GHCVM.Main.Plugins

-- Variable naming
import GHCVM.BasicTypes.RdrName
import GHCVM.BasicTypes.OccName  hiding  ( varName {- conflicts with Var.varName -} )
import GHCVM.BasicTypes.Name     hiding  ( varName {- reexport from OccName, conflicts with Var.varName -} )
import GHCVM.BasicTypes.Var
import GHCVM.BasicTypes.Id       hiding  ( lazySetIdInfo, setIdExported, setIdNotExported {- all three conflict with Var -} )
import GHCVM.BasicTypes.IdInfo

-- Core
import GHCVM.SimplCore.CoreMonad
import GHCVM.Core.CoreSyn
import GHCVM.BasicTypes.Literal
import GHCVM.BasicTypes.DataCon
import GHCVM.Core.CoreUtils
import GHCVM.Core.MkCore
import GHCVM.Core.CoreFVs
import GHCVM.Core.CoreSubst

-- Core "extras"
import GHCVM.Specialise.Rules
import GHCVM.Main.Annotations

-- Pipeline-related stuff
import GHCVM.Main.DynFlags
import GHCVM.Main.Packages

-- Important GHC types
import GHCVM.BasicTypes.Module
import GHCVM.Types.Type     hiding {- conflict with CoreSubst -}
                ( substTy, extendTvSubst, extendTvSubstList, isInScope )
import GHCVM.Types.Coercion hiding {- conflict with CoreSubst -}
                ( substTy, extendTvSubst, substCo, substTyVarBndr, lookupTyVar )
import GHCVM.Types.TyCon
import GHCVM.Prelude.TysWiredIn
import GHCVM.Main.HscTypes
import GHCVM.BasicTypes.BasicTypes hiding ( Version {- conflicts with Packages.Version -} )

-- Collections and maps
import GHCVM.BasicTypes.VarSet
import GHCVM.BasicTypes.VarEnv
import GHCVM.BasicTypes.NameSet
import GHCVM.BasicTypes.NameEnv
import GHCVM.Utils.UniqSet
import GHCVM.Utils.UniqFM
-- Conflicts with UniqFM:
--import LazyUniqFM
import GHCVM.Utils.FiniteMap

-- Common utilities
import GHCVM.Utils.Util
import GHCVM.Utils.Serialized
import GHCVM.BasicTypes.SrcLoc
import GHCVM.Utils.Outputable
import GHCVM.BasicTypes.UniqSupply
import GHCVM.BasicTypes.Unique           ( Unique, Uniquable(..) )
import GHCVM.Utils.FastString
import GHCVM.Utils.FastTypes
