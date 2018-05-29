{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module is not used by GHC itself.  Rather, it exports all of
-- the functions and types you are likely to need when writing a
-- plugin for GHC. So authors of plugins can probably get away simply
-- with saying "import GhcPlugins".
-- 
-- Particularly interesting modules for plugin writers include
-- "CoreSyn" and "CoreMonad".
module Eta.Main.GhcPlugins(
        module Eta.Main.Plugins,
        module Eta.BasicTypes.RdrName,
        module Eta.BasicTypes.OccName,
        module Eta.BasicTypes.Name,
        module Eta.BasicTypes.Var,
        module Eta.BasicTypes.Id,
        module Eta.BasicTypes.IdInfo,
        module Eta.SimplCore.CoreMonad,
        module Eta.Core.CoreSyn,
        module Eta.BasicTypes.Literal,
        module Eta.BasicTypes.DataCon,
        module Eta.Core.CoreUtils,
        module Eta.Core.MkCore,
        module Eta.Core.CoreFVs,
        module Eta.Core.CoreSubst,
        module Eta.Specialise.Rules,
        module Eta.Main.Annotations,
        module Eta.Main.DynFlags,
        module Eta.Main.Packages,
        module Eta.BasicTypes.Module,
        module Eta.Types.Type,
        module Eta.Types.TyCon,
        module Eta.Types.Coercion,
        module Eta.Prelude.TysWiredIn,
        module Eta.Main.HscTypes,
        module Eta.BasicTypes.BasicTypes,
        module Eta.BasicTypes.VarSet,
        module Eta.BasicTypes.VarEnv,
        module Eta.BasicTypes.NameSet,
        module Eta.BasicTypes.NameEnv,
        module Eta.Utils.UniqSet,
        module Eta.Utils.UniqFM,
        module Eta.Utils.FiniteMap,
        module Eta.Utils.Util,
        module Eta.Serialized,
        module Eta.BasicTypes.SrcLoc,
        module Eta.Utils.Outputable,
        module Eta.BasicTypes.UniqSupply,
        module Eta.BasicTypes.Unique,
        module Eta.Utils.FastString,
        module Eta.Utils.FastTypes
    ) where

-- Plugin stuff itself
import Eta.Main.Plugins

-- Variable naming
import Eta.BasicTypes.RdrName
import Eta.BasicTypes.OccName  hiding  ( varName {- conflicts with Var.varName -} )
import Eta.BasicTypes.Name     hiding  ( varName {- reexport from OccName, conflicts with Var.varName -} )
import Eta.BasicTypes.Var
import Eta.BasicTypes.Id       hiding  ( lazySetIdInfo, setIdExported, setIdNotExported {- all three conflict with Var -} )
import Eta.BasicTypes.IdInfo

-- Core
import Eta.SimplCore.CoreMonad
import Eta.Core.CoreSyn
import Eta.BasicTypes.Literal
import Eta.BasicTypes.DataCon
import Eta.Core.CoreUtils
import Eta.Core.MkCore
import Eta.Core.CoreFVs
import Eta.Core.CoreSubst

-- Core "extras"
import Eta.Specialise.Rules
import Eta.Main.Annotations

-- Pipeline-related stuff
import Eta.Main.DynFlags
import Eta.Main.Packages

-- Important GHC types
import Eta.BasicTypes.Module
import Eta.Types.Type     hiding {- conflict with CoreSubst -}
                ( substTy, extendTvSubst, extendTvSubstList, isInScope )
import Eta.Types.Coercion hiding {- conflict with CoreSubst -}
                ( substTy, extendTvSubst, substCo, substTyVarBndr, lookupTyVar )
import Eta.Types.TyCon
import Eta.Prelude.TysWiredIn
import Eta.Main.HscTypes
import Eta.BasicTypes.BasicTypes hiding ( Version {- conflicts with Packages.Version -} )

-- Collections and maps
import Eta.BasicTypes.VarSet
import Eta.BasicTypes.VarEnv
import Eta.BasicTypes.NameSet
import Eta.BasicTypes.NameEnv
import Eta.Utils.UniqSet
import Eta.Utils.UniqFM
-- Conflicts with UniqFM:
--import LazyUniqFM
import Eta.Utils.FiniteMap

-- Common utilities
import Eta.Utils.Util
import Eta.Serialized
import Eta.BasicTypes.SrcLoc
import Eta.Utils.Outputable
import Eta.BasicTypes.UniqSupply
import Eta.BasicTypes.Unique           ( Unique, Uniquable(..) )
import Eta.Utils.FastString
import Eta.Utils.FastTypes
