{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module is not used by GHC itself.  Rather, it exports all of
-- the functions and types you are likely to need when writing a
-- plugin for GHC. So authors of plugins can probably get away simply
-- with saying "import GhcPlugins".
-- 
-- Particularly interesting modules for plugin writers include
-- "CoreSyn" and "CoreMonad".
module GhcPlugins(
        module Plugins,
        module RdrName, module OccName, module Name, module Var, module Id, module IdInfo,
        module CoreMonad, module CoreSyn, module Literal, module DataCon,
        module CoreUtils, module MkCore, module CoreFVs, module CoreSubst,
        module Rules, module Annotations,
        module DynFlags, module Packages,
        module Module, module Type, module TyCon, module Coercion, 
        module TysWiredIn, module HscTypes, module BasicTypes,
        module VarSet, module VarEnv, module NameSet, module NameEnv, 
        module UniqSet, module UniqFM, module FiniteMap,
        module Util, module Serialized, module SrcLoc, module Outputable, 
        module UniqSupply, module Unique, module FastString, module FastTypes
    ) where

-- Plugin stuff itself
import Plugins

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
import FiniteMap

-- Common utilities
import GHCVM.Utils.Util
import GHCVM.Utils.Serialized
import GHCVM.BasicTypes.SrcLoc
import GHCVM.Utils.Outputable
import GHCVM.BasicTypes.UniqSupply
import GHCVM.BasicTypes.Unique           ( Unique, Uniquable(..) )
import GHCVM.Utils.FastString
import GHCVM.Utils.FastTypes
