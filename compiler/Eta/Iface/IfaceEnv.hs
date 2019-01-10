-- (c) The University of Glasgow 2002-2006

{-# LANGUAGE RankNTypes, CPP, BangPatterns #-}

module Eta.Iface.IfaceEnv (
        newGlobalBinder, newInteractiveBinder, newImplicitBinder,
        lookupIfaceTop,
        lookupOrig, lookupOrigNameCache, extendNameCache,
        newIfaceName, newIfaceNames,
        extendIfaceIdEnv, extendIfaceTyVarEnv,
        tcIfaceLclId, tcIfaceTyVar, lookupIfaceTyVar,
        setNameModule,

        ifaceExportNames,

        -- Name-cache stuff
        allocateGlobalBinder, updNameCache, mkNameCacheUpdater,
        NameCacheUpdater(..)
   ) where

import Eta.TypeCheck.TcRnMonad
import Eta.Main.HscTypes
import Eta.Types.Type
import Eta.BasicTypes.Var
import Eta.BasicTypes.Name
import Eta.BasicTypes.Avail
import Eta.BasicTypes.Module
import Eta.Utils.UniqFM
import Eta.Utils.FastString
import Eta.BasicTypes.UniqSupply
import Eta.BasicTypes.SrcLoc
import Eta.BasicTypes.NameCache

import Eta.Utils.Outputable

import Data.IORef
import Control.Exception

#include "HsVersions.h"

{-
*********************************************************
*                                                      *
        Allocating new Names in the Name Cache
*                                                      *
*********************************************************

Note [The Name Cache]
~~~~~~~~~~~~~~~~~~~~~
The Name Cache makes sure that, during any invovcation of GHC, each
External Name "M.x" has one, and only one globally-agreed Unique.

* The first time we come across M.x we make up a Unique and record that
  association in the Name Cache.

* When we come across "M.x" again, we look it up in the Name Cache,
  and get a hit.

The functions newGlobalBinder, allocateGlobalBinder do the main work.
When you make an External name, you should probably be calling one
of them.
-}

newGlobalBinder :: Module -> OccName -> SrcSpan -> TcRnIf a b Name
-- Used for source code and interface files, to make the
-- Name for a thing, given its Module and OccName
-- See Note [The Name Cache]
--
-- The cache may already already have a binding for this thing,
-- because we may have seen an occurrence before, but now is the
-- moment when we know its Module and SrcLoc in their full glory

newGlobalBinder mod occ loc
  = do mod `seq` occ `seq` return ()    -- See notes with lookupOrig
--     traceIf (text "newGlobalBinder" <+> ppr mod <+> ppr occ <+> ppr loc)
       updNameCache $ \name_cache ->
         allocateGlobalBinder name_cache mod occ loc

newInteractiveBinder :: HscEnv -> OccName -> SrcSpan -> IO Name
-- Works in the IO monad, and gets the Module
-- from the interactive context
newInteractiveBinder hsc_env occ loc
  = do { let mod = icInteractiveModule (hsc_IC hsc_env)
        ; updNameCacheIO hsc_env $ \name_cache ->
          allocateGlobalBinder name_cache mod occ loc }

allocateGlobalBinder
  :: NameCache
  -> Module -> OccName -> SrcSpan
  -> (NameCache, Name)
-- See Note [The Name Cache]
allocateGlobalBinder name_supply mod occ loc
  = case lookupOrigNameCache (nsNames name_supply) mod occ of
        -- A hit in the cache!  We are at the binding site of the name.
        -- This is the moment when we know the SrcLoc
        -- of the Name, so we set this field in the Name we return.
        --
        -- Then (bogus) multiple bindings of the same Name
        -- get different SrcLocs can can be reported as such.
        --
        -- Possible other reason: it might be in the cache because we
        --      encountered an occurrence before the binding site for an
        --      implicitly-imported Name.  Perhaps the current SrcLoc is
        --      better... but not really: it'll still just say 'imported'
        --
        -- IMPORTANT: Don't mess with wired-in names.
        --            Their wired-in-ness is in their NameSort
        --            and their Module is correct.

        Just name | isWiredInName name
                  -> (name_supply, name)
                  | otherwise
                  -> (new_name_supply, name')
                  where
                    uniq            = nameUnique name
                    name'           = mkExternalName uniq mod occ loc
                                      -- name' is like name, but with the right SrcSpan
                    new_cache       = extendNameCache (nsNames name_supply) mod occ name'
                    new_name_supply = name_supply {nsNames = new_cache}

        -- Miss in the cache!
        -- Build a completely new Name, and put it in the cache
        _ -> (new_name_supply, name)
                  where
                    (uniq, us')     = takeUniqFromSupply (nsUniqs name_supply)
                    name            = mkExternalName uniq mod occ loc
                    new_cache       = extendNameCache (nsNames name_supply) mod occ name
                    new_name_supply = name_supply {nsUniqs = us', nsNames = new_cache}

newImplicitBinder :: Name                       -- Base name
                  -> (OccName -> OccName)       -- Occurrence name modifier
                  -> TcRnIf m n Name            -- Implicit name
-- Called in BuildTyCl to allocate the implicit binders of type/class decls
-- For source type/class decls, this is the first occurrence
-- For iface ones, the LoadIface has alrady allocated a suitable name in the cache
newImplicitBinder base_name mk_sys_occ
  | Just mod <- nameModule_maybe base_name
  = newGlobalBinder mod occ loc
  | otherwise           -- When typechecking a [d| decl bracket |],
                        -- TH generates types, classes etc with Internal names,
                        -- so we follow suit for the implicit binders
  = do  { uniq <- newUnique
        ; return (mkInternalName uniq occ loc) }
  where
    occ = mk_sys_occ (nameOccName base_name)
    loc = nameSrcSpan base_name

ifaceExportNames :: [IfaceExport] -> TcRnIf gbl lcl [AvailInfo]
ifaceExportNames exports = return exports

lookupOrig :: Module -> OccName ->  TcRnIf a b Name
lookupOrig mod occ
  = do  {       -- First ensure that mod and occ are evaluated
                -- If not, chaos can ensue:
                --      we read the name-cache
                --      then pull on mod (say)
                --      which does some stuff that modifies the name cache
                -- This did happen, with tycon_mod in TcIface.tcIfaceAlt (DataAlt..)
          mod `seq` occ `seq` return ()
        ; traceIf (text "lookup_orig" <+> ppr mod <+> ppr occ)

        ; updNameCache $ \name_cache ->
            case lookupOrigNameCache (nsNames name_cache) mod occ of {
              Just name -> (name_cache, name);
              Nothing   ->
              case takeUniqFromSupply (nsUniqs name_cache) of {
              (uniq, us) ->
                  let
                    name      = mkExternalName uniq mod occ noSrcSpan
                    new_cache = extendNameCache (nsNames name_cache) mod occ name
                  in (name_cache{ nsUniqs = us, nsNames = new_cache }, name)
    }}}

-- | Set the 'Module' of a 'Name'.
setNameModule :: Maybe Module -> Name -> TcRnIf m n Name
setNameModule Nothing n = return n
setNameModule (Just m) n =
    newGlobalBinder m (nameOccName n) (nameSrcSpan n)

{-
************************************************************************
*                                                                      *
                Type variables and local Ids
*                                                                      *
************************************************************************
-}

tcIfaceLclId :: FastString -> IfL Id
tcIfaceLclId occ
  = do  { lcl <- getLclEnv
        ; case (lookupUFM (if_id_env lcl) occ) of
            Just ty_var -> return ty_var
            Nothing     -> failIfM (text "Iface id out of scope: " <+> ppr occ)
        }

extendIfaceIdEnv :: [Id] -> IfL a -> IfL a
extendIfaceIdEnv ids thing_inside
  = do  { env <- getLclEnv
        ; let { id_env' = addListToUFM (if_id_env env) pairs
              ; pairs   = [(occNameFS (getOccName id), id) | id <- ids] }
        ; setLclEnv (env { if_id_env = id_env' }) thing_inside }


tcIfaceTyVar :: FastString -> IfL TyVar
tcIfaceTyVar occ
  = do  { lcl <- getLclEnv
        ; case (lookupUFM (if_tv_env lcl) occ) of
            Just ty_var -> return ty_var
            Nothing     -> failIfM (text "Iface type variable out of scope: " <+> ppr occ)
        }

lookupIfaceTyVar :: FastString -> IfL (Maybe TyVar)
lookupIfaceTyVar occ
  = do  { lcl <- getLclEnv
        ; return (lookupUFM (if_tv_env lcl) occ) }

extendIfaceTyVarEnv :: [TyVar] -> IfL a -> IfL a
extendIfaceTyVarEnv tyvars thing_inside
  = do  { env <- getLclEnv
        ; let { tv_env' = addListToUFM (if_tv_env env) pairs
              ; pairs   = [(occNameFS (getOccName tv), tv) | tv <- tyvars] }
        ; setLclEnv (env { if_tv_env = tv_env' }) thing_inside }

{-
************************************************************************
*                                                                      *
                Getting from RdrNames to Names
*                                                                      *
************************************************************************
-}

lookupIfaceTop :: OccName -> IfL Name
lookupIfaceTop occ
  = do  { env <- getLclEnv; lookupOrig (if_mod env) occ }

newIfaceName :: OccName -> IfL Name
newIfaceName occ
  = do  { uniq <- newUnique
        ; return $! mkInternalName uniq occ noSrcSpan }

newIfaceNames :: [OccName] -> IfL [Name]
newIfaceNames occs
  = do  { uniqs <- newUniqueSupply
        ; return [ mkInternalName uniq occ noSrcSpan
                 | (occ,uniq) <- occs `zip` uniqsFromSupply uniqs] }

updNameCache :: (NameCache -> (NameCache, c)) -> TcRnIf a b c
updNameCache upd_fn = do
 HscEnv { hsc_NC = nc_var } <- getTopEnv
 atomicUpdMutVar' nc_var upd_fn

newtype NameCacheUpdater = NCU { updateNameCache :: forall c. (NameCache -> (NameCache, c)) -> IO c }

-- | Return a function to atomically update the name cache.
mkNameCacheUpdater :: TcRnIf a b NameCacheUpdater
mkNameCacheUpdater = do
  !nc_var <- hsc_NC `fmap` getTopEnv
  let update_nc f = do r <- atomicModifyIORef nc_var f
                       _ <- evaluate =<< readIORef nc_var
                       return r
  return (NCU update_nc)
