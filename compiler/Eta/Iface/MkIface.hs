{-
(c) Rahul Muttineni 2016-2017
(c) The University of Glasgow 2006-2008
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998
-}

{-# LANGUAGE CPP, NondecreasingIndentation #-}

-- | Module for constructing @ModIface@ values (interface files),
-- writing them to disk and comparing two versions to see if
-- recompilation is required.
module Eta.Iface.MkIface (
        mkIface,        -- Build a ModIface from a ModGuts,
                        -- including computing version information

        mkIface_,
        mkIfaceTc,

        writeIfaceFile, -- Write the interface file

        checkOldIface,  -- See if recompilation is required, by
                        -- comparing version information
        RecompileRequired(..), recompileRequired,
        mkIfaceExports,

        tyThingToIfaceDecl -- Converting things to their Iface equivalents
 ) where

{-
  -----------------------------------------------
          Recompilation checking
  -----------------------------------------------

A complete description of how recompilation checking works can be
found in the wiki commentary:

 https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/recompilation-avoidance

Please read the above page for a top-down description of how this all
works.  Notes below cover specific issues related to the implementation.

Basic idea:

  * In the mi_usages information in an interface, we record the
    fingerprint of each free variable of the module

  * In mkIface, we compute the fingerprint of each exported thing A.f.
    For each external thing that A.f refers to, we include the fingerprint
    of the external reference when computing the fingerprint of A.f.  So
    if anything that A.f depends on changes, then A.f's fingerprint will
    change.
    Also record any dependent files added with
      * addDependentFile
      * #include
      * -optP-include

  * In checkOldIface we compare the mi_usages for the module with
    the actual fingerprint for all each thing recorded in mi_usages
-}

import Eta.Iface.IfaceSyn
import Eta.Iface.BinFingerprint
import Eta.Iface.LoadIface
import Eta.Iface.FlagChecker
import Eta.DeSugar.DsUsage ( mkUsageInfo, mkUsedNames, mkDependencies )

import Eta.BasicTypes.Id
import Eta.BasicTypes.IdInfo
import Eta.BasicTypes.Demand
import Eta.Types.Coercion( tidyCo )
import Eta.Main.Annotations
import Eta.Core.CoreSyn
import Eta.Core.CoreFVs
import Eta.Types.Class
import Eta.Types.Kind
import Eta.Types.TyCon
import Eta.Types.CoAxiom
import Eta.BasicTypes.ConLike
import Eta.BasicTypes.DataCon
import Eta.BasicTypes.PatSyn
import Eta.Types.Type
import Eta.TypeCheck.TcType hiding (orphNamesOfCoCon)
import Eta.Prelude.TysPrim ( alphaTyVars )
import Eta.Types.InstEnv
import Eta.Types.FamInstEnv
import Eta.TypeCheck.TcRnMonad
import Eta.HsSyn.HsSyn
import Eta.Main.HscTypes
import Eta.Main.Finder
import Eta.Main.DynFlags
import Eta.BasicTypes.VarEnv
import Eta.BasicTypes.VarSet

import qualified Eta.BasicTypes.Var as Var
import Eta.BasicTypes.Name
import Eta.BasicTypes.Avail
import Eta.BasicTypes.RdrName
import Eta.BasicTypes.NameEnv
import Eta.BasicTypes.NameSet
import Eta.BasicTypes.Module
import Eta.Iface.BinIface
import Eta.Main.ErrUtils
import Eta.Utils.Digraph
import Eta.BasicTypes.SrcLoc
import Eta.Utils.Outputable
import qualified Eta.Utils.Outputable as Outputable
import Eta.BasicTypes.BasicTypes       hiding ( SuccessFlag(..) )
import Eta.BasicTypes.Unique
import Eta.Utils.Util             hiding ( eqListBy )
import Eta.Utils.FastString
import Eta.Utils.Maybes
import Eta.Utils.Binary
import Eta.Utils.Fingerprint
import Eta.Utils.Bag
import Eta.Utils.Exception

import Control.Monad
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Ord
import Data.IORef
import System.Directory
import System.FilePath

#include "HsVersions.h"

{-
************************************************************************
*                                                                      *
\subsection{Completing an interface}
*                                                                      *
************************************************************************
-}

mkIface :: HscEnv
        -> Maybe Fingerprint    -- The old fingerprint, if we have it
        -> ModDetails           -- The trimmed, tidied interface
        -> ModGuts              -- Usages, deprecations, etc
        -> IO (Messages,
               Maybe (ModIface, -- The new one
                      Bool))    -- True <=> there was an old Iface, and the
                                --          new one is identical, so no need
                                --          to write it

mkIface hsc_env maybe_old_fingerprint mod_details
         ModGuts{     mg_module       = this_mod,
                      mg_hsc_src      = hsc_src,
                      mg_usages       = usages,
                      mg_used_th      = used_th,
                      mg_deps         = deps,
                      mg_rdr_env      = rdr_env,
                      mg_fix_env      = fix_env,
                      mg_warns        = warns,
                      mg_hpc_info     = hpc_info,
                      mg_safe_haskell = safe_mode,
                      mg_trust_pkg    = self_trust
                    }
        = mkIface_ hsc_env maybe_old_fingerprint
                   this_mod hsc_src used_th deps rdr_env fix_env
                   warns hpc_info self_trust
                   safe_mode usages mod_details

-- | make an interface from the results of typechecking only.  Useful
-- for non-optimising compilation, or where we aren't generating any
-- object code at all ('HscNothing').
mkIfaceTc :: HscEnv
          -> Maybe Fingerprint  -- The old fingerprint, if we have it
          -> SafeHaskellMode    -- The safe haskell mode
          -> ModDetails         -- gotten from mkBootModDetails, probably
          -> TcGblEnv           -- Usages, deprecations, etc
          -> IO (Messages, Maybe (ModIface, Bool))
mkIfaceTc hsc_env maybe_old_fingerprint safe_mode mod_details
  tc_result@TcGblEnv{ tcg_mod = this_mod,
                      tcg_semantic_mod    = _semantic_mod,
                      tcg_src             = hsc_src,
                      tcg_imports         = imports,
                      tcg_rdr_env         = rdr_env,
                      tcg_fix_env         = fix_env,
                      tcg_merged          = merged,
                      tcg_warns           = warns,
                      tcg_hpc             = other_hpc_info,
                      tcg_th_splice_used  = tc_splice_used,
                      tcg_dependent_files = dependent_files
                    }
  = do
          let used_names = mkUsedNames tc_result
          let pluginModules = []
              --  map lpModule (plugins (hsc_dflags hsc_env))
          deps <- mkDependencies
                    (thisInstalledUnitId (hsc_dflags hsc_env))
                    pluginModules tc_result
          let hpc_info = emptyHpcInfo other_hpc_info
          used_th <- readIORef tc_splice_used
          dep_files <- (readIORef dependent_files)
          usages <- mkUsageInfo hsc_env this_mod (imp_mods imports) used_names dep_files merged
          mkIface_ hsc_env maybe_old_fingerprint
                   this_mod hsc_src
                   used_th deps rdr_env
                   fix_env warns hpc_info
                   (imp_trust_own_pkg imports) safe_mode usages mod_details

mkIface_ :: HscEnv -> Maybe Fingerprint -> Module -> HscSource
         -> Bool -> Dependencies -> GlobalRdrEnv
         -> NameEnv FixItem -> Warnings -> HpcInfo
         -> Bool
         -> SafeHaskellMode
         -> [Usage]
         -> ModDetails
         -> IO (Messages, Maybe (ModIface, Bool))
mkIface_ hsc_env maybe_old_fingerprint
         this_mod hsc_src used_th deps rdr_env fix_env src_warns
         hpc_info pkg_trust_req safe_mode usages
         ModDetails{  md_insts     = insts,
                      md_fam_insts = fam_insts,
                      md_rules     = rules,
                      md_anns      = anns,
                      md_vect_info = vect_info,
                      md_types     = type_env,
                      md_exports   = exports }
-- NB:  notice that mkIface does not look at the bindings
--      only at the TypeEnv.  The previous Tidy phase has
--      put exactly the info into the TypeEnv that we want
--      to expose in the interface

  = do
    let semantic_mod = canonicalizeHomeModule (hsc_dflags hsc_env) (moduleName this_mod)
        entities = typeEnvElts type_env
        decls    = [ tyThingToIfaceDecl entity
                   | entity <- entities,
                     let name = getName entity,
                     not (isImplicitTyThing entity),
                        -- No implicit Ids and class tycons in the interface file
                     not (isWiredInName name),
                        -- Nor wired-in things; the compiler knows about them anyhow
                     nameIsLocalOrFrom semantic_mod name  ]
                        -- Sigh: see Note [Root-main Id] in TcRnDriver
                        -- NB: ABSOLUTELY need to check against semantic_mod,
                        -- because all of the names in an hsig p[H=<H>]:H
                        -- are going to be for <H>, not the former id!
                        -- See Note [Identity versus semantic module]

        fixities    = [(occ,fix) | FixItem occ fix <- nameEnvElts fix_env]
        warns       = src_warns
        iface_rules = map (coreRuleToIfaceRule this_mod) rules
        iface_insts = map instanceToIfaceInst insts
        iface_fam_insts = map famInstToIfaceFamInst fam_insts
        iface_vect_info = flattenVectInfo vect_info
        trust_info  = setSafeMode safe_mode
        annotations = map mkIfaceAnnotation anns

        intermediate_iface = ModIface {
              mi_module      = this_mod,
              -- Need to record this because it depends on the -instantiated-with flag
              -- which could change
              mi_sig_of      = if semantic_mod == this_mod
                                 then Nothing
                                 else Just semantic_mod,
              mi_hsc_src     = hsc_src,
              mi_deps        = deps,
              mi_usages      = usages,
              mi_exports     = mkIfaceExports exports,

              -- Sort these lexicographically, so that
              -- the result is stable across compilations
              mi_insts       = sortBy cmp_inst     iface_insts,
              mi_fam_insts   = sortBy cmp_fam_inst iface_fam_insts,
              mi_rules       = sortBy cmp_rule     iface_rules,

              mi_vect_info   = iface_vect_info,

              mi_fixities    = fixities,
              mi_warns       = warns,
              mi_anns        = annotations,
              mi_globals     = maybeGlobalRdrEnv rdr_env,

              -- Left out deliberately: filled in by addFingerprints
              mi_iface_hash  = fingerprint0,
              mi_mod_hash    = fingerprint0,
              mi_flag_hash   = fingerprint0,
              mi_exp_hash    = fingerprint0,
              mi_used_th     = used_th,
              mi_orphan_hash = fingerprint0,
              mi_orphan      = False, -- Always set by addFingerprints, but
                                      -- it's a strict field, so we can't omit it.
              mi_finsts      = False, -- Ditto
              mi_decls       = deliberatelyOmitted "decls",
              mi_hash_fn     = deliberatelyOmitted "hash_fn",
              mi_hpc         = isHpcUsed hpc_info,
              mi_trust       = trust_info,
              mi_trust_pkg   = pkg_trust_req,

              -- And build the cached values
              mi_warn_fn     = mkIfaceWarnCache warns,
              mi_fix_fn      = mkIfaceFixCache fixities }

    (new_iface, no_change_at_all)
          <- {-# SCC "versioninfo" #-}
                   addFingerprints hsc_env maybe_old_fingerprint
                                   intermediate_iface decls

    -- Warn about orphans
    -- See Note [Orphans and auto-generated rules]
    let warn_orphs      = wopt Opt_WarnOrphans dflags
        warn_auto_orphs = wopt Opt_WarnAutoOrphans dflags
        orph_warnings   --- Laziness means no work done unless -fwarn-orphans
          | warn_orphs || warn_auto_orphs = rule_warns `unionBags` inst_warns
          | otherwise                     = emptyBag
        errs_and_warns = (orph_warnings, emptyBag)
        unqual = mkPrintUnqualified dflags rdr_env
        inst_warns = listToBag [ instOrphWarn dflags unqual d
                               | (d,i) <- insts `zip` iface_insts
                               , isOrphan (ifInstOrph i) ]
        rule_warns = listToBag [ ruleOrphWarn dflags unqual this_mod r
                               | r <- iface_rules
                               , isOrphan (ifRuleOrph r)
                               , if ifRuleAuto r then warn_auto_orphs
                                                 else warn_orphs ]

    if errorsFound dflags errs_and_warns
      then return ( errs_and_warns, Nothing )
      else do
        -- Debug printing
        dumpIfSet_dyn dflags Opt_D_dump_hi "FINAL INTERFACE"
                      (pprModIface new_iface)

        -- bug #1617: on reload we weren't updating the PrintUnqualified
        -- correctly.  This stems from the fact that the interface had
        -- not changed, so addFingerprints returns the old ModIface
        -- with the old GlobalRdrEnv (mi_globals).
        let final_iface = new_iface{ mi_globals = maybeGlobalRdrEnv rdr_env }

        return (errs_and_warns, Just (final_iface, no_change_at_all))
  where
     cmp_rule     = comparing ifRuleName
     -- Compare these lexicographically by OccName, *not* by unique,
     -- because the latter is not stable across compilations:
     cmp_inst     = comparing (nameOccName . ifDFun)
     cmp_fam_inst = comparing (nameOccName . ifFamInstTcName)

     dflags = hsc_dflags hsc_env

     -- We only fill in mi_globals if the module was compiled to byte
     -- code.  Otherwise, the compiler may not have retained all the
     -- top-level bindings and they won't be in the TypeEnv (see
     -- Desugar.addExportFlagsAndRules).  The mi_globals field is used
     -- by GHCi to decide whether the module has its full top-level
     -- scope available. (#5534)
     maybeGlobalRdrEnv :: GlobalRdrEnv -> Maybe GlobalRdrEnv
     maybeGlobalRdrEnv rdr_env
         | targetRetainsAllBindings (hscTarget dflags) = Just rdr_env
         | otherwise                                   = Nothing

     deliberatelyOmitted :: String -> a
     deliberatelyOmitted x = panic ("Deliberately omitted: " ++ x)

     ifFamInstTcName = ifFamInstFam

     flattenVectInfo (VectInfo { vectInfoVar            = vVar
                               , vectInfoTyCon          = vTyCon
                               , vectInfoParallelVars     = vParallelVars
                               , vectInfoParallelTyCons = vParallelTyCons
                               }) =
       IfaceVectInfo
       { ifaceVectInfoVar            = [Var.varName v | (v, _  ) <- varEnvElts  vVar]
       , ifaceVectInfoTyCon          = [tyConName t   | (t, t_v) <- nameEnvElts vTyCon, t /= t_v]
       , ifaceVectInfoTyConReuse     = [tyConName t   | (t, t_v) <- nameEnvElts vTyCon, t == t_v]
       , ifaceVectInfoParallelVars   = [Var.varName v | v <- varSetElems vParallelVars]
       , ifaceVectInfoParallelTyCons = nameSetElems vParallelTyCons
       }

-----------------------------
writeIfaceFile :: DynFlags -> FilePath -> ModIface -> IO ()
writeIfaceFile dflags hi_file_path new_iface
    = do createDirectoryIfMissing True (takeDirectory hi_file_path)
         writeBinIface dflags hi_file_path new_iface


-- -----------------------------------------------------------------------------
-- Look up parents and versions of Names

-- This is like a global version of the mi_hash_fn field in each ModIface.
-- Given a Name, it finds the ModIface, and then uses mi_hash_fn to get
-- the parent and version info.

mkHashFun
        :: HscEnv                       -- needed to look up versions
        -> ExternalPackageState         -- ditto
        -> (Name -> IO Fingerprint)
mkHashFun hsc_env eps name
  | isHoleModule orig_mod
  = lookup (mkModule (thisPackage dflags) (moduleName orig_mod))
  | otherwise
  = lookup orig_mod
  where
    dflags = hsc_dflags hsc_env
    hpt = hsc_HPT hsc_env
    pit = eps_PIT eps
    occ = nameOccName name
    orig_mod = nameModule name
    lookup mod = do
      MASSERT2( isExternalName name, ppr name )
      iface <- case lookupIfaceByModule dflags hpt pit mod of
                Just iface -> return iface
                Nothing -> do
                    -- This can occur when we're writing out ifaces for
                    -- requirements; we didn't do any /real/ typechecking
                    -- so there's no guarantee everything is loaded.
                    -- Kind of a heinous hack.
                    iface <- initIfaceLoad hsc_env . withException
                          $ loadInterface (text "lookupVers2") mod ImportBySystem
                    return iface
      return $ snd (mi_hash_fn iface occ `orElse`
                pprPanic "lookupVers1" (ppr mod <+> ppr occ))

-- ---------------------------------------------------------------------------
-- Compute fingerprints for the interface

{-
Note [Fingerprinting IfaceDecls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The general idea here is that we first examine the 'IfaceDecl's and determine
the recursive groups of them. We then walk these groups in dependency order,
serializing each contained 'IfaceDecl' to a "Binary" buffer which we then
hash using MD5 to produce a fingerprint for the group.

However, the serialization that we use is a bit funny: we override the @putName@
operation with our own which serializes the hash of a 'Name' instead of the
'Name' itself. This ensures that the fingerprint of a decl changes if anything
in its transitive closure changes. This trick is why we must be careful about
traversing in dependency order: we need to ensure that we have hashes for
everything referenced by the decl which we are fingerprinting.

Moreover, we need to be careful to distinguish between serialization of binding
Names (e.g. the ifName field of a IfaceDecl) and non-binding (e.g. the ifInstCls
field of a IfaceClsInst): only in the non-binding case should we include the
fingerprint; in the binding case we shouldn't since it is merely the name of the
thing that we are currently fingerprinting.
-}

-- | Add fingerprints for top-level declarations to a 'ModIface'.
--
-- See Note [Fingerprinting IfaceDecls]

addFingerprints
        :: HscEnv
        -> Maybe Fingerprint -- the old fingerprint, if any
        -> ModIface          -- The new interface (lacking decls)
        -> [IfaceDecl]       -- The new decls
        -> IO (ModIface,     -- Updated interface
               Bool)         -- True <=> no changes at all;
                             -- no need to write Iface

addFingerprints hsc_env mb_old_fingerprint iface0 new_decls
 = do
   eps <- hscEPS hsc_env
   let
        -- The ABI of a declaration represents everything that is made
        -- visible about the declaration that a client can depend on.
        -- see IfaceDeclABI below.
       declABI :: IfaceDecl -> IfaceDeclABI
       -- TODO: I'm not sure if this should be semantic_mod or this_mod.
       -- See also Note [Identity versus semantic module]
       declABI decl = (this_mod, decl, extras)
        where extras = declExtras fix_fn ann_fn non_orph_rules non_orph_insts
                                  non_orph_fis decl

       edges :: [(IfaceDeclABI, Unique, [Unique])]
       edges = [ (abi, getUnique (getOccName decl), out)
               | decl <- new_decls
               , let abi = declABI decl
               , let out = localOccs $ freeNamesDeclABI abi
               ]

       name_module n = ASSERT2( isExternalName n, ppr n ) nameModule n
       localOccs = map (getUnique . getParent . getOccName)
                         -- NB: names always use semantic module, so
                         -- filtering must be on the semantic module!
                         -- See Note [Identity versus semantic module]
                        . filter ((== semantic_mod) . name_module)
                        . nameSetElems
          where getParent :: OccName -> OccName
                getParent occ = lookupOccEnv parent_map occ `orElse` occ

        -- maps OccNames to their parents in the current module.
        -- e.g. a reference to a constructor must be turned into a reference
        -- to the TyCon for the purposes of calculating dependencies.
       parent_map :: OccEnv OccName
       parent_map = foldr extend emptyOccEnv new_decls
          where extend d env =
                  extendOccEnvList env [ (b,n) | b <- ifaceDeclImplicitBndrs d ]
                  where n = getOccName d

        -- strongly-connected groups of declarations, in dependency order
       groups :: [SCC IfaceDeclABI]
       groups = stronglyConnCompFromEdgedVertices edges

       global_hash_fn = mkHashFun hsc_env eps

        -- How to output Names when generating the data to fingerprint.
        -- Here we want to output the fingerprint for each top-level
        -- Name, whether it comes from the current module or another
        -- module.  In this way, the fingerprint for a declaration will
        -- change if the fingerprint for anything it refers to (transitively)
        -- changes.
       mk_put_name :: OccEnv (OccName,Fingerprint)
                   -> BinHandle -> Name -> IO  ()
       mk_put_name local_env bh name
          | isWiredInName name  =  putNameLiterally bh name
           -- wired-in names don't have fingerprints
          | otherwise
          = ASSERT2( isExternalName name, ppr name )
            let hash | nameModule name /= semantic_mod =  global_hash_fn name
                     -- Get it from the REAL interface!!
                     -- This will trigger when we compile an hsig file
                     -- and we know a backing impl for it.
                     -- See Note [Identity versus semantic module]
                     | semantic_mod /= this_mod
                     , not (isHoleModule semantic_mod) = global_hash_fn name
                     | otherwise = return (snd (lookupOccEnv local_env (getOccName name)
                           `orElse` pprPanic "urk! lookup local fingerprint"
                                       (ppr name)))
                -- This panic indicates that we got the dependency
                -- analysis wrong, because we needed a fingerprint for
                -- an entity that wasn't in the environment.  To debug
                -- it, turn the panic into a trace, uncomment the
                -- pprTraces below, run the compile again, and inspect
                -- the output and the generated .hi file with
                -- --show-iface.
            in hash >>= put_ bh

        -- take a strongly-connected group of declarations and compute
        -- its fingerprint.

       fingerprint_group :: (OccEnv (OccName,Fingerprint),
                             [(Fingerprint,IfaceDecl)])
                         -> SCC IfaceDeclABI
                         -> IO (OccEnv (OccName,Fingerprint),
                                [(Fingerprint,IfaceDecl)])

       fingerprint_group (local_env, decls_w_hashes) (AcyclicSCC abi)
          = do let hash_fn = mk_put_name local_env
                   decl = abiDecl abi
               -- pprTrace "fingerprinting" (ppr (ifName decl) ) $ do
               hash <- computeFingerprint hash_fn abi
               env' <- extend_hash_env local_env (hash,decl)
               return (env', (hash,decl) : decls_w_hashes)

       fingerprint_group (local_env, decls_w_hashes) (CyclicSCC abis)
          = do let decls = map abiDecl abis
               local_env1 <- foldM extend_hash_env local_env
                                   (zip (repeat fingerprint0) decls)
               let hash_fn = mk_put_name local_env1
               -- pprTrace "fingerprinting" (ppr (map ifName decls) ) $ do
               let stable_abis = sortBy cmp_abiNames abis
                -- put the cycle in a canonical order
               hash <- computeFingerprint hash_fn stable_abis
               let pairs = zip (repeat hash) decls
               local_env2 <- foldM extend_hash_env local_env pairs
               return (local_env2, pairs ++ decls_w_hashes)

       -- we have fingerprinted the whole declaration, but we now need
       -- to assign fingerprints to all the OccNames that it binds, to
       -- use when referencing those OccNames in later declarations.
       --
       extend_hash_env :: OccEnv (OccName,Fingerprint)
                       -> (Fingerprint,IfaceDecl)
                       -> IO (OccEnv (OccName,Fingerprint))
       extend_hash_env env0 (hash,d) = do
          return (foldr (\(b,fp) env -> extendOccEnv env b (b,fp)) env0
                 (ifaceDeclFingerprints hash d))

   --
   (local_env, decls_w_hashes) <-
       foldM fingerprint_group (emptyOccEnv, []) groups

   -- when calculating fingerprints, we always need to use canonical
   -- ordering for lists of things.  In particular, the mi_deps has various
   -- lists of modules and suchlike, so put these all in canonical order:
   let sorted_deps = sortDependencies (mi_deps iface0)

   -- the export hash of a module depends on the orphan hashes of the
   -- orphan modules below us in the dependency tree.  This is the way
   -- that changes in orphans get propagated all the way up the
   -- dependency tree.  We only care about orphan modules in the current
   -- package, because changes to orphans outside this package will be
   -- tracked by the usage on the ABI hash of package modules that we import.
   let orph_mods = filter ((== this_pkg) . moduleUnitId)
                   $ dep_orphs sorted_deps
   dep_orphan_hashes <- getOrphanHashes hsc_env orph_mods

   orphan_hash <- computeFingerprint (mk_put_name local_env)
                                     (map ifDFun orph_insts, orph_rules, orph_fis)

   -- the export list hash doesn't depend on the fingerprints of
   -- the Names it mentions, only the Names themselves, hence putNameLiterally.
   export_hash <- computeFingerprint putNameLiterally
                      (mi_exports iface0,
                       orphan_hash,
                       dep_orphan_hashes,
                       dep_pkgs (mi_deps iface0),
                        -- dep_pkgs: see "Package Version Changes" on
                        -- wiki/Commentary/Compiler/RecompilationAvoidance
                       mi_trust iface0)
                        -- Make sure change of Safe Haskell mode causes recomp.

   -- put the declarations in a canonical order, sorted by OccName
   let sorted_decls = Map.elems $ Map.fromList $
                          [(getOccName d, e) | e@(_, d) <- decls_w_hashes]

   -- the flag hash depends on:
   --   - (some of) dflags
   -- it returns two hashes, one that shouldn't change
   -- the abi hash and one that should
   flag_hash <- fingerprintDynFlags dflags this_mod putNameLiterally

   -- the ABI hash depends on:
   --   - decls
   --   - export list
   --   - orphans
   --   - deprecations
   --   - vect info
   --   - flag abi hash
   mod_hash <- computeFingerprint putNameLiterally
                      (map fst sorted_decls,
                       export_hash,  -- includes orphan_hash
                       mi_warns iface0,
                       mi_vect_info iface0)

   -- The interface hash depends on:
   --   - the ABI hash, plus
   --   - the module level annotations,
   --   - usages
   --   - deps (home and external packages, dependent files)
   --   - hpc
   iface_hash <- computeFingerprint putNameLiterally
                      (mod_hash,
                       ann_fn (mkVarOcc "module"),  -- See mkIfaceAnnCache
                       mi_usages iface0,
                       sorted_deps,
                       mi_hpc iface0)

   let
    no_change_at_all = Just iface_hash == mb_old_fingerprint

    final_iface = iface0 {
                mi_mod_hash    = mod_hash,
                mi_iface_hash  = iface_hash,
                mi_exp_hash    = export_hash,
                mi_orphan_hash = orphan_hash,
                mi_flag_hash   = flag_hash,
                mi_orphan      = not (   all ifRuleAuto orph_rules
                                           -- See Note [Orphans and auto-generated rules]
                                      && null orph_insts
                                      && null orph_fis
                                      && isNoIfaceVectInfo (mi_vect_info iface0)),
                mi_finsts      = not . null $ mi_fam_insts iface0,
                mi_decls       = sorted_decls,
                mi_hash_fn     = lookupOccEnv local_env }
   --
   return (final_iface, no_change_at_all)

  where
    this_mod = mi_module iface0
    semantic_mod = mi_semantic_module iface0
    dflags = hsc_dflags hsc_env
    this_pkg = thisPackage dflags
    (non_orph_insts, orph_insts) = mkOrphMap ifInstOrph    (mi_insts iface0)
    (non_orph_rules, orph_rules) = mkOrphMap ifRuleOrph    (mi_rules iface0)
    (non_orph_fis,   orph_fis)   = mkOrphMap ifFamInstOrph (mi_fam_insts iface0)
    fix_fn = mi_fix_fn iface0
    ann_fn = mkIfaceAnnCache (mi_anns iface0)

getOrphanHashes :: HscEnv -> [Module] -> IO [Fingerprint]
getOrphanHashes hsc_env mods = do
  eps <- hscEPS hsc_env
  let
    hpt        = hsc_HPT hsc_env
    pit        = eps_PIT eps
    dflags     = hsc_dflags hsc_env
    get_orph_hash mod =
          case lookupIfaceByModule dflags hpt pit mod of
            Nothing    -> pprPanic "moduleOrphanHash" (ppr mod)
            Just iface -> mi_orphan_hash iface
  --
  return (map get_orph_hash mods)


sortDependencies :: Dependencies -> Dependencies
sortDependencies d
 = Deps { dep_mods   = sortBy (compare `on` (moduleNameFS.fst)) (dep_mods d),
          dep_pkgs   = sortBy (compare `on` fst) (dep_pkgs d),
          dep_orphs  = sortBy stableModuleCmp (dep_orphs d),
          dep_finsts = sortBy stableModuleCmp (dep_finsts d) }

-- | Creates cached lookup for the 'mi_anns' field of ModIface
-- Hackily, we use "module" as the OccName for any module-level annotations
mkIfaceAnnCache :: [IfaceAnnotation] -> OccName -> [AnnPayload]
mkIfaceAnnCache anns
  = \n -> lookupOccEnv env n `orElse` []
  where
    pair (IfaceAnnotation target value) =
      (case target of
          NamedTarget occn -> occn
          ModuleTarget _   -> mkVarOcc "module"
      , [value])
    -- flipping (++), so the first argument is always short
    env = mkOccEnv_C (flip (++)) (map pair anns)

{-
Note [Orphans and auto-generated rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we specialise an INLINEABLE function, or when we have
-fspecialise-aggressively, we auto-generate RULES that are orphans.
We don't want to warn about these, at least not by default, or we'd
generate a lot of warnings.  Hence -fwarn-auto-orphans.

Indeed, we don't even treat the module as an oprhan module if it has
auto-generated *rule* orphans.  Orphan modules are read every time we
compile, so they are pretty obtrusive and slow down every compilation,
even non-optimised ones.  (Reason: for type class instances it's a
type correctness issue.)  But specialisation rules are strictly for
*optimisation* only so it's fine not to read the interface.

What this means is that a SPEC rules from auto-specialisation in
module M will be used in other modules only if M.hi has been read for
some other reason, which is actually pretty likely.


************************************************************************
*                                                                      *
          The ABI of an IfaceDecl
*                                                                      *
************************************************************************

Note [The ABI of an IfaceDecl]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The ABI of a declaration consists of:

   (a) the full name of the identifier (inc. module and package,
       because these are used to construct the symbol name by which
       the identifier is known externally).

   (b) the declaration itself, as exposed to clients.  That is, the
       definition of an Id is included in the fingerprint only if
       it is made available as an unfolding in the interface.

   (c) the fixity of the identifier
   (d) for Ids: rules
   (e) for classes: instances, fixity & rules for methods
   (f) for datatypes: instances, fixity & rules for constrs

Items (c)-(f) are not stored in the IfaceDecl, but instead appear
elsewhere in the interface file.  But they are *fingerprinted* with
the declaration itself. This is done by grouping (c)-(f) in IfaceDeclExtras,
and fingerprinting that as part of the declaration.
-}

type IfaceDeclABI = (Module, IfaceDecl, IfaceDeclExtras)

data IfaceDeclExtras
  = IfaceIdExtras IfaceIdExtras

  | IfaceDataExtras
       Fixity                   -- Fixity of the tycon itself
       [IfaceInstABI]           -- Local class and family instances of this tycon
                                -- See Note [Orphans] in InstEnv
       [AnnPayload]             -- Annotations of the type itself
       [IfaceIdExtras]          -- For each constructor: fixity, RULES and annotations

  | IfaceClassExtras
       Fixity                   -- Fixity of the class itself
       [IfaceInstABI]           -- Local instances of this class *or*
                                --   of its associated data types
                                -- See Note [Orphans] in InstEnv
       [AnnPayload]             -- Annotations of the type itself
       [IfaceIdExtras]          -- For each class method: fixity, RULES and annotations

  | IfaceSynonymExtras Fixity [AnnPayload]

  | IfaceFamilyExtras   Fixity [IfaceInstABI] [AnnPayload]

  | IfaceOtherDeclExtras

data IfaceIdExtras
  = IdExtras
       Fixity                   -- Fixity of the Id
       [IfaceRule]              -- Rules for the Id
       [AnnPayload]             -- Annotations for the Id

-- When hashing a class or family instance, we hash only the
-- DFunId or CoAxiom, because that depends on all the
-- information about the instance.
--
type IfaceInstABI = IfExtName   -- Name of DFunId or CoAxiom that is evidence for the instance

abiDecl :: IfaceDeclABI -> IfaceDecl
abiDecl (_, decl, _) = decl

cmp_abiNames :: IfaceDeclABI -> IfaceDeclABI -> Ordering
cmp_abiNames abi1 abi2 = getOccName (abiDecl abi1) `compare`
                         getOccName (abiDecl abi2)

freeNamesDeclABI :: IfaceDeclABI -> NameSet
freeNamesDeclABI (_mod, decl, extras) =
  freeNamesIfDecl decl `unionNameSet` freeNamesDeclExtras extras

freeNamesDeclExtras :: IfaceDeclExtras -> NameSet
freeNamesDeclExtras (IfaceIdExtras id_extras)
  = freeNamesIdExtras id_extras
freeNamesDeclExtras (IfaceDataExtras  _ insts _ subs)
  = unionNameSets (mkNameSet insts : map freeNamesIdExtras subs)
freeNamesDeclExtras (IfaceClassExtras _ insts _ subs)
  = unionNameSets (mkNameSet insts : map freeNamesIdExtras subs)
freeNamesDeclExtras (IfaceSynonymExtras _ _)
  = emptyNameSet
freeNamesDeclExtras (IfaceFamilyExtras _ insts _)
  = mkNameSet insts
freeNamesDeclExtras IfaceOtherDeclExtras
  = emptyNameSet

freeNamesIdExtras :: IfaceIdExtras -> NameSet
freeNamesIdExtras (IdExtras _ rules _) = unionNameSets (map freeNamesIfRule rules)

instance Outputable IfaceDeclExtras where
  ppr IfaceOtherDeclExtras       = Outputable.empty
  ppr (IfaceIdExtras  extras)    = ppr_id_extras extras
  ppr (IfaceSynonymExtras fix anns) = vcat [ppr fix, ppr anns]
  ppr (IfaceFamilyExtras fix finsts anns) = vcat [ppr fix, ppr finsts, ppr anns]
  ppr (IfaceDataExtras fix insts anns stuff) = vcat [ppr fix, ppr_insts insts, ppr anns,
                                                ppr_id_extras_s stuff]
  ppr (IfaceClassExtras fix insts anns stuff) = vcat [ppr fix, ppr_insts insts, ppr anns,
                                                 ppr_id_extras_s stuff]

ppr_insts :: [IfaceInstABI] -> SDoc
ppr_insts _ = ptext (sLit "<insts>")

ppr_id_extras_s :: [IfaceIdExtras] -> SDoc
ppr_id_extras_s stuff = vcat (map ppr_id_extras stuff)

ppr_id_extras :: IfaceIdExtras -> SDoc
ppr_id_extras (IdExtras fix rules anns) = ppr fix $$ vcat (map ppr rules) $$ vcat (map ppr anns)

-- This instance is used only to compute fingerprints
instance Binary IfaceDeclExtras where
  get _bh = panic "no get for IfaceDeclExtras"
  put_ bh (IfaceIdExtras extras) = do
   putByte bh 1; put_ bh extras
  put_ bh (IfaceDataExtras fix insts anns cons) = do
   putByte bh 2; put_ bh fix; put_ bh insts; put_ bh anns; put_ bh cons
  put_ bh (IfaceClassExtras fix insts anns methods) = do
   putByte bh 3; put_ bh fix; put_ bh insts; put_ bh anns; put_ bh methods
  put_ bh (IfaceSynonymExtras fix anns) = do
   putByte bh 4; put_ bh fix; put_ bh anns
  put_ bh (IfaceFamilyExtras fix finsts anns) = do
   putByte bh 5; put_ bh fix; put_ bh finsts; put_ bh anns
  put_ bh IfaceOtherDeclExtras = putByte bh 6

instance Binary IfaceIdExtras where
  get _bh = panic "no get for IfaceIdExtras"
  put_ bh (IdExtras fix rules anns)= do { put_ bh fix; put_ bh rules; put_ bh anns }

declExtras :: (OccName -> Fixity)
           -> (OccName -> [AnnPayload])
           -> OccEnv [IfaceRule]
           -> OccEnv [IfaceClsInst]
           -> OccEnv [IfaceFamInst]
           -> IfaceDecl
           -> IfaceDeclExtras

declExtras fix_fn ann_fn rule_env inst_env fi_env decl
  = case decl of
      IfaceId{} -> IfaceIdExtras (id_extras n)
      IfaceData{ifCons=cons} ->
                     IfaceDataExtras (fix_fn n)
                        (map ifFamInstAxiom (lookupOccEnvL fi_env n) ++
                         map ifDFun         (lookupOccEnvL inst_env n))
                        (ann_fn n)
                        (map (id_extras . occName . ifConName) (visibleIfConDecls cons))
      IfaceClass{ifSigs=sigs, ifATs=ats} ->
                     IfaceClassExtras (fix_fn n)
                        (map ifDFun $ (concatMap at_extras ats)
                                    ++ lookupOccEnvL inst_env n)
                           -- Include instances of the associated types
                           -- as well as instances of the class (Trac #5147)
                        (ann_fn n)
                        [id_extras (getOccName op) | IfaceClassOp op _ _ <- sigs]
      IfaceSynonym{} -> IfaceSynonymExtras (fix_fn n)
                                           (ann_fn n)
      IfaceFamily{} -> IfaceFamilyExtras (fix_fn n)
                        (map ifFamInstAxiom (lookupOccEnvL fi_env n))
                        (ann_fn n)
      _other -> IfaceOtherDeclExtras
  where
        n = getOccName decl
        id_extras occ = IdExtras (fix_fn occ) (lookupOccEnvL rule_env occ) (ann_fn occ)
        at_extras (IfaceAT decl _) = lookupOccEnvL inst_env (getOccName decl)


lookupOccEnvL :: OccEnv [v] -> OccName -> [v]
lookupOccEnvL env k = lookupOccEnv env k `orElse` []

{-
-- for testing: use the md5sum command to generate fingerprints and
-- compare the results against our built-in version.
  fp' <- oldMD5 dflags bh
  if fp /= fp' then pprPanic "computeFingerprint" (ppr fp <+> ppr fp')
               else return fp

oldMD5 dflags bh = do
  tmp <- newTempName dflags "bin"
  writeBinMem bh tmp
  tmp2 <- newTempName dflags "md5"
  let cmd = "md5sum " ++ tmp ++ " >" ++ tmp2
  r <- system cmd
  case r of
    ExitFailure _ -> throwGhcExceptionIO (PhaseFailed cmd r)
    ExitSuccess -> do
        hash_str <- readFile tmp2
        return $! readHexFingerprint hash_str
-}

instOrphWarn :: DynFlags -> PrintUnqualified -> ClsInst -> WarnMsg
instOrphWarn dflags unqual inst
  = mkWarnMsg dflags (getSrcSpan inst) unqual $
    hang (ptext (sLit "Orphan instance:")) 2 (pprInstanceHdr inst)
    $$ text "To avoid this"
    $$ nest 4 (vcat possibilities)
  where
    possibilities =
      text "move the instance declaration to the module of the class or of the type, or" :
      text "wrap the type with a newtype and declare the instance on the new type." :
      []

ruleOrphWarn :: DynFlags -> PrintUnqualified -> Module -> IfaceRule -> WarnMsg
ruleOrphWarn dflags unqual mod rule
  = mkWarnMsg dflags silly_loc unqual $
    ptext (sLit "Orphan rule:") <+> ppr rule
  where
    silly_loc = srcLocSpan (mkSrcLoc (moduleNameFS (moduleName mod)) 1 1)
    -- We don't have a decent SrcSpan for a Rule, not even the CoreRule
    -- Could readily be fixed by adding a SrcSpan to CoreRule, if we wanted to

----------------------
-- mkOrphMap partitions instance decls or rules into
--      (a) an OccEnv for ones that are not orphans,
--          mapping the local OccName to a list of its decls
--      (b) a list of orphan decls
mkOrphMap :: (decl -> IsOrphan) -- Extract orphan status from decl
          -> [decl]             -- Sorted into canonical order
          -> (OccEnv [decl],    -- Non-orphan decls associated with their key;
                                --      each sublist in canonical order
              [decl])           -- Orphan decls; in canonical order
mkOrphMap get_key decls
  = foldl go (emptyOccEnv, []) decls
  where
    go (non_orphs, orphs) d
        | NotOrphan occ <- get_key d
        = (extendOccEnv_Acc (:) singleton non_orphs occ d, orphs)
        | otherwise = (non_orphs, d:orphs)

{-
************************************************************************
*                                                                      *
       Keeping track of what we've slurped, and fingerprints
*                                                                      *
************************************************************************
-}

mkIfaceAnnotation :: Annotation -> IfaceAnnotation
mkIfaceAnnotation (Annotation { ann_target = target, ann_value = payload })
  = IfaceAnnotation {
        ifAnnotatedTarget = fmap nameOccName target,
        ifAnnotatedValue = payload
    }

mkIfaceExports :: [AvailInfo] -> [IfaceExport]  -- Sort to make canonical
mkIfaceExports exports
  = sortBy stableAvailCmp (map sort_subs exports)
  where
    sort_subs :: AvailInfo -> AvailInfo
    sort_subs (Avail n) = Avail n
    sort_subs (AvailTC n []) = AvailTC n []
    sort_subs (AvailTC n (m:ms))
       | n==m      = AvailTC n (m:sortBy stableNameCmp ms)
       | otherwise = AvailTC n (sortBy stableNameCmp (m:ms))
       -- Maintain the AvailTC Invariant

{-
Note [Orignal module]
~~~~~~~~~~~~~~~~~~~~~
Consider this:
        module X where { data family T }
        module Y( T(..) ) where { import X; data instance T Int = MkT Int }
The exported Avail from Y will look like
        X.T{X.T, Y.MkT}
That is, in Y,
  - only MkT is brought into scope by the data instance;
  - but the parent (used for grouping and naming in T(..) exports) is X.T
  - and in this case we export X.T too

In the result of MkIfaceExports, the names are grouped by defining module,
so we may need to split up a single Avail into multiple ones.

Note [Internal used_names]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Most of the used_names are External Names, but we can have Internal
Names too: see Note [Binders in Template Haskell] in Convert, and
Trac #5362 for an example.  Such Names are always
  - Such Names are always for locally-defined things, for which we
    don't gather usage info, so we can just ignore them in ent_map
  - They are always System Names, hence the assert, just as a double check.


************************************************************************
*                                                                      *
        Load the old interface file for this module (unless
        we have it already), and check whether it is up to date
*                                                                      *
************************************************************************
-}

data RecompileRequired
  = UpToDate
       -- ^ everything is up to date, recompilation is not required
  | MustCompile
       -- ^ The .hs file has been touched, or the .o/.hi file does not exist
  | RecompBecause String
       -- ^ The .o/.hi files are up to date, but something else has changed
       -- to force recompilation; the String says what (one-line summary)
   deriving Eq

recompileRequired :: RecompileRequired -> Bool
recompileRequired UpToDate = False
recompileRequired _ = True



-- | Top level function to check if the version of an old interface file
-- is equivalent to the current source file the user asked us to compile.
-- If the same, we can avoid recompilation. We return a tuple where the
-- first element is a bool saying if we should recompile the object file
-- and the second is maybe the interface file, where Nothng means to
-- rebuild the interface file not use the exisitng one.
checkOldIface
  :: HscEnv
  -> ModSummary
  -> SourceModified
  -> Maybe ModIface         -- Old interface from compilation manager, if any
  -> IO (RecompileRequired, Maybe ModIface)

checkOldIface hsc_env mod_summary source_modified maybe_iface
  = do  let dflags = hsc_dflags hsc_env
        showPass dflags $
            "Checking old interface for " ++
              (showPpr dflags $ ms_mod mod_summary)
        initIfaceCheck hsc_env $
            check_old_iface hsc_env mod_summary source_modified maybe_iface

check_old_iface
  :: HscEnv
  -> ModSummary
  -> SourceModified
  -> Maybe ModIface
  -> IfG (RecompileRequired, Maybe ModIface)

check_old_iface hsc_env mod_summary src_modified maybe_iface
  = let dflags = hsc_dflags hsc_env
        getIface =
            case maybe_iface of
                Just _  -> do
                    traceIf (text "We already have the old interface for" <+>
                      ppr (ms_mod mod_summary))
                    return maybe_iface
                Nothing -> loadIface

        loadIface = do
             let iface_path = msHiFilePath mod_summary
             read_result <- readIface (ms_mod mod_summary) iface_path
             case read_result of
                 Failed err -> do
                     traceIf (text "FYI: cannot read old interface file:" $$ nest 4 err)
                     return Nothing
                 Succeeded iface -> do
                     traceIf (text "Read the interface file" <+> text iface_path)
                     return $ Just iface

        src_changed
            | gopt Opt_ForceRecomp (hsc_dflags hsc_env) = True
            | SourceModified <- src_modified = True
            | otherwise = False
    in do
        when src_changed $
            traceHiDiffs (nest 4 $ text "Source file changed or recompilation check turned off")

        case src_changed of
            -- If the source has changed and we're in interactive mode,
            -- avoid reading an interface; just return the one we might
            -- have been supplied with.
            True | not (isObjectTarget $ hscTarget dflags) ->
                return (MustCompile, maybe_iface)

            -- Try and read the old interface for the current module
            -- from the .hi file left from the last time we compiled it
            True -> do
                maybe_iface' <- getIface
                return (MustCompile, maybe_iface')

            False -> do
                maybe_iface' <- getIface
                case maybe_iface' of
                    -- We can't retrieve the iface
                    Nothing    -> return (MustCompile, Nothing)

                    -- We have got the old iface; check its versions
                    -- even in the SourceUnmodifiedAndStable case we
                    -- should check versions because some packages
                    -- might have changed or gone away.
                    Just iface -> checkVersions hsc_env mod_summary iface

-- | Check if a module is still the same 'version'.
--
-- This function is called in the recompilation checker after we have
-- determined that the module M being checked hasn't had any changes
-- to its source file since we last compiled M. So at this point in general
-- two things may have changed that mean we should recompile M:
--   * The interface export by a dependency of M has changed.
--   * The compiler flags specified this time for M have changed
--     in a manner that is significant for recompilaiton.
-- We return not just if we should recompile the object file but also
-- if we should rebuild the interface file.
checkVersions :: HscEnv
              -> ModSummary
              -> ModIface       -- Old interface
              -> IfG (RecompileRequired, Maybe ModIface)
checkVersions hsc_env mod_summary iface
  = do { traceHiDiffs (text "Considering whether compilation is required for" <+>
                        ppr (mi_module iface) <> colon)

       ; recomp <- checkFlagHash hsc_env iface
       ; if recompileRequired recomp then return (recomp, Nothing) else do {
       ; recomp <- checkHsig mod_summary iface
       ; if recompileRequired recomp then return (recomp, Nothing) else do {
       ; recomp <- checkDependencies hsc_env mod_summary iface
       ; if recompileRequired recomp then return (recomp, Just iface) else do {

       -- Source code unchanged and no errors yet... carry on
       --
       -- First put the dependent-module info, read from the old
       -- interface, into the envt, so that when we look for
       -- interfaces we look for the right one (.hi or .hi-boot)
       --
       -- It's just temporary because either the usage check will succeed
       -- (in which case we are done with this module) or it'll fail (in which
       -- case we'll compile the module from scratch anyhow).
       --
       -- We do this regardless of compilation mode, although in --make mode
       -- all the dependent modules should be in the HPT already, so it's
       -- quite redundant
       ; updateEps_ $ \eps  -> eps { eps_is_boot = mod_deps }
       ; recomp <- checkList [checkModUsage this_pkg u | u <- mi_usages iface]
       ; return (recomp, Just iface)
    }}}}
  where
    this_pkg = thisPackage (hsc_dflags hsc_env)
    -- This is a bit of a hack really
    mod_deps :: ModuleNameEnv (ModuleName, IsBootInterface)
    mod_deps = mkModDeps (dep_mods (mi_deps iface))

-- | Check if an hsig file needs recompilation because its
-- implementing module has changed.
checkHsig :: ModSummary -> ModIface -> IfG RecompileRequired
checkHsig mod_summary iface = do
    dflags <- getDynFlags
    let outer_mod = ms_mod mod_summary
        inner_mod = canonicalizeHomeModule dflags (moduleName outer_mod)
    MASSERT( thisPackage dflags == moduleUnitId outer_mod )
    case inner_mod == mi_semantic_module iface of
        True -> up_to_date (text "implementing module unchanged")
        False -> return (RecompBecause "implementing module changed")

-- | Check the flags haven't changed
checkFlagHash :: HscEnv -> ModIface -> IfG RecompileRequired
checkFlagHash hsc_env iface = do
    let old_hash = mi_flag_hash iface
    new_hash <- liftIO $ fingerprintDynFlags (hsc_dflags hsc_env)
                                             (mi_module iface)
                                             putNameLiterally
    case old_hash == new_hash of
        True  -> up_to_date (ptext $ sLit "Module flags unchanged")
        False -> out_of_date_hash "flags changed"
                     (ptext $ sLit "  Module flags have changed")
                     old_hash new_hash

-- If the direct imports of this module are resolved to targets that
-- are not among the dependencies of the previous interface file,
-- then we definitely need to recompile.  This catches cases like
--   - an exposed package has been upgraded
--   - we are compiling with different package flags
--   - a home module that was shadowing a package module has been removed
--   - a new home module has been added that shadows a package module
-- See bug #1372.
--
-- Returns True if recompilation is required.
checkDependencies :: HscEnv -> ModSummary -> ModIface -> IfG RecompileRequired
checkDependencies hsc_env summary iface
 = checkList (map dep_missing (ms_imps summary ++ ms_srcimps summary))
  where
   prev_dep_mods = dep_mods (mi_deps iface)
   prev_dep_pkgs = dep_pkgs (mi_deps iface)

   this_pkg = thisPackage (hsc_dflags hsc_env)

   dep_missing (mb_pkg, L _ mod) = do
     find_res <- liftIO $ findImportedModule hsc_env mod mb_pkg
     let reason = moduleNameString mod ++ " changed"
     case find_res of
        Found _ mod
          | pkg == this_pkg
           -> if moduleName mod `notElem` map fst prev_dep_mods
                 then do traceHiDiffs $
                           text "imported module " <> quotes (ppr mod) <>
                           text " not among previous dependencies"
                         return (RecompBecause reason)
                 else
                         return UpToDate
          | otherwise
           -> if toInstalledUnitId pkg `notElem` (map fst prev_dep_pkgs)
                 then do traceHiDiffs $
                           text "imported module " <> quotes (ppr mod) <>
                           text " is from package " <> quotes (ppr pkg) <>
                           text ", which is not among previous dependencies"
                         return (RecompBecause reason)
                 else
                         return UpToDate
           where pkg = moduleUnitId mod
        _otherwise  -> return (RecompBecause reason)

needInterface :: Module -> (ModIface -> IfG RecompileRequired)
              -> IfG RecompileRequired
needInterface mod continue
  = do  -- Load the imported interface if possible
    let doc_str = sep [ptext (sLit "need version info for"), ppr mod]
    traceHiDiffs (text "Checking usages for module" <+> ppr mod)

    mb_iface <- loadInterface doc_str mod ImportBySystem
        -- Load the interface, but don't complain on failure;
        -- Instead, get an Either back which we can test

    case mb_iface of
      Failed _ -> do
        traceHiDiffs (sep [ptext (sLit "Couldn't load interface for module"),
                           ppr mod])
        return MustCompile
                  -- Couldn't find or parse a module mentioned in the
                  -- old interface file.  Don't complain: it might
                  -- just be that the current module doesn't need that
                  -- import and it's been deleted
      Succeeded iface -> continue iface


-- | Given the usage information extracted from the old
-- M.hi file for the module being compiled, figure out
-- whether M needs to be recompiled.
checkModUsage :: UnitId -> Usage -> IfG RecompileRequired
checkModUsage _this_pkg UsagePackageModule{
                                usg_mod = mod,
                                usg_mod_hash = old_mod_hash }
  = needInterface mod $ \iface -> do
    let reason = moduleNameString (moduleName mod) ++ " changed"
    checkModuleFingerprint reason old_mod_hash (mi_mod_hash iface)
        -- We only track the ABI hash of package modules, rather than
        -- individual entity usages, so if the ABI hash changes we must
        -- recompile.  This is safe but may entail more recompilation when
        -- a dependent package has changed.

checkModUsage _ UsageMergedRequirement{ usg_mod = mod, usg_mod_hash = old_mod_hash }
  = needInterface mod $ \iface -> do
    let reason = moduleNameString (moduleName mod) ++ " changed (raw)"
    checkModuleFingerprint reason old_mod_hash (mi_mod_hash iface)

checkModUsage this_pkg UsageHomeModule{
                                usg_mod_name = mod_name,
                                usg_mod_hash = old_mod_hash,
                                usg_exports = maybe_old_export_hash,
                                usg_entities = old_decl_hash }
  = do
    let mod = mkModule this_pkg mod_name
    needInterface mod $ \iface -> do

    let
        new_mod_hash    = mi_mod_hash    iface
        new_decl_hash   = mi_hash_fn     iface
        new_export_hash = mi_exp_hash    iface

        reason = moduleNameString mod_name ++ " changed"

        -- CHECK MODULE
    recompile <- checkModuleFingerprint reason old_mod_hash new_mod_hash
    if not (recompileRequired recompile)
      then return UpToDate
      else do

        -- CHECK EXPORT LIST
        checkMaybeHash reason maybe_old_export_hash new_export_hash
            (ptext (sLit "  Export list changed")) $ do

        -- CHECK ITEMS ONE BY ONE
        recompile <- checkList [ checkEntityUsage reason new_decl_hash u
                               | u <- old_decl_hash]
        if recompileRequired recompile
          then return recompile     -- This one failed, so just bail out now
          else up_to_date (ptext (sLit "  Great!  The bits I use are up to date"))


checkModUsage _this_pkg UsageFile{ usg_file_path = file,
                                   usg_file_hash = old_hash } =
  liftIO $
    handleIO handle $ do
      new_hash <- getFileHash file
      if (old_hash /= new_hash)
         then return recomp
         else return UpToDate
 where
   recomp = RecompBecause (file ++ " changed")
   handle =
#ifdef DEBUG
       \e -> pprTrace "UsageFile" (text (show e)) $ return recomp
#else
       \_ -> return recomp -- if we can't find the file, just recompile, don't fail
#endif

------------------------
checkModuleFingerprint :: String -> Fingerprint -> Fingerprint
                       -> IfG RecompileRequired
checkModuleFingerprint reason old_mod_hash new_mod_hash
  | new_mod_hash == old_mod_hash
  = up_to_date (ptext (sLit "Module fingerprint unchanged"))

  | otherwise
  = out_of_date_hash reason (ptext (sLit "  Module fingerprint has changed"))
                     old_mod_hash new_mod_hash

------------------------
checkMaybeHash :: String -> Maybe Fingerprint -> Fingerprint -> SDoc
               -> IfG RecompileRequired -> IfG RecompileRequired
checkMaybeHash reason maybe_old_hash new_hash doc continue
  | Just hash <- maybe_old_hash, hash /= new_hash
  = out_of_date_hash reason doc hash new_hash
  | otherwise
  = continue

------------------------
checkEntityUsage :: String
                 -> (OccName -> Maybe (OccName, Fingerprint))
                 -> (OccName, Fingerprint)
                 -> IfG RecompileRequired
checkEntityUsage reason new_hash (name,old_hash)
  = case new_hash name of

        Nothing       ->        -- We used it before, but it ain't there now
                          out_of_date reason (sep [ptext (sLit "No longer exported:"), ppr name])

        Just (_, new_hash)      -- It's there, but is it up to date?
          | new_hash == old_hash -> do traceHiDiffs (text "  Up to date" <+> ppr name <+> parens (ppr new_hash))
                                       return UpToDate
          | otherwise            -> out_of_date_hash reason (ptext (sLit "  Out of date:") <+> ppr name)
                                                     old_hash new_hash

up_to_date :: SDoc -> IfG RecompileRequired
up_to_date  msg = traceHiDiffs msg >> return UpToDate

out_of_date :: String -> SDoc -> IfG RecompileRequired
out_of_date reason msg = traceHiDiffs msg >> return (RecompBecause reason)

out_of_date_hash :: String -> SDoc -> Fingerprint -> Fingerprint -> IfG RecompileRequired
out_of_date_hash reason msg old_hash new_hash
  = out_of_date reason (hsep [msg, ppr old_hash, ptext (sLit "->"), ppr new_hash])

----------------------
checkList :: [IfG RecompileRequired] -> IfG RecompileRequired
-- This helper is used in two places
checkList []             = return UpToDate
checkList (check:checks) = do recompile <- check
                              if recompileRequired recompile
                                then return recompile
                                else checkList checks

{-
************************************************************************
*                                                                      *
                Converting things to their Iface equivalents
*                                                                      *
************************************************************************
-}

tyThingToIfaceDecl :: TyThing -> IfaceDecl
tyThingToIfaceDecl (AnId id)      = idToIfaceDecl id
tyThingToIfaceDecl (ATyCon tycon) = snd (tyConToIfaceDecl emptyTidyEnv tycon)
tyThingToIfaceDecl (ACoAxiom ax)  = coAxiomToIfaceDecl ax
tyThingToIfaceDecl (AConLike cl)  = case cl of
    RealDataCon dc -> dataConToIfaceDecl dc -- for ppr purposes only
    PatSynCon ps   -> patSynToIfaceDecl ps

--------------------------
idToIfaceDecl :: Id -> IfaceDecl
-- The Id is already tidied, so that locally-bound names
-- (lambdas, for-alls) already have non-clashing OccNames
-- We can't tidy it here, locally, because it may have
-- free variables in its type or IdInfo
idToIfaceDecl id
  = IfaceId { ifName      = getName id,
              ifType      = toIfaceType (idType id),
              ifIdDetails = toIfaceIdDetails (idDetails id),
              ifIdInfo    = toIfaceIdInfo (idInfo id) }

--------------------------
dataConToIfaceDecl :: DataCon -> IfaceDecl
dataConToIfaceDecl dataCon
  = IfaceId { ifName      = getName dataCon,
              ifType      = toIfaceType (dataConUserType dataCon),
              ifIdDetails = IfVanillaId,
              ifIdInfo    = NoInfo }

--------------------------
patSynToIfaceDecl :: PatSyn -> IfaceDecl
patSynToIfaceDecl ps
  = IfacePatSyn { ifName          = getName $ ps
                , ifPatMatcher    = to_if_pr (patSynMatcher ps)
                , ifPatBuilder    = fmap to_if_pr (patSynBuilder ps)
                , ifPatIsInfix    = patSynIsInfix ps
                , ifPatUnivTvs    = toIfaceTvBndrs univ_tvs'
                , ifPatExTvs      = toIfaceTvBndrs ex_tvs'
                , ifPatProvCtxt   = tidyToIfaceContext env2 prov_theta
                , ifPatReqCtxt    = tidyToIfaceContext env2 req_theta
                , ifPatArgs       = map (tidyToIfaceType env2) args
                , ifPatTy         = tidyToIfaceType env2 rhs_ty
                }
  where
    (univ_tvs, ex_tvs, prov_theta, req_theta, args, rhs_ty) = patSynSig ps
    (env1, univ_tvs') = tidyTyVarBndrs emptyTidyEnv univ_tvs
    (env2, ex_tvs')   = tidyTyVarBndrs env1 ex_tvs
    to_if_pr (id, needs_dummy) = (idName id, needs_dummy)

--------------------------
coAxiomToIfaceDecl :: CoAxiom br -> IfaceDecl
-- We *do* tidy Axioms, because they are not (and cannot
-- conveniently be) built in tidy form
coAxiomToIfaceDecl ax@(CoAxiom { co_ax_tc = tycon, co_ax_branches = branches
                               , co_ax_role = role })
 = IfaceAxiom { ifName       = getName ax
              , ifTyCon      = toIfaceTyCon tycon
              , ifRole       = role
              , ifAxBranches = brListMap (coAxBranchToIfaceBranch tycon
                                            (brListMap coAxBranchLHS branches))
                                         branches }
 -- where
 --   name = getOccName ax

-- 2nd parameter is the list of branch LHSs, for conversion from incompatible branches
-- to incompatible indices
-- See Note [Storing compatibility] in CoAxiom
coAxBranchToIfaceBranch :: TyCon -> [[Type]] -> CoAxBranch -> IfaceAxBranch
coAxBranchToIfaceBranch tc lhs_s
                        branch@(CoAxBranch { cab_incomps = incomps })
  = (coAxBranchToIfaceBranch' tc branch) { ifaxbIncomps = iface_incomps }
  where
    iface_incomps = map (expectJust "iface_incomps"
                        . (flip findIndex lhs_s
                          . eqTypes)
                        . coAxBranchLHS) incomps

-- use this one for standalone branches without incompatibles
coAxBranchToIfaceBranch' :: TyCon -> CoAxBranch -> IfaceAxBranch
coAxBranchToIfaceBranch' tc (CoAxBranch { cab_tvs = tvs, cab_lhs = lhs
                                        , cab_roles = roles, cab_rhs = rhs })
  = IfaceAxBranch { ifaxbTyVars = toIfaceTvBndrs tv_bndrs
                  , ifaxbLHS    = tidyToIfaceTcArgs env1 tc lhs
                  , ifaxbRoles  = roles
                  , ifaxbRHS    = tidyToIfaceType env1 rhs
                  , ifaxbIncomps = [] }
  where
    (env1, tv_bndrs) = tidyTyClTyVarBndrs emptyTidyEnv tvs
    -- Don't re-bind in-scope tyvars
    -- See Note [CoAxBranch type variables] in CoAxiom

-----------------
tyConToIfaceDecl :: TidyEnv -> TyCon -> (TidyEnv, IfaceDecl)
-- We *do* tidy TyCons, because they are not (and cannot
-- conveniently be) built in tidy form
-- The returned TidyEnv is the one after tidying the tyConTyVars
tyConToIfaceDecl env tycon
  | Just clas <- tyConClass_maybe tycon
  = classToIfaceDecl env clas

  | Just syn_rhs <- synTyConRhs_maybe tycon
  = ( tc_env1
    , IfaceSynonym { ifName    = getName tycon,
                     ifTyVars  = if_tc_tyvars,
                     ifRoles   = tyConRoles tycon,
                     ifSynRhs  = if_syn_type syn_rhs,
                     ifSynKind = tidyToIfaceType tc_env1 (synTyConResKind tycon)
                   })

  | Just fam_flav <- famTyConFlav_maybe tycon
  = ( tc_env1
    , IfaceFamily { ifName    = getName tycon,
                    ifTyVars  = if_tc_tyvars,
                    ifFamFlav = to_if_fam_flav fam_flav,
                    ifFamKind = tidyToIfaceType tc_env1 (synTyConResKind tycon)
                  })

  | isAlgTyCon tycon
  = ( tc_env1
    , IfaceData { ifName    = getName tycon,
                  ifCType   = tyConCType tycon,
                  ifTyVars  = if_tc_tyvars,
                  ifRoles   = tyConRoles tycon,
                  ifCtxt    = tidyToIfaceContext tc_env1 (tyConStupidTheta tycon),
                  ifCons    = ifaceConDecls (algTyConRhs tycon),
                  ifRec     = boolToRecFlag (isRecursiveTyCon tycon),
                  ifGadtSyntax = isGadtSyntaxTyCon tycon,
                  ifPromotable = isJust (promotableTyCon_maybe tycon),
                  ifParent  = parent })

  | otherwise  -- FunTyCon, PrimTyCon, promoted TyCon/DataCon
  -- For pretty printing purposes only.
  = ( env
    , IfaceData { ifName       = getName tycon,
                  ifCType      = Nothing,
                  ifTyVars     = funAndPrimTyVars,
                  ifRoles      = tyConRoles tycon,
                  ifCtxt       = [],
                  ifCons       = IfDataTyCon [],
                  ifRec        = boolToRecFlag False,
                  ifGadtSyntax = False,
                  ifPromotable = False,
                  ifParent     = IfNoParent })
  where
    (tc_env1, tc_tyvars) = tidyTyClTyVarBndrs env (tyConTyVars tycon)
    if_tc_tyvars = toIfaceTvBndrs tc_tyvars
    if_syn_type ty = tidyToIfaceType tc_env1 ty

    funAndPrimTyVars = toIfaceTvBndrs $ take (tyConArity tycon) alphaTyVars

    parent = case tyConFamInstSig_maybe tycon of
               Just (tc, ty, ax) -> IfDataInstance (coAxiomName ax)
                                                   (toIfaceTyCon tc)
                                                   (tidyToIfaceTcArgs tc_env1 tc ty)
               Nothing           -> IfNoParent

    to_if_fam_flav OpenSynFamilyTyCon        = IfaceOpenSynFamilyTyCon
    to_if_fam_flav (ClosedSynFamilyTyCon ax) = IfaceClosedSynFamilyTyCon axn ibr
      where defs = fromBranchList $ coAxiomBranches ax
            ibr  = map (coAxBranchToIfaceBranch' tycon) defs
            axn  = coAxiomName ax
    to_if_fam_flav AbstractClosedSynFamilyTyCon
      = IfaceAbstractClosedSynFamilyTyCon

    to_if_fam_flav (BuiltInSynFamTyCon {})
      = IfaceBuiltInSynFamTyCon


    ifaceConDecls (NewTyCon { data_con = con })     = IfNewTyCon  (ifaceConDecl con)
    ifaceConDecls (DataTyCon { data_cons = cons })  = IfDataTyCon (map ifaceConDecl cons)
    ifaceConDecls (DataFamilyTyCon {})              = IfDataFamTyCon
    ifaceConDecls (AbstractTyCon distinct)          = IfAbstractTyCon distinct
        -- The last case happens when a TyCon has been trimmed during tidying
        -- Furthermore, tyThingToIfaceDecl is also used
        -- in TcRnDriver for GHCi, when browsing a module, in which case the
        -- AbstractTyCon case is perfectly sensible.

    ifaceConDecl data_con
        = IfCon   { ifConName       = getName (dataConName data_con),
                    ifConInfix      = dataConIsInfix data_con,
                    ifConWrapper    = isJust (dataConWrapId_maybe data_con),
                    ifConExTvs      = toIfaceTvBndrs ex_tvs',
                    ifConEqSpec     = map to_eq_spec eq_spec,
                    ifConCtxt       = tidyToIfaceContext con_env2 theta,
                    ifConArgTys     = map (tidyToIfaceType con_env2) arg_tys,
                    ifConFields     = dataConFieldLabels data_con,
                    ifConStricts    = map (toIfaceBang con_env2)
                                       (dataConImplBangs data_con),
                    ifConSrcStricts = map toIfaceSrcBang
                                         (dataConSrcBangs data_con) }
        where
          (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _) = dataConFullSig data_con

          -- Tidy the univ_tvs of the data constructor to be identical
          -- to the tyConTyVars of the type constructor.  This means
          -- (a) we don't need to redundantly put them into the interface file
          -- (b) when pretty-printing an Iface data declaration in H98-style syntax,
          --     we know that the type variables will line up
          -- The latter (b) is important because we pretty-print type construtors
          -- by converting to IfaceSyn and pretty-printing that
          con_env1 = (fst tc_env1, mkVarEnv (zipEqual "ifaceConDecl" univ_tvs tc_tyvars))
                     -- A bit grimy, perhaps, but it's simple!

          (con_env2, ex_tvs') = tidyTyVarBndrs con_env1 ex_tvs
          to_eq_spec (tv,ty)  = (toIfaceTyVar (tidyTyVar con_env2 tv), tidyToIfaceType con_env2 ty)

toIfaceBang :: TidyEnv -> HsImplBang -> IfaceBang
toIfaceBang _    HsLazy              = IfNoBang
toIfaceBang _   (HsUnpack Nothing)   = IfUnpack
toIfaceBang env (HsUnpack (Just co)) = IfUnpackCo (toIfaceCoercion (tidyCo env co))
toIfaceBang _   HsStrict             = IfStrict

toIfaceSrcBang :: HsSrcBang -> IfaceSrcBang
toIfaceSrcBang (HsSrcBang _ unpk bang) = IfSrcBang unpk bang

classToIfaceDecl :: TidyEnv -> Class -> (TidyEnv, IfaceDecl)
classToIfaceDecl env clas
  = ( env1
    , IfaceClass { ifCtxt   = tidyToIfaceContext env1 sc_theta,
                   ifName   = getName (classTyCon clas),
                   ifTyVars = toIfaceTvBndrs clas_tyvars',
                   ifRoles  = tyConRoles (classTyCon clas),
                   ifFDs    = map toIfaceFD clas_fds,
                   ifATs    = map toIfaceAT clas_ats,
                   ifSigs   = map toIfaceClassOp op_stuff,
                   ifMinDef = fmap getFS (classMinimalDef clas),
                   ifRec    = boolToRecFlag (isRecursiveTyCon tycon) })
  where
    (clas_tyvars, clas_fds, sc_theta, _, clas_ats, op_stuff)
      = classExtraBigSig clas
    tycon = classTyCon clas

    (env1, clas_tyvars') = tidyTyVarBndrs env clas_tyvars

    toIfaceAT :: ClassATItem -> IfaceAT
    toIfaceAT (ATI tc def)
      = IfaceAT if_decl (fmap (tidyToIfaceType env2 . fst) def)
      where
        (env2, if_decl) = tyConToIfaceDecl env1 tc

    toIfaceClassOp (sel_id, def_meth)
        = ASSERT(sel_tyvars == clas_tyvars)
          IfaceClassOp (getName sel_id) (toDmSpec def_meth)
                       (tidyToIfaceType env1 op_ty)
        where
                -- Be careful when splitting the type, because of things
                -- like         class Foo a where
                --                op :: (?x :: String) => a -> a
                -- and          class Baz a where
                --                op :: (Ord a) => a -> a
          (sel_tyvars, rho_ty) = splitForAllTys (idType sel_id)
          op_ty                = funResultTy rho_ty

    toDmSpec NoDefMeth      = NoDM
    toDmSpec (GenDefMeth _) = GenericDM
    toDmSpec (DefMeth _)    = VanillaDM

    toIfaceFD (tvs1, tvs2) = (map (getFS . tidyTyVar env1) tvs1,
                              map (getFS . tidyTyVar env1) tvs2)

--------------------------
tidyToIfaceType :: TidyEnv -> Type -> IfaceType
tidyToIfaceType env ty = toIfaceType (tidyType env ty)

tidyToIfaceTcArgs :: TidyEnv -> TyCon -> [Type] -> IfaceTcArgs
tidyToIfaceTcArgs env tc tys = toIfaceTcArgs tc (tidyTypes env tys)

tidyToIfaceContext :: TidyEnv -> ThetaType -> IfaceContext
tidyToIfaceContext env theta = map (tidyToIfaceType env) theta

tidyTyClTyVarBndrs :: TidyEnv -> [TyVar] -> (TidyEnv, [TyVar])
tidyTyClTyVarBndrs env tvs = mapAccumL tidyTyClTyVarBndr env tvs

tidyTyClTyVarBndr :: TidyEnv -> TyVar -> (TidyEnv, TyVar)
-- If the type variable "binder" is in scope, don't re-bind it
-- In a class decl, for example, the ATD binders mention
-- (amd must mention) the class tyvars
tidyTyClTyVarBndr env@(_, subst) tv
 | Just tv' <- lookupVarEnv subst tv = (env, tv')
 | otherwise                         = tidyTyVarBndr env tv

tidyTyVar :: TidyEnv -> TyVar -> TyVar
tidyTyVar (_, subst) tv = lookupVarEnv subst tv `orElse` tv
   -- TcType.tidyTyVarOcc messes around with FlatSkols

getFS :: NamedThing a => a -> FastString
getFS x = occNameFS (getOccName x)

--------------------------
instanceToIfaceInst :: ClsInst -> IfaceClsInst
instanceToIfaceInst (ClsInst { is_dfun = dfun_id, is_flag = oflag
                             , is_cls_nm = cls_name, is_cls = cls
                             , is_tcs = mb_tcs
                             , is_orphan = orph })
  = ASSERT( cls_name == className cls )
    IfaceClsInst { ifDFun    = dfun_name,
                ifOFlag   = oflag,
                ifInstCls = cls_name,
                ifInstTys = map do_rough mb_tcs,
                ifInstOrph = orph }
  where
    do_rough Nothing  = Nothing
    do_rough (Just n) = Just (toIfaceTyCon_name n)

    dfun_name = idName dfun_id


--------------------------
famInstToIfaceFamInst :: FamInst -> IfaceFamInst
famInstToIfaceFamInst (FamInst { fi_axiom    = axiom,
                                 fi_fam      = fam,
                                 fi_tcs      = roughs })
  = IfaceFamInst { ifFamInstAxiom    = coAxiomName axiom
                 , ifFamInstFam      = fam
                 , ifFamInstTys      = map do_rough roughs
                 , ifFamInstOrph     = orph }
  where
    do_rough Nothing  = Nothing
    do_rough (Just n) = Just (toIfaceTyCon_name n)

    fam_decl = tyConName $ coAxiomTyCon axiom
    mod = ASSERT( isExternalName (coAxiomName axiom) )
          nameModule (coAxiomName axiom)
    is_local name = nameIsLocalOrFrom mod name

    lhs_names = filterNameSet is_local (orphNamesOfCoCon axiom)

    orph | is_local fam_decl
         = NotOrphan (nameOccName fam_decl)

         | otherwise
         = chooseOrphanAnchor lhs_names

--------------------------
toIfaceLetBndr :: Id -> IfaceLetBndr
toIfaceLetBndr id  = IfLetBndr (occNameFS (getOccName id))
                               (toIfaceType (idType id))
                               (toIfaceIdInfo (idInfo id))
  -- Put into the interface file any IdInfo that CoreTidy.tidyLetBndr
  -- has left on the Id.  See Note [IdInfo on nested let-bindings] in IfaceSyn

--------------------------
toIfaceIdDetails :: IdDetails -> IfaceIdDetails
toIfaceIdDetails VanillaId                      = IfVanillaId
toIfaceIdDetails (DFunId ns _)                  = IfDFunId ns
toIfaceIdDetails (RecSelId { sel_naughty = n
                           , sel_tycon = tc })  = IfRecSelId (toIfaceTyCon tc) n
toIfaceIdDetails other                          = pprTrace "toIfaceIdDetails" (ppr other)
                                                  IfVanillaId   -- Unexpected

toIfaceIdInfo :: IdInfo -> IfaceIdInfo
toIfaceIdInfo id_info
  = case catMaybes [arity_hsinfo, caf_hsinfo, strict_hsinfo,
                    inline_hsinfo,  unfold_hsinfo] of
       []    -> NoInfo
       infos -> HasInfo infos
               -- NB: strictness and arity must appear in the list before unfolding
               -- See TcIface.tcUnfolding
  where
    ------------  Arity  --------------
    arity_info = arityInfo id_info
    arity_hsinfo | arity_info == 0 = Nothing
                 | otherwise       = Just (HsArity arity_info)

    ------------ Caf Info --------------
    caf_info   = cafInfo id_info
    caf_hsinfo = case caf_info of
                   NoCafRefs -> Just HsNoCafRefs
                   _other    -> Nothing

    ------------  Strictness  --------------
        -- No point in explicitly exporting TopSig
    sig_info = strictnessInfo id_info
    strict_hsinfo | not (isTopSig sig_info) = Just (HsStrictness sig_info)
                  | otherwise               = Nothing

    ------------  Unfolding  --------------
    unfold_hsinfo = toIfUnfolding loop_breaker (unfoldingInfo id_info)
    loop_breaker  = isStrongLoopBreaker (occInfo id_info)

    ------------  Inline prag  --------------
    inline_prag = inlinePragInfo id_info
    inline_hsinfo | isDefaultInlinePragma inline_prag = Nothing
                  | otherwise = Just (HsInline inline_prag)

--------------------------
toIfUnfolding :: Bool -> Unfolding -> Maybe IfaceInfoItem
toIfUnfolding lb (CoreUnfolding { uf_tmpl = rhs
                                , uf_src = src
                                , uf_guidance = guidance })
  = Just $ HsUnfold lb $
    case src of
        InlineStable
          -> case guidance of
               UnfWhen {ug_arity = arity, ug_unsat_ok = unsat_ok, ug_boring_ok =  boring_ok }
                      -> IfInlineRule arity unsat_ok boring_ok if_rhs
               _other -> IfCoreUnfold True if_rhs
        InlineCompulsory -> IfCompulsory if_rhs
        InlineRhs        -> IfCoreUnfold False if_rhs
        -- Yes, even if guidance is UnfNever, expose the unfolding
        -- If we didn't want to expose the unfolding, TidyPgm would
        -- have stuck in NoUnfolding.  For supercompilation we want
        -- to see that unfolding!
  where
    if_rhs = toIfaceExpr rhs

toIfUnfolding lb (DFunUnfolding { df_bndrs = bndrs, df_args = args })
  = Just (HsUnfold lb (IfDFunUnfold (map toIfaceBndr bndrs) (map toIfaceExpr args)))
      -- No need to serialise the data constructor;
      -- we can recover it from the type of the dfun

toIfUnfolding _ _
  = Nothing

--------------------------
coreRuleToIfaceRule :: Module -> CoreRule -> IfaceRule
coreRuleToIfaceRule _ (BuiltinRule { ru_fn = fn})
  = pprTrace "toHsRule: builtin" (ppr fn) $
    bogusIfaceRule fn

coreRuleToIfaceRule mod rule@(Rule { ru_name = name, ru_fn = fn,
                                     ru_act = act, ru_bndrs = bndrs,
                                     ru_args = args, ru_rhs = rhs,
                                     ru_auto = auto })
  = IfaceRule { ifRuleName  = name, ifActivation = act,
                ifRuleBndrs = map toIfaceBndr bndrs,
                ifRuleHead  = fn,
                ifRuleArgs  = map do_arg args,
                ifRuleRhs   = toIfaceExpr rhs,
                ifRuleAuto  = auto,
                ifRuleOrph  = orph }
  where
        -- For type args we must remove synonyms from the outermost
        -- level.  Reason: so that when we read it back in we'll
        -- construct the same ru_rough field as we have right now;
        -- see tcIfaceRule
    do_arg (Type ty)     = IfaceType (toIfaceType (deNoteType ty))
    do_arg (Coercion co) = IfaceCo   (toIfaceCoercion co)
    do_arg arg           = toIfaceExpr arg

        -- Compute orphanhood.  See Note [Orphans] in InstEnv
        -- A rule is an orphan only if none of the variables
        -- mentioned on its left-hand side are locally defined
    lhs_names = nameSetElems (ruleLhsOrphNames rule)

    orph = case filter (nameIsLocalOrFrom mod) lhs_names of
                        (n : _) -> NotOrphan (nameOccName n)
                        []      -> IsOrphan

bogusIfaceRule :: Name -> IfaceRule
bogusIfaceRule id_name
  = IfaceRule { ifRuleName = fsLit "bogus", ifActivation = NeverActive,
        ifRuleBndrs = [], ifRuleHead = id_name, ifRuleArgs = [],
        ifRuleRhs = IfaceExt id_name, ifRuleOrph = IsOrphan,
        ifRuleAuto = True }

---------------------
toIfaceExpr :: CoreExpr -> IfaceExpr
toIfaceExpr (Var v)         = toIfaceVar v
toIfaceExpr (Lit l)         = IfaceLit l
toIfaceExpr (Type ty)       = IfaceType (toIfaceType ty)
toIfaceExpr (Coercion co)   = IfaceCo   (toIfaceCoercion co)
toIfaceExpr (Lam x b)       = IfaceLam (toIfaceBndr x, toIfaceOneShot x) (toIfaceExpr b)
toIfaceExpr (App f a)       = toIfaceApp f [a]
toIfaceExpr (Case s x ty as)
  | null as                 = IfaceECase (toIfaceExpr s) (toIfaceType ty)
  | otherwise               = IfaceCase (toIfaceExpr s) (getFS x) (map toIfaceAlt as)
toIfaceExpr (Let b e)       = IfaceLet (toIfaceBind b) (toIfaceExpr e)
toIfaceExpr (Cast e co)     = IfaceCast (toIfaceExpr e) (toIfaceCoercion co)
toIfaceExpr (Tick t e)
  | Just t' <- toIfaceTickish t = IfaceTick t' (toIfaceExpr e)
  | otherwise                   = toIfaceExpr e

toIfaceOneShot :: Id -> IfaceOneShot
toIfaceOneShot id | isId id
                  , OneShotLam <- oneShotInfo (idInfo id)
                  = IfaceOneShot
                  | otherwise
                  = IfaceNoOneShot

---------------------
toIfaceTickish :: Tickish Id -> Maybe IfaceTickish
toIfaceTickish (ProfNote cc tick push) = Just (IfaceSCC cc tick push)
toIfaceTickish (HpcTick modl ix)       = Just (IfaceHpcTick modl ix)
toIfaceTickish (SourceNote src names)  = Just (IfaceSource src names)
toIfaceTickish (Breakpoint {})         = Nothing
   -- Ignore breakpoints, since they are relevant only to GHCi, and
   -- should not be serialised (Trac #8333)

---------------------
toIfaceBind :: Bind Id -> IfaceBinding
toIfaceBind (NonRec b r) = IfaceNonRec (toIfaceLetBndr b) (toIfaceExpr r)
toIfaceBind (Rec prs)    = IfaceRec [(toIfaceLetBndr b, toIfaceExpr r) | (b,r) <- prs]

---------------------
toIfaceAlt :: (AltCon, [Var], CoreExpr)
           -> (IfaceConAlt, [FastString], IfaceExpr)
toIfaceAlt (c,bs,r) = (toIfaceCon c, map getFS bs, toIfaceExpr r)

---------------------
toIfaceCon :: AltCon -> IfaceConAlt
toIfaceCon (DataAlt dc) = IfaceDataAlt (getName dc)
toIfaceCon (LitAlt l)   = IfaceLitAlt l
toIfaceCon DEFAULT      = IfaceDefault

---------------------
toIfaceApp :: Expr CoreBndr -> [Arg CoreBndr] -> IfaceExpr
toIfaceApp (App f a) as = toIfaceApp f (a:as)
toIfaceApp (Var v) as
  = case isDataConWorkId_maybe v of
        -- We convert the *worker* for tuples into IfaceTuples
        Just dc |  isTupleTyCon tc && saturated
                -> IfaceTuple (tupleTyConSort tc) tup_args
          where
            val_args  = dropWhile isTypeArg as
            saturated = val_args `lengthIs` idArity v
            tup_args  = map toIfaceExpr val_args
            tc        = dataConTyCon dc

        _ -> mkIfaceApps (toIfaceVar v) as

toIfaceApp e as = mkIfaceApps (toIfaceExpr e) as

mkIfaceApps :: IfaceExpr -> [CoreExpr] -> IfaceExpr
mkIfaceApps f as = foldl (\f a -> IfaceApp f (toIfaceExpr a)) f as

---------------------
toIfaceVar :: Id -> IfaceExpr
toIfaceVar v
    | Just fcall <- isFCallId_maybe v            = IfaceFCall fcall (toIfaceType (idType v))
    -- TODO: Implement boot unfoldings
    --    -- Foreign calls have special syntax
    -- | isBootUnfolding (idUnfolding v)
    -- = IfaceApp (IfaceApp (IfaceExt noinlineIdName) (IfaceType (toIfaceType (idType v))))
    --            (IfaceExt name) -- don't use mkIfaceApps, or infinite loop
    --    -- See Note [Inlining and hs-boot files]
    --    -- Foreign calls have special syntax
    | isExternalName name                        = IfaceExt name
    | otherwise                                  = IfaceLcl (getFS name)
  where name = idName v
