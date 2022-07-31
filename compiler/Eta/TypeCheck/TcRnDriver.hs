{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[TcMovectle]{Typechecking a whole module}
-}

{-# LANGUAGE CPP, NondecreasingIndentation, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Eta.TypeCheck.TcRnDriver (
#ifdef ETA_REPL
        tcRnStmt, tcRnExpr, tcRnType,
        tcRnImportDecls,
        tcRnLookupRdrName,
        getModuleInterface,
        tcRnDeclsi,
        isGHCiMonad,
        runTcInteractive,    -- Used by GHC API clients (Trac #8878)
        loadUnqualIfaces,
#endif
        tcRnLookupName,
        tcRnGetInfo,
        tcRnModule, tcRnModuleTcRnM,
        tcTopSrcDecls,
        rnTopSrcDecls,
        checkBootDecl, checkHiBootIface',
        findExtraSigImports,
        implicitRequirements,
        checkUnitId,
        mergeSignatures,
        tcRnMergeSignatures,
        instantiateSignature,
        tcRnInstantiateSignature,
        -- More private...
        badReexportedBootThing,
        checkBootDeclM,
        missingBootThing,
    ) where

#ifdef ETA_REPL
import {-# SOURCE #-} Eta.TypeCheck.TcSplice ( finishTH )
import Eta.Rename.RnSplice ( rnTopSpliceDecls, traceSplice, SpliceInfo(..) )
#endif

import Eta.Main.DynFlags
import Eta.Main.StaticFlags
import Eta.HsSyn.HsSyn
import Eta.Prelude.PrelNames
import Eta.Prelude.THNames
import Eta.BasicTypes.RdrName
import Eta.TypeCheck.TcHsSyn
import Eta.TypeCheck.TcExpr
import Eta.TypeCheck.TcRnMonad
import Eta.TypeCheck.TcEvidence
-- import Eta.Main.PprTyThing( pprTyThing )
import qualified Eta.Main.PprTyThing as PprTyThing
import Eta.Types.Coercion( pprCoAxiom )
import Eta.TypeCheck.FamInst
import Eta.Types.InstEnv
import Eta.Types.FamInstEnv
import Eta.TypeCheck.TcAnnotations
import Eta.TypeCheck.TcBinds
import Eta.Main.HeaderInfo       ( mkPrelImports )
import Eta.TypeCheck.TcDefaults
import Eta.TypeCheck.TcEnv
import Eta.TypeCheck.TcRules
import Eta.TypeCheck.TcForeign
import Eta.TypeCheck.TcInstDcls
import Eta.Iface.TcIface
import Eta.TypeCheck.TcMType
import Eta.Iface.MkIface
import Eta.TypeCheck.TcSimplify
import Eta.TypeCheck.TcTyClsDecls
import Eta.TypeCheck.TcBackpack
import Eta.Iface.LoadIface
import Eta.Rename.RnNames
import Eta.Rename.RnEnv
import Eta.Rename.RnSource
import Eta.Main.ErrUtils
import Eta.BasicTypes.Id
import qualified Eta.BasicTypes.Id as Id
import Eta.BasicTypes.IdInfo( IdDetails( VanillaId ) )
import Eta.BasicTypes.VarEnv
import Eta.BasicTypes.Module
import Eta.Utils.UniqFM
import Eta.BasicTypes.Name
import Eta.BasicTypes.NameEnv
import Eta.BasicTypes.NameSet
import Eta.BasicTypes.Avail
import Eta.Types.TyCon
import Eta.BasicTypes.SrcLoc
import Eta.Main.HscTypes
import Eta.Utils.ListSetOps
import Eta.Utils.Outputable
import Eta.BasicTypes.ConLike
import Eta.BasicTypes.DataCon
import Eta.Types.Type
import Eta.Types.Class
import Eta.Types.CoAxiom
import Eta.Main.Annotations
import Data.List ( sortBy )
import Data.Ord
#ifdef ETA_REPL
import Eta.BasicTypes.BasicTypes hiding( SuccessFlag(..) )
import Eta.TypeCheck.TcType   ( isUnitTy, isTauTy )
import Eta.TypeCheck.TcHsType
import Eta.TypeCheck.TcMatches
import Eta.Rename.RnTypes
import Eta.Rename.RnExpr
import Eta.Prelude.PrelInfo
import Eta.BasicTypes.MkId
import Eta.Main.TidyPgm    ( globaliseAndTidyId )
import Eta.Prelude.TysWiredIn ( unitTy, mkListTy )
#endif

import qualified Eta.LanguageExtensions as LangExt
import Eta.Utils.FastString
import Eta.Utils.Maybes
import Eta.Utils.Util
import Eta.Utils.Bag

import qualified Data.Set as S

import Control.Monad

#include "HsVersions.h"
{-
************************************************************************
*                                                                      *
        Typecheck and rename a module
*                                                                      *
************************************************************************
-}

-- | Top level entry point for typechecker and renamer
tcRnModule :: HscEnv
           -> HscSource
           -> Bool              -- True <=> save renamed syntax
           -> HsParsedModule
           -> IO (Messages, Maybe TcGblEnv)

tcRnModule hsc_env hsc_src save_rn_syntax
   parsedModule@HsParsedModule {hpm_module=L loc this_module}
 | RealSrcSpan real_loc <- loc
 = do { showPass (hsc_dflags hsc_env) "Renamer/typechecker" ;

      ; initTc hsc_env hsc_src save_rn_syntax this_mod real_loc $
               withTcPlugins hsc_env $
               tcRnModuleTcRnM hsc_env hsc_src parsedModule pair }

  | otherwise
  = return ((emptyBag, unitBag err_msg), Nothing)

  where
    err_msg = mkPlainErrMsg (hsc_dflags hsc_env) loc $
              text "Module does not have a RealSrcSpan:" <+> ppr this_mod

    this_pkg = thisPackage (hsc_dflags hsc_env)

    pair :: (Module, SrcSpan)
    pair@(this_mod,_)
      | Just (L mod_loc mod) <- hsmodName this_module
      = (mkModule this_pkg mod, mod_loc)

      | otherwise   -- 'module M where' is omitted
      = (mAIN, srcLocSpan (srcSpanStart loc))

tcRnModuleTcRnM :: HscEnv
                -> HscSource
                -> HsParsedModule
                -> (Module, SrcSpan)
                -> TcRn TcGblEnv
-- Factored out separately so that a Core plugin can
-- call the type checker directly
tcRnModuleTcRnM hsc_env hsc_src
                (HsParsedModule {
                   hpm_module =
                      (L loc (HsModule maybe_mod export_ies
                                       import_decls local_decls mod_deprec
                                       maybe_doc_hdr)),
                   hpm_src_files = src_files
                })
                (this_mod, prel_imp_loc)
 = setSrcSpan loc $
   do { let { explicit_mod_hdr = isJust maybe_mod } ;

                         -- Load the hi-boot interface for this module, if any
                         -- We do this now so that the boot_names can be passed
                         -- to tcTyAndClassDecls, because the boot_names are
                         -- automatically considered to be loop breakers
        tcg_env <- getGblEnv ;
        boot_info <- tcHiBootIface hsc_src this_mod ;
        setGblEnv (tcg_env { tcg_self_boot = boot_info }) $ do {

        -- Deal with imports; first add implicit prelude
        implicit_prelude <- xoptM LangExt.ImplicitPrelude;
        let { prel_imports = mkPrelImports (moduleName this_mod) prel_imp_loc
                                         implicit_prelude import_decls } ;

        whenWOptM Opt_WarnImplicitPrelude $
             when (notNull prel_imports) $
                  addWarn (Reason Opt_WarnImplicitPrelude) (implicitPreludeWarn) ;

            -- TODO This is a little skeevy; maybe handle a bit more directly
           raw_sig_imports <- liftIO $ findExtraSigImports hsc_env hsc_src (moduleName this_mod) ;
           raw_req_imports <- liftIO $
               implicitRequirements hsc_env (ideclsSimplified (prel_imports ++ import_decls)) ;
           let { mkImport (Nothing, L _ mod_name) = noLoc $ (simpleImportDecl mod_name) {
                   ideclHiding = Just (False, noLoc [])
                   } ;
                 mkImport _ = panic "mkImport" } ;

           let { all_imports = prel_imports ++ import_decls
                          ++ map mkImport (raw_sig_imports ++ raw_req_imports) } ;

           -- OK now finally rename the imports
        tcg_env <- {-# SCC "tcRnImports" #-} tcRnImports hsc_env all_imports ;

          -- If the whole module is warned about or deprecated
          -- (via mod_deprec) record that in tcg_warns. If we do thereby add
          -- a WarnAll, it will override any subsequent depracations added to tcg_warns
        let { tcg_env1 = case mod_deprec of
                           Just (L _ txt) -> tcg_env { tcg_warns = WarnAll txt }
                           Nothing        -> tcg_env
            } ;

        setGblEnv tcg_env1 $ do {

                -- Rename and type check the declarations
        traceRn "rn1a" empty ;

        tcg_env <- if isHsBootOrSig hsc_src then
                        tcRnHsBootDecls hsc_src local_decls
                   else
                        {-# SCC "tcRnSrcDecls" #-}
                        tcRnSrcDecls explicit_mod_hdr local_decls ;
        setGblEnv tcg_env               $ do {

                -- Process the export list
        traceRn "rn4a: before exports" empty;
        tcg_env <- rnExports explicit_mod_hdr export_ies tcg_env ;
        traceRn "rn4b: after exports" empty ;

                -- Check that main is exported (must be after rnExports)
        checkMainExported tcg_env ;

        -- Compare the hi-boot iface (if any) with the real thing
        -- Must be done after processing the exports
        tcg_env <- checkHiBootIface tcg_env boot_info ;

        -- Compare the hsig tcg_env with the real thing
        -- checkHsigIface hsc_env tcg_env ;
        --
        -- -- Nub out type class instances now that we've checked them,
        -- -- if we're compiling an hsig with sig-of.
        -- -- See Note [Signature files and type class instances]
        -- tcg_env <- (case tcg_sig_of tcg_env of
        --     Just _ -> return tcg_env {
        --                 tcg_inst_env = emptyInstEnv,
        --                 tcg_fam_inst_env = emptyFamInstEnv,
        --                 tcg_insts = [],
        --                 tcg_fam_insts = []
        --                 }
        --     Nothing -> return tcg_env) ;

        -- The new type env is already available to stuff slurped from
        -- interface files, via TcEnv.updateGlobalTypeEnv
        -- It's important that this includes the stuff in checkHiBootIface,
        -- because the latter might add new bindings for boot_dfuns,
        -- which may be mentioned in imported unfoldings

                -- Don't need to rename the Haddock documentation,
                -- it's not parsed by GHC anymore.
        tcg_env <- return (tcg_env { tcg_doc_hdr = maybe_doc_hdr }) ;

                -- Report unused names
        reportUnusedNames export_ies tcg_env ;

                -- add extra source files to tcg_dependent_files
        addDependentFiles src_files ;

                -- Dump output and return
        tcDump tcg_env ;
        return tcg_env
    }}}}

implicitPreludeWarn :: SDoc
implicitPreludeWarn
  = ptext (sLit "Module `Prelude' implicitly imported")

{-
************************************************************************
*                                                                      *
                Import declarations
*                                                                      *
************************************************************************
-}

tcRnImports :: HscEnv -> [LImportDecl RdrName] -> TcM TcGblEnv
tcRnImports hsc_env import_decls
  = do  { (rn_imports, rdr_env, imports, hpc_info) <- rnImports import_decls

        ; this_mod <- getModule
        ; let { dep_mods :: ModuleNameEnv (ModuleName, IsBootInterface)
              ; dep_mods = imp_dep_mods imports

                -- We want instance declarations from all home-package
                -- modules below this one, including boot modules, except
                -- ourselves.  The 'except ourselves' is so that we don't
                -- get the instances from this module's hs-boot file.  This
                -- filtering also ensures that we don't see instances from
                -- modules batch (@--make@) compiled before this one, but
                -- which are not below this one.
              ; want_instances :: ModuleName -> Bool
              ; want_instances mod = mod `elemUFM` dep_mods
                                   && mod /= moduleName this_mod
              ; (home_insts, home_fam_insts) = hptInstances hsc_env
                                                            want_instances
              } ;

                -- Record boot-file info in the EPS, so that it's
                -- visible to loadHiBootInterface in tcRnSrcDecls,
                -- and any other incrementally-performed imports
        ; updateEps_ (\eps -> eps { eps_is_boot = dep_mods }) ;

                -- Update the gbl env
        ; updGblEnv ( \ gbl ->
            gbl {
              tcg_rdr_env      = tcg_rdr_env gbl `plusGlobalRdrEnv` rdr_env,
              tcg_imports      = tcg_imports gbl `plusImportAvails` imports,
              tcg_rn_imports   = rn_imports,
              tcg_visible_orphan_mods = foldl extendModuleSet
                                              (tcg_visible_orphan_mods gbl)
                                              (imp_orphs imports),
              tcg_inst_env     = extendInstEnvList (tcg_inst_env gbl) home_insts,
              tcg_fam_inst_env = extendFamInstEnvList (tcg_fam_inst_env gbl)
                                                      home_fam_insts,
              tcg_hpc          = hpc_info
            }) $ do {

        ; traceRn "rn1" (ppr (imp_dep_mods imports))
                -- Fail if there are any errors so far
                -- The error printing (if needed) takes advantage
                -- of the tcg_env we have now set
--      ; traceIf (text "rdr_env: " <+> ppr rdr_env)
        ; failIfErrsM

                -- Load any orphan-module and family instance-module
                -- interfaces, so that their rules and instance decls will be
                -- found.
        ; loadModuleInterfaces (ptext (sLit "Loading orphan modules"))
                               (imp_orphs imports)

                -- Check type-family consistency
        ; traceRn "rn1: checking family instance consistency" empty
        ; let { dir_imp_mods = moduleEnvKeys
                             . imp_mods
                             $ imports }
        ; checkFamInstConsistency (imp_finsts imports) dir_imp_mods ;

        ; getGblEnv } }

{-
************************************************************************
*                                                                      *
        Type-checking the top level of a module
*                                                                      *
************************************************************************
-}

tcRnSrcDecls :: Bool
             -> [LHsDecl RdrName]
             -> TcM TcGblEnv
        -- Returns the variables free in the decls
        -- Reason: solely to report unused imports and bindings
tcRnSrcDecls explicit_mod_hdr decls
 = do {  -- Do all the declarations
        ((tcg_env, tcl_env), lie) <- captureTopConstraints $
              do { (tcg_env, tcl_env) <- tc_rn_src_decls decls

                   -- Check for the 'main' declaration
                   -- Must do this inside the captureTopConstraints
                 ; tcg_env <- setEnvs (tcg_env, tcl_env) $
                              checkMain explicit_mod_hdr
                 ; return (tcg_env, tcl_env) }

      ; traceTc "Tc8" empty ;

      ; setEnvs (tcg_env, tcl_env) $ do {

             --         Simplify constraints
             --
             -- We do this after checkMain, so that we use the type info
             -- that checkMain adds
             --
             -- We do it with both global and local env in scope:
             --  * the global env exposes the instances to simplifyTop
             --  * the local env exposes the local Ids to simplifyTop,
             --    so that we get better error messages (monomorphism restriction)
      ; new_ev_binds <- {-# SCC "simplifyTop" #-}
                        simplifyTop lie

        -- Finalizers must run after constraints are simplified, or some types
        -- might not be complete when using reify (see #12777).
      ; (tcg_env, tcl_env) <- setGblEnv tcg_env run_th_modfinalizers

      ; setEnvs (tcg_env, tcl_env) $ do {

      ; finishTH

      ; traceTc "Tc9" empty

      ; failIfErrsM     -- Don't zonk if there have been errors
                        -- It's a waste of time; and we may get debug warnings
                        -- about strangely-typed TyCons!
      ; traceTc "Tc10" empty

        -- Zonk the final code.  This must be done last.
        -- Even simplifyTop may do some unification.
        -- This pass also warns about missing type signatures
      ; let { TcGblEnv { tcg_type_env  = type_env,
                         tcg_binds     = binds,
                         tcg_ev_binds  = cur_ev_binds,
                         tcg_imp_specs = imp_specs,
                         tcg_rules     = rules,
                         tcg_vects     = vects,
                         tcg_fords     = fords } = tcg_env
            ; all_ev_binds = cur_ev_binds `unionBags` new_ev_binds } ;

      ; (bind_ids, ev_binds', binds', fords', imp_specs', rules', vects')
            <- {-# SCC "zonkTopDecls" #-}
               zonkTopDecls all_ev_binds binds rules vects imp_specs fords ;

      ; traceTc "Tc11" empty


      ; let { final_type_env = extendTypeEnvWithIds type_env bind_ids
            ; tcg_env' = tcg_env { tcg_binds    = binds',
                                   tcg_ev_binds = ev_binds',
                                   tcg_imp_specs = imp_specs',
                                   tcg_rules    = rules',
                                   tcg_vects    = vects',
                                   tcg_fords    = fords' } } ;

        setGlobalTypeEnv tcg_env' final_type_env
   }
   } }

-- | Runs TH finalizers and renames and typechecks the top-level declarations
-- that they could introduce.
run_th_modfinalizers :: TcM (TcGblEnv, TcLclEnv)
run_th_modfinalizers = do
  th_modfinalizers_var <- fmap tcg_th_modfinalizers getGblEnv
  th_modfinalizers <- readTcRef th_modfinalizers_var
  if null th_modfinalizers
  then getEnvs
  else do
    writeTcRef th_modfinalizers_var []
    (envs, lie) <- captureTopConstraints $ do
      sequence_ th_modfinalizers
      -- Finalizers can add top-level declarations with addTopDecls.
      tc_rn_src_decls []
    setEnvs envs $ do
      -- Subsequent rounds of finalizers run after any new constraints are
      -- simplified, or some types might not be complete when using reify
      -- (see #12777).
      new_ev_binds <- {-# SCC "simplifyTop2" #-}
                      simplifyTop lie
      updGblEnv (\tcg_env ->
        tcg_env { tcg_ev_binds = tcg_ev_binds tcg_env `unionBags` new_ev_binds }
        )
        -- addTopDecls can add declarations which add new finalizers.
        run_th_modfinalizers

tc_rn_src_decls :: [LHsDecl RdrName]
                -> TcM (TcGblEnv, TcLclEnv)
-- Loops around dealing with each top level inter-splice group
-- in turn, until it's dealt with the entire module
tc_rn_src_decls ds
 = {-# SCC "tc_rn_src_decls" #-}
   do { (first_group, group_tail) <- findSplice ds
                -- If ds is [] we get ([], Nothing)

        -- The extra_deps are needed while renaming type and class declarations
        -- See Note [Extra dependencies from .hs-boot files] in RnSource
      -- ; let { extra_deps = map tyConName (typeEnvTyCons (md_types boot_details)) }
        -- Deal with decls up to, but not including, the first splice
      ; (tcg_env, rn_decls) <- rnTopSrcDecls first_group
                -- rnTopSrcDecls fails if there are any errors

#ifdef ETA_REPL
        -- Get TH-generated top-level declarations and make sure they don't
        -- contain any splices since we don't handle that at the moment
        --
        -- The plumbing here is a bit odd: see Trac #10853
      ; th_topdecls_var <- fmap tcg_th_topdecls getGblEnv
      ; th_ds <- readTcRef th_topdecls_var
      ; writeTcRef th_topdecls_var []

      ; (tcg_env, rn_decls) <-
            if null th_ds
            then return (tcg_env, rn_decls)
            else do { (th_group, th_group_tail) <- findSplice th_ds
                    ; case th_group_tail of
                        { Nothing -> return () ;
                        ; Just (SpliceDecl (L loc _) _, _)
                            -> setSrcSpan loc $
                               addErr (text "Declaration splices are not permitted inside top-level declarations added with addTopDecls")
                        } ;

                    -- Rename TH-generated top-level declarations
                    ; (tcg_env, th_rn_decls) <- setGblEnv tcg_env $
                      rnTopSrcDecls th_group

                    -- Dump generated top-level declarations
                    ; let msg = "top-level declarations added with addTopDecls"
                    ; traceSplice $ SpliceInfo { spliceDescription = msg
                                               , spliceIsDecl    = True
                                               , spliceSource    = Nothing
                                               , spliceGenerated = ppr th_rn_decls }

                    ; return (tcg_env, appendGroups rn_decls th_rn_decls)
                    }
#endif /* ETA_REPL */

      -- Type check all declarations
      ; (tcg_env, tcl_env) <- setGblEnv tcg_env $
                              tcTopSrcDecls rn_decls

        -- If there is no splice, we're nearly done
      ; setEnvs (tcg_env, tcl_env) $
        case group_tail of
          { Nothing ->  return (tcg_env, tcl_env)
#ifndef ETA_REPL
            -- There shouldn't be a splice
          ; Just (SpliceDecl {}, _) ->
            failWithTc (text "Can't do a top-level splice; need a bootstrapped compiler")
          }
#else
            -- If there's a splice, we must carry on
          ; Just (SpliceDecl (L _ splice) _, rest_ds) ->
            do { -- Rename the splice expression, and get its supporting decls
                 (spliced_decls, splice_fvs) <- checkNoErrs (rnTopSpliceDecls splice)

                 -- Glue them on the front of the remaining decls and loop
               ; setGblEnv (tcg_env `addTcgDUs` usesOnly splice_fvs) $
                 tc_rn_src_decls (spliced_decls ++ rest_ds)
               }
          }
#endif /* ETA_REPL */
      }

{-
************************************************************************
*                                                                      *
        Compiling hs-boot source files, and
        comparing the hi-boot interface with the real thing
*                                                                      *
************************************************************************
-}

tcRnHsBootDecls :: HscSource -> [LHsDecl RdrName] -> TcM TcGblEnv
tcRnHsBootDecls hsc_src decls
   = do { (first_group, group_tail) <- findSplice decls

                -- Rename the declarations
        ; (tcg_env, HsGroup {
                   hs_tyclds = tycl_decls,
                   hs_instds = inst_decls,
                   hs_derivds = deriv_decls,
                   hs_fords  = for_decls,
                   hs_defds  = def_decls,
                   hs_ruleds = rule_decls,
                   hs_vects  = vect_decls,
                   hs_annds  = _,
                   hs_valds  = val_binds }) <- rnTopSrcDecls first_group
        -- The empty list is for extra dependencies coming from .hs-boot files
        -- See Note [Extra dependencies from .hs-boot files] in RnSource
        ; (gbl_env, lie) <- captureTopConstraints $ setGblEnv tcg_env $ do {


                -- Check for illegal declarations
        ; case group_tail of
             Just (SpliceDecl d _, _) -> badBootDecl hsc_src "splice" d
             Nothing                  -> return ()
        ; mapM_ (badBootDecl hsc_src "foreign") for_decls
        ; mapM_ (badBootDecl hsc_src "default") def_decls
        ; mapM_ (badBootDecl hsc_src "rule")    rule_decls
        ; mapM_ (badBootDecl hsc_src "vect")    vect_decls

                -- Typecheck type/class/instance decls
        ; traceTc "Tc2 (boot)" empty
        ; (tcg_env, inst_infos, _deriv_binds)
             <- tcTyClsInstDecls tycl_decls inst_decls deriv_decls
        ; setGblEnv tcg_env     $ do {

                -- Typecheck value declarations
        ; traceTc "Tc5" empty
        ; val_ids <- tcHsBootSigs val_binds

                -- Wrap up
                -- No simplification or zonking to do
        ; traceTc "Tc7a" empty
        ; gbl_env <- getGblEnv

                -- Make the final type-env
                -- Include the dfun_ids so that their type sigs
                -- are written into the interface file.
        ; let { type_env0 = tcg_type_env gbl_env
              ; type_env1 = extendTypeEnvWithIds type_env0 val_ids
              -- Don't add the dictionaries for hsig, we don't actually want
              -- to /define/ the instance
              ; type_env2 = extendTypeEnvWithIds type_env1 dfun_ids
              ; dfun_ids = map iDFunId inst_infos
              }

        ; setGlobalTypeEnv gbl_env type_env2
   }}
   ; traceTc "boot" (ppr lie); return gbl_env }

badBootDecl :: HscSource -> String -> Located decl -> TcM ()
badBootDecl hsc_src what (L loc _)
  = addErrAt loc (char 'A' <+> text what
      <+> ptext (sLit "declaration is not (currently) allowed in a")
      <+> (case hsc_src of
            HsBootFile -> ptext (sLit "hs-boot")
            HsigFile -> ptext (sLit "hsig")
            _ -> panic "badBootDecl: should be an hsig or hs-boot file")
      <+> ptext (sLit "file"))

{-
Once we've typechecked the body of the module, we want to compare what
we've found (gathered in a TypeEnv) with the hi-boot details (if any).
-}

checkHiBootIface :: TcGblEnv -> SelfBootInfo -> TcM TcGblEnv
-- Compare the hi-boot file for this module (if there is one)
-- with the type environment we've just come up with
-- In the common case where there is no hi-boot file, the list
-- of boot_names is empty.
--
-- The bindings we return give bindings for the dfuns defined in the
-- hs-boot file, such as        $fbEqT = $fEqT

checkHiBootIface tcg_env boot_info
  | NoSelfBoot <- boot_info  -- Common case
  = return tcg_env

  | HsBootFile <- tcg_src tcg_env   -- Current module is already a hs-boot file!
  = return tcg_env

  | SelfBoot { sb_mds = boot_details } <- boot_info
  , TcGblEnv { tcg_binds    = binds
             , tcg_insts    = local_insts
             , tcg_type_env = local_type_env
             , tcg_exports  = local_exports } <- tcg_env

        -- tcg_env@(TcGblEnv { tcg_src = hs_src, tcg_binds = binds,
        --                     tcg_insts = local_insts,
        --                     tcg_type_env = local_type_env, tcg_exports = local_exports })

  = do  { -- This code is tricky, see Note [DFun knot-tying]
        ; let boot_dfuns = filter isDFunId (typeEnvIds (md_types boot_details))
              type_env'  = extendTypeEnvWithIds local_type_env boot_dfuns
        -- Why the seq?  Without, we will put a TypeEnv thunk in
        -- tcg_type_env_var.  That thunk will eventually get
        -- forced if we are typechecking interfaces, but that
        -- is no good if we are trying to typecheck the very
        -- DFun we were going to put in.
        -- TODO: Maybe setGlobalTypeEnv should be strict.
        ; tcg_env <- type_env' `seq` setGlobalTypeEnv tcg_env type_env'
        ; dfun_prs <- checkHiBootIface' local_insts type_env'
                                  local_exports boot_details
        ; let dfun_binds = listToBag [ mkVarBind boot_dfun (nlHsVar dfun)
                               | (boot_dfun, dfun) <- dfun_prs ]
        ; return tcg_env { tcg_binds = binds `unionBags` dfun_binds } }

  | otherwise = panic "checkHiBootIface: unreachable code"

-- Note [DFun knot-tying]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- The 'SelfBootInfo' that is fed into 'checkHiBootIface' comes
-- from typechecking the hi-boot file that we are presently
-- implementing.  Suppose we are typechecking the module A:
-- when we typecheck the hi-boot file, whenever we see an
-- identifier A.T, we knot-tie this identifier to the
-- *local* type environment (via if_rec_types.)  The contract
-- then is that we don't *look* at 'SelfBootInfo' until
-- we've finished typechecking the module and updated the
-- type environment with the new tycons and ids.
--
-- This most works well, but there is one problem: DFuns!
-- In general, it's not possible to know a priori what an
-- hs-boot file named a DFun (see Note [DFun impedance matching]),
-- so we look at the ClsInsts from the boot file to figure out
-- what DFuns to add to the type environment.  But we're not
-- allowed to poke the DFuns of the ClsInsts in the SelfBootInfo
-- until we've added the DFuns to the type environment.  A
-- Gordian knot!
--
-- We cut the knot by a little trick: we first *unconditionally*
-- add all of the boot-declared DFuns to the type environment
-- (so that knot tying works, see Trac #4003), without the
-- actual bindings for them.  Then, we compute the impedance
-- matching bindings, and add them to the environment.
--
-- There is one subtlety to doing this: we have to get the
-- DFuns from md_types, not md_insts, even though involves
-- filtering a bunch of TyThings we don't care about.  The
-- reason is only the TypeEnv in md_types has the actual
-- Id we want to add to the environment; the DFun fields
-- in md_insts are typechecking thunks that will attempt to
-- go through if_rec_types to lookup the real Id... but
-- that's what we're trying to setup right now.


checkHiBootIface' :: [ClsInst] -> TypeEnv -> [AvailInfo]
                  -> ModDetails -> TcM [(Id, Id)]
-- Variant which doesn't require a full TcGblEnv; you could get the
-- local components from another ModDetails.

-- Note [DFun impedance matching]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We return a list of "impedance-matching" bindings for the dfuns
-- defined in the hs-boot file, such as
--           $fxEqT = $fEqT
-- We need these because the module and hi-boot file might differ in
-- the name it chose for the dfun: the name of a dfun is not
-- uniquely determined by its type; there might be multiple dfuns
-- which, individually, would map to the same name (in which case
-- we have to disambiguate them.)  There's no way for the hi file
-- to know exactly what disambiguation to use... without looking
-- at the hi-boot file itself.
--
-- In fact, the names will always differ because we always pick names
-- prefixed with "$fx" for boot dfuns, and "$f" for real dfuns
-- (so that this impedance matching is always possible).

checkHiBootIface'
        local_insts local_type_env local_exports
        (ModDetails { md_insts = boot_insts, md_fam_insts = boot_fam_insts,
                      md_types = boot_type_env, md_exports = boot_exports })
  = do  { traceTc "checkHiBootIface" $ vcat
             [ ppr boot_type_env, ppr boot_insts, ppr boot_exports]

                -- Check the exports of the boot module, one by one
        ; mapM_ check_export boot_exports

                -- Check for no family instances
        ; unless (null boot_fam_insts) $
            panic ("TcRnDriver.checkHiBootIface: Cannot handle family " ++
                   "instances in boot files yet...")
            -- FIXME: Why?  The actual comparison is not hard, but what would
            --        be the equivalent to the dfun bindings returned for class
            --        instances?  We can't easily equate tycons...

                -- Check instance declarations
        ; mb_dfun_prs <- mapM check_inst boot_insts

        ; failIfErrsM

        ; return (catMaybes mb_dfun_prs) }

  where
    check_export boot_avail     -- boot_avail is exported by the boot iface
      | name `elem` dfun_names = return ()
      | isWiredInName name     = return ()      -- No checking for wired-in names.  In particular,
                                                -- 'error' is handled by a rather gross hack
                                                -- (see comments in GHC.Err.hs-boot)

        -- Check that the actual module exports the same thing
      | not (null missing_names)
      = addErrAt (nameSrcSpan (head missing_names))
                 (missingBootThing True (head missing_names) "exported by")

        -- If the boot module does not *define* the thing, we are done
        -- (it simply re-exports it, and names match, so nothing further to do)
      | isNothing mb_boot_thing = return ()

        -- Check that the actual module also defines the thing, and
        -- then compare the definitions
      | Just real_thing <- lookupTypeEnv local_type_env name,
        Just boot_thing <- mb_boot_thing
      = checkBootDeclM True boot_thing real_thing

      | otherwise
      = addErrTc (missingBootThing True name "defined in")
      where
        name          = availName boot_avail
        mb_boot_thing = lookupTypeEnv boot_type_env name
        missing_names = case lookupNameEnv local_export_env name of
                          Nothing    -> [name]
                          Just avail -> availNames boot_avail `minusList` availNames avail

    dfun_names = map getName boot_insts

    local_export_env :: NameEnv AvailInfo
    local_export_env = availsToNameEnv local_exports

    check_inst :: ClsInst -> TcM (Maybe (Id, Id))
        -- Returns a pair of the boot dfun in terms of the equivalent real dfun
    check_inst boot_inst
        = case [dfun | inst <- local_insts,
                       let dfun = instanceDFunId inst,
                       idType dfun `eqType` boot_inst_ty ] of
            [] -> do { traceTc "check_inst" (vcat [ text "local_insts" <+> vcat (map (ppr . idType . instanceDFunId) local_insts)
                                                  , text "boot_inst"   <+> ppr boot_inst
                                                  , text "boot_inst_ty" <+> ppr boot_inst_ty
                                                  ])
                     ; addErrTc (instMisMatch True boot_inst); return Nothing }
            (dfun:_) -> return (Just (local_boot_dfun, dfun))
        where
          boot_dfun = instanceDFunId boot_inst
          boot_inst_ty = idType boot_dfun
          local_boot_dfun = Id.mkExportedLocalId VanillaId (idName boot_dfun) boot_inst_ty

-- In general, to perform these checks we have to
-- compare the TyThing from the .hi-boot file to the TyThing
-- in the current source file.  We must be careful to allow alpha-renaming
-- where appropriate, and also the boot declaration is allowed to omit
-- constructors and class methods.
--
-- See rnfail055 for a good test of this stuff.

-- | Compares two things for equivalence between boot-file and normal code,
-- reporting an error if they don't match up.
checkBootDeclM :: Bool  -- ^ True <=> an hs-boot file (could also be a sig)
               -> TyThing -> TyThing -> TcM ()
checkBootDeclM is_boot boot_thing real_thing
  = whenIsJust (checkBootDecl is_boot boot_thing real_thing) $ \ err ->
    addErrAt span
             (bootMisMatch is_boot err real_thing boot_thing)
  where
   -- Here we use the span of the boot thing or, if it doesn't have a sensible
   -- span, that of the real thing,
     span
       | let span = nameSrcSpan (getName boot_thing)
       , isGoodSrcSpan span
       = span
       | otherwise
       = nameSrcSpan (getName real_thing)

-- | Compares the two things for equivalence between boot-file and normal
-- code. Returns @Nothing@ on success or @Just "some helpful info for user"@
-- failure. If the difference will be apparent to the user, @Just empty@ is
-- perfectly suitable.
checkBootDecl :: Bool -> TyThing -> TyThing -> Maybe SDoc

checkBootDecl _ (AnId id1) (AnId id2)
  = ASSERT(id1 == id2)
    check (idType id1 `eqType` idType id2)
          (text "The two types are different")

checkBootDecl is_boot (ATyCon tc1) (ATyCon tc2)
  = checkBootTyCon is_boot tc1 tc2

checkBootDecl _ (AConLike (RealDataCon dc1)) (AConLike (RealDataCon _))
  = pprPanic "checkBootDecl" (ppr dc1)

checkBootDecl _ _ _ = Just empty -- probably shouldn't happen

-- | Combines two potential error messages
andThenCheck :: Maybe SDoc -> Maybe SDoc -> Maybe SDoc
Nothing `andThenCheck` msg     = msg
msg     `andThenCheck` Nothing = msg
Just d1 `andThenCheck` Just d2 = Just (d1 $$ d2)
infixr 0 `andThenCheck`

-- | If the test in the first parameter is True, succeed with @Nothing@;
-- otherwise, return the provided check
checkUnless :: Bool -> Maybe SDoc -> Maybe SDoc
checkUnless True  _ = Nothing
checkUnless False k = k

-- | Run the check provided for every pair of elements in the lists.
-- The provided SDoc should name the element type, in the plural.
checkListBy :: (a -> a -> Maybe SDoc) -> [a] -> [a] -> SDoc
            -> Maybe SDoc
checkListBy check_fun as bs whats = go [] as bs
  where
    herald = text "The" <+> whats <+> text "do not match"

    go []   [] [] = Nothing
    go docs [] [] = Just (hang (herald <> colon) 2 (vcat $ reverse docs))
    go docs (x:xs) (y:ys) = case check_fun x y of
      Just doc -> go (doc:docs) xs ys
      Nothing  -> go docs       xs ys
    go _    _  _ = Just (hang (herald <> colon)
                            2 (text "There are different numbers of" <+> whats))

-- | If the test in the first parameter is True, succeed with @Nothing@;
-- otherwise, fail with the given SDoc.
check :: Bool -> SDoc -> Maybe SDoc
check True  _   = Nothing
check False doc = Just doc

-- | A more perspicuous name for @Nothing@, for @checkBootDecl@ and friends.
checkSuccess :: Maybe SDoc
checkSuccess = Nothing

----------------
checkBootTyCon :: Bool -> TyCon -> TyCon -> Maybe SDoc
checkBootTyCon is_boot tc1 tc2
  | not (eqKind (tyConKind tc1) (tyConKind tc2))
  = Just $ text "The types have different kinds"    -- First off, check the kind

  | Just c1 <- tyConClass_maybe tc1
  , Just c2 <- tyConClass_maybe tc2
  , let (clas_tvs1, clas_fds1, sc_theta1, _, ats1, op_stuff1)
          = classExtraBigSig c1
        (clas_tvs2, clas_fds2, sc_theta2, _, ats2, op_stuff2)
          = classExtraBigSig c2
  , Just env <- eqTyVarBndrs emptyRnEnv2 clas_tvs1 clas_tvs2
  = let
       eqSig (id1, def_meth1) (id2, def_meth2)
         = check (name1 == name2)
                 (text "The names" <+> pname1 <+> text "and" <+> pname2 <+>
                  text "are different") `andThenCheck`
           check (eqTypeX env op_ty1 op_ty2)
                 (text "The types of" <+> pname1 <+>
                  text "are different") `andThenCheck`
           check (def_meth1 == def_meth2)
                 (text "The default methods associated with" <+> pname1 <+>
                  text "are different")
         where
          name1 = idName id1
          name2 = idName id2
          pname1 = quotes (ppr name1)
          pname2 = quotes (ppr name2)
          (_, rho_ty1) = splitForAllTys (idType id1)
          op_ty1 = funResultTy rho_ty1
          (_, rho_ty2) = splitForAllTys (idType id2)
          op_ty2 = funResultTy rho_ty2

       eqAT (ATI tc1 def_ats1) (ATI tc2 def_ats2)
         = checkBootTyCon is_boot tc1 tc2 `andThenCheck`
           check (eqATDef def_ats1 def_ats2)
                 (text "The associated type defaults differ")

       -- Ignore the location of the defaults
       eqATDef Nothing             Nothing             = True
       eqATDef (Just (ty1, _loc1)) (Just (ty2, _loc2)) = eqTypeX env ty1 ty2
       eqATDef _ _ = False

       eqFD (as1,bs1) (as2,bs2) =
         eqListBy (eqTypeX env) (mkTyVarTys as1) (mkTyVarTys as2) &&
         eqListBy (eqTypeX env) (mkTyVarTys bs1) (mkTyVarTys bs2)
    in
    check (roles1 == roles2) roles_msg `andThenCheck`
          -- Checks kind of class
    check (eqListBy eqFD clas_fds1 clas_fds2)
          (text "The functional dependencies do not match") `andThenCheck`
    checkUnless (null sc_theta1 && null op_stuff1 && null ats1) $
                     -- Above tests for an "abstract" class
    check (eqListBy (eqPredX env) sc_theta1 sc_theta2)
          (text "The class constraints do not match") `andThenCheck`
    checkListBy eqSig op_stuff1 op_stuff2 (text "methods") `andThenCheck`
    checkListBy eqAT ats1 ats2 (text "associated types")

  | Just syn_rhs1 <- synTyConRhs_maybe tc1
  , Just syn_rhs2 <- synTyConRhs_maybe tc2
  , Just env <- eqTyVarBndrs emptyRnEnv2 (tyConTyVars tc1) (tyConTyVars tc2)
  = ASSERT(tc1 == tc2)
    check (roles1 == roles2) roles_msg `andThenCheck`
    check (eqTypeX env syn_rhs1 syn_rhs2) empty   -- nothing interesting to say

  -- Type synonyms for hs-boot are questionable, so they
  -- are not supported at the moment
  | not is_boot && isAbstractTyCon tc1 && isTypeSynonymTyCon tc2
  = check (roles1 == roles2) roles_msg

  | Just fam_flav1 <- famTyConFlav_maybe tc1
  , Just fam_flav2 <- famTyConFlav_maybe tc2
  = ASSERT(tc1 == tc2)
    let eqFamFlav OpenSynFamilyTyCon OpenSynFamilyTyCon = True
        eqFamFlav AbstractClosedSynFamilyTyCon (ClosedSynFamilyTyCon {}) = True
        eqFamFlav (ClosedSynFamilyTyCon {}) AbstractClosedSynFamilyTyCon = True
        eqFamFlav (ClosedSynFamilyTyCon ax1) (ClosedSynFamilyTyCon ax2)
            = eqClosedFamilyAx ax1 ax2
        eqFamFlav (BuiltInSynFamTyCon _) (BuiltInSynFamTyCon _) = tc1 == tc2
        eqFamFlav _ _ = False
    in
    check (roles1 == roles2) roles_msg `andThenCheck`
    check (eqFamFlav fam_flav1 fam_flav2) empty   -- nothing interesting to say

  | isAlgTyCon tc1 && isAlgTyCon tc2
  , Just env <- eqTyVarBndrs emptyRnEnv2 (tyConTyVars tc1) (tyConTyVars tc2)
  = ASSERT(tc1 == tc2)
    check (roles1 == roles2) roles_msg `andThenCheck`
    check (eqListBy (eqPredX env)
                     (tyConStupidTheta tc1) (tyConStupidTheta tc2))
          (text "The datatype contexts do not match") `andThenCheck`
    eqAlgRhs tc1 (algTyConRhs tc1) (algTyConRhs tc2)

  | otherwise = Just empty   -- two very different types -- should be obvious
  where
    roles1 = tyConRoles tc1
    roles2 = tyConRoles tc2
    roles_msg = text "The roles do not match." <+>
                (text "Roles default to" <+>
                 quotes (text "representational") <+> text "in boot files")

    eqAlgRhs tc (AbstractTyCon dis1) rhs2
      | dis1      = check (isDistinctAlgRhs rhs2)   --Check compatibility
                          (text "The natures of the declarations for" <+>
                           quotes (ppr tc) <+> text "are different")
      | otherwise = checkSuccess
    eqAlgRhs _  DataFamilyTyCon{} DataFamilyTyCon{} = checkSuccess
    eqAlgRhs _  tc1@DataTyCon{} tc2@DataTyCon{} =
        checkListBy eqCon (data_cons tc1) (data_cons tc2) (text "constructors")
    eqAlgRhs _  tc1@NewTyCon{} tc2@NewTyCon{} =
        eqCon (data_con tc1) (data_con tc2)
    eqAlgRhs _ _ _ = Just (text "Cannot match a" <+> quotes (text "data") <+>
                           text "definition with a" <+> quotes (text "newtype") <+>
                           text "definition")

    eqCon c1 c2
      =  check (name1 == name2)
               (text "The names" <+> pname1 <+> text "and" <+> pname2 <+>
                text "differ") `andThenCheck`
         check (dataConIsInfix c1 == dataConIsInfix c2)
               (text "The fixities of" <+> pname1 <+>
                text "differ") `andThenCheck`
         check (eqListBy eqHsBang (dataConImplBangs c1) (dataConImplBangs c2))
               (text "The strictness annotations for" <+> pname1 <+>
                text "differ") `andThenCheck`
         check (dataConFieldLabels c1 == dataConFieldLabels c2)
               (text "The record label lists for" <+> pname1 <+>
                text "differ") `andThenCheck`
         check (eqType (dataConUserType c1) (dataConUserType c2))
               (text "The types for" <+> pname1 <+> text "differ")
      where
        name1 = dataConName c1
        name2 = dataConName c2
        pname1 = quotes (ppr name1)
        pname2 = quotes (ppr name2)

    eqClosedFamilyAx (CoAxiom { co_ax_branches = branches1 })
                     (CoAxiom { co_ax_branches = branches2 })
      =  brListLength branches1 == brListLength branches2
      && (and $ brListZipWith eqClosedFamilyBranch branches1 branches2)

    eqClosedFamilyBranch (CoAxBranch { cab_tvs = tvs1, cab_lhs = lhs1, cab_rhs = rhs1 })
                         (CoAxBranch { cab_tvs = tvs2, cab_lhs = lhs2, cab_rhs = rhs2 })
      | Just env <- eqTyVarBndrs emptyRnEnv2 tvs1 tvs2
      = eqListBy (eqTypeX env) lhs1 lhs2 &&
        eqTypeX env rhs1 rhs2

      | otherwise = False

emptyRnEnv2 :: RnEnv2
emptyRnEnv2 = mkRnEnv2 emptyInScopeSet

----------------
missingBootThing :: Bool -> Name -> String -> SDoc
missingBootThing is_boot name what
  = ppr name <+> ptext (sLit "is exported by the") <+>
              (if is_boot then ptext (sLit "hs-boot") else ptext (sLit "hsig"))
              <+> ptext (sLit "file, but not")
              <+> text what <+> ptext (sLit "the module")

badReexportedBootThing :: DynFlags -> Bool -> Name -> Name -> SDoc
badReexportedBootThing dflags is_boot name name'
  = withPprStyle (mkUserStyle dflags alwaysQualify AllTheWay) $ vcat
        [ text "The" <+> (if is_boot then text "hs-boot" else text "hsig")
           <+> text "file (re)exports" <+> quotes (ppr name)
        , text "but the implementing module exports a different identifier" <+> quotes (ppr name')
        ]

bootMisMatch :: Bool -> SDoc -> TyThing -> TyThing -> SDoc
bootMisMatch is_boot extra_info real_thing boot_thing
  = vcat [ppr real_thing <+>
          ptext (sLit "has conflicting definitions in the module"),
          ptext (sLit "and its") <+>
            (if is_boot then ptext (sLit "hs-boot file")
                       else ptext (sLit "hsig file")),
          ptext (sLit "Main module:") <+> PprTyThing.pprTyThing real_thing,
          (if is_boot
            then ptext (sLit "Boot file:  ")
            else ptext (sLit "Hsig file: "))
            <+> PprTyThing.pprTyThing boot_thing,
          extra_info]

instMisMatch :: Bool -> ClsInst -> SDoc
instMisMatch is_boot inst
  = hang (ppr inst)
       2 (ptext (sLit "is defined in the") <+>
        (if is_boot then ptext (sLit "hs-boot") else ptext (sLit "hsig"))
       <+> ptext (sLit "file, but not in the module itself"))

{-
************************************************************************
*                                                                      *
        Type-checking the top level of a module
*                                                                      *
************************************************************************

tcRnGroup takes a bunch of top-level source-code declarations, and
 * renames them
 * gets supporting declarations from interface files
 * typechecks them
 * zonks them
 * and augments the TcGblEnv with the results

In Template Haskell it may be called repeatedly for each group of
declarations.  It expects there to be an incoming TcGblEnv in the
monad; it augments it and returns the new TcGblEnv.
-}

------------------------------------------------
rnTopSrcDecls :: HsGroup RdrName -> TcM (TcGblEnv, HsGroup Name)
-- Fails if there are any errors
rnTopSrcDecls group
 = do { -- Rename the source decls
        traceTc "rn12" empty ;
        (tcg_env, rn_decls) <- checkNoErrs $ rnSrcDecls group ;
        traceTc "rn13" empty ;

        -- save the renamed syntax, if we want it
        let { tcg_env'
                | Just grp <- tcg_rn_decls tcg_env
                  = tcg_env{ tcg_rn_decls = Just (appendGroups grp rn_decls) }
                | otherwise
                   = tcg_env };

                -- Dump trace of renaming part
        rnDump (ppr rn_decls) ;

        return (tcg_env', rn_decls)
   }

{-
************************************************************************
*                                                                      *
                tcTopSrcDecls
*                                                                      *
************************************************************************
-}

tcTopSrcDecls :: HsGroup Name -> TcM (TcGblEnv, TcLclEnv)
tcTopSrcDecls (HsGroup { hs_tyclds  = tycl_decls,
                         hs_instds  = inst_decls,
                         hs_derivds = deriv_decls,
                         hs_fords   = foreign_decls,
                         hs_defds   = default_decls,
                         hs_annds   = annotation_decls,
                         hs_ruleds  = rule_decls,
                         hs_vects   = vect_decls,
                         hs_valds   = val_binds })
 = do {         -- Type-check the type and class decls, and all imported decls
                -- The latter come in via tycl_decls
        traceTc "Tc2 (src)" empty ;

                -- Source-language instances, including derivings,
                -- and import the supporting declarations
        traceTc "Tc3" empty ;
        (tcg_env, inst_infos, deriv_binds)
            <- tcTyClsInstDecls tycl_decls inst_decls deriv_decls ;
        setGblEnv tcg_env       $ do {


                -- Generate Applicative/Monad proposal (AMP) warnings
        traceTc "Tc3b" empty ;

                -- Foreign import declarations next.
        traceTc "Tc4" empty ;
        (fi_ids, fi_decls, fi_gres) <- tcForeignImports foreign_decls ;
        tcExtendGlobalValEnv fi_ids     $ do {

                -- Default declarations
        traceTc "Tc4a" empty ;
        default_tys <- tcDefaults default_decls ;
        updGblEnv (\gbl -> gbl { tcg_default = default_tys }) $ do {
        -- Value declarations next
        traceTc "Tc5" empty ;
        tc_envs <- tcTopBinds val_binds;
        setEnvs tc_envs $ do {
        -- Now GHC-generated derived bindings, generics, and selectors
        -- Do not generate warnings from compiler-generated code;
        -- hence the use of discardWarnings
        tc_envs@(tcg_env, tcl_env) <- discardWarnings (tcTopBinds deriv_binds) ;
        setEnvs tc_envs $ do {  -- Environment doesn't change now

                -- Second pass over class and instance declarations,
                -- now using the kind-checked decls
        traceTc "Tc6" empty ;
        inst_binds <- tcInstDecls2 (tyClGroupConcat tycl_decls) inst_infos ;

                -- Foreign exports
        traceTc "Tc7" empty ;
        (foe_binds, foe_decls, foe_gres) <- tcForeignExports foreign_decls ;

                -- Annotations
        annotations <- tcAnnotations annotation_decls ;

                -- Rules
        rules <- tcRules rule_decls ;

                -- Vectorisation declarations
        vects <- tcVectDecls vect_decls ;

                -- Wrap up
        traceTc "Tc7a" empty ;
        let { all_binds = inst_binds     `unionBags`
                          foe_binds

            ; fo_gres = fi_gres `unionBags` foe_gres
            ; fo_fvs = foldrBag (\gre fvs -> fvs `addOneFV` gre_name gre)
                                emptyFVs fo_gres
            ; fo_rdr_names :: [RdrName]
            ; fo_rdr_names = foldrBag gre_to_rdr_name [] fo_gres

            ; sig_names = mkNameSet (collectHsValBinders val_binds)
                          `minusNameSet` getTypeSigNames val_binds

                -- Extend the GblEnv with the (as yet un-zonked)
                -- bindings, rules, foreign decls
            ; tcg_env' = tcg_env { tcg_binds   = tcg_binds tcg_env `unionBags` all_binds
                                 , tcg_sigs    = tcg_sigs tcg_env `unionNameSet` sig_names
                                 , tcg_rules   = tcg_rules tcg_env
                                                      ++ flattenRuleDecls rules
                                 , tcg_vects   = tcg_vects tcg_env ++ vects
                                 , tcg_anns    = tcg_anns tcg_env ++ annotations
                                 , tcg_ann_env = extendAnnEnvList (tcg_ann_env tcg_env) annotations
                                 , tcg_fords   = tcg_fords tcg_env ++ foe_decls ++ fi_decls
                                 , tcg_dus     = tcg_dus tcg_env `plusDU` usesOnly fo_fvs } } ;
                                 -- tcg_dus: see Note [Newtype constructor usage in foreign declarations]

        addUsedRdrNames fo_rdr_names ;
        return (tcg_env', tcl_env)
    }}}}}}
  where
    gre_to_rdr_name :: GlobalRdrElt -> [RdrName] -> [RdrName]
        -- For *imported* newtype data constructors, we want to
        -- make sure that at least one of the imports for them is used
        -- See Note [Newtype constructor usage in foreign declarations]
    gre_to_rdr_name gre rdrs
      = case gre_prov gre of
           LocalDef          -> rdrs
           Imported []       -> panic "gre_to_rdr_name: Imported []"
           Imported (is : _) -> mkRdrQual modName occName : rdrs
              where
                modName = is_as (is_decl is)
                occName = nameOccName (gre_name gre)

---------------------------
tcTyClsInstDecls :: [TyClGroup Name]
                 -> [LInstDecl Name]
                 -> [LDerivDecl Name]
                 -> TcM (TcGblEnv,            -- The full inst env
                         [InstInfo Name],     -- Source-code instance decls to process;
                                              -- contains all dfuns for this module
                          HsValBinds Name)    -- Supporting bindings for derived instances

tcTyClsInstDecls tycl_decls inst_decls deriv_decls
 = tcExtendKindEnv2 [ (con, APromotionErr FamDataConPE)
                    | lid <- inst_decls, con <- get_cons lid ] $
      -- Note [AFamDataCon: not promoting data family constructors]
   do { tcg_env <- tcTyAndClassDecls tycl_decls ;
      ; setGblEnv tcg_env $
        tcInstDecls1 (tyClGroupConcat tycl_decls) inst_decls deriv_decls }
  where
    -- get_cons extracts the *constructor* bindings of the declaration
    get_cons :: LInstDecl Name -> [Name]
    get_cons (L _ (TyFamInstD {}))                     = []
    get_cons (L _ (DataFamInstD { dfid_inst = fid }))  = get_fi_cons fid
    get_cons (L _ (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = fids } }))
      = concatMap (get_fi_cons . unLoc) fids

    get_fi_cons :: DataFamInstDecl Name -> [Name]
    get_fi_cons (DataFamInstDecl { dfid_defn = HsDataDefn { dd_cons = cons } })
      = map unLoc $ concatMap (con_names . unLoc) cons

{-
Note [AFamDataCon: not promoting data family constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data family T a
  data instance T Int = MkT
  data Proxy (a :: k)
  data S = MkS (Proxy 'MkT)

Is it ok to use the promoted data family instance constructor 'MkT' in
the data declaration for S?  No, we don't allow this. It *might* make
sense, but at least it would mean that we'd have to interleave
typechecking instances and data types, whereas at present we do data
types *then* instances.

So to check for this we put in the TcLclEnv a binding for all the family
constructors, bound to AFamDataCon, so that if we trip over 'MkT' when
type checking 'S' we'll produce a decent error message.


************************************************************************
*                                                                      *
        Checking for 'main'
*                                                                      *
************************************************************************
-}

checkMain :: Bool  -- False => no 'module M(..) where' header at all
          -> TcM TcGblEnv
-- If we are in module Main, check that 'main' is defined.
checkMain explicit_mod_hdr
 = do   { dflags  <- getDynFlags
        ; tcg_env <- getGblEnv
        ; check_main dflags tcg_env explicit_mod_hdr }

check_main :: DynFlags -> TcGblEnv -> Bool -> TcM TcGblEnv
check_main dflags tcg_env explicit_mod_hdr
 | mod /= main_mod
 = traceTc "checkMain not" (ppr main_mod <+> ppr mod) >>
   return tcg_env

 | otherwise
 = do   { mb_main <- lookupGlobalOccRn_maybe main_fn
                -- Check that 'main' is in scope
                -- It might be imported from another module!
        ; case mb_main of {
             Nothing -> do { traceTc "checkMain fail" (ppr main_mod <+> ppr main_fn)
                           ; complain_no_main
                           ; return tcg_env } ;
             Just main_name -> do

        { traceTc "checkMain found" (ppr main_mod <+> ppr main_fn)
        ; let loc = srcLocSpan (getSrcLoc main_name)
        ; ioTyCon <- tcLookupTyCon ioTyConName
        ; res_ty <- newFlexiTyVarTy liftedTypeKind
        ; main_expr
                <- addErrCtxt mainCtxt    $
                   tcMonoExpr (L loc (HsVar main_name)) (mkTyConApp ioTyCon [res_ty])

                -- See Note [Root-main Id]
                -- Construct the binding
                --      :Main.main :: IO res_ty = runMainIO res_ty main
        ; run_main_id <- tcLookupId runMainIOName
        ; let { root_main_name =  mkExternalName rootMainKey rOOT_MAIN
                                   (mkVarOccFS (fsLit "main"))
                                   (getSrcSpan main_name)
              ; root_main_id = Id.mkExportedLocalId VanillaId root_main_name
                                                    (mkTyConApp ioTyCon [res_ty])
              ; co  = mkWpTyApps [res_ty]
              ; rhs = nlHsApp (mkLHsWrap co (nlHsVar run_main_id)) main_expr
              ; main_bind = mkVarBind root_main_id rhs }

        ; return (tcg_env { tcg_main  = Just main_name,
                            tcg_binds = tcg_binds tcg_env
                                        `snocBag` main_bind,
                            tcg_dus   = tcg_dus tcg_env
                                        `plusDU` usesOnly (unitFV main_name)
                        -- Record the use of 'main', so that we don't
                        -- complain about it being defined but not used
                 })
    }}}
  where
    mod              = tcg_mod tcg_env
    main_mod         = mainModIs dflags
    main_fn          = getMainFun dflags
    interactive      = ghcLink dflags == LinkInMemory
    complain_no_main = checkTc (interactive && not explicit_mod_hdr) noMainMsg
        -- In interactive mode, without an explicit module header, don't
        -- worry about the absence of 'main'.
        -- In other modes, fail altogether, so that we don't go on
        -- and complain a second time when processing the export list.

    mainCtxt  = text "When checking the type of the" <+> pp_main_fn
    noMainMsg = text "The" <+> pp_main_fn
                <+> text "is not defined in module" <+> quotes (ppr main_mod)
    pp_main_fn = ppMainFn main_fn

-- | Get the unqualified name of the function to use as the \"main\" for the main module.
-- Either returns the default name or the one configured on the command line with -main-is
getMainFun :: DynFlags -> RdrName
getMainFun dflags = case mainFunIs dflags of
                      Just fn -> mkRdrUnqual (mkVarOccFS (mkFastString fn))
                      Nothing -> main_RDR_Unqual

checkMainExported :: TcGblEnv -> TcM ()
checkMainExported tcg_env
  = case tcg_main tcg_env of
      Nothing -> return () -- not the main module
      Just main_name ->
         do { dflags <- getDynFlags
            ; let main_mod = mainModIs dflags
            ; checkTc (main_name `elem` concatMap availNames (tcg_exports tcg_env)) $
                ptext (sLit "The") <+> ppMainFn (nameRdrName main_name) <+>
                ptext (sLit "is not exported by module") <+> quotes (ppr main_mod) }

ppMainFn :: RdrName -> SDoc
ppMainFn main_fn
  | rdrNameOcc main_fn == mainOcc
  = ptext (sLit "IO action") <+> quotes (ppr main_fn)
  | otherwise
  = ptext (sLit "main IO action") <+> quotes (ppr main_fn)

mainOcc :: OccName
mainOcc = mkVarOccFS (fsLit "main")

{-
Note [Root-main Id]
~~~~~~~~~~~~~~~~~~~
The function that the RTS invokes is always :Main.main, which we call
root_main_id.  (Because GHC allows the user to have a module not
called Main as the main module, we can't rely on the main function
being called "Main.main".  That's why root_main_id has a fixed module
":Main".)

This is unusual: it's a LocalId whose Name has a Module from another
module.  Tiresomely, we must filter it out again in MkIface, les we
get two defns for 'main' in the interface file!


*********************************************************
*                                                       *
                GHCi stuff
*                                                       *
*********************************************************
-}

runTcInteractive :: HscEnv -> TcRn a -> IO (Messages, Maybe a)
-- Initialise the tcg_inst_env with instances from all home modules.
-- This mimics the more selective call to hptInstances in tcRnImports
runTcInteractive hsc_env thing_inside
  = initTcInteractive hsc_env $ withTcPlugins hsc_env $
    do { traceTc "setInteractiveContext" $
            vcat [ text "ic_tythings:" <+> vcat (map ppr (ic_tythings icxt))
                 , text "ic_insts:" <+> vcat (map (pprBndr LetBind . instanceDFunId) ic_insts)
                 , text "ic_rn_gbl_env (LocalDef)" <+>
                      vcat (map ppr [ local_gres | gres <- occEnvElts (ic_rn_gbl_env icxt)
                                                 , let local_gres = filter isLocalGRE gres
                                                 , not (null local_gres) ]) ]
       ; let getOrphans m = fmap (\iface -> mi_module iface
                                          : dep_orphs (mi_deps iface))
                                 (loadSrcInterface (text "runTcInteractive") m
                                                   False Nothing)
       ; ic_visible_mods <- fmap concat . forM (ic_imports icxt) $ \i ->
            case i of
                IIModule n -> getOrphans n
                IIDecl i -> getOrphans (unLoc (ideclName i))
       ; gbl_env <- getGblEnv
       ; let gbl_env' = gbl_env {
                           tcg_rdr_env      = ic_rn_gbl_env icxt
                         , tcg_type_env     = type_env
                         , tcg_inst_env     = extendInstEnvList
                                               (extendInstEnvList (tcg_inst_env gbl_env) ic_insts)
                                               home_insts
                         , tcg_fam_inst_env = extendFamInstEnvList
                                               (extendFamInstEnvList (tcg_fam_inst_env gbl_env)
                                                                     ic_finsts)
                                               home_fam_insts
                         , tcg_field_env    = RecFields (mkNameEnv con_fields)
                                                        (mkNameSet (concatMap snd con_fields))
                              -- setting tcg_field_env is necessary
                              -- to make RecordWildCards work (test: ghci049)
                         , tcg_fix_env      = ic_fix_env icxt
                         , tcg_default      = ic_default icxt
                         , tcg_visible_orphan_mods = mkModuleSet ic_visible_mods
                              -- I guess there's a risk ic_imports will be
                              -- desynchronized with the true RdrEnv; probably
                              -- should insert some ASSERTs somehow.
                              -- TODO: Cache this
                         }

       ; setGblEnv gbl_env' $
         tcExtendGhciIdEnv ty_things $   -- See Note [Initialising the type environment for GHCi]
         thing_inside }                  -- in TcEnv
  where
    (home_insts, home_fam_insts) = hptInstances hsc_env (\_ -> True)

    icxt                  = hsc_IC hsc_env
    (ic_insts, ic_finsts) = ic_instances icxt
    ty_things             = ic_tythings icxt

    type_env1 = mkTypeEnvWithImplicits ty_things
    type_env  = extendTypeEnvWithIds type_env1 (map instanceDFunId ic_insts)
                -- Putting the dfuns in the type_env
                -- is just to keep Core Lint happy

    con_fields = [ (dataConName c, dataConFieldLabels c)
                 | ATyCon t <- ty_things
                 , c <- tyConDataCons t ]


#ifdef ETA_REPL
-- | The returned [Id] is the list of new Ids bound by this statement. It can
-- be used to extend the InteractiveContext via extendInteractiveContext.
--
-- The returned TypecheckedHsExpr is of type IO [ () ], a list of the bound
-- values, coerced to ().
tcRnStmt :: HscEnv -> GhciLStmt RdrName
         -> IO (Messages, Maybe (Either Reinterpret ([Id], LHsExpr Id, FixityEnv)))
tcRnStmt hsc_env rdr_stmt
  = runTcInteractive hsc_env $ do {

    -- The real work is done here
    (ePlan, fix_env) <- tcUserStmt rdr_stmt ;
    case ePlan of
      Right (bound_ids, tc_expr) -> do {
        zonked_expr <- zonkTopLExpr tc_expr ;
        zonked_ids  <- zonkTopBndrs bound_ids ;

            -- None of the Ids should be of unboxed type, because we
            -- cast them all to HValues in the end!
        mapM_ bad_unboxed (filter (isUnLiftedType . idType) zonked_ids) ;

        traceTc "tcs 1" empty ;
        let { global_ids = map globaliseAndTidyId zonked_ids } ;
            -- Note [Interactively-bound Ids in GHCi] in HscTypes

        {- ---------------------------------------------
        At one stage I removed any shadowed bindings from the type_env;
        they are inaccessible but might, I suppose, cause a space leak if we leave them there.
        However, with Template Haskell they aren't necessarily inaccessible.  Consider this
        GHCi session
                Prelude> let f n = n * 2 :: Int
                Prelude> fName <- runQ [| f |]
                Prelude> $(return $ AppE fName (LitE (IntegerL 7)))
                14
                Prelude> let f n = n * 3 :: Int
                Prelude> $(return $ AppE fName (LitE (IntegerL 7)))
        In the last line we use 'fName', which resolves to the *first* 'f'
        in scope. If we delete it from the type env, GHCi crashes because
        it doesn't expect that.

        Hence this code is commented out

        -------------------------------------------------- -}

        traceOptTcRn Opt_D_dump_tc
            (vcat [text "Bound Ids" <+> pprWithCommas ppr global_ids,
                text "Typechecked expr" <+> ppr zonked_expr]) ;

        return $ Right (global_ids, zonked_expr, fix_env) }
      Left reinterpret -> return $ Left reinterpret
    }
  where
    bad_unboxed id = addErr (sep [ptext (sLit "Eta REPL can't bind a variable of unlifted type:"),
                                  nest 2 (ppr id <+> dcolon <+> ppr (idType id))])

{-
--------------------------------------------------------------------------
                Typechecking Stmts in GHCi

Here is the grand plan, implemented in tcUserStmt

        What you type                   The IO [HValue] that hscStmt returns
        -------------                   ------------------------------------
        let pat = expr          ==>     let pat = expr in return [coerce HVal x, coerce HVal y, ...]
                                        bindings: [x,y,...]

        pat <- expr             ==>     expr >>= \ pat -> return [coerce HVal x, coerce HVal y, ...]
                                        bindings: [x,y,...]

        expr (of IO type)       ==>     expr >>= \ it -> return [coerce HVal it]
          [NB: result not printed]      bindings: [it]

        expr (of non-IO type,   ==>     let it = expr in print it >> return [coerce HVal it]
          result showable)              bindings: [it]

        expr (of non-IO type,
          result not showable)  ==>     error
-}

-- | A plan is an attempt to lift some code into the IO monad.
type PlanResult = Either Reinterpret ([Id], LHsExpr Id)
type Plan = TcM PlanResult

-- | Try the plans in order. If one fails (by raising an exn), try the next.
-- If one succeeds, take it.
runPlans :: [Plan] -> TcM PlanResult
runPlans []     = panic "runPlans"
runPlans [p]    = p
runPlans (p:ps) = tryTcLIE_ (runPlans ps) p

-- | Typecheck (and 'lift') a stmt entered by the user in GHCi into the
-- GHCi 'environment'.
--
-- By 'lift' and 'environment we mean that the code is changed to
-- execute properly in an IO monad. See Note [Interactively-bound Ids
-- in GHCi] in HscTypes for more details. We do this lifting by trying
-- different ways ('plans') of lifting the code into the IO monad and
-- type checking each plan until one succeeds.
tcUserStmt :: GhciLStmt RdrName -> TcM (PlanResult, FixityEnv)

-- An expression typed at the prompt is treated very specially
tcUserStmt (L loc (BodyStmt expr _ _ _))
  = do  { (rn_expr, fvs) <- checkNoErrs (rnLExpr expr)
               -- Don't try to typecheck if the renamer fails!
        ; ghciStep <- getGhciStepIO
        ; fresh_it <- getItName loc
        ; interPrintName <- getInteractivePrintName
        ; let matches   = [mkMatch [] rn_expr emptyLocalBinds]
              -- [it = expr]
              the_bind  = L loc $ (mkTopFunBind FromSource (L loc fresh_it) matches) { bind_fvs = fvs }
                          -- Care here!  In GHCi the expression might have
                          -- free variables, and they in turn may have free type variables
                          -- (if we are at a breakpoint, say).  We must put those free vars

              -- [let it = expr]
              let_stmt  = L loc $ LetStmt $ HsValBinds $
                          ValBindsOut [(NonRecursive,unitBag the_bind)] []

              -- [it <- e]
              bind_stmt = L loc $ BindStmt (L loc (VarPat fresh_it))
                                           (nlHsApp ghciStep rn_expr)
                                           (HsVar bindIOName) noSyntaxExpr

              -- [; print it]
              print_it print_name =
                L loc $ BodyStmt (nlHsApp (nlHsVar print_name) (nlHsVar fresh_it))
                                          (HsVar thenIOName) noSyntaxExpr placeHolderType

              plansWith print_name = [
                    -- Plan A
                    do { stuff@(Right ([it_id], _)) <- tcGhciStmts
                           [bind_stmt, print_it print_name]
                       ; it_ty <- zonkTcType (idType it_id)
                       ; when (isUnitTy $ it_ty) failM
                       ; return stuff },

                        -- Plan B; a naked bind statement
                    tcGhciStmts [bind_stmt],

                        -- Plan C; check that the let-binding is typeable all by itself.
                        -- If not, fail; if so, try to print it.
                        -- The two-step process avoids getting two errors: one from
                        -- the expression itself, and one from the 'print it' part
                        -- This two-step story is very clunky, alas
                    do { Right (ids, _) <- checkNoErrs (tcGhciStmts [let_stmt])
                                --- checkNoErrs defeats the error recovery of let-bindings
                       ; let cont = tcGhciStmts [let_stmt, print_it print_name]
                       ; case map idType ids of
                           [idTy]
                             | Just (tc, [ty]) <- splitTyConApp_maybe idTy
                             , getUnique tc == qTyConKey
                             -> case splitTyConApp_maybe ty of
                                 Just (tc1, [ty1])
                                   | getUnique tc1 == listTyConKey
                                   , Just (tc2, _) <- splitTyConApp_maybe ty1
                                   , getUnique tc2 == decTyConKey
                                   -> return (Left ReinterpretDecl)
                                 Just (tc1, [])
                                   | getUnique tc1 == decTyConKey
                                   -> return (Left ReinterpretAsWrappedDecl)
                                   | getUnique tc1 == expTyConKey
                                   -> return (Left ReinterpretSplice)
                                 Just _ -> return (Left ReinterpretRunQ)
                                 _ -> cont
                           _ -> cont

                       } ]

        -- The plans are:
        --   A. [it <- e; print it]     but not if it::()
        --   B. [it <- e]
        --   C. [let it = e; print it]
        --
        -- Ensure that type errors don't get deferred when type checking the
        -- naked expression. Deferring type errors here is unhelpful because the
        -- expression gets evaluated right away anyway. It also would potentially
        -- emit two redundant type-error warnings, one from each plan.
        ; plan <- unsetGOptM Opt_DeferTypeErrors $ runPlans $
                    plansWith interPrintName ++ plansWith printRawName

        ; fix_env <- getFixityEnv
        ; return (plan, fix_env) }

tcUserStmt rdr_stmt@(L loc _)
  = do { (([rn_stmt], fix_env), fvs) <- checkNoErrs $
           rnStmts GhciStmtCtxt rnLExpr [rdr_stmt] $ \_ -> do
             fix_env <- getFixityEnv
             return (fix_env, emptyFVs)
            -- Don't try to typecheck if the renamer fails!
       ; traceRn "tcRnStmt" (vcat [ppr rdr_stmt, ppr rn_stmt, ppr fvs])
       ; rnDump (ppr rn_stmt) ;

       ; ghciStep <- getGhciStepIO
       ; let gi_stmt
               | (L loc (BindStmt pat expr op1 op2)) <- rn_stmt
                           = L loc $ BindStmt pat (nlHsApp ghciStep expr) op1 op2
               | otherwise = rn_stmt

       ; opt_pr_flag <- goptM Opt_PrintBindResult
       ; let print_result_plan
               | opt_pr_flag                         -- The flag says "print result"
               , [v] <- collectLStmtBinders gi_stmt  -- One binder
                           =  [mk_print_result_plan gi_stmt v]
               | otherwise = []

        -- The plans are:
        --      [stmt; print v]         if one binder and not v::()
        --      [stmt]                  otherwise
       ; plan <- runPlans (print_result_plan ++ [tcGhciStmts [gi_stmt]])
       ; return (plan, fix_env) }
  where
    mk_print_result_plan stmt v
      = do { stuff@(Right ([v_id], _)) <- tcGhciStmts [stmt, print_v]
           ; v_ty <- zonkTcType (idType v_id)
           ; when (isUnitTy v_ty || not (isTauTy v_ty)) failM
           ; return stuff }
      where
        print_v  = L loc $ BodyStmt (nlHsApp (nlHsVar printName) (nlHsVar v))
                                    (HsVar thenIOName) noSyntaxExpr
                                    placeHolderType

{-
Note [GHCi Plans]

When a user types an expression in the repl we try to print it in three different
ways. Also, depending on whether -fno-it is set, we bind a variable called `it`
which can be used to refer to the result of the expression subsequently in the repl.

The normal plans are :
  A. [it <- e; print e]     but not if it::()
  B. [it <- e]
  C. [let it = e; print it]

When -fno-it is set, the plans are:
  A. [e >>= print]
  B. [e]
  C. [let it = e in print it]

The reason for -fno-it is explained in #14336. `it` can lead to the repl
leaking memory as it is repeatedly queried.
-}

-- | Typecheck the statements given and then return the results of the
-- statement in the form 'IO [()]'.
tcGhciStmts :: [GhciLStmt Name] -> TcM PlanResult
tcGhciStmts stmts
 = do { ioTyCon <- tcLookupTyCon ioTyConName ;
        ret_id  <- tcLookupId returnIOName ;            -- return @ IO
        let {
            ret_ty      = mkListTy unitTy ;
            io_ret_ty   = mkTyConApp ioTyCon [ret_ty] ;
            tc_io_stmts = tcStmtsAndThen GhciStmtCtxt tcDoStmt stmts io_ret_ty ;
            names = collectLStmtsBinders stmts ;
         } ;

        -- OK, we're ready to typecheck the stmts
        traceTc "TcRnDriver.tcGhciStmts: tc stmts" empty ;
        ((tc_stmts, ids), lie) <- captureTopConstraints $
                                  tc_io_stmts $ \ _ ->
                                  mapM tcLookupId names  ;
                        -- Look up the names right in the middle,
                        -- where they will all be in scope

        -- Simplify the context
        traceTc "TcRnDriver.tcGhciStmts: simplify ctxt" empty ;
        const_binds <- checkNoErrs (simplifyInteractive lie) ;
                -- checkNoErrs ensures that the plan fails if context redn fails

        traceTc "TcRnDriver.tcGhciStmts: done" empty ;
        let {   -- mk_return builds the expression
                --      returnIO @ [()] [coerce () x, ..,  coerce () z]
                --
                -- Despite the inconvenience of building the type applications etc,
                -- this *has* to be done in type-annotated post-typecheck form
                -- because we are going to return a list of *polymorphic* values
                -- coerced to type (). If we built a *source* stmt
                --      return [coerce x, ..., coerce z]
                -- then the type checker would instantiate x..z, and we wouldn't
                -- get their *polymorphic* values.  (And we'd get ambiguity errs
                -- if they were overloaded, since they aren't applied to anything.)
            ret_expr = nlHsApp (nlHsTyApp ret_id [ret_ty])
                       (noLoc $ ExplicitList unitTy Nothing (map mk_item ids)) ;
            mk_item id = nlHsApp (nlHsTyApp unsafeCoerceId [idType id, unitTy])
                                 (nlHsVar id) ;
            stmts = tc_stmts ++ [noLoc (mkLastStmt ret_expr)]
        } ;
        return $ Right (ids, mkHsDictLet (EvBinds const_binds) $
                               noLoc (HsDo GhciStmtCtxt stmts io_ret_ty))
    }

-- | Generate a typed ghciStepIO expression (ghciStep :: Ty a -> IO a)
getGhciStepIO :: TcM (LHsExpr Name)
getGhciStepIO = do
    ghciTy <- getGHCiMonad
    fresh_a <- newUnique
    let a_tv   = mkTcTyVarName fresh_a (fsLit "a")
        ghciM  = nlHsAppTy (nlHsTyVar ghciTy) (nlHsTyVar a_tv)
        ioM    = nlHsAppTy (nlHsTyVar ioTyConName) (nlHsTyVar a_tv)

        stepTy :: LHsType Name    -- Renamed, so needs all binders in place
        stepTy = noLoc $ HsForAllTy Implicit Nothing
                            (HsQTvs { hsq_tvs = [noLoc (UserTyVar a_tv)]
                                    , hsq_kvs = [] })
                            (noLoc [])
                            (nlHsFunTy ghciM ioM)
        step   = noLoc $ ExprWithTySig (nlHsVar ghciStepIoMName) stepTy []
    return step

isGHCiMonad :: HscEnv -> String -> IO (Messages, Maybe Name)
isGHCiMonad hsc_env ty
  = runTcInteractive hsc_env $ do
        rdrEnv <- getGlobalRdrEnv
        let occIO = lookupOccEnv rdrEnv (mkOccName tcName ty)
        case occIO of
            Just [n] -> do
                let name = gre_name n
                ghciClass <- tcLookupClass ghciIoClassName
                userTyCon <- tcLookupTyCon name
                let userTy = mkTyConApp userTyCon []
                _ <- tcLookupInstance ghciClass [userTy]
                return name

            Just _  -> failWithTc $ text "Ambiguous type!"
            Nothing -> failWithTc $ text ("Can't find type:" ++ ty)

-- tcRnExpr just finds the type of an expression

tcRnExpr :: HscEnv
         -> LHsExpr RdrName
         -> IO (Messages, Maybe Type)
-- Type checks the expression and returns its most general type
tcRnExpr hsc_env rdr_expr
  = runTcInteractive hsc_env $
    do {

    (rn_expr, _fvs) <- rnLExpr rdr_expr ;
    failIfErrsM ;

        -- Now typecheck the expression;
        -- it might have a rank-2 type (e.g. :t runST)
    fresh_it <- getItName (getLoc rdr_expr) ;
    (((_tc_expr, res_ty), tclvl), lie) <- captureConstraints $
                                          captureTcLevel     $
                                          tcInferRho rn_expr ;
    ((qtvs, dicts, _, _), lie_top) <- captureTopConstraints $
                                      {-# SCC "simplifyInfer" #-}
                                      simplifyInfer tclvl
                                                    False {- No MR for now -}
                                                    [(fresh_it, res_ty)]
                                                    lie ;
    -- Ignore the dictionary bindings
    _ <- simplifyInteractive lie_top ;

    let { all_expr_ty = mkForAllTys qtvs (mkPiTypes dicts res_ty) } ;
    ty <- zonkTcType all_expr_ty ;

    -- We normalise type families, so that the type of an expression is the
    -- same as of a bound expression (TcBinds.mkInferredPolyId). See Trac
    -- #10321 for further discussion.
    fam_envs <- tcGetFamInstEnvs ;
    -- normaliseType returns a coercion which we discard, so the Role is
    -- irrelevant
    return (snd (normaliseType fam_envs Nominal ty))
    }

--------------------------
tcRnImportDecls :: HscEnv
                -> [LImportDecl RdrName]
                -> IO (Messages, Maybe GlobalRdrEnv)
-- Find the new chunk of GlobalRdrEnv created by this list of import
-- decls.  In contract tcRnImports *extends* the TcGblEnv.
tcRnImportDecls hsc_env import_decls
 =  runTcInteractive hsc_env $
    do { gbl_env <- updGblEnv zap_rdr_env $ tcRnImports hsc_env import_decls
       ; return (tcg_rdr_env gbl_env) }
  where
    zap_rdr_env gbl_env = gbl_env { tcg_rdr_env = emptyGlobalRdrEnv }

-- tcRnType just finds the kind of a type

tcRnType :: HscEnv
         -> Bool        -- Normalise the returned type
         -> LHsType RdrName
         -> IO (Messages, Maybe (Type, Kind))
tcRnType hsc_env normalise rdr_type
  = runTcInteractive hsc_env $
    setXOptM LangExt.PolyKinds $   -- See Note [Kind-generalise in tcRnType]
    do { (wcs, rdr_type') <- extractWildcards rdr_type
       ; (rn_type, wcs)   <- bindLocatedLocalsRn wcs $ \wcs_new -> do {
       ; (rn_type, _fvs)  <- rnLHsType GHCiCtx rdr_type'
       ; failIfErrsM
       ; return (rn_type, wcs_new) }

        -- Now kind-check the type
        -- It can have any rank or kind
       ; nwc_tvs <- mapM newWildcardVarMetaKind wcs
       ; ty <- tcExtendTyVarEnv nwc_tvs $ tcHsSigType GhciCtxt rn_type

       ; ty' <- if normalise
                then do { fam_envs <- tcGetFamInstEnvs
                        ; return (snd (normaliseType fam_envs Nominal ty)) }
                        -- normaliseType returns a coercion
                        -- which we discard, so the Role is irrelevant
                else return ty ;

       ; return (ty', typeKind ty) }

{-
Note [Kind-generalise in tcRnType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We switch on PolyKinds when kind-checking a user type, so that we will
kind-generalise the type.  This gives the right default behaviour at
the GHCi prompt, where if you say ":k T", and T has a polymorphic
kind, you'd like to see that polymorphism. Of course.  If T isn't
kind-polymorphic you won't get anything unexpected, but the apparent
*loss* of polymorphism, for types that you know are polymorphic, is
quite surprising.  See Trac #7688 for a discussion.


************************************************************************
*                                                                      *
                 tcRnDeclsi
*                                                                      *
************************************************************************

tcRnDeclsi exists to allow class, data, and other declarations in GHCi.
-}

tcRnDeclsi :: HscEnv
           -> [LHsDecl RdrName]
           -> IO (Messages, Maybe TcGblEnv)

tcRnDeclsi hsc_env local_decls =
  runTcInteractive hsc_env $ do

    ((tcg_env, tclcl_env), lie) <- captureTopConstraints $
                                   tc_rn_src_decls local_decls
    setEnvs (tcg_env, tclcl_env) $ do

    new_ev_binds <- simplifyTop lie

    failIfErrsM
    let TcGblEnv { tcg_type_env  = type_env,
                   tcg_binds     = binds,
                   tcg_ev_binds  = cur_ev_binds,
                   tcg_imp_specs = imp_specs,
                   tcg_rules     = rules,
                   tcg_vects     = vects,
                   tcg_fords     = fords } = tcg_env
        all_ev_binds = cur_ev_binds `unionBags` new_ev_binds

    (bind_ids, ev_binds', binds', fords', imp_specs', rules', vects')
        <- zonkTopDecls all_ev_binds binds rules vects imp_specs fords

    let --global_ids = map globaliseAndTidyId bind_ids
        final_type_env = extendTypeEnvWithIds type_env bind_ids --global_ids
        tcg_env' = tcg_env { tcg_binds     = binds',
                             tcg_ev_binds  = ev_binds',
                             tcg_imp_specs = imp_specs',
                             tcg_rules     = rules',
                             tcg_vects     = vects',
                             tcg_fords     = fords' }

    setGlobalTypeEnv tcg_env' final_type_env

#endif /* GHCi */

{-
************************************************************************
*                                                                      *
        More GHCi stuff, to do with browsing and getting info
*                                                                      *
************************************************************************
-}

#ifdef ETA_REPL
-- | ASSUMES that the module is either in the 'HomePackageTable' or is
-- a package module with an interface on disk.  If neither of these is
-- true, then the result will be an error indicating the interface
-- could not be found.
getModuleInterface :: HscEnv -> Module -> IO (Messages, Maybe ModIface)
getModuleInterface hsc_env mod
  = runTcInteractive hsc_env $
    loadModuleInterface (ptext (sLit "getModuleInterface")) mod

tcRnLookupRdrName :: HscEnv -> Located RdrName
                  -> IO (Messages, Maybe [Name])
-- ^ Find all the Names that this RdrName could mean, in GHCi
tcRnLookupRdrName hsc_env (L loc rdr_name)
  = runTcInteractive hsc_env $
    setSrcSpan loc           $
    do {   -- If the identifier is a constructor (begins with an
           -- upper-case letter), then we need to consider both
           -- constructor and type class identifiers.
         let rdr_names = dataTcOccs rdr_name
       ; names_s <- mapM lookupInfoOccRn rdr_names
       ; let names = concat names_s
       ; when (null names) (addErrTc (ptext (sLit "Not in scope:") <+> quotes (ppr rdr_name)))
       ; return names }
#endif

tcRnLookupName :: HscEnv -> Name -> IO (Messages, Maybe TyThing)
tcRnLookupName hsc_env name
  = runTcInteractive hsc_env $
    tcRnLookupName' name

-- To look up a name we have to look in the local environment (tcl_lcl)
-- as well as the global environment, which is what tcLookup does.
-- But we also want a TyThing, so we have to convert:

tcRnLookupName' :: Name -> TcRn TyThing
tcRnLookupName' name = do
   tcthing <- tcLookup name
   case tcthing of
     AGlobal thing    -> return thing
     ATcId{tct_id=id} -> return (AnId id)
     _ -> panic "tcRnLookupName'"

tcRnGetInfo :: HscEnv
            -> Name
            -> IO (Messages, Maybe (TyThing, Fixity, [ClsInst], [FamInst], SDoc))

-- Used to implement :info in GHCi
--
-- Look up a RdrName and return all the TyThings it might be
-- A capitalised RdrName is given to us in the DataName namespace,
-- but we want to treat it as *both* a data constructor
--  *and* as a type or class constructor;
-- hence the call to dataTcOccs, and we return up to two results
tcRnGetInfo hsc_env name
  = runTcInteractive hsc_env $
    do { loadUnqualIfaces hsc_env (hsc_IC hsc_env)
           -- Load the interface for all unqualified types and classes
           -- That way we will find all the instance declarations
           -- (Packages have not orphan modules, and we assume that
           --  in the home package all relevant modules are loaded.)

       ; thing  <- tcRnLookupName' name
       ; fixity <- lookupFixityRn name
       ; (cls_insts, fam_insts) <- lookupInsts thing
       ; let info = lookupKnownNameInfo name
       ; return (thing, fixity, cls_insts, fam_insts, info) }

lookupInsts :: TyThing -> TcM ([ClsInst],[FamInst])
lookupInsts (ATyCon tc)
  = do  { InstEnvs { ie_global = pkg_ie, ie_local = home_ie, ie_visible = vis_mods } <- tcGetInstEnvs
        ; (pkg_fie, home_fie) <- tcGetFamInstEnvs
                -- Load all instances for all classes that are
                -- in the type environment (which are all the ones
                -- we've seen in any interface file so far)

          -- Return only the instances relevant to the given thing, i.e.
          -- the instances whose head contains the thing's name.
        ; let cls_insts =
                 [ ispec        -- Search all
                 | ispec <- instEnvElts home_ie ++ instEnvElts pkg_ie
                 , instIsVisible vis_mods ispec
                 , tc_name `elemNameSet` orphNamesOfClsInst ispec ]
        ; let fam_insts =
                 [ fispec
                 | fispec <- famInstEnvElts home_fie ++ famInstEnvElts pkg_fie
                 , tc_name `elemNameSet` orphNamesOfFamInst fispec ]
        ; return (cls_insts, fam_insts) }
  where
    tc_name     = tyConName tc

lookupInsts _ = return ([],[])

loadUnqualIfaces :: HscEnv -> InteractiveContext -> TcM ()
-- Load the interface for everything that is in scope unqualified
-- This is so that we can accurately report the instances for
-- something
loadUnqualIfaces hsc_env ictxt
  = initIfaceTcRn $ do
    mapM_ (loadSysInterface doc) (moduleSetElts (mkModuleSet unqual_mods))
  where
    this_pkg = thisPackage (hsc_dflags hsc_env)

    unqual_mods = [ nameModule name
                  | gre <- globalRdrEnvElts (ic_rn_gbl_env ictxt)
                  , let name = gre_name gre
                  , from_external_package name
                  , isTcOcc (nameOccName name)   -- Types and classes only
                  , unQualOK gre ]               -- In scope unqualified
    doc = ptext (sLit "Need interface for module whose export(s) are in scope unqualified")

    from_external_package name  -- True <=> the Name comes from some other package
                                --          (not the home package, not the interactive package)
      | Just mod <- nameModule_maybe name
      , moduleUnitId mod /= this_pkg    -- Not the home package
      , not (isInteractiveModule mod)       -- Not the 'interactive' package
      = True
      | otherwise
      = False


{-
************************************************************************
*                                                                      *
                Debugging output
*                                                                      *
************************************************************************
-}

rnDump :: SDoc -> TcRn ()
-- Dump, with a banner, if -ddump-rn
rnDump doc = do { traceOptTcRn Opt_D_dump_rn (mkDumpDoc "Renamer" doc) }

tcDump :: TcGblEnv -> TcRn ()
tcDump env
 = do { dflags <- getDynFlags ;

        -- Dump short output if -ddump-types or -ddump-tc
        when (dopt Opt_D_dump_types dflags || dopt Opt_D_dump_tc dflags)
             (printForUserTcRn short_dump) ;

        -- Dump bindings if -ddump-tc
        traceOptTcRn Opt_D_dump_tc (mkDumpDoc "Typechecker" full_dump)
   }
  where
    short_dump = pprTcGblEnv env
    full_dump  = pprLHsBinds (tcg_binds env)
        -- NB: foreign x-d's have undefined's in their types;
        --     hence can't show the tc_fords

-- It's unpleasant having both pprModGuts and pprModDetails here
pprTcGblEnv :: TcGblEnv -> SDoc
pprTcGblEnv (TcGblEnv { tcg_type_env  = type_env,
                        tcg_insts     = insts,
                        tcg_fam_insts = fam_insts,
                        tcg_rules     = rules,
                        tcg_vects     = vects,
                        tcg_imports   = imports })
  = vcat [ ppr_types insts type_env
         , ppr_tycons fam_insts type_env
         , ppr_insts insts
         , ppr_fam_insts fam_insts
         , vcat (map ppr rules)
         , vcat (map ppr vects)
         , ptext (sLit "Dependent modules:") <+>
                ppr (sortBy cmp_mp $ eltsUFM (imp_dep_mods imports))
         , ptext (sLit "Dependent packages:") <+>
                ppr (S.toList $ imp_dep_pkgs imports)]
  where         -- The two uses of sortBy are just to reduce unnecessary
                -- wobbling in testsuite output
    cmp_mp (mod_name1, is_boot1) (mod_name2, is_boot2)
        = (mod_name1 `stableModuleNameCmp` mod_name2)
                  `thenCmp`
          (is_boot1 `compare` is_boot2)

ppr_types :: [ClsInst] -> TypeEnv -> SDoc
ppr_types insts type_env
  = text "TYPE SIGNATURES" $$ nest 2 (ppr_sigs ids)
  where
    dfun_ids = map instanceDFunId insts
    ids = [id | id <- typeEnvIds type_env, want_sig id]
    want_sig id | opt_PprStyle_Debug = True
                | otherwise          = isLocalId id &&
                                       isExternalName (idName id) &&
                                       not (id `elem` dfun_ids)
        -- isLocalId ignores data constructors, records selectors etc.
        -- The isExternalName ignores local dictionary and method bindings
        -- that the type checker has invented.  Top-level user-defined things
        -- have External names.

ppr_tycons :: [FamInst] -> TypeEnv -> SDoc
ppr_tycons fam_insts type_env
  = vcat [ text "TYPE CONSTRUCTORS"
         ,   nest 2 (ppr_tydecls tycons)
         , text "COERCION AXIOMS"
         ,   nest 2 (vcat (map pprCoAxiom (typeEnvCoAxioms type_env))) ]
  where
    fi_tycons = famInstsRepTyCons fam_insts
    tycons = [tycon | tycon <- typeEnvTyCons type_env, want_tycon tycon]
    want_tycon tycon | opt_PprStyle_Debug = True
                     | otherwise          = not (isImplicitTyCon tycon) &&
                                            isExternalName (tyConName tycon) &&
                                            not (tycon `elem` fi_tycons)

ppr_insts :: [ClsInst] -> SDoc
ppr_insts []     = empty
ppr_insts ispecs = text "INSTANCES" $$ nest 2 (pprInstances ispecs)

ppr_fam_insts :: [FamInst] -> SDoc
ppr_fam_insts []        = empty
ppr_fam_insts fam_insts =
  text "FAMILY INSTANCES" $$ nest 2 (pprFamInsts fam_insts)

ppr_sigs :: [Var] -> SDoc
ppr_sigs ids
        -- Print type signatures; sort by OccName
  = vcat (map ppr_sig (sortBy (comparing getOccName) ids))
  where
    ppr_sig id = hang (ppr id <+> dcolon) 2 (ppr (tidyTopType (idType id)))

ppr_tydecls :: [TyCon] -> SDoc
ppr_tydecls tycons
        -- Print type constructor info; sort by OccName
  = vcat (map ppr_tycon (sortBy (comparing getOccName) tycons))
  where
    ppr_tycon tycon = vcat [ ppr (tyThingToIfaceDecl (ATyCon tycon)) ]

{-
********************************************************************************

Type Checker Plugins

********************************************************************************
-}

withTcPlugins :: HscEnv -> TcM a -> TcM a
withTcPlugins hsc_env m =
  do plugins <- liftIO (loadTcPlugins hsc_env)
     case plugins of
       [] -> m  -- Common fast case
       _  -> do (solvers,stops) <- unzip `fmap` mapM startPlugin plugins
                -- This ensures that tcPluginStop is called even if a type
                -- error occurs during compilation (Fix of #10078)
                eitherRes <- tryM $ do
                  updGblEnv (\e -> e { tcg_tc_plugins = solvers }) m
                mapM_ runTcPluginM stops
                case eitherRes of
                  Left _ -> failM
                  Right res -> return res
  where
  startPlugin (TcPlugin start solve stop) =
    do s <- runTcPluginM start
       return (solve s, stop s)

loadTcPlugins :: HscEnv -> IO [TcPlugin]
loadTcPlugins _ = return []
