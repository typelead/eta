{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


The Desugarer: turning HsSyn into Core.
-}

{-# LANGUAGE CPP #-}

module Eta.DeSugar.DeSugar (
-- * Desugaring operations
    deSugar, deSugarExpr
    ) where

#include "HsVersions.h"

import Eta.Main.DynFlags
import Eta.Main.HscTypes
import Eta.HsSyn.HsSyn
import Eta.DeSugar.DsUsage
import Eta.TypeCheck.TcRnTypes
import Eta.TypeCheck.TcRnMonad ( finalSafeMode )
import Eta.BasicTypes.Id
import Eta.BasicTypes.Name
import Eta.Types.Type
import Eta.Types.FamInstEnv
import Eta.Types.Coercion
import Eta.Types.InstEnv
import Eta.Types.Class
import Eta.BasicTypes.Avail
import Eta.Core.CoreSyn
import Eta.Core.CoreSubst
import Eta.Core.PprCore
import Eta.DeSugar.DsMonad
import Eta.DeSugar.DsExpr
import Eta.DeSugar.DsBinds
import Eta.DeSugar.DsForeign
import Eta.BasicTypes.Module
import Eta.BasicTypes.NameSet
import Eta.BasicTypes.NameEnv
import Eta.Specialise.Rules
import Eta.Prelude.TysPrim (eqReprPrimTyCon)
import Eta.Prelude.TysWiredIn (coercibleTyCon )
import Eta.BasicTypes.BasicTypes       ( Activation(.. ) )
import Eta.SimplCore.CoreMonad        ( CoreToDo(..) )
import Eta.Core.CoreLint         ( endPassIO )
import Eta.Core.MkCore
import Eta.Utils.FastString
import Eta.Main.ErrUtils
import Eta.Utils.Outputable
import Eta.BasicTypes.SrcLoc
import Eta.DeSugar.Coverage
import Eta.Utils.Util
import Eta.Utils.MonadUtils
import Eta.Utils.OrdList
import Eta.DeSugar.StaticPtrTable
import Data.List
import Data.IORef
import Control.Monad( when )

{-
************************************************************************
*                                                                      *
*              The main function: deSugar
*                                                                      *
************************************************************************
-}

-- | Main entry point to the desugarer.
deSugar :: HscEnv -> ModLocation -> TcGblEnv -> IO (Messages, Maybe ModGuts)
-- Can modify PCS by faulting in more declarations

deSugar hsc_env
        mod_loc
        tcg_env@(TcGblEnv { tcg_mod          = _id_mod,
                            tcg_semantic_mod = mod,
                            tcg_src          = hsc_src,
                            tcg_type_env     = type_env,
                            tcg_imports      = imports,
                            tcg_exports      = exports,
                            tcg_keep         = keep_var,
                            tcg_th_splice_used = tc_splice_used,
                            tcg_rdr_env      = rdr_env,
                            tcg_fix_env      = fix_env,
                            tcg_inst_env     = inst_env,
                            tcg_fam_inst_env = fam_inst_env,
                            tcg_merged = merged,
                            tcg_warns        = warns,
                            tcg_anns         = anns,
                            tcg_binds        = binds,
                            tcg_imp_specs    = imp_specs,
                            tcg_dependent_files = dependent_files,
                            tcg_ev_binds     = ev_binds,
                            tcg_fords        = fords,
                            tcg_rules        = rules,
                            tcg_vects        = vects,
                            tcg_patsyns      = patsyns,
                            tcg_tcs          = tcs,
                            tcg_insts        = insts,
                            tcg_fam_insts    = fam_insts,
                            tcg_hpc          = other_hpc_info})

  = do { let dflags = hsc_dflags hsc_env
             print_unqual = mkPrintUnqualified dflags rdr_env
        ; showPass dflags "Desugar"

        -- Desugar the program
        ; let export_set = availsToNameSet exports
              target     = hscTarget dflags
              hpcInfo    = emptyHpcInfo other_hpc_info

        ; (binds_cvr, ds_hpc_info, modBreaks)
                         <- if not (isHsBootOrSig hsc_src)
                              then addTicksToBinds dflags mod mod_loc export_set
                                          (typeEnvTyCons type_env) binds
                              else return (binds, hpcInfo, emptyModBreaks)

        ; (msgs, mb_res) <- initDs hsc_env mod rdr_env type_env fam_inst_env $
                       do { ds_ev_binds <- dsEvBinds ev_binds
                          ; core_prs <- dsTopLHsBinds binds_cvr
                          ; (spec_prs, spec_rules) <- dsImpSpecs imp_specs
                          ; (ds_fords, foreign_prs) <- dsForeigns fords
                          ; ds_rules <- mapMaybeM dsRule rules
                          ; ds_vects <- mapM dsVect vects
                          ; stBinds <- dsGetStaticBindsVar >>=
                                           liftIO . readIORef
                          ; let _hpc_init
                                  | gopt Opt_Hpc dflags = hpcInitCode mod ds_hpc_info
                                  | otherwise = empty
                                -- Stub to insert the static entries of the
                                -- module into the static pointer table
                                _spt_init = sptInitCode mod stBinds
                          ; return ( ds_ev_binds
                                   , foreign_prs `appOL` core_prs `appOL` spec_prs
                                                 `appOL` toOL (map snd stBinds)
                                   , spec_rules ++ ds_rules, ds_vects
                                   , ds_fords --`appendStubC` hpc_init
                                              --`appendStubC` spt_init
                                   ) }

        ; case mb_res of {
           Nothing -> return (msgs, Nothing) ;
           Just (ds_ev_binds, all_prs, all_rules, vects0, ds_fords) -> do

     do {       -- Add export flags to bindings
          keep_alive <- readIORef keep_var
        ; let (rules_for_locals, rules_for_imps) = partition isLocalRule all_rules
              final_prs = addExportFlagsAndRules target export_set keep_alive
                                                 rules_for_locals (fromOL all_prs)

              final_pgm = combineEvBinds ds_ev_binds final_prs
        -- Notice that we put the whole lot in a big Rec, even the foreign binds
        -- When compiling PrelFloat, which defines data Float = F# Float#
        -- we want F# to be in scope in the foreign marshalling code!
        -- You might think it doesn't matter, but the simplifier brings all top-level
        -- things into the in-scope set before simplifying; so we get no unfolding for F#!

#ifdef DEBUG
          -- Debug only as pre-simple-optimisation program may be really big
        ; endPassIO hsc_env print_unqual CoreDesugar final_pgm rules_for_imps
#endif
        ; (ds_binds, ds_rules_for_imps, ds_vects)
            <- simpleOptPgm dflags mod final_pgm rules_for_imps vects0
                         -- The simpleOptPgm gets rid of type
                         -- bindings plus any stupid dead code

        ; endPassIO hsc_env print_unqual CoreDesugarOpt ds_binds ds_rules_for_imps

        ; let used_names = mkUsedNames tcg_env
              pluginModules = []
                -- map lpModule (plugins (hsc_dflags hsc_env))
        ; deps <- mkDependencies (thisInstalledUnitId (hsc_dflags hsc_env))
                                 pluginModules tcg_env

        ; used_th <- readIORef tc_splice_used
        ; dep_files <- readIORef dependent_files
        ; safe_mode <- finalSafeMode dflags tcg_env
        ; usages <- mkUsageInfo hsc_env mod (imp_mods imports) used_names dep_files merged
       -- id_mod /= mod when we are processing an hsig, but hsigs
       -- never desugared and compiled (there's no code!)
       -- ; MASSERT ( id_mod == mod )
        ; let mod_guts = ModGuts {
                mg_module       = mod,
                mg_hsc_src      = hsc_src,
                mg_exports      = exports,
                mg_usages       = usages,
                mg_deps         = deps,
                mg_used_th      = used_th,
                mg_rdr_env      = rdr_env,
                mg_fix_env      = fix_env,
                mg_warns        = warns,
                mg_anns         = anns,
                mg_tcs          = tcs,
                mg_insts        = insts,
                mg_fam_insts    = fam_insts,
                mg_inst_env     = inst_env,
                mg_fam_inst_env = fam_inst_env,
                mg_patsyns      = patsyns,
                mg_rules        = ds_rules_for_imps,
                mg_binds        = ds_binds,
                mg_foreign      = ds_fords,
                mg_hpc_info     = ds_hpc_info,
                mg_modBreaks    = modBreaks,
                mg_vect_decls   = ds_vects,
                mg_vect_info    = noVectInfo,
                mg_safe_haskell = safe_mode,
                mg_trust_pkg    = imp_trust_own_pkg imports
              }
        ; return (msgs, Just mod_guts)
        }}}

dsImpSpecs :: [LTcSpecPrag] -> DsM (OrdList (Id,CoreExpr), [CoreRule])
dsImpSpecs imp_specs
 = do { spec_prs <- mapMaybeM (dsSpec Nothing) imp_specs
      ; let (spec_binds, spec_rules) = unzip spec_prs
      ; return (concatOL spec_binds, spec_rules) }

combineEvBinds :: [CoreBind] -> [(Id,CoreExpr)] -> [CoreBind]
-- Top-level bindings can include coercion bindings, but not via superclasses
-- See Note [Top-level evidence]
combineEvBinds [] val_prs
  = [Rec val_prs]
combineEvBinds (NonRec b r : bs) val_prs
  | isId b    = combineEvBinds bs ((b,r):val_prs)
  | otherwise = NonRec b r : combineEvBinds bs val_prs
combineEvBinds (Rec prs : bs) val_prs
  = combineEvBinds bs (prs ++ val_prs)

{-
Note [Top-level evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~
Top-level evidence bindings may be mutually recursive with the top-level value
bindings, so we must put those in a Rec.  But we can't put them *all* in a Rec
because the occurrence analyser doesn't teke account of type/coercion variables
when computing dependencies.

So we pull out the type/coercion variables (which are in dependency order),
and Rec the rest.
-}

deSugarExpr :: HscEnv -> LHsExpr Id -> IO (Messages, Maybe CoreExpr)

deSugarExpr hsc_env tc_expr
  = do { let dflags       = hsc_dflags hsc_env
             icntxt       = hsc_IC hsc_env
             rdr_env      = ic_rn_gbl_env icntxt
             type_env     = mkTypeEnvWithImplicits (ic_tythings icntxt)
             fam_insts    = snd (ic_instances icntxt)
             fam_inst_env = extendFamInstEnvList emptyFamInstEnv fam_insts
             -- This stuff is a half baked version of TcRnDriver.setInteractiveContext

       ; showPass dflags "Desugar"

         -- Do desugaring
       ; (msgs, mb_core_expr) <- initDs hsc_env (icInteractiveModule icntxt) rdr_env
                                        type_env fam_inst_env $
                                 dsLExpr tc_expr

       ; case mb_core_expr of
            Nothing   -> return ()
            Just expr -> dumpIfSet_dyn dflags Opt_D_dump_ds "Desugared" (pprCoreExpr expr)

       ; return (msgs, mb_core_expr) }

{-
************************************************************************
*                                                                      *
*              Add rules and export flags to binders
*                                                                      *
************************************************************************
-}

addExportFlagsAndRules
    :: HscTarget -> NameSet -> NameSet -> [CoreRule]
    -> [(Id, t)] -> [(Id, t)]
addExportFlagsAndRules target exports keep_alive rules prs
  = mapFst add_one prs
  where
    add_one bndr = add_rules name (add_export name bndr)
       where
         name = idName bndr

    ---------- Rules --------
        -- See Note [Attach rules to local ids]
        -- NB: the binder might have some existing rules,
        -- arising from specialisation pragmas
    add_rules name bndr
        | Just rules <- lookupNameEnv rule_base name
        = bndr `addIdSpecialisations` rules
        | otherwise
        = bndr
    rule_base = extendRuleBaseList emptyRuleBase rules

    ---------- Export flag --------
    -- See Note [Adding export flags]
    add_export name bndr
        | dont_discard name = setIdExported bndr
        | otherwise         = bndr

    dont_discard :: Name -> Bool
    dont_discard name = is_exported name
                     || name `elemNameSet` keep_alive

        -- In interactive mode, we don't want to discard any top-level
        -- entities at all (eg. do not inline them away during
        -- simplification), and retain them all in the TypeEnv so they are
        -- available from the command line.
        --
        -- isExternalName separates the user-defined top-level names from those
        -- introduced by the type checker.
    is_exported :: Name -> Bool
    is_exported | targetRetainsAllBindings target = isExternalName
                | otherwise                       = (`elemNameSet` exports)

{-
Note [Adding export flags]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Set the no-discard flag if either
        a) the Id is exported
        b) it's mentioned in the RHS of an orphan rule
        c) it's in the keep-alive set

It means that the binding won't be discarded EVEN if the binding
ends up being trivial (v = w) -- the simplifier would usually just
substitute w for v throughout, but we don't apply the substitution to
the rules (maybe we should?), so this substitution would make the rule
bogus.

You might wonder why exported Ids aren't already marked as such;
it's just because the type checker is rather busy already and
I didn't want to pass in yet another mapping.

Note [Attach rules to local ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Find the rules for locally-defined Ids; then we can attach them
to the binders in the top-level bindings

Reason
  - It makes the rules easier to look up
  - It means that transformation rules and specialisations for
    locally defined Ids are handled uniformly
  - It keeps alive things that are referred to only from a rule
    (the occurrence analyser knows about rules attached to Ids)
  - It makes sure that, when we apply a rule, the free vars
    of the RHS are more likely to be in scope
  - The imported rules are carried in the in-scope set
    which is extended on each iteration by the new wave of
    local binders; any rules which aren't on the binding will
    thereby get dropped


************************************************************************
*                                                                      *
*              Desugaring transformation rules
*                                                                      *
************************************************************************
-}

dsRule :: LRuleDecl Id -> DsM (Maybe CoreRule)
dsRule (L loc (HsRule name act vars lhs _tv_lhs rhs _fv_rhs))
  = putSrcSpanDs loc $
    do  { let bndrs' = [var | L _ (RuleBndr (L _ var)) <- vars]

        ; lhs' <- unsetGOptM Opt_EnableRewriteRules $
                  unsetWOptM Opt_WarnIdentities $
                  dsLExpr lhs   -- Note [Desugaring RULE left hand sides]

        ; rhs' <- dsLExpr rhs
        ; dflags <- getDynFlags

        ; (bndrs'', lhs'', rhs'') <- unfold_coerce bndrs' lhs' rhs'

        -- Substitute the dict bindings eagerly,
        -- and take the body apart into a (f args) form
        ; case decomposeRuleLhs bndrs'' lhs'' of {
                Left msg -> do { warnDs NoReason msg; return Nothing } ;
                Right (final_bndrs, fn_id, args) -> do

        { let is_local = isLocalId fn_id
                -- NB: isLocalId is False of implicit Ids.  This is good because
                -- we don't want to attach rules to the bindings of implicit Ids,
                -- because they don't show up in the bindings until just before code gen
              fn_name   = idName fn_id
              final_rhs = simpleOptExpr rhs''    -- De-crap it
              rule      = mkRule False {- Not auto -} is_local
                                 (unLoc name) act fn_name final_bndrs args
                                 final_rhs

              inline_shadows_rule   -- Function can be inlined before rule fires
                | wopt Opt_WarnInlineRuleShadowing dflags
                , isLocalId fn_id || hasSomeUnfolding (idUnfolding fn_id)
                       -- If imported with no unfolding, no worries
                = case (idInlineActivation fn_id, act) of
                    (NeverActive, _)    -> False
                    (AlwaysActive, _)   -> True
                    (ActiveBefore {}, _) -> True
                    (ActiveAfter {}, NeverActive)     -> True
                    (ActiveAfter n, ActiveAfter r)    -> r < n  -- Rule active strictly first
                    (ActiveAfter {}, AlwaysActive)    -> False
                    (ActiveAfter {}, ActiveBefore {}) -> False
                | otherwise = False

        ; when inline_shadows_rule $
          warnDs (Reason Opt_WarnInlineRuleShadowing)
                 (vcat [ hang (ptext (sLit "Rule")
                               <+> doubleQuotes (ftext $ unLoc name)
                               <+> ptext (sLit "may never fire"))
                            2 (ptext (sLit "because") <+> quotes (ppr fn_id)
                               <+> ptext (sLit "might inline first"))
                       , ptext (sLit "Probable fix: add an INLINE[n] or NOINLINE[n] pragma on")
                         <+> quotes (ppr fn_id) ])

        ; return (Just rule)
        } } }

-- See Note [Desugaring coerce as cast]
unfold_coerce :: [Id] -> CoreExpr -> CoreExpr -> DsM ([Var], CoreExpr, CoreExpr)
unfold_coerce bndrs lhs rhs = do
    (bndrs', wrap) <- go bndrs
    return (bndrs', wrap lhs, wrap rhs)
  where
    go :: [Id] -> DsM ([Id], CoreExpr -> CoreExpr)
    go []     = return ([], id)
    go (v:vs)
        | Just (tc, args) <- splitTyConApp_maybe (idType v)
        , tc == coercibleTyCon = do
            let ty' = mkTyConApp eqReprPrimTyCon args
            v' <- mkDerivedLocalM mkRepEqOcc v ty'

            (bndrs, wrap) <- go vs
            return (v':bndrs, mkCoreLet (NonRec v (mkEqBox (mkCoVarCo v'))) . wrap)
        | otherwise = do
            (bndrs,wrap) <- go vs
            return (v:bndrs, wrap)

{-
Note [Desugaring RULE left hand sides]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For the LHS of a RULE we do *not* want to desugar
    [x]   to    build (\cn. x `c` n)
We want to leave explicit lists simply as chains
of cons's. We can achieve that slightly indirectly by
switching off EnableRewriteRules.  See DsExpr.dsExplicitList.

That keeps the desugaring of list comprehensions simple too.



Nor do we want to warn of conversion identities on the LHS;
the rule is precisely to optimise them:
  {-# RULES "fromRational/id" fromRational = id :: Rational -> Rational #-}


Note [Desugaring coerce as cast]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want the user to express a rule saying roughly “mapping a coercion over a
list can be replaced by a coercion”. But the cast operator of Core (▷) cannot
be written in Haskell. So we use `coerce` for that (#2110). The user writes
    map coerce = coerce
as a RULE, and this optimizes any kind of mapped' casts aways, including `map
MkNewtype`.

For that we replace any forall'ed `c :: Coercible a b` value in a RULE by
corresponding `co :: a ~#R b` and wrap the LHS and the RHS in
`let c = MkCoercible co in ...`. This is later simplified to the desired form
by simpleOptExpr (for the LHS) resp. the simplifiers (for the RHS).

************************************************************************
*                                                                      *
*              Desugaring vectorisation declarations
*                                                                      *
************************************************************************
-}

dsVect :: LVectDecl Id -> DsM CoreVect
dsVect (L loc (HsVect _ (L _ v) rhs))
  = putSrcSpanDs loc $
    do { rhs' <- dsLExpr rhs
       ; return $ Vect v rhs'
       }
dsVect (L _loc (HsNoVect _ (L _ v)))
  = return $ NoVect v
dsVect (L _loc (HsVectTypeOut isScalar tycon rhs_tycon))
  = return $ VectType isScalar tycon' rhs_tycon
  where
    tycon' | Just ty <- coreView $ mkTyConTy tycon
           , (tycon', []) <- splitTyConApp ty      = tycon'
           | otherwise                             = tycon
dsVect vd@(L _ (HsVectTypeIn _ _ _ _))
  = pprPanic "Desugar.dsVect: unexpected 'HsVectTypeIn'" (ppr vd)
dsVect (L _loc (HsVectClassOut cls))
  = return $ VectClass (classTyCon cls)
dsVect vc@(L _ (HsVectClassIn _ _))
  = pprPanic "Desugar.dsVect: unexpected 'HsVectClassIn'" (ppr vc)
dsVect (L _loc (HsVectInstOut inst))
  = return $ VectInst (instanceDFunId inst)
dsVect vi@(L _ (HsVectInstIn _))
  = pprPanic "Desugar.dsVect: unexpected 'HsVectInstIn'" (ppr vi)
