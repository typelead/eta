{-# LANGUAGE ScopedTypeVariables, CPP #-}

module Eta.TypeCheck.TcErrors(
       reportUnsolved, reportAllUnsolved,
       warnDefaulting,

       solverDepthErrorTcS
  ) where

import Eta.TypeCheck.TcRnTypes
import Eta.TypeCheck.TcRnMonad
import Eta.TypeCheck.TcMType
import Eta.TypeCheck.TcType
import Eta.Types.TypeRep
import Eta.Types.Type
import Eta.Types.Kind ( isKind )
import Eta.Types.Unify            ( tcMatchTys )
import Eta.BasicTypes.Module
import Eta.TypeCheck.FamInst
import Eta.TypeCheck.Inst
import Eta.Types.InstEnv
import Eta.Types.TyCon
import Eta.BasicTypes.DataCon
import Eta.TypeCheck.TcEvidence
import qualified Eta.LanguageExtensions as LangExt
import Eta.BasicTypes.Name
import Eta.BasicTypes.RdrName          ( lookupGRE_Name, GlobalRdrEnv )
import Eta.BasicTypes.Id
import Eta.BasicTypes.Var
import Eta.BasicTypes.VarSet
import Eta.BasicTypes.VarEnv
import Eta.BasicTypes.NameEnv
import Eta.Utils.Bag
import Eta.Main.ErrUtils         ( ErrMsg, makeIntoWarning, pprLocErrMsg, isWarning )
import Eta.BasicTypes.BasicTypes
import Eta.Utils.Util
import Eta.Utils.FastString
import Eta.Utils.Outputable
import Eta.BasicTypes.SrcLoc
import Eta.Main.DynFlags
import Eta.Main.StaticFlags      ( opt_PprStyle_Debug )
import Eta.Utils.ListSetOps       ( equivClasses )

import Control.Monad    ( when )
import Data.Maybe
import Data.List        ( partition, mapAccumL, nub, sortBy )

#include "HsVersions.h"

{-
************************************************************************
*                                                                      *
\section{Errors and contexts}
*                                                                      *
************************************************************************

ToDo: for these error messages, should we note the location as coming
from the insts, or just whatever seems to be around in the monad just
now?

Note [Deferring coercion errors to runtime]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While developing, sometimes it is desirable to allow compilation to succeed even
if there are type errors in the code. Consider the following case:

  module Main where

  a :: Int
  a = 'a'

  main = print "b"

Even though `a` is ill-typed, it is not used in the end, so if all that we're
interested in is `main` it is handy to be able to ignore the problems in `a`.

Since we treat type equalities as evidence, this is relatively simple. Whenever
we run into a type mismatch in TcUnify, we normally just emit an error. But it
is always safe to defer the mismatch to the main constraint solver. If we do
that, `a` will get transformed into

  co :: Int ~ Char
  co = ...

  a :: Int
  a = 'a' `cast` co

The constraint solver would realize that `co` is an insoluble constraint, and
emit an error with `reportUnsolved`. But we can also replace the right-hand side
of `co` with `error "Deferred type error: Int ~ Char"`. This allows the program
to compile, and it will run fine unless we evaluate `a`. This is what
`deferErrorsToRuntime` does.

It does this by keeping track of which errors correspond to which coercion
in TcErrors. TcErrors.reportTidyWanteds does not print the errors
and does not fail if -fdefer-type-errors is on, so that we can continue
compilation. The errors are turned into warnings in `reportUnsolved`.
-}

reportUnsolved :: WantedConstraints -> TcM (Bag EvBind)
reportUnsolved wanted
  = do { binds_var <- newTcEvBinds
       ; defer_errors <- goptM Opt_DeferTypeErrors
       ; defer_holes <- goptM Opt_DeferTypedHoles
       ; warn_holes <- woptM Opt_WarnTypedHoles
       ; warn_partial_sigs <- woptM Opt_WarnPartialTypeSignatures
       ; report_unsolved (Just binds_var) defer_errors defer_holes
             warn_holes warn_partial_sigs wanted
       ; getTcEvBinds binds_var }

reportAllUnsolved :: WantedConstraints -> TcM ()
-- Report all unsolved goals, even if -fdefer-type-errors is on
-- See Note [Deferring coercion errors to runtime]
reportAllUnsolved wanted = do
    warn_holes <- woptM Opt_WarnTypedHoles
    warn_partial_sigs <- woptM Opt_WarnPartialTypeSignatures
    report_unsolved Nothing False False warn_holes warn_partial_sigs wanted

report_unsolved :: Maybe EvBindsVar  -- cec_binds
                -> Bool              -- cec_defer_type_errors
                -> Bool              -- cec_defer_holes
                -> Bool              -- cec_warn_holes
                -> Bool              -- cec_warn_partial_type_signatures
                -> WantedConstraints -> TcM ()
-- Important precondition:
-- WantedConstraints are fully zonked and unflattened, that is,
-- zonkWC has already been applied to these constraints.
report_unsolved mb_binds_var defer_errors defer_holes warn_holes
                warn_partial_sigs wanted
  | isEmptyWC wanted
  = return ()
  | otherwise
  = do { traceTc "reportUnsolved (before unflattening)" (ppr wanted)

       ; env0 <- tcInitTidyEnv

            -- If we are deferring we are going to need /all/ evidence around,
            -- including the evidence produced by unflattening (zonkWC)
       ; let tidy_env = tidyFreeTyVars env0 free_tvs
             free_tvs = tyVarsOfWC wanted
             err_ctxt = CEC { cec_encl  = []
                            , cec_tidy  = tidy_env
                            , cec_defer_type_errors = defer_errors
                            , cec_defer_holes = defer_holes
                            , cec_warn_holes = warn_holes
                            , cec_warn_partial_type_signatures = warn_partial_sigs
                            , cec_suppress = False -- See Note [Suppressing error messages]
                            , cec_binds    = mb_binds_var }

       ; traceTc "reportUnsolved (after unflattening):" $
         vcat [ pprTvBndrs (varSetElems free_tvs)
              , ppr wanted ]

       ; reportWanteds err_ctxt wanted }

--------------------------------------------
--      Internal functions
--------------------------------------------

data ReportErrCtxt
    = CEC { cec_encl :: [Implication]  -- Enclosing implications
                                       --   (innermost first)
                                       -- ic_skols and givens are tidied, rest are not
          , cec_tidy  :: TidyEnv
          , cec_binds :: Maybe EvBindsVar
                         -- Nothing <=> Report all errors, including holes; no bindings
                         -- Just ev  <=> make some errors (depending on cec_defer)
                         --              into warnings, and emit evidence bindings
                         --              into 'ev' for unsolved constraints

          , cec_defer_type_errors :: Bool -- True <=> -fdefer-type-errors
                                          -- Defer type errors until runtime
                                          -- Irrelevant if cec_binds = Nothing

          , cec_defer_holes :: Bool     -- True <=> -fdefer-typed-holes
                                        -- Turn typed holes into runtime errors
                                        -- Irrelevant if cec_binds = Nothing

          , cec_warn_holes :: Bool  -- True <=> -fwarn-typed-holes
                                    -- Controls whether typed holes produce warnings
          , cec_warn_partial_type_signatures :: Bool
                                    -- True <=> -fwarn-partial-type-signatures
                                    -- Controls whether holes in partial type
                                    -- signatures produce warnings
          , cec_suppress :: Bool    -- True <=> More important errors have occurred,
                                    --          so create bindings if need be, but
                                    --          don't issue any more errors/warnings
                                    -- See Note [Suppressing error messages]
      }

{-
Note [Suppressing error messages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The cec_suppress flag says "don't report any errors.  Instead, just create
evidence bindings (as usual).  It's used when more important errors have occurred.
Specifically (see reportWanteds)
  * If there are insoluble Givens, then we are in unreachable code and all bets
    are off.  So don't report any further errors.
  * If there are any insolubles (eg Int~Bool), here or in a nested implication,
    then suppress errors from the simple constraints here.  Sometimes the
    simple-constraint errors are a knock-on effect of the insolubles.
-}

reportImplic :: ReportErrCtxt -> Implication -> TcM ()
reportImplic ctxt implic@(Implic { ic_skols = tvs, ic_given = given
                                 , ic_wanted = wanted, ic_binds = evb
                                 , ic_insol = ic_insoluble, ic_info = info })
  | BracketSkol <- info
  , not ic_insoluble -- For Template Haskell brackets report only
  = return ()        -- definite errors. The whole thing will be re-checked
                     -- later when we plug it in, and meanwhile there may
                     -- certainly be un-satisfied constraints

  | otherwise
  = reportWanteds ctxt' wanted
  where
    (env1, tvs') = mapAccumL tidyTyVarBndr (cec_tidy ctxt) tvs
    (env2, info') = tidySkolemInfo env1 info
    implic' = implic { ic_skols = tvs'
                     , ic_given = map (tidyEvVar env2) given
                     , ic_info  = info' }
    ctxt' = ctxt { cec_tidy  = env2
                 , cec_encl  = implic' : cec_encl ctxt
                 , cec_binds = case cec_binds ctxt of
                                 Nothing -> Nothing
                                 Just {} -> Just evb }

reportWanteds :: ReportErrCtxt -> WantedConstraints -> TcM ()
reportWanteds ctxt wanted@(WC { wc_simple = simples, wc_insol = insols, wc_impl = implics })
  = do { reportSimples ctxt  (mapBag (tidyCt env) insol_given)
       ; reportSimples ctxt1 (mapBag (tidyCt env) insol_wanted)
       ; reportSimples ctxt2 (mapBag (tidyCt env) simples)
            -- All the Derived ones have been filtered out of simples
            -- by the constraint solver. This is ok; we don't want
            -- to report unsolved Derived goals as errors
            -- See Note [Do not report derived but soluble errors]
       ; mapBagM_ (reportImplic ctxt1) implics }
            -- NB ctxt1: don't suppress inner insolubles if there's only a
            -- wanted insoluble here; but do suppress inner insolubles
            -- if there's a given insoluble here (= inaccessible code)
 where
    (insol_given, insol_wanted) = partitionBag isGivenCt insols
    env = cec_tidy ctxt

      -- See Note [Suppressing error messages]
    suppress0 = cec_suppress ctxt
    suppress1 = suppress0 || not (isEmptyBag insol_given)
    suppress2 = suppress0 || insolubleWC wanted
    ctxt1     = ctxt { cec_suppress = suppress1 }
    ctxt2     = ctxt { cec_suppress = suppress2 }

reportSimples :: ReportErrCtxt -> Cts -> TcM ()
reportSimples ctxt simples    -- Here 'simples' includes insoluble goals
  =  traceTc "reportSimples" (vcat [ ptext (sLit "Simples =") <+> ppr simples
                                   , ptext (sLit "Suppress =") <+> ppr (cec_suppress ctxt)])
  >> tryReporters
      [ -- First deal with things that are utterly wrong
        -- Like Int ~ Bool (incl nullary TyCons)
        -- or  Int ~ t a   (AppTy on one side)
        ("custom_error", is_user_type_error, True, mkUserTypeErrorReporter)
      , ("Utterly wrong",  utterly_wrong,    True,  mkGroupReporter mkEqErr)
      , ("Holes",          is_hole,          False, mkHoleReporter mkHoleError)

        -- Report equalities of form (a~ty).  They are usually
        -- skolem-equalities, and they cause confusing knock-on
        -- effects in other errors; see test T4093b.
      , ("Skolem equalities", skolem_eq,  True,  mkSkolReporter)

        -- Other equalities; also confusing knock on effects
      , ("Equalities",      is_equality, True,  mkGroupReporter mkEqErr)

      , ("Implicit params", is_ip,       False, mkGroupReporter mkIPErr)
      , ("Irreds",          is_irred,    False, mkGroupReporter mkIrredErr)
      , ("Dicts",           is_dict,     False, mkGroupReporter mkDictErr)
      ]
      panicReporter ctxt (bagToList simples)
          -- TuplePreds should have been expanded away by the constraint
          -- simplifier, so they shouldn't show up at this point
  where
    utterly_wrong, skolem_eq, is_hole, is_dict,
      is_equality, is_ip, is_irred :: Ct -> PredTree -> Bool

    utterly_wrong _ (EqPred _ ty1 ty2) = isRigid ty1 && isRigid ty2
    utterly_wrong _ _ = False

    is_hole ct _ = isHoleCt ct
    is_user_type_error ct _ = isUserTypeErrorCt ct
    skolem_eq _ (EqPred NomEq ty1 ty2) = isRigidOrSkol ty1 && isRigidOrSkol ty2
    skolem_eq _ _ = False

    is_equality _ (EqPred {}) = True
    is_equality _ _           = False

    is_dict _ (ClassPred {}) = True
    is_dict _ _              = False

    is_ip _ (ClassPred cls _) = isIPClass cls
    is_ip _ _                 = False

    is_irred _ (IrredPred {}) = True
    is_irred _ _              = False


---------------
isRigid, isRigidOrSkol :: Type -> Bool
isRigid ty
  | Just (tc,_) <- tcSplitTyConApp_maybe ty = isDecomposableTyCon tc
  | Just {} <- tcSplitAppTy_maybe ty        = True
  | isForAllTy ty                           = True
  | otherwise                               = False

isRigidOrSkol ty
  | Just tv <- getTyVar_maybe ty = isSkolemTyVar tv
  | otherwise                    = isRigid ty

isTyFun_maybe :: Type -> Maybe TyCon
isTyFun_maybe ty = case tcSplitTyConApp_maybe ty of
                      Just (tc,_) | isTypeFamilyTyCon tc -> Just tc
                      _ -> Nothing


--------------------------------------------
--      Reporters
--------------------------------------------

type Reporter
  = ReportErrCtxt -> [Ct] -> TcM ()
type ReporterSpec
  = ( String                     -- Name
    , Ct -> PredTree -> Bool     -- Pick these ones
    , Bool                       -- True <=> suppress subsequent reporters
    , Reporter)                  -- The reporter itself

panicReporter :: Reporter
panicReporter _ cts
  | null cts  = return ()
  | otherwise =  pprPanic "reportSimples" (ppr cts)

mkSkolReporter :: Reporter
-- Suppress duplicates with the same LHS
mkSkolReporter ctxt cts
  = mapM_ (reportGroup mkEqErr ctxt) (equivClasses cmp_lhs_type cts)
  where
    cmp_lhs_type ct1 ct2
      = case (classifyPredType (ctPred ct1), classifyPredType (ctPred ct2)) of
           (EqPred eq_rel1 ty1 _, EqPred eq_rel2 ty2 _) ->
             (eq_rel1 `compare` eq_rel2) `thenCmp` (ty1 `cmpType` ty2)
           _ -> pprPanic "mkSkolReporter" (ppr ct1 $$ ppr ct2)

mkHoleReporter :: (ReportErrCtxt -> Ct -> TcM ErrMsg) -> Reporter
-- Reports errors one at a time
mkHoleReporter mk_err ctxt
  = mapM_ $ \ct ->
    do { err <- mk_err ctxt ct
       ; maybeReportHoleError ctxt err
       ; maybeAddDeferredHoleBinding ctxt err ct }

mkUserTypeErrorReporter :: Reporter
mkUserTypeErrorReporter ctxt
 = mapM_ $ \ct -> do { err <- mkUserTypeError ctxt ct
                     ; maybeReportError ctxt err }
                --     ; addDeferredBinding ctxt err ct }

mkUserTypeError :: ReportErrCtxt -> Ct -> TcM ErrMsg
mkUserTypeError ctxt ct = mkErrorMsg ctxt ct
                        $ pprUserTypeErrorTy
                        $ case getUserTypeErrorMsg ct of
                            Just (_,msg) -> msg
                            Nothing      -> pprPanic "mkUserTypeError" (ppr ct)

mkGroupReporter :: (ReportErrCtxt -> [Ct] -> TcM ErrMsg)
                             -- Make error message for a group
                -> Reporter  -- Deal with lots of constraints
-- Group together errors from same location,
-- and report only the first (to avoid a cascade)
mkGroupReporter mk_err ctxt cts
  = mapM_ (reportGroup mk_err ctxt) (equivClasses cmp_loc cts)
  where
    cmp_loc ct1 ct2 = ctLocSpan (ctLoc ct1) `compare` ctLocSpan (ctLoc ct2)

reportGroup :: (ReportErrCtxt -> [Ct] -> TcM ErrMsg) -> ReportErrCtxt
            -> [Ct] -> TcM ()
reportGroup mk_err ctxt cts
  = do { err <- mk_err ctxt cts
       ; maybeReportError ctxt err
       ; mapM_ (maybeAddDeferredBinding ctxt err) cts }
               -- Add deferred bindings for all
               -- But see Note [Always warn with -fdefer-type-errors]

maybeReportHoleError :: ReportErrCtxt -> ErrMsg -> TcM ()
maybeReportHoleError ctxt err
  -- When -XPartialTypeSignatures is on, warnings (instead of errors) are
  -- generated for holes in partial type signatures. Unless
  -- -fwarn_partial_type_signatures is not on, in which case the messages are
  -- discarded.
  | isWarning err
  = when (cec_warn_partial_type_signatures ctxt)
            (reportWarning $ makeIntoWarning (Reason Opt_WarnPartialTypeSignatures) err)
  | cec_defer_holes ctxt
  = when (cec_warn_holes ctxt)
            (reportWarning (makeIntoWarning (Reason Opt_WarnTypedHoles) err))
  | otherwise
  = reportError err

maybeReportError :: ReportErrCtxt -> ErrMsg -> TcM ()
-- Report the error and/or make a deferred binding for it
maybeReportError ctxt err
  -- See Note [Always warn with -fdefer-type-errors]
  | cec_defer_type_errors ctxt
  = reportWarning (makeIntoWarning NoReason err)
  | cec_suppress ctxt
  = return ()
  | otherwise
  = reportError err

addDeferredBinding :: ReportErrCtxt -> ErrMsg -> Ct -> TcM ()
-- See Note [Deferring coercion errors to runtime]
addDeferredBinding ctxt err ct
  | CtWanted { ctev_pred = pred, ctev_evar = ev_id } <- ctEvidence ct
    -- Only add deferred bindings for Wanted constraints
  , Just ev_binds_var <- cec_binds ctxt  -- We have somewhere to put the bindings
  = do { dflags <- getDynFlags
       ; let err_msg = pprLocErrMsg err
             err_fs  = mkFastString $ showSDoc dflags $
                       err_msg $$ text "(deferred type error)"

         -- Create the binding
       ; addTcEvBind ev_binds_var ev_id (EvDelayedError pred err_fs) }

  | otherwise   -- Do not set any evidence for Given/Derived
  = return ()

maybeAddDeferredHoleBinding :: ReportErrCtxt -> ErrMsg -> Ct -> TcM ()
maybeAddDeferredHoleBinding ctxt err ct
    | cec_defer_holes ctxt && isTypedHoleCt ct
    = addDeferredBinding ctxt err ct
    | otherwise
    = return ()

maybeAddDeferredBinding :: ReportErrCtxt -> ErrMsg -> Ct -> TcM ()
maybeAddDeferredBinding ctxt err ct
    | cec_defer_type_errors ctxt
    = addDeferredBinding ctxt err ct
    | otherwise
    = return ()

tryReporters :: [ReporterSpec] -> Reporter -> Reporter
-- Use the first reporter in the list whose predicate says True
tryReporters reporters deflt ctxt cts
  = do { traceTc "tryReporters {" (ppr cts)
       ; go ctxt reporters cts
       ; traceTc "tryReporters }" empty }
  where
    go ctxt [] cts = deflt ctxt cts
    go ctxt ((str, pred, suppress_after, reporter) : rs) cts
      | null yeses  = do { traceTc "tryReporters: no" (text str)
                         ; go ctxt rs cts }
      | otherwise   = do { traceTc "tryReporters: yes" (text str <+> ppr yeses)
                         ; reporter ctxt yeses :: TcM ()
                         ; let ctxt' = ctxt { cec_suppress = suppress_after || cec_suppress ctxt }
                         ; go ctxt' rs nos }
                         -- Carry on with the rest, because we must make
                         -- deferred bindings for them if we have
                         -- -fdefer-type-errors
                         -- But suppress their error messages
      where
       (yeses, nos) = partition keep_me cts
       keep_me ct = pred ct (classifyPredType (ctPred ct))

-- Add the "arising from..." part to a message about bunch of dicts
addArising :: CtOrigin -> SDoc -> SDoc
addArising orig msg = hang msg 2 (pprArising orig)

pprWithArising :: [Ct] -> (CtLoc, SDoc)
-- Print something like
--    (Eq a) arising from a use of x at y
--    (Show a) arising from a use of p at q
-- Also return a location for the error message
-- Works for Wanted/Derived only
pprWithArising []
  = panic "pprWithArising"
pprWithArising (ct:cts)
  | null cts
  = (loc, addArising (ctLocOrigin loc)
                     (pprTheta [ctPred ct]))
  | otherwise
  = (loc, vcat (map ppr_one (ct:cts)))
  where
    loc = ctLoc ct
    ppr_one ct' = hang (parens (pprType (ctPred ct')))
                     2 (pprArisingAt (ctLoc ct'))

mkErrorMsg :: ReportErrCtxt -> Ct -> SDoc -> TcM ErrMsg
mkErrorMsg ctxt ct msg
  = do { let tcl_env = ctLocEnv (ctLoc ct)
       ; err_info <- mkErrInfo (cec_tidy ctxt) (tcl_ctxt tcl_env)
       ; mkLongErrAt (RealSrcSpan (tcl_loc tcl_env)) msg err_info }

type UserGiven = ([EvVar], SkolemInfo, Bool, RealSrcSpan)

getUserGivens :: ReportErrCtxt -> [UserGiven]
-- One item for each enclosing implication
getUserGivens (CEC {cec_encl = ctxt})
  = reverse $
    [ (givens, info, no_eqs, tcl_loc env)
    | Implic { ic_given = givens, ic_env = env
             , ic_no_eqs = no_eqs, ic_info = info } <- ctxt
    , not (null givens) ]

{-
Note [Always warn with -fdefer-type-errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When -fdefer-type-errors is on we warn about *all* type errors, even
if cec_suppress is on.  This can lead to a lot more warnings than you
would get errors without -fdefer-type-errors, but if we suppress any of
them you might get a runtime error that wasn't warned about at compile
time.

This is an easy design choice to change; just flip the order of the
first two equations for maybeReportError

To be consistent, we should also report multiple warnings from a single
location in mkGroupReporter, when -fdefer-type-errors is on.  But that
is perhaps a bit *over*-consistent! Again, an easy choice to change.


Note [Do not report derived but soluble errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The wc_simples include Derived constraints that have not been solved, but are
not insoluble (in that case they'd be in wc_insols).  We do not want to report
these as errors:

* Superclass constraints. If we have an unsolved [W] Ord a, we'll also have
  an unsolved [D] Eq a, and we do not want to report that; it's just noise.

* Functional dependencies.  For givens, consider
      class C a b | a -> b
      data T a where
         MkT :: C a d => [d] -> T a
      f :: C a b => T a -> F Int
      f (MkT xs) = length xs
  Then we get a [D] b~d.  But there *is* a legitimate call to
  f, namely   f (MkT [True]) :: T Bool, in which b=d.  So we should
  not reject the program.

  For wanteds, something similar
      data T a where
        MkT :: C Int b => a -> b -> T a
      g :: C Int c => c -> ()
      f :: T a -> ()
      f (MkT x y) = g x
  Here we get [G] C Int b, [W] C Int a, hence [D] a~b.
  But again f (MkT True True) is a legitimate call.

(We leave the Deriveds in wc_simple until reportErrors, so that we don't lose
derived superclasses between iterations of the solver.)

For functional dependencies, here is a real example,
stripped off from libraries/utf8-string/Codec/Binary/UTF8/Generic.hs

  class C a b | a -> b
  g :: C a b => a -> b -> ()
  f :: C a b => a -> b -> ()
  f xa xb =
      let loop = g xa
      in loop xb

We will first try to infer a type for loop, and we will succeed:
    C a b' => b' -> ()
Subsequently, we will type check (loop xb) and all is good. But,
recall that we have to solve a final implication constraint:
    C a b => (C a b' => .... cts from body of loop .... ))
And now we have a problem as we will generate an equality b ~ b' and fail to
solve it.


************************************************************************
*                  *
                Irreducible predicate errors
*                  *
************************************************************************
-}

mkIrredErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkIrredErr ctxt cts
  = do { (ctxt, binds_msg) <- relevantBindings True ctxt ct1
       ; mkErrorMsg ctxt ct1 (msg $$ binds_msg) }
  where
    (ct1:_) = cts
    orig    = ctLocOrigin (ctLoc ct1)
    givens  = getUserGivens ctxt
    msg = couldNotDeduce givens (map ctPred cts, orig)

----------------
mkHoleError :: ReportErrCtxt -> Ct -> TcM ErrMsg
mkHoleError ctxt ct@(CHoleCan { cc_occ = occ })
  = do { partial_sigs <- xoptM LangExt.PartialTypeSignatures
       ; let tyvars = varSetElems (tyVarsOfCt ct)
             tyvars_msg = map loc_msg tyvars
             msg = vcat [ hang (ptext (sLit "Found hole") <+> quotes (ppr occ))
                             2 (ptext (sLit "with type:") <+> pprType (ctEvPred (ctEvidence ct)))
                        , ppUnless (null tyvars_msg) (ptext (sLit "Where:") <+> vcat tyvars_msg)
                        , if in_typesig && not partial_sigs then pts_hint else empty ]
       ; (ctxt, binds_doc) <- relevantBindings False ctxt ct
               -- The 'False' means "don't filter the bindings; see Trac #8191
       ; errMsg <- mkErrorMsg ctxt ct (msg $$ binds_doc)
       ; if in_typesig && partial_sigs
           then return $ makeIntoWarning NoReason errMsg
           else return errMsg }
  where
    in_typesig = not $ isTypedHoleCt ct
    pts_hint = ptext (sLit "To use the inferred type, enable PartialTypeSignatures")
    loc_msg tv
       = case tcTyVarDetails tv of
          SkolemTv {} -> quotes (ppr tv) <+> skol_msg
          MetaTv {}   -> quotes (ppr tv) <+> ptext (sLit "is an ambiguous type variable")
          det -> pprTcTyVarDetails det
       where
          skol_msg = pprSkol (getSkolemInfo (cec_encl ctxt) tv) (getSrcLoc tv)

mkHoleError _ ct = pprPanic "mkHoleError" (ppr ct)

----------------
mkIPErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkIPErr ctxt cts
  = do { (ctxt, bind_msg) <- relevantBindings True ctxt ct1
       ; mkErrorMsg ctxt ct1 (msg $$ bind_msg) }
  where
    (ct1:_) = cts
    orig    = ctLocOrigin (ctLoc ct1)
    preds   = map ctPred cts
    givens  = getUserGivens ctxt
    msg | null givens
        = addArising orig $
          sep [ ptext (sLit "Unbound implicit parameter") <> plural cts
              , nest 2 (pprTheta preds) ]
        | otherwise
        = couldNotDeduce givens (preds, orig)

{-
************************************************************************
*                                                                      *
                Equality errors
*                                                                      *
************************************************************************

Note [Inaccessible code]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   data T a where
     T1 :: T a
     T2 :: T Bool

   f :: (a ~ Int) => T a -> Int
   f T1 = 3
   f T2 = 4   -- Unreachable code

Here the second equation is unreachable. The original constraint
(a~Int) from the signature gets rewritten by the pattern-match to
(Bool~Int), so the danger is that we report the error as coming from
the *signature* (Trac #7293).  So, for Given errors we replace the
env (and hence src-loc) on its CtLoc with that from the immediately
enclosing implication.
-}

mkEqErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
-- Don't have multiple equality errors from the same location
-- E.g.   (Int,Bool) ~ (Bool,Int)   one error will do!
mkEqErr ctxt (ct:_) = mkEqErr1 ctxt ct
mkEqErr _ [] = panic "mkEqErr"

mkEqErr1 :: ReportErrCtxt -> Ct -> TcM ErrMsg
-- Wanted constraints only!
mkEqErr1 ctxt ct
  | isGiven ev
  = do { (ctxt, binds_msg) <- relevantBindings True ctxt ct
       ; let (given_loc, given_msg) = mk_given (cec_encl ctxt)
       ; dflags <- getDynFlags
       ; mkEqErr_help dflags ctxt (given_msg $$ binds_msg)
                      (ct { cc_ev = ev {ctev_loc = given_loc}}) -- Note [Inaccessible code]
                      Nothing ty1 ty2 }

  | otherwise   -- Wanted or derived
  = do { (ctxt, binds_msg) <- relevantBindings True ctxt ct
       ; (env1, tidy_orig) <- zonkTidyOrigin (cec_tidy ctxt) (ctLocOrigin loc)
       ; rdr_env <- getGlobalRdrEnv
       ; fam_envs <- tcGetFamInstEnvs
       ; let (is_oriented, wanted_msg) = mk_wanted_extra tidy_orig
             coercible_msg = case ctEvEqRel ev of
               NomEq  -> empty
               ReprEq -> mkCoercibleExplanation rdr_env fam_envs ty1 ty2
       ; dflags <- getDynFlags
       ; traceTc "mkEqErr1" (ppr ct $$ pprCtOrigin (ctLocOrigin loc) $$ pprCtOrigin tidy_orig)
       ; mkEqErr_help dflags (ctxt {cec_tidy = env1})
                      (wanted_msg $$ coercible_msg $$ binds_msg)
                      ct is_oriented ty1 ty2 }
  where
    ev         = ctEvidence ct
    loc        = ctEvLoc ev
    (ty1, ty2) = getEqPredTys (ctEvPred ev)

    mk_given :: [Implication] -> (CtLoc, SDoc)
    -- For given constraints we overwrite the env (and hence src-loc)
    -- with one from the implication.  See Note [Inaccessible code]
    mk_given []           = (loc, empty)
    mk_given (implic : _) = (setCtLocEnv loc (ic_env implic)
                            , hang (ptext (sLit "Inaccessible code in"))
                                 2 (ppr (ic_info implic)))

       -- If the types in the error message are the same as the types
       -- we are unifying, don't add the extra expected/actual message
    mk_wanted_extra orig@(TypeEqOrigin {})
      = mkExpectedActualMsg ty1 ty2 orig

    mk_wanted_extra (KindEqOrigin cty1 cty2 sub_o)
      = (Nothing, msg1 $$ msg2)
      where
        msg1 = hang (ptext (sLit "When matching types"))
                  2 (vcat [ ppr cty1 <+> dcolon <+> ppr (typeKind cty1)
                          , ppr cty2 <+> dcolon <+> ppr (typeKind cty2) ])
        msg2 = case sub_o of
                 TypeEqOrigin {} -> snd (mkExpectedActualMsg cty1 cty2 sub_o)
                 _ -> empty

    mk_wanted_extra orig@(FunDepOrigin1 {})     = (Nothing, pprArising orig)
    mk_wanted_extra orig@(FunDepOrigin2 {})     = (Nothing, pprArising orig)
    mk_wanted_extra orig@(DerivOriginCoerce _ oty1 oty2)
      = (Nothing, pprArising orig $+$ mkRoleSigs oty1 oty2)
    mk_wanted_extra orig@(CoercibleOrigin oty1 oty2)
        -- if the origin types are the same as the final types, don't
        -- clutter output with repetitive information
      | not (oty1 `eqType` ty1 && oty2 `eqType` ty2) &&
        not (oty1 `eqType` ty2 && oty2 `eqType` ty1)
      = (Nothing, pprArising orig $+$ mkRoleSigs oty1 oty2)
      | otherwise
        -- still print role sigs even if types line up
      = (Nothing, mkRoleSigs oty1 oty2)
    mk_wanted_extra _                           = (Nothing, empty)

-- | This function tries to reconstruct why a "Coercible ty1 ty2" constraint
-- is left over.
mkCoercibleExplanation :: GlobalRdrEnv -> FamInstEnvs
                       -> TcType -> TcType -> SDoc
mkCoercibleExplanation rdr_env fam_envs ty1 ty2
  | Just (tc, tys) <- tcSplitTyConApp_maybe ty1
  , (rep_tc, _, _) <- tcLookupDataFamInst fam_envs tc tys
  , Just msg <- coercible_msg_for_tycon rep_tc
  = msg
  | Just (tc, tys) <- splitTyConApp_maybe ty2
  , (rep_tc, _, _) <- tcLookupDataFamInst fam_envs tc tys
  , Just msg <- coercible_msg_for_tycon rep_tc
  = msg
  | Just (s1, _) <- tcSplitAppTy_maybe ty1
  , Just (s2, _) <- tcSplitAppTy_maybe ty2
  , s1 `eqType` s2
  , has_unknown_roles s1
  = hang (text "NB: We cannot know what roles the parameters to" <+>
          quotes (ppr s1) <+> text "have;")
       2 (text "we must assume that the role is nominal")
  | otherwise
  = empty
  where
    coercible_msg_for_tycon tc
        | isAbstractTyCon tc
        = Just $ hsep [ text "NB: The type constructor"
                      , quotes (pprSourceTyCon tc)
                      , text "is abstract" ]
        | isNewTyCon tc
        , [data_con] <- tyConDataCons tc
        , let dc_name = dataConName data_con
        , null (lookupGRE_Name rdr_env dc_name)
        = Just $ hang (text "The data constructor" <+> quotes (ppr dc_name))
                    2 (sep [ text "of newtype" <+> quotes (pprSourceTyCon tc)
                           , text "is not in scope" ])
        | otherwise = Nothing

    has_unknown_roles ty
      | Just (tc, tys) <- tcSplitTyConApp_maybe ty
      = length tys >= tyConArity tc  -- oversaturated tycon
      | Just (s, _) <- tcSplitAppTy_maybe ty
      = has_unknown_roles s
      | isTyVarTy ty
      = True
      | otherwise
      = False

-- | Make a listing of role signatures for all the parameterised tycons
-- used in the provided types
mkRoleSigs :: Type -> Type -> SDoc
mkRoleSigs ty1 ty2
  = ppUnless (null role_sigs) $
    hang (text "Relevant role signatures:")
       2 (vcat role_sigs)
  where
    tcs = nameEnvElts $ tyConsOfType ty1 `plusNameEnv` tyConsOfType ty2
    role_sigs = mapMaybe ppr_role_sig tcs

    ppr_role_sig tc
      | null roles  -- if there are no parameters, don't bother printing
      = Nothing
      | otherwise
      = Just $ hsep $ [text "type role", ppr tc] ++ map ppr roles
      where
        roles = tyConRoles tc

mkEqErr_help :: DynFlags -> ReportErrCtxt -> SDoc
             -> Ct
             -> Maybe SwapFlag   -- Nothing <=> not sure
             -> TcType -> TcType -> TcM ErrMsg
mkEqErr_help dflags ctxt extra ct oriented ty1 ty2
  | Just tv1 <- tcGetTyVar_maybe ty1 = mkTyVarEqErr dflags ctxt extra ct oriented tv1 ty2
  | Just tv2 <- tcGetTyVar_maybe ty2 = mkTyVarEqErr dflags ctxt extra ct swapped  tv2 ty1
  | otherwise                        = reportEqErr  ctxt extra ct oriented ty1 ty2
  where
    swapped = fmap flipSwap oriented

reportEqErr :: ReportErrCtxt -> SDoc
            -> Ct
            -> Maybe SwapFlag   -- Nothing <=> not sure
            -> TcType -> TcType -> TcM ErrMsg
reportEqErr ctxt extra1 ct oriented ty1 ty2
  = do { let extra2 = mkEqInfoMsg ct ty1 ty2
       ; mkErrorMsg ctxt ct (vcat [ misMatchOrCND ctxt ct oriented ty1 ty2
                                   , extra2, extra1]) }

mkTyVarEqErr :: DynFlags -> ReportErrCtxt -> SDoc -> Ct
             -> Maybe SwapFlag -> TcTyVar -> TcType -> TcM ErrMsg
-- tv1 and ty2 are already tidied
mkTyVarEqErr dflags ctxt extra ct oriented tv1 ty2
  | isUserSkolem ctxt tv1   -- ty2 won't be a meta-tyvar, or else the thing would
                            -- be oriented the other way round;
                            -- see TcCanonical.canEqTyVarTyVar
  || isSigTyVar tv1 && not (isTyVarTy ty2)
  || ctEqRel ct == ReprEq && not (isTyVarUnderDatatype tv1 ty2)
     -- the cases below don't really apply to ReprEq (except occurs check)
  = mkErrorMsg ctxt ct (vcat [ misMatchOrCND ctxt ct oriented ty1 ty2
                             , extraTyVarInfo ctxt tv1 ty2
                             , extra ])

  -- So tv is a meta tyvar (or started that way before we
  -- generalised it).  So presumably it is an *untouchable*
  -- meta tyvar or a SigTv, else it'd have been unified
  | not (k2 `tcIsSubKind` k1)            -- Kind error
  = mkErrorMsg ctxt ct $ (kindErrorMsg (mkTyVarTy tv1) ty2 $$ extra)

  | OC_Occurs <- occ_check_expand
  , ctEqRel ct == NomEq || isTyVarUnderDatatype tv1 ty2
         -- See Note [Occurs check error] in TcCanonical
  = do { let occCheckMsg = hang (text "Occurs check: cannot construct the infinite type:")
                              2 (sep [ppr ty1, char '~', ppr ty2])
             extra2 = mkEqInfoMsg ct ty1 ty2
       ; mkErrorMsg ctxt ct (occCheckMsg $$ extra2 $$ extra) }

  | OC_Forall <- occ_check_expand
  = do { let msg = vcat [ ptext (sLit "Cannot instantiate unification variable")
                          <+> quotes (ppr tv1)
                        , hang (ptext (sLit "with a type involving foralls:")) 2 (ppr ty2)
                        , nest 2 (ptext (sLit "Perhaps you want ImpredicativeTypes")) ]
       ; mkErrorMsg ctxt ct msg }

  -- If the immediately-enclosing implication has 'tv' a skolem, and
  -- we know by now its an InferSkol kind of skolem, then presumably
  -- it started life as a SigTv, else it'd have been unified, given
  -- that there's no occurs-check or forall problem
  | (implic:_) <- cec_encl ctxt
  , Implic { ic_skols = skols } <- implic
  , tv1 `elem` skols
  = mkErrorMsg ctxt ct (vcat [ misMatchMsg oriented eq_rel ty1 ty2
                             , extraTyVarInfo ctxt tv1 ty2
                             , extra ])

  -- Check for skolem escape
  | (implic:_) <- cec_encl ctxt   -- Get the innermost context
  , Implic { ic_env = env, ic_skols = skols, ic_info = skol_info } <- implic
  , let esc_skols = filter (`elemVarSet` (tyVarsOfType ty2)) skols
  , not (null esc_skols)
  = do { let msg = misMatchMsg oriented eq_rel ty1 ty2
             esc_doc = sep [ ptext (sLit "because type variable") <> plural esc_skols
                             <+> pprQuotedList esc_skols
                           , ptext (sLit "would escape") <+>
                             if isSingleton esc_skols then ptext (sLit "its scope")
                                                      else ptext (sLit "their scope") ]
             tv_extra = vcat [ nest 2 $ esc_doc
                             , sep [ (if isSingleton esc_skols
                                      then ptext (sLit "This (rigid, skolem) type variable is")
                                      else ptext (sLit "These (rigid, skolem) type variables are"))
                               <+> ptext (sLit "bound by")
                             , nest 2 $ ppr skol_info
                             , nest 2 $ ptext (sLit "at") <+> ppr (tcl_loc env) ] ]
       ; mkErrorMsg ctxt ct (msg $$ tv_extra $$ extra) }

  -- Nastiest case: attempt to unify an untouchable variable
  | (implic:_) <- cec_encl ctxt   -- Get the innermost context
  , Implic { ic_env = env, ic_given = given, ic_info = skol_info } <- implic
  = do { let msg = misMatchMsg oriented eq_rel ty1 ty2
             tclvl_extra
                = nest 2 $
                  sep [ quotes (ppr tv1) <+> ptext (sLit "is untouchable")
                      , nest 2 $ ptext (sLit "inside the constraints") <+> pprEvVarTheta given
                      , nest 2 $ ptext (sLit "bound by") <+> ppr skol_info
                      , nest 2 $ ptext (sLit "at") <+> ppr (tcl_loc env) ]
             tv_extra = extraTyVarInfo ctxt tv1 ty2
             add_sig  = suggestAddSig ctxt ty1 ty2
       ; mkErrorMsg ctxt ct (vcat [msg, tclvl_extra, tv_extra, add_sig, extra]) }

  | otherwise
  = reportEqErr ctxt extra ct oriented (mkTyVarTy tv1) ty2
        -- This *can* happen (Trac #6123, and test T2627b)
        -- Consider an ambiguous top-level constraint (a ~ F a)
        -- Not an occurs check, because F is a type function.
  where
    occ_check_expand = occurCheckExpand dflags tv1 ty2
    k1     = tyVarKind tv1
    k2     = typeKind ty2
    ty1    = mkTyVarTy tv1
    eq_rel = ctEqRel ct

mkEqInfoMsg :: Ct -> TcType -> TcType -> SDoc
-- Report (a) ambiguity if either side is a type function application
--            e.g. F a0 ~ Int
--        (b) warning about injectivity if both sides are the same
--            type function application   F a ~ F b
--            See Note [Non-injective type functions]
mkEqInfoMsg ct ty1 ty2
  = tyfun_msg $$ ambig_msg
  where
    mb_fun1 = isTyFun_maybe ty1
    mb_fun2 = isTyFun_maybe ty2

    ambig_msg | isJust mb_fun1 || isJust mb_fun2
              = snd (mkAmbigMsg ct)
              | otherwise = empty

    tyfun_msg | Just tc1 <- mb_fun1
              , Just tc2 <- mb_fun2
              , tc1 == tc2
              = ptext (sLit "NB:") <+> quotes (ppr tc1)
                <+> ptext (sLit "is a type function, and may not be injective")
              | otherwise = empty

isUserSkolem :: ReportErrCtxt -> TcTyVar -> Bool
-- See Note [Reporting occurs-check errors]
isUserSkolem ctxt tv
  = isSkolemTyVar tv && any is_user_skol_tv (cec_encl ctxt)
  where
    is_user_skol_tv (Implic { ic_skols = sks, ic_info = skol_info })
      = tv `elem` sks && is_user_skol_info skol_info

    is_user_skol_info (InferSkol {}) = False
    is_user_skol_info _ = True

misMatchOrCND :: ReportErrCtxt -> Ct -> Maybe SwapFlag -> TcType -> TcType -> SDoc
-- If oriented then ty1 is actual, ty2 is expected
misMatchOrCND ctxt ct oriented ty1 ty2
  | null givens ||
    (isRigid ty1 && isRigid ty2) ||
    isGivenCt ct
       -- If the equality is unconditionally insoluble
       -- or there is no context, don't report the context
  = misMatchMsg oriented eq_rel ty1 ty2
  | otherwise
  = couldNotDeduce givens ([eq_pred], orig)
  where
    eq_rel = ctEqRel ct
    givens = [ given | given@(_, _, no_eqs, _) <- getUserGivens ctxt, not no_eqs]
             -- Keep only UserGivens that have some equalities

    (eq_pred, orig) = case eq_rel of
      NomEq  -> ( mkTcEqPred ty1 ty2
                , TypeEqOrigin { uo_actual = ty1, uo_expected = ty2 })
      ReprEq -> ( mkCoerciblePred ty1 ty2
                , CoercibleOrigin ty1 ty2 )

couldNotDeduce :: [UserGiven] -> (ThetaType, CtOrigin) -> SDoc
couldNotDeduce givens (wanteds, orig)
  = vcat [ addArising orig (ptext (sLit "Could not deduce") <+> pprTheta wanteds)
         , vcat (pp_givens givens)]

pp_givens :: [UserGiven] -> [SDoc]
pp_givens givens
   = case givens of
         []     -> []
         (g:gs) ->      ppr_given (ptext (sLit "from the context")) g
                 : map (ppr_given (ptext (sLit "or from"))) gs
    where
       ppr_given herald (gs, skol_info, _, loc)
           = hang (herald <+> pprEvVarTheta gs)
                2 (sep [ ptext (sLit "bound by") <+> ppr skol_info
                       , ptext (sLit "at") <+> ppr loc])

extraTyVarInfo :: ReportErrCtxt -> TcTyVar -> TcType -> SDoc
-- Add on extra info about skolem constants
-- NB: The types themselves are already tidied
extraTyVarInfo ctxt tv1 ty2
  = nest 2 (tv_extra tv1 $$ ty_extra ty2)
  where
    implics = cec_encl ctxt
    ty_extra ty = case tcGetTyVar_maybe ty of
                    Just tv -> tv_extra tv
                    Nothing -> empty

    tv_extra tv | isTcTyVar tv, isSkolemTyVar tv
                , let pp_tv = quotes (ppr tv)
                = case tcTyVarDetails tv of
                    SkolemTv {}   -> pp_tv <+> pprSkol (getSkolemInfo implics tv) (getSrcLoc tv)
                    FlatSkol {}   -> pp_tv <+> ptext (sLit "is a flattening type variable")
                    RuntimeUnk {} -> pp_tv <+> ptext (sLit "is an interactive-debugger skolem")
                    MetaTv {}     -> empty

                | otherwise             -- Normal case
                = empty

suggestAddSig :: ReportErrCtxt -> TcType -> TcType -> SDoc
-- See Note [Suggest adding a type signature]
suggestAddSig ctxt ty1 ty2
  | null inferred_bndrs
  = empty
  | [bndr] <- inferred_bndrs
  = ptext (sLit "Possible fix: add a type signature for") <+> quotes (ppr bndr)
  | otherwise
  = ptext (sLit "Possible fix: add type signatures for some or all of") <+> (ppr inferred_bndrs)
  where
    inferred_bndrs = nub (get_inf ty1 ++ get_inf ty2)
    get_inf ty | Just tv <- tcGetTyVar_maybe ty
               , isTcTyVar tv, isSkolemTyVar tv
               , InferSkol prs <- getSkolemInfo (cec_encl ctxt) tv
               = map fst prs
               | otherwise
               = []

kindErrorMsg :: TcType -> TcType -> SDoc   -- Types are already tidy
kindErrorMsg ty1 ty2
  = vcat [ ptext (sLit "Kind incompatibility when matching types:")
         , nest 2 (vcat [ ppr ty1 <+> dcolon <+> ppr k1
                        , ppr ty2 <+> dcolon <+> ppr k2 ]) ]
  where
    k1 = typeKind ty1
    k2 = typeKind ty2

--------------------
misMatchMsg :: Maybe SwapFlag -> EqRel -> TcType -> TcType -> SDoc
-- Types are already tidy
-- If oriented then ty1 is actual, ty2 is expected
misMatchMsg oriented eq_rel ty1 ty2
  | Just IsSwapped <- oriented
  = misMatchMsg (Just NotSwapped) eq_rel ty2 ty1
  | Just NotSwapped <- oriented
  = sep [ text "Couldn't match" <+> repr1 <+> text "expected" <+>
          what <+> quotes (ppr ty2)
        , nest (12 + extra_space) $
          text "with" <+> repr2 <+> text "actual" <+> what <+> quotes (ppr ty1)
        , sameOccExtra ty2 ty1 ]
  | otherwise
  = sep [ text "Couldn't match" <+> repr1 <+> what <+> quotes (ppr ty1)
        , nest (15 + extra_space) $
          text "with" <+> repr2 <+> quotes (ppr ty2)
        , sameOccExtra ty1 ty2 ]
  where
    what | isKind ty1 = ptext (sLit "kind")
         | otherwise  = ptext (sLit "type")

    (repr1, repr2, extra_space) = case eq_rel of
      NomEq  -> (empty, empty, 0)
      ReprEq -> (text "representation of", text "that of", 10)

mkExpectedActualMsg :: Type -> Type -> CtOrigin -> (Maybe SwapFlag, SDoc)
-- NotSwapped means (actual, expected), IsSwapped is the reverse
mkExpectedActualMsg ty1 ty2 (TypeEqOrigin { uo_actual = act, uo_expected = exp })
  | act `pickyEqType` ty1, exp `pickyEqType` ty2 = (Just NotSwapped,  empty)
  | exp `pickyEqType` ty1, act `pickyEqType` ty2 = (Just IsSwapped, empty)
  | otherwise                                    = (Nothing, msg)
  where
    msg = vcat [ text "Expected type:" <+> ppr exp
               , text "  Actual type:" <+> ppr act ]

mkExpectedActualMsg _ _ _ = panic "mkExprectedActualMsg"

sameOccExtra :: TcType -> TcType -> SDoc
-- See Note [Disambiguating (X ~ X) errors]
sameOccExtra ty1 ty2
  | Just (tc1, _) <- tcSplitTyConApp_maybe ty1
  , Just (tc2, _) <- tcSplitTyConApp_maybe ty2
  , let n1 = tyConName tc1
        n2 = tyConName tc2
        same_occ = nameOccName n1                  == nameOccName n2
        same_pkg = moduleUnitId (nameModule n1) == moduleUnitId (nameModule n2)
  , n1 /= n2   -- Different Names
  , same_occ   -- but same OccName
  = ptext (sLit "NB:") <+> (ppr_from same_pkg n1 $$ ppr_from same_pkg n2)
  | otherwise
  = empty
  where
    ppr_from same_pkg nm
      | isGoodSrcSpan loc
      = hang (quotes (ppr nm) <+> ptext (sLit "is defined at"))
           2 (ppr loc)
      | otherwise  -- Imported things have an UnhelpfulSrcSpan
      = hang (quotes (ppr nm))
           2 (sep [ ptext (sLit "is defined in") <+> quotes (ppr (moduleName mod))
                  , ppUnless (same_pkg || pkg == mainUnitId) $
                    nest 4 $ ptext (sLit "in package") <+> quotes (ppr pkg) ])
       where
         pkg = moduleUnitId mod
         mod = nameModule nm
         loc = nameSrcSpan nm

{-
Note [Suggest adding a type signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The OutsideIn algorithm rejects GADT programs that don't have a principal
type, and indeed some that do.  Example:
   data T a where
     MkT :: Int -> T Int

   f (MkT n) = n

Does this have type f :: T a -> a, or f :: T a -> Int?
The error that shows up tends to be an attempt to unify an
untouchable type variable.  So suggestAddSig sees if the offending
type variable is bound by an *inferred* signature, and suggests
adding a declared signature instead.

This initially came up in Trac #8968, concerning pattern synonyms.

Note [Disambiguating (X ~ X) errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Trac #8278

Note [Reporting occurs-check errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given (a ~ [a]), if 'a' is a rigid type variable bound by a user-supplied
type signature, then the best thing is to report that we can't unify
a with [a], because a is a skolem variable.  That avoids the confusing
"occur-check" error message.

But nowadays when inferring the type of a function with no type signature,
even if there are errors inside, we still generalise its signature and
carry on. For example
   f x = x:x
Here we will infer something like
   f :: forall a. a -> [a]
with a suspended error of (a ~ [a]).  So 'a' is now a skolem, but not
one bound by the programmer!  Here we really should report an occurs check.

So isUserSkolem distinguishes the two.

Note [Non-injective type functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very confusing to get a message like
     Couldn't match expected type `Depend s'
            against inferred type `Depend s1'
so mkTyFunInfoMsg adds:
       NB: `Depend' is type function, and hence may not be injective

Warn of loopy local equalities that were dropped.


************************************************************************
*                                                                      *
                 Type-class errors
*                                                                      *
************************************************************************
-}

mkDictErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkDictErr ctxt cts
  = ASSERT( not (null cts) )
    do { inst_envs <- tcGetInstEnvs
       ; let (ct1:_) = cts  -- ct1 just for its location
             min_cts = elim_superclasses cts
       ; lookups   <- mapM (lookup_cls_inst inst_envs) min_cts
       ; let (no_inst_cts, overlap_cts) = partition is_no_inst lookups

       -- Report definite no-instance errors,
       -- or (iff there are none) overlap errors
       -- But we report only one of them (hence 'head') because they all
       -- have the same source-location origin, to try avoid a cascade
       -- of error from one location
       ; (ctxt, err) <- mk_dict_err ctxt (head (no_inst_cts ++ overlap_cts))
       ; mkErrorMsg ctxt ct1 err }
  where
    no_givens = null (getUserGivens ctxt)

    is_no_inst (ct, (matches, unifiers, _))
      =  no_givens
      && null matches
      && (null unifiers || all (not . isAmbiguousTyVar) (varSetElems (tyVarsOfCt ct)))

    lookup_cls_inst inst_envs ct
      = do { tys_flat <- mapM quickFlattenTy tys
                -- Note [Flattening in error message generation]
           ; return (ct, lookupInstEnv inst_envs clas tys_flat) }
      where
        (clas, tys) = getClassPredTys (ctPred ct)


    -- When simplifying [W] Ord (Set a), we need
    --    [W] Eq a, [W] Ord a
    -- but we really only want to report the latter
    elim_superclasses cts
      = filter (\ct -> any (eqPred (ctPred ct)) min_preds) cts
      where
        min_preds = mkMinimalBySCs (map ctPred cts)

mk_dict_err :: ReportErrCtxt -> (Ct, ClsInstLookupResult)
            -> TcM (ReportErrCtxt, SDoc)
-- Report an overlap error if this class constraint results
-- from an overlap (returning Left clas), otherwise return (Right pred)
mk_dict_err ctxt (ct, (matches, unifiers, safe_haskell))
  | null matches  -- No matches but perhaps several unifiers
  = do { let (is_ambig, ambig_msg) = mkAmbigMsg ct
       ; (ctxt, binds_msg) <- relevantBindings True ctxt ct
       ; traceTc "mk_dict_err" (ppr ct $$ ppr is_ambig $$ ambig_msg)
       ; return (ctxt, cannot_resolve_msg is_ambig binds_msg ambig_msg) }

  | not safe_haskell   -- Some matches => overlap errors
  = return (ctxt, overlap_msg)

  | otherwise
  = return (ctxt, safe_haskell_msg)
  where
    orig        = ctLocOrigin (ctLoc ct)
    pred        = ctPred ct
    (clas, tys) = getClassPredTys pred
    ispecs      = [ispec | (ispec, _) <- matches]
    givens      = getUserGivens ctxt
    all_tyvars  = all isTyVarTy tys

    cannot_resolve_msg has_ambig_tvs binds_msg ambig_msg
      = vcat [ addArising orig no_inst_msg
             , vcat (pp_givens givens)
             , ppWhen (has_ambig_tvs && not (null unifiers && null givens))
               (vcat [ ambig_msg, binds_msg, potential_msg ])
             , show_fixes (add_to_ctxt_fixes has_ambig_tvs ++ drv_fixes) ]

    potential_msg
      = ppWhen (not (null unifiers) && want_potential orig) $
        hang (if isSingleton unifiers
              then ptext (sLit "Note: there is a potential instance available:")
              else ptext (sLit "Note: there are several potential instances:"))
           2 (ppr_insts (sortBy fuzzyClsInstCmp unifiers))

    -- Report "potential instances" only when the constraint arises
    -- directly from the user's use of an overloaded function
    want_potential (TypeEqOrigin {}) = False
    want_potential _                 = True

    add_to_ctxt_fixes has_ambig_tvs
      | not has_ambig_tvs && all_tyvars
      , (orig:origs) <- usefulContext ctxt pred
      = [sep [ ptext (sLit "add") <+> pprParendType pred
               <+> ptext (sLit "to the context of")
             , nest 2 $ ppr_skol orig $$
                        vcat [ ptext (sLit "or") <+> ppr_skol orig
                             | orig <- origs ] ] ]
      | otherwise = []

    ppr_skol (PatSkol dc _) = ptext (sLit "the data constructor") <+> quotes (ppr dc)
    ppr_skol skol_info      = ppr skol_info

    no_inst_msg
      | null givens && null matches
      = ptext (sLit "No instance for")
        <+> pprParendType pred
        $$ if type_has_arrow pred
            then nest 2 $ ptext (sLit "(maybe you haven't applied enough arguments to a function?)")
            else empty

      | otherwise
      = ptext (sLit "Could not deduce") <+> pprParendType pred

    type_has_arrow (TyVarTy _)      = False
    type_has_arrow (AppTy t1 t2)    = type_has_arrow t1 || type_has_arrow t2
    type_has_arrow (TyConApp _ ts)  = or $ map type_has_arrow ts
    type_has_arrow (FunTy _ _)      = True
    type_has_arrow (ForAllTy _ t)   = type_has_arrow t
    type_has_arrow (LitTy _)        = False

    drv_fixes = case orig of
                   DerivOrigin      -> [drv_fix]
                   DerivOriginDC {} -> [drv_fix]
                   DerivOriginCoerce {} -> [drv_fix]
                   _                -> []

    drv_fix = hang (ptext (sLit "use a standalone 'deriving instance' declaration,"))
                 2 (ptext (sLit "so you can specify the instance context yourself"))

    -- Normal overlap error
    overlap_msg
      = ASSERT( not (null matches) )
        vcat [  addArising orig (ptext (sLit "Overlapping instances for")
                                <+> pprType (mkClassPred clas tys))

             ,  ppUnless (null matching_givens) $
                  sep [ptext (sLit "Matching givens (or their superclasses):")
                      , nest 2 (vcat matching_givens)]

             ,  sep [ptext (sLit "Matching instances:"),
                     nest 2 (vcat [pprInstances ispecs, pprInstances unifiers])]

             ,  ppWhen (null matching_givens && isSingleton matches && null unifiers) $
                -- Intuitively, some given matched the wanted in their
                -- flattened or rewritten (from given equalities) form
                -- but the matcher can't figure that out because the
                -- constraints are non-flat and non-rewritten so we
                -- simply report back the whole given
                -- context. Accelerate Smart.hs showed this problem.
                  sep [ ptext (sLit "There exists a (perhaps superclass) match:")
                      , nest 2 (vcat (pp_givens givens))]

             ,  ppWhen (isSingleton matches) $
                parens (vcat [ ptext (sLit "The choice depends on the instantiation of") <+>
                                  quotes (pprWithCommas ppr (tyVarsOfTypesList tys))
                             , ppWhen (null (matching_givens)) $
                               vcat [ ptext (sLit "To pick the first instance above, use IncoherentInstances")
                                    , ptext (sLit "when compiling the other instance declarations")]
                        ])]
        where
            ispecs = [ispec | (ispec, _) <- matches]

            givens = getUserGivens ctxt
            matching_givens = mapMaybe matchable givens

            matchable (evvars,skol_info,_,loc)
              = case ev_vars_matching of
                     [] -> Nothing
                     _  -> Just $ hang (pprTheta ev_vars_matching)
                                    2 (sep [ ptext (sLit "bound by") <+> ppr skol_info
                                           , ptext (sLit "at") <+> ppr loc])
                where ev_vars_matching = filter ev_var_matches (map evVarPred evvars)
                      ev_var_matches ty = case getClassPredTys_maybe ty of
                         Just (clas', tys')
                           | clas' == clas
                           , Just _ <- tcMatchTys (tyVarsOfTypes tys) tys tys'
                           -> True
                           | otherwise
                           -> any ev_var_matches (immSuperClasses clas' tys')
                         Nothing -> False

    -- Overlap error because of Safe Haskell (first
    -- match should be the most specific match)
    safe_haskell_msg
      = ASSERT( length matches > 1 )
        vcat [ addArising orig (ptext (sLit "Unsafe overlapping instances for")
                        <+> pprType (mkClassPred clas tys))
             , sep [ptext (sLit "The matching instance is:"),
                    nest 2 (pprInstance $ head ispecs)]
             , vcat [ ptext $ sLit "It is compiled in a Safe module and as such can only"
                    , ptext $ sLit "overlap instances from the same module, however it"
                    , ptext $ sLit "overlaps the following instances from different modules:"
                    , nest 2 (vcat [pprInstances $ tail ispecs])
                    ]
             ]

usefulContext :: ReportErrCtxt -> TcPredType -> [SkolemInfo]
usefulContext ctxt pred
  = go (cec_encl ctxt)
  where
    pred_tvs = tyVarsOfType pred
    go [] = []
    go (ic : ics)
       = case ic_info ic of
               -- Do not suggest adding constraints to an *inferred* type signature!
           SigSkol (InfSigCtxt {}) _ -> rest
           info                      -> info : rest
       where
          -- Stop when the context binds a variable free in the predicate
          rest | any (`elemVarSet` pred_tvs) (ic_skols ic) = []
               | otherwise                                 = go ics

show_fixes :: [SDoc] -> SDoc
show_fixes []     = empty
show_fixes (f:fs) = sep [ ptext (sLit "Possible fix:")
                        , nest 2 (vcat (f : map (ptext (sLit "or") <+>) fs))]

ppr_insts :: [ClsInst] -> SDoc
ppr_insts insts
  = pprInstances (take 3 insts) $$ dot_dot_message
  where
    n_extra = length insts - 3
    dot_dot_message
       | n_extra <= 0 = empty
       | otherwise    = ptext (sLit "...plus")
                        <+> speakNOf n_extra (ptext (sLit "other"))

----------------------
quickFlattenTy :: TcType -> TcM TcType
-- See Note [Flattening in error message generation]
quickFlattenTy ty | Just ty' <- tcView ty = quickFlattenTy ty'
quickFlattenTy ty@(TyVarTy {})  = return ty
quickFlattenTy ty@(ForAllTy {}) = return ty     -- See
quickFlattenTy ty@(LitTy {})    = return ty
  -- Don't flatten because of the danger or removing a bound variable
quickFlattenTy (AppTy ty1 ty2) = do { fy1 <- quickFlattenTy ty1
                                    ; fy2 <- quickFlattenTy ty2
                                    ; return (AppTy fy1 fy2) }
quickFlattenTy (FunTy ty1 ty2) = do { fy1 <- quickFlattenTy ty1
                                    ; fy2 <- quickFlattenTy ty2
                                    ; return (FunTy fy1 fy2) }
quickFlattenTy (TyConApp tc tys)
    | not (isTypeFamilyTyCon tc)
    = do { fys <- mapM quickFlattenTy tys
         ; return (TyConApp tc fys) }
    | otherwise
    = do { let (funtys,resttys) = splitAt (tyConArity tc) tys
                -- Ignore the arguments of the type family funtys
         ; v <- newMetaTyVar (TauTv False) (typeKind (TyConApp tc funtys))
         ; flat_resttys <- mapM quickFlattenTy resttys
         ; return (foldl AppTy (mkTyVarTy v) flat_resttys) }

{-
Note [Flattening in error message generation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (C (Maybe (F x))), where F is a type function, and we have
instances
                C (Maybe Int) and C (Maybe a)
Since (F x) might turn into Int, this is an overlap situation, and
indeed (because of flattening) the main solver will have refrained
from solving.  But by the time we get to error message generation, we've
un-flattened the constraint.  So we must *re*-flatten it before looking
up in the instance environment, lest we only report one matching
instance when in fact there are two.

Re-flattening is pretty easy, because we don't need to keep track of
evidence.  We don't re-use the code in TcCanonical because that's in
the TcS monad, and we are in TcM here.

Note [Quick-flatten polytypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we see C (Ix a => blah) or C (forall a. blah) we simply refrain from
flattening any further.  After all, there can be no instance declarations
that match such things.  And flattening under a for-all is problematic
anyway; consider C (forall a. F a)

Note [Suggest -fprint-explicit-kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It can be terribly confusing to get an error message like (Trac #9171)
    Couldn't match expected type ‘GetParam Base (GetParam Base Int)’
                with actual type ‘GetParam Base (GetParam Base Int)’
The reason may be that the kinds don't match up.  Typically you'll get
more useful information, but not when it's as a result of ambiguity.
This test suggests -fprint-explicit-kinds when all the ambiguous type
variables are kind variables.
-}

mkAmbigMsg :: Ct -> (Bool, SDoc)
mkAmbigMsg ct
  | null ambig_tkvs = (False, empty)
  | otherwise       = (True,  msg)
  where
    ambig_tkv_set = filterVarSet isAmbiguousTyVar (tyVarsOfCt ct)
    ambig_tkvs    = varSetElems ambig_tkv_set
    (ambig_kvs, ambig_tvs) = partition isKindVar ambig_tkvs

    msg | any isRuntimeUnkSkol ambig_tkvs  -- See Note [Runtime skolems]
        =  vcat [ ptext (sLit "Cannot resolve unknown runtime type") <> plural ambig_tvs
                     <+> pprQuotedList ambig_tvs
                , ptext (sLit "Use :print or :force to determine these types")]

        | not (null ambig_tvs)
        = pp_ambig (ptext (sLit "type")) ambig_tvs

        | otherwise  -- All ambiguous kind variables; suggest -fprint-explicit-kinds
        = vcat [ pp_ambig (ptext (sLit "kind")) ambig_kvs
               , sdocWithDynFlags suggest_explicit_kinds ]

    pp_ambig what tkvs
      = ptext (sLit "The") <+> what <+> ptext (sLit "variable") <> plural tkvs
        <+> pprQuotedList tkvs <+> is_or_are tkvs <+> ptext (sLit "ambiguous")

    is_or_are [_] = text "is"
    is_or_are _   = text "are"

    suggest_explicit_kinds dflags  -- See Note [Suggest -fprint-explicit-kinds]
      | gopt Opt_PrintExplicitKinds dflags = empty
      | otherwise = ptext (sLit "Use -fprint-explicit-kinds to see the kind arguments")

pprSkol :: SkolemInfo -> SrcLoc -> SDoc
pprSkol UnkSkol   _
  = ptext (sLit "is an unknown type variable")
pprSkol skol_info tv_loc
  = sep [ ptext (sLit "is a rigid type variable bound by"),
          sep [ppr skol_info, ptext (sLit "at") <+> ppr tv_loc]]

getSkolemInfo :: [Implication] -> TcTyVar -> SkolemInfo
-- Get the skolem info for a type variable
-- from the implication constraint that binds it
getSkolemInfo [] tv
  = pprPanic "No skolem info:" (ppr tv)

getSkolemInfo (implic:implics) tv
  | tv `elem` ic_skols implic = ic_info implic
  | otherwise                 = getSkolemInfo implics tv

-----------------------
-- relevantBindings looks at the value environment and finds values whose
-- types mention any of the offending type variables.  It has to be
-- careful to zonk the Id's type first, so it has to be in the monad.
-- We must be careful to pass it a zonked type variable, too.
--
-- We always remove closed top-level bindings, though,
-- since they are never relevant (cf Trac #8233)

relevantBindings :: Bool  -- True <=> filter by tyvar; False <=> no filtering
                          -- See Trac #8191
                 -> ReportErrCtxt -> Ct
                 -> TcM (ReportErrCtxt, SDoc)
relevantBindings want_filtering ctxt ct
  = do { dflags <- getDynFlags
       ; (tidy_env', docs, discards)
              <- go (cec_tidy ctxt) (maxRelevantBinds dflags)
                    emptyVarSet [] False
                    (tcl_bndrs lcl_env)
         -- tcl_bndrs has the innermost bindings first,
         -- which are probably the most relevant ones

       ; traceTc "relevantBindings" (ppr ct $$ ppr [id | TcIdBndr id _ <- tcl_bndrs lcl_env])
       ; let doc = hang (ptext (sLit "Relevant bindings include"))
                      2 (vcat docs $$ max_msg)
             max_msg | discards
                     = ptext (sLit "(Some bindings suppressed; use -fmax-relevant-binds=N or -fno-max-relevant-binds)")
                     | otherwise = empty

       ; if null docs
         then return (ctxt, empty)
         else do { traceTc "rb" doc
                 ; return (ctxt { cec_tidy = tidy_env' }, doc) } }
  where
    loc       = ctLoc ct
    lcl_env   = ctLocEnv loc
    ct_tvs    = tyVarsOfCt ct `unionVarSet` extra_tvs

    -- For *kind* errors, report the relevant bindings of the
    -- enclosing *type* equality, because that's more useful for the programmer
    extra_tvs = case ctLocOrigin loc of
                  KindEqOrigin t1 t2 _ -> tyVarsOfTypes [t1,t2]
                  _                    -> emptyVarSet

    run_out :: Maybe Int -> Bool
    run_out Nothing = False
    run_out (Just n) = n <= 0

    dec_max :: Maybe Int -> Maybe Int
    dec_max = fmap (\n -> n - 1)

    go :: TidyEnv -> Maybe Int -> TcTyVarSet -> [SDoc]
       -> Bool                          -- True <=> some filtered out due to lack of fuel
       -> [TcIdBinder]
       -> TcM (TidyEnv, [SDoc], Bool)   -- The bool says if we filtered any out
                                        -- because of lack of fuel
    go tidy_env _ _ docs discards []
       = return (tidy_env, reverse docs, discards)
    go tidy_env n_left tvs_seen docs discards (TcIdBndr id top_lvl : tc_bndrs)
       = do { (tidy_env', tidy_ty) <- zonkTidyTcType tidy_env (idType id)
            ; traceTc "relevantBindings 1" (ppr id <+> dcolon <+> ppr tidy_ty)
            ; let id_tvs = tyVarsOfType tidy_ty
                  doc = sep [ pprPrefixOcc id <+> dcolon <+> ppr tidy_ty
                            , nest 2 (parens (ptext (sLit "bound at")
                                 <+> ppr (getSrcLoc id)))]
                  new_seen = tvs_seen `unionVarSet` id_tvs

            ; if (want_filtering && not opt_PprStyle_Debug
                                 && id_tvs `disjointVarSet` ct_tvs)
                       -- We want to filter out this binding anyway
                       -- so discard it silently
              then go tidy_env n_left tvs_seen docs discards tc_bndrs

              else if isTopLevel top_lvl && not (isNothing n_left)
                       -- It's a top-level binding and we have not specified
                       -- -fno-max-relevant-bindings, so discard it silently
              then go tidy_env n_left tvs_seen docs discards tc_bndrs

              else if run_out n_left && id_tvs `subVarSet` tvs_seen
                       -- We've run out of n_left fuel and this binding only
                       -- mentions already-seen type variables, so discard it
              then go tidy_env n_left tvs_seen docs True tc_bndrs

                       -- Keep this binding, decrement fuel
              else go tidy_env' (dec_max n_left) new_seen (doc:docs) discards tc_bndrs }

-----------------------
warnDefaulting :: Cts -> Type -> TcM ()
warnDefaulting wanteds default_ty
  = do { warn_default <- woptM Opt_WarnTypeDefaults
       ; env0 <- tcInitTidyEnv
       ; let tidy_env = tidyFreeTyVars env0 $
                        tyVarsOfCts wanteds
             tidy_wanteds = mapBag (tidyCt tidy_env) wanteds
             (loc, ppr_wanteds) = pprWithArising (bagToList tidy_wanteds)
             warn_msg  = hang (ptext (sLit "Defaulting the following constraint(s) to type")
                                <+> quotes (ppr default_ty))
                            2 ppr_wanteds
       ; setCtLoc loc $ warnTc (Reason Opt_WarnTypeDefaults) warn_default warn_msg }

{-
Note [Runtime skolems]
~~~~~~~~~~~~~~~~~~~~~~
We want to give a reasonably helpful error message for ambiguity
arising from *runtime* skolems in the debugger.  These
are created by in RtClosureInspect.zonkRTTIType.

************************************************************************
*                                                                      *
                 Error from the canonicaliser
         These ones are called *during* constraint simplification
*                                                                      *
************************************************************************
-}

solverDepthErrorTcS :: SubGoalCounter -> CtEvidence -> TcM a
solverDepthErrorTcS cnt ev
  = setCtLoc loc $
    do { pred <- zonkTcType (ctEvPred ev)
       ; env0 <- tcInitTidyEnv
       ; let tidy_env  = tidyFreeTyVars env0 (tyVarsOfType pred)
             tidy_pred = tidyType tidy_env pred
       ; failWithTcM (tidy_env, hang (msg cnt) 2 (ppr tidy_pred)) }
  where
    loc   = ctEvLoc ev
    depth = ctLocDepth loc
    value = subGoalCounterValue cnt depth
    msg CountConstraints =
        vcat [ ptext (sLit "Context reduction stack overflow; size =") <+> int value
             , ptext (sLit "Use -fcontext-stack=N to increase stack size to N") ]
    msg CountTyFunApps =
        vcat [ ptext (sLit "Type function application stack overflow; size =") <+> int value
             , ptext (sLit "Use -ftype-function-depth=N to increase stack size to N") ]
