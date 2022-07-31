module Eta.TypeCheck.TcSimplify(
       simplifyInfer,
       quantifyPred, growThetaTyVars,
       simplifyAmbiguityCheck,
       simplifyDefault,
       simplifyRule, simplifyTop, simplifyInteractive,
       solveWantedsTcM, simplifyWantedsTcM,
       tcCheckSatisfiability,
       captureTopConstraints
  ) where

import qualified Eta.LanguageExtensions as LangExt
import Eta.TypeCheck.TcRnTypes
import Eta.TypeCheck.TcRnMonad
import qualified Eta.TypeCheck.TcRnMonad as TcRnMonad
import qualified Eta.TypeCheck.TcRnMonad as TcM
import Eta.TypeCheck.TcErrors
import Eta.TypeCheck.TcMType as TcM
import Eta.TypeCheck.TcType
import Eta.TypeCheck.TcSMonad as TcS
import Eta.TypeCheck.TcInteract
import Eta.Types.Kind     ( isKind, isSubKind, defaultKind_maybe )
import Eta.TypeCheck.Inst
import Eta.Types.Type     ( classifyPredType, isIPClass, PredTree(..)
                , getClassPredTys_maybe, EqRel(..))
import Eta.Types.TyCon    ( isTypeFamilyTyCon )
import Eta.Types.Class    ( Class )
import Eta.BasicTypes.Id       ( idType )
import Eta.BasicTypes.Var
import Eta.BasicTypes.Unique
import Eta.BasicTypes.VarSet
import Eta.TypeCheck.TcEvidence
import Eta.BasicTypes.Name
import Eta.Utils.Bag
import Eta.Utils.ListSetOps
import Eta.Utils.Util
import Eta.Prelude.PrelInfo
import Eta.Prelude.PrelNames
import Control.Monad    ( unless )
import Eta.Types.Class            ( classKey )
import Eta.BasicTypes.BasicTypes       ( RuleName )
import Eta.Utils.Outputable
import Eta.Utils.FastString
import Eta.Core.TrieMap () -- DV: for now
import Data.List( partition )

{-
*********************************************************************************
*                                                                               *
*                           External interface                                  *
*                                                                               *
*********************************************************************************
-}

captureTopConstraints :: TcM a -> TcM (a, WantedConstraints)
-- (captureTopConstraints m) runs m, and returns the type constraints it
-- generates plus the constraints produced by static forms inside.
-- If it fails with an exception, it reports any insolubles
-- (out of scope variables) before doing so
captureTopConstraints thing_inside
  = do { static_wc_var <- TcM.newTcRef emptyWC ;
       ; (mb_res, lie) <- TcM.updGblEnv (\env -> env { tcg_static_wc = static_wc_var } ) $
                          TcM.tryCaptureConstraints thing_inside
       ; stWC <- TcM.readTcRef static_wc_var

       -- See TcRnMonad Note [Constraints and errors]
       -- If the thing_inside threw an exception, but generated some insoluble
       -- constraints, report the latter before propagating the exception
       -- Otherwise they will be lost altogether
       ; case mb_res of
           Right res -> return (res, lie `andWC` stWC)
           Left {}   -> do { _ <- reportUnsolved lie; failM } }
                -- This call to reportUnsolved is the reason
                -- this function is here instead of TcRnMonad

simplifyTop :: WantedConstraints -> TcM (Bag EvBind)
-- Simplify top-level constraints
-- Usually these will be implications,
-- but when there is nothing to quantify we don't wrap
-- in a degenerate implication, so we do that here instead
simplifyTop wanteds
  = do { traceTc "simplifyTop {" $ text "wanted = " <+> ppr wanteds
       ; ev_binds_var <- TcM.newTcEvBinds
       ; zonked_final_wc <- solveWantedsTcMWithEvBinds ev_binds_var wanteds simpl_top
       ; binds1 <- TcRnMonad.getTcEvBinds ev_binds_var
       ; traceTc "End simplifyTop }" empty

       ; traceTc "reportUnsolved {" empty
       ; binds2 <- reportUnsolved zonked_final_wc
       ; traceTc "reportUnsolved }" empty

       ; return (binds1 `unionBags` binds2) }

simpl_top :: WantedConstraints -> TcS WantedConstraints
    -- See Note [Top-level Defaulting Plan]
simpl_top wanteds
  = do { (again, wc_first_go) <- reportUnifiedExtends $ nestTcS (solveWantedsAndDrop wanteds)
                            -- This is where the main work happens
       ; if again then simpl_top wc_first_go else try_tyvar_defaulting wc_first_go }
  where
    try_tyvar_defaulting :: WantedConstraints -> TcS WantedConstraints
    try_tyvar_defaulting wc
      | isEmptyWC wc
      = return wc
      | otherwise
      = do { free_tvs <- TcS.zonkTyVarsAndFV (tyVarsOfWC wc)
           ; let meta_tvs = varSetElems (filterVarSet isMetaTyVar free_tvs)
                   -- zonkTyVarsAndFV: the wc_first_go is not yet zonked
                   -- filter isMetaTyVar: we might have runtime-skolems in GHCi,
                   -- and we definitely don't want to try to assign to those!

           ; meta_tvs' <- mapM defaultTyVar meta_tvs   -- Has unification side effects
           ; if meta_tvs' == meta_tvs   -- No defaulting took place;
                                        -- (defaulting returns fresh vars)
             then try_class_defaulting wc
             else do { wc_residual <- nestTcS (solveWantedsAndDrop wc)
                            -- See Note [Must simplify after defaulting]
                     ; try_class_defaulting wc_residual } }

    try_class_defaulting :: WantedConstraints -> TcS WantedConstraints
    try_class_defaulting wc
      | isEmptyWC wc
      = return wc
      | otherwise  -- See Note [When to do type-class defaulting]
      = do { something_happened <- applyDefaultingRules (approximateWC wc)
                                   -- See Note [Top-level Defaulting Plan]
           ; if something_happened
             then do { wc_residual <- nestTcS (solveWantedsAndDrop wc)
                     ; try_class_defaulting wc_residual }
            -- See Note [Overview of implicit CallStacks]
             else try_callstack_defaulting wc }

    try_callstack_defaulting :: WantedConstraints -> TcS WantedConstraints
    try_callstack_defaulting wc
      | isEmptyWC wc
      = return wc
      | otherwise
      = defaultCallStacks wc

-- | Default any remaining @CallStack@ constraints to empty @CallStack@s.
defaultCallStacks :: WantedConstraints -> TcS WantedConstraints
-- See Note [Overview of implicit CallStacks]
defaultCallStacks wanteds
  = do simples <- handle_simples (wc_simple wanteds)
       implics <- mapBagM handle_implic (wc_impl wanteds)
       return (wanteds { wc_simple = simples, wc_impl = implics })

  where

  handle_simples simples
    = catBagMaybes <$> mapBagM defaultCallStack simples

  handle_implic implic = do
    wanteds <- defaultCallStacks (ic_wanted implic)
    return (implic { ic_wanted = wanteds })

  defaultCallStack ct
    | Just _ <- isCallStackPred (ctPred ct)
    = do { solveCallStack (cc_ev ct) EvCsEmpty
         ; return Nothing }

  defaultCallStack ct
    = return (Just ct)

{-
Note [When to do type-class defaulting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In GHC 7.6 and 7.8.2, we did type-class defaulting only if insolubleWC
was false, on the grounds that defaulting can't help solve insoluble
constraints.  But if we *don't* do defaulting we may report a whole
lot of errors that would be solved by defaulting; these errors are
quite spurious because fixing the single insoluble error means that
defaulting happens again, which makes all the other errors go away.
This is jolly confusing: Trac #9033.

So it seems better to always do type-class defaulting.

However, always doing defaulting does mean that we'll do it in
situations like this (Trac #5934):
   run :: (forall s. GenST s) -> Int
   run = fromInteger 0
We don't unify the return type of fromInteger with the given function
type, because the latter involves foralls.  So we're left with
    (Num alpha, alpha ~ (forall s. GenST s) -> Int)
Now we do defaulting, get alpha := Integer, and report that we can't
match Integer with (forall s. GenST s) -> Int.  That's not totally
stupid, but perhaps a little strange.

Another potential alternative would be to suppress *all* non-insoluble
errors if there are *any* insoluble errors, anywhere, but that seems
too drastic.

Note [Must simplify after defaulting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We may have a deeply buried constraint
    (t:*) ~ (a:Open)
which we couldn't solve because of the kind incompatibility, and 'a' is free.
Then when we default 'a' we can solve the constraint.  And we want to do
that before starting in on type classes.  We MUST do it before reporting
errors, because it isn't an error!  Trac #7967 was due to this.

Note [Top-level Defaulting Plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have considered two design choices for where/when to apply defaulting.
   (i) Do it in SimplCheck mode only /whenever/ you try to solve some
       simple constraints, maybe deep inside the context of implications.
       This used to be the case in GHC 7.4.1.
   (ii) Do it in a tight loop at simplifyTop, once all other constraint has
        finished. This is the current story.

Option (i) had many disadvantages:
   a) First it was deep inside the actual solver,
   b) Second it was dependent on the context (Infer a type signature,
      or Check a type signature, or Interactive) since we did not want
      to always start defaulting when inferring (though there is an exception to
      this see Note [Default while Inferring])
   c) It plainly did not work. Consider typecheck/should_compile/DfltProb2.hs:
          f :: Int -> Bool
          f x = const True (\y -> let w :: a -> a
                                      w a = const a (y+1)
                                  in w y)
      We will get an implication constraint (for beta the type of y):
               [untch=beta] forall a. 0 => Num beta
      which we really cannot default /while solving/ the implication, since beta is
      untouchable.

Instead our new defaulting story is to pull defaulting out of the solver loop and
go with option (i), implemented at SimplifyTop. Namely:
     - First have a go at solving the residual constraint of the whole program
     - Try to approximate it with a simple constraint
     - Figure out derived defaulting equations for that simple constraint
     - Go round the loop again if you did manage to get some equations

Now, that has to do with class defaulting. However there exists type variable /kind/
defaulting. Again this is done at the top-level and the plan is:
     - At the top-level, once you had a go at solving the constraint, do
       figure out /all/ the touchable unification variables of the wanted constraints.
     - Apply defaulting to their kinds

More details in Note [DefaultTyVar].
-}

------------------
simplifyAmbiguityCheck :: Type -> WantedConstraints -> TcM ()
simplifyAmbiguityCheck ty wanteds
  = do { traceTc "simplifyAmbiguityCheck {" (text "type = " <+> ppr ty $$ text "wanted = " <+> ppr wanteds)
       ; ev_binds_var <- TcM.newTcEvBinds
       ; zonked_final_wc <- solveWantedsTcMWithEvBinds ev_binds_var wanteds simpl_top
       ; traceTc "End simplifyAmbiguityCheck }" empty

       -- Normally report all errors; but with -XAllowAmbiguousTypes
       -- report only insoluble ones, since they represent genuinely
       -- inaccessible code
       ; allow_ambiguous <- xoptM LangExt.AllowAmbiguousTypes
       ; traceTc "reportUnsolved(ambig) {" empty
       ; unless (allow_ambiguous && not (insolubleWC zonked_final_wc))
                (discardResult (reportUnsolved zonked_final_wc))
       ; traceTc "reportUnsolved(ambig) }" empty

       ; return () }

------------------
simplifyInteractive :: WantedConstraints -> TcM (Bag EvBind)
simplifyInteractive wanteds
  = traceTc "simplifyInteractive" empty >>
    simplifyTop wanteds

------------------
simplifyDefault :: ThetaType    -- Wanted; has no type variables in it
                -> TcM ()       -- Succeeds iff the constraint is soluble
simplifyDefault theta
  = do { traceTc "simplifyInteractive" empty
       ; wanted <- newSimpleWanteds DefaultOrigin theta
       ; (unsolved, _binds) <- solveWantedsTcM (mkSimpleWC wanted)

       ; traceTc "reportUnsolved {" empty
       -- See Note [Deferring coercion errors to runtime]
       ; reportAllUnsolved unsolved
         -- Postcondition of solveWantedsTcM is that returned
         -- constraints are zonked. So Precondition of reportUnsolved
         -- is true.
       ; traceTc "reportUnsolved }" empty

       ; return () }

{-
*********************************************************************************
*                                                                                 *
*                            Inference
*                                                                                 *
***********************************************************************************

Note [Inferring the type of a let-bound variable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f x = rhs

To infer f's type we do the following:
 * Gather the constraints for the RHS with ambient level *one more than*
   the current one.  This is done by the call
        captureConstraints (captureTcLevel (tcMonoBinds...))
   in TcBinds.tcPolyInfer

 * Call simplifyInfer to simplify the constraints and decide what to
   quantify over. We pass in the level used for the RHS constraints,
   here called rhs_tclvl.

This ensures that the implication constraint we generate, if any,
has a strictly-increased level compared to the ambient level outside
the let binding.
-}

simplifyInfer :: TcLevel          -- Used when generating the constraints
              -> Bool                  -- Apply monomorphism restriction
              -> [(Name, TcTauType)]   -- Variables to be generalised,
                                       -- and their tau-types
              -> WantedConstraints
              -> TcM ([TcTyVar],    -- Quantify over these type variables
                      [EvVar],      -- ... and these constraints
                      Bool,         -- The monomorphism restriction did something
                                    --   so the results type is not as general as
                                    --   it could be
                      TcEvBinds)    -- ... binding these evidence variables
simplifyInfer rhs_tclvl apply_mr name_taus wanteds
  | isEmptyWC wanteds
  = do { gbl_tvs <- tcGetGlobalTyVars
       ; qtkvs <- quantifyTyVars gbl_tvs (tyVarsOfTypes (map snd name_taus))
       ; traceTc "simplifyInfer: empty WC" (ppr name_taus $$ ppr qtkvs)
       ; return (qtkvs, [], False, emptyTcEvBinds) }

  | otherwise
  = do { traceTc "simplifyInfer {"  $ vcat
             [ ptext (sLit "binds =") <+> ppr name_taus
             , ptext (sLit "rhs_tclvl =") <+> ppr rhs_tclvl
             , ptext (sLit "apply_mr =") <+> ppr apply_mr
             , ptext (sLit "(unzonked) wanted =") <+> ppr wanteds
             ]

              -- Historical note: Before step 2 we used to have a
              -- HORRIBLE HACK described in Note [Avoid unnecessary
              -- constraint simplification] but, as described in Trac
              -- #4361, we have taken in out now.  That's why we start
              -- with step 2!

              -- Step 2) First try full-blown solving

              -- NB: we must gather up all the bindings from doing
              -- this solving; hence (runTcSWithEvBinds ev_binds_var).
              -- And note that since there are nested implications,
              -- calling solveWanteds will side-effect their evidence
              -- bindings, so we can't just revert to the input
              -- constraint.

       ; ev_binds_var <- TcM.newTcEvBinds
       ; wanted_transformed_incl_derivs <- setTcLevel rhs_tclvl $
                                           runTcSWithEvBinds ev_binds_var (solveWanteds wanteds)
       ; wanted_transformed_incl_derivs <- zonkWC wanted_transformed_incl_derivs

              -- Step 4) Candidates for quantification are an approximation of wanted_transformed
              -- NB: Already the fixpoint of any unifications that may have happened
              -- NB: We do not do any defaulting when inferring a type, this can lead
              -- to less polymorphic types, see Note [Default while Inferring]

       ; tc_lcl_env <- TcRnMonad.getLclEnv
       ; null_ev_binds_var <- TcM.newTcEvBinds
       ; let wanted_transformed = dropDerivedWC wanted_transformed_incl_derivs
       ; quant_pred_candidates   -- Fully zonked
           <- if insolubleWC wanted_transformed_incl_derivs
              then return []   -- See Note [Quantification with errors]
                               -- NB: must include derived errors in this test,
                               --     hence "incl_derivs"

              else do { let quant_cand = approximateWC wanted_transformed
                            meta_tvs   = filter isMetaTyVar (varSetElems (tyVarsOfCts quant_cand))
                      ; gbl_tvs <- tcGetGlobalTyVars
                            -- Miminise quant_cand.  We are not interested in any evidence
                            -- produced, because we are going to simplify wanted_transformed
                            -- again later. All we want here is the predicates over which to
                            -- quantify.
                            --
                            -- If any meta-tyvar unifications take place (unlikely), we'll
                            -- pick that up later.


                      ; WC { wc_simple = simples }
                           <- setTcLevel rhs_tclvl                $
                              runTcSWithEvBinds null_ev_binds_var $
                              do { mapM_ (promoteAndDefaultTyVar rhs_tclvl gbl_tvs) meta_tvs
                                     -- See Note [Promote _and_ default when inferring]
                                 ; solveSimpleWanteds quant_cand }

                      ; return [ ctEvPred ev | ct <- bagToList simples
                                             , let ev = ctEvidence ct
                                             , isWanted ev ] }

       -- NB: quant_pred_candidates is already the fixpoint of any
       --     unifications that may have happened

       ; zonked_taus <- mapM (TcM.zonkTcType . snd) name_taus
       ; let zonked_tau_tvs = tyVarsOfTypes zonked_taus
       ; (promote_tvs, qtvs, bound, mr_bites) <- decideQuantification apply_mr quant_pred_candidates zonked_tau_tvs

       ; outer_tclvl <- TcRnMonad.getTcLevel
       ; runTcSWithEvBinds null_ev_binds_var $  -- runTcS just to get the types right :-(
         mapM_ (promoteTyVar outer_tclvl) (varSetElems promote_tvs)

       ; let minimal_simple_preds = mkMinimalBySCs bound
                  -- See Note [Minimize by Superclasses]
             skol_info = InferSkol [ (name, mkSigmaTy [] minimal_simple_preds ty)
                                   | (name, ty) <- name_taus ]
                        -- Don't add the quantified variables here, because
                        -- they are also bound in ic_skols and we want them to be
                        -- tidied uniformly

       ; minimal_bound_ev_vars <- mapM TcM.newEvVar minimal_simple_preds
       ; let implic = Implic { ic_tclvl    = rhs_tclvl
                             , ic_skols    = qtvs
                             , ic_no_eqs   = False
                             , ic_given    = minimal_bound_ev_vars
                             , ic_wanted   = wanted_transformed
                             , ic_insol    = False
                             , ic_binds    = ev_binds_var
                             , ic_info     = skol_info
                             , ic_env      = tc_lcl_env }
       ; emitImplication implic

       ; traceTc "} simplifyInfer/produced residual implication for quantification" $
         vcat [ ptext (sLit "quant_pred_candidates =") <+> ppr quant_pred_candidates
              , ptext (sLit "zonked_taus") <+> ppr zonked_taus
              , ptext (sLit "zonked_tau_tvs=") <+> ppr zonked_tau_tvs
              , ptext (sLit "promote_tvs=") <+> ppr promote_tvs
              , ptext (sLit "bound =") <+> ppr bound
              , ptext (sLit "minimal_bound =") <+> vcat [ ppr v <+> dcolon <+> ppr (idType v)
                                                        | v <- minimal_bound_ev_vars]
              , ptext (sLit "mr_bites =") <+> ppr mr_bites
              , ptext (sLit "qtvs =") <+> ppr qtvs
              , ptext (sLit "implic =") <+> ppr implic ]

       ; return ( qtvs, minimal_bound_ev_vars
                , mr_bites,  TcEvBinds ev_binds_var) }

{-
************************************************************************
*                                                                      *
                Quantification
*                                                                      *
************************************************************************

Note [Deciding quantification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the monomorphism restriction does not apply, then we quantify as follows:
  * Take the global tyvars, and "grow" them using the equality constraints
    E.g.  if x:alpha is in the environment, and alpha ~ [beta] (which can
          happen because alpha is untouchable here) then do not quantify over
          beta
    These are the mono_tvs

  * Take the free vars of the tau-type (zonked_tau_tvs) and "grow" them
    using all the constraints, but knocking out the mono_tvs

    The result is poly_qtvs, which we will quantify over.

  * Filter the constraints using quantifyPred and the poly_qtvs

If the MR does apply, mono_tvs includes all the constrained tyvars,
and the quantified constraints are empty.
-}

decideQuantification :: Bool -> [PredType] -> TcTyVarSet
                     -> TcM ( TcTyVarSet      -- Promote these
                            , [TcTyVar]       -- Do quantify over these
                            , [PredType]      -- and these
                            , Bool )          -- Did the MR bite?
-- See Note [Deciding quantification]
decideQuantification apply_mr constraints zonked_tau_tvs
  | apply_mr     -- Apply the Monomorphism restriction
  = do { gbl_tvs <- tcGetGlobalTyVars
       ; let mono_tvs = gbl_tvs `unionVarSet` constrained_tvs
             mr_bites = constrained_tvs `intersectsVarSet` zonked_tau_tvs
             promote_tvs = constrained_tvs `unionVarSet` (zonked_tau_tvs `intersectVarSet` gbl_tvs)
       ; qtvs <- quantifyTyVars mono_tvs zonked_tau_tvs
       ; traceTc "decideQuantification 1" (vcat [ppr constraints, ppr gbl_tvs, ppr mono_tvs, ppr qtvs])
       ; return (promote_tvs, qtvs, [], mr_bites) }

  | otherwise
  = do { gbl_tvs <- tcGetGlobalTyVars
       ; let mono_tvs    = growThetaTyVars (filter isEqPred constraints) gbl_tvs
             poly_qtvs   = growThetaTyVars constraints zonked_tau_tvs
                           `minusVarSet` mono_tvs
             theta       = filter (quantifyPred poly_qtvs) constraints
             promote_tvs = mono_tvs `intersectVarSet` (constrained_tvs `unionVarSet` zonked_tau_tvs)
       ; qtvs <- quantifyTyVars mono_tvs poly_qtvs
       ; traceTc "decideQuantification 2" (vcat [ppr constraints, ppr gbl_tvs, ppr mono_tvs, ppr poly_qtvs, ppr qtvs, ppr theta])
       ; return (promote_tvs, qtvs, theta, False) }
  where
    constrained_tvs = tyVarsOfTypes constraints

------------------
quantifyPred :: TyVarSet           -- Quantifying over these
             -> PredType -> Bool   -- True <=> quantify over this wanted
quantifyPred qtvs pred
  = case classifyPredType pred of
      ClassPred cls tys
         | isIPClass cls    -> True -- See note [Inheriting implicit parameters]
         | otherwise        -> tyVarsOfTypes tys `intersectsVarSet` qtvs
      EqPred NomEq ty1 ty2  -> quant_fun ty1 || quant_fun ty2
        -- representational equality is like a class constraint
      EqPred ReprEq ty1 ty2 -> tyVarsOfTypes [ty1, ty2] `intersectsVarSet` qtvs
      IrredPred ty          -> tyVarsOfType ty `intersectsVarSet` qtvs
      TuplePred {}          -> False
  where
    -- Only quantify over (F tys ~ ty) if tys mentions a quantified variable
    -- In particular, quantifying over (F Int ~ ty) is a bit like quantifying
    -- over (Eq Int); the instance should kick in right here
    quant_fun ty
      = case tcSplitTyConApp_maybe ty of
          Just (tc, tys) | isTypeFamilyTyCon tc
                         -> tyVarsOfTypes tys `intersectsVarSet` qtvs
          _ -> False

------------------
growThetaTyVars :: ThetaType -> TyVarSet -> TyVarSet
-- See Note [Growing the tau-tvs using constraints]
growThetaTyVars theta tvs
  | null theta             = tvs
  | isEmptyVarSet seed_tvs = tvs
  | otherwise              = fixVarSet mk_next seed_tvs
  where
    seed_tvs = tvs `unionVarSet` tyVarsOfTypes ips
    (ips, non_ips) = partition isIPPred theta
                         -- See note [Inheriting implicit parameters]
    mk_next tvs = foldr grow_one tvs non_ips
    grow_one pred tvs
       | pred_tvs `intersectsVarSet` tvs = tvs `unionVarSet` pred_tvs
       | otherwise                       = tvs
       where
         pred_tvs = tyVarsOfType pred

{-
Note [Growing the tau-tvs using constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(growThetaTyVars insts tvs) is the result of extending the set
    of tyvars tvs using all conceivable links from pred

E.g. tvs = {a}, preds = {H [a] b, K (b,Int) c, Eq e}
Then growThetaTyVars preds tvs = {a,b,c}

Notice that
   growThetaTyVars is conservative       if v might be fixed by vs
                                         => v `elem` grow(vs,C)

Note [Inheriting implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:

        f x = (x::Int) + ?y

where f is *not* a top-level binding.
From the RHS of f we'll get the constraint (?y::Int).
There are two types we might infer for f:

        f :: Int -> Int

(so we get ?y from the context of f's definition), or

        f :: (?y::Int) => Int -> Int

At first you might think the first was better, because then
?y behaves like a free variable of the definition, rather than
having to be passed at each call site.  But of course, the WHOLE
IDEA is that ?y should be passed at each call site (that's what
dynamic binding means) so we'd better infer the second.

BOTTOM LINE: when *inferring types* you must quantify over implicit
parameters, *even if* they don't mention the bound type variables.
Reason: because implicit parameters, uniquely, have local instance
declarations. See the predicate quantifyPred.

Note [Quantification with errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we find that the RHS of the definition has some absolutely-insoluble
constraints, we abandon all attempts to find a context to quantify
over, and instead make the function fully-polymorphic in whatever
type we have found.  For two reasons
  a) Minimise downstream errors
  b) Avoid spurious errors from this function

But NB that we must include *derived* errors in the check. Example:
    (a::*) ~ Int#
We get an insoluble derived error *~#, and we don't want to discard
it before doing the isInsolubleWC test!  (Trac #8262)

Note [Default while Inferring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Our current plan is that defaulting only happens at simplifyTop and
not simplifyInfer.  This may lead to some insoluble deferred constraints
Example:

instance D g => C g Int b

constraint inferred = (forall b. 0 => C gamma alpha b) /\ Num alpha
type inferred       = gamma -> gamma

Now, if we try to default (alpha := Int) we will be able to refine the implication to
  (forall b. 0 => C gamma Int b)
which can then be simplified further to
  (forall b. 0 => D gamma)
Finally we /can/ approximate this implication with (D gamma) and infer the quantified
type:  forall g. D g => g -> g

Instead what will currently happen is that we will get a quantified type
(forall g. g -> g) and an implication:
       forall g. 0 => (forall b. 0 => C g alpha b) /\ Num alpha

which, even if the simplifyTop defaults (alpha := Int) we will still be left with an
unsolvable implication:
       forall g. 0 => (forall b. 0 => D g)

The concrete example would be:
       h :: C g a s => g -> a -> ST s a
       f (x::gamma) = (\_ -> x) (runST (h x (undefined::alpha)) + 1)

But it is quite tedious to do defaulting and resolve the implication constraints and
we have not observed code breaking because of the lack of defaulting in inference so
we don't do it for now.



Note [Minimize by Superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we quantify over a constraint, in simplifyInfer we need to
quantify over a constraint that is minimal in some sense: For
instance, if the final wanted constraint is (Eq alpha, Ord alpha),
we'd like to quantify over Ord alpha, because we can just get Eq alpha
from superclass selection from Ord alpha. This minimization is what
mkMinimalBySCs does. Then, simplifyInfer uses the minimal constraint
to check the original wanted.


Note [Avoid unnecessary constraint simplification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -------- NB NB NB (Jun 12) -------------
    This note not longer applies; see the notes with Trac #4361.
    But I'm leaving it in here so we remember the issue.)
    ----------------------------------------
When inferring the type of a let-binding, with simplifyInfer,
try to avoid unnecessarily simplifying class constraints.
Doing so aids sharing, but it also helps with delicate
situations like

   instance C t => C [t] where ..

   f :: C [t] => ....
   f x = let g y = ...(constraint C [t])...
         in ...
When inferring a type for 'g', we don't want to apply the
instance decl, because then we can't satisfy (C t).  So we
just notice that g isn't quantified over 't' and partition
the constraints before simplifying.

This only half-works, but then let-generalisation only half-works.


*********************************************************************************
*                                                                                 *
*                             RULES                                               *
*                                                                                 *
***********************************************************************************

See note [Simplifying RULE constraints] in TcRule

Note [RULE quantification over equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Deciding which equalities to quantify over is tricky:
 * We do not want to quantify over insoluble equalities (Int ~ Bool)
    (a) because we prefer to report a LHS type error
    (b) because if such things end up in 'givens' we get a bogus
        "inaccessible code" error

 * But we do want to quantify over things like (a ~ F b), where
   F is a type function.

The difficulty is that it's hard to tell what is insoluble!
So we see whether the simplification step yielded any type errors,
and if so refrain from quantifying over *any* equalities.
-}

simplifyRule :: RuleName
             -> WantedConstraints       -- Constraints from LHS
             -> WantedConstraints       -- Constraints from RHS
             -> TcM ([EvVar], WantedConstraints)   -- LHS evidence variables
-- See Note [Simplifying RULE constraints] in TcRule
simplifyRule name lhs_wanted rhs_wanted
  = do {         -- We allow ourselves to unify environment
                 -- variables: runTcS runs with topTcLevel
         (resid_wanted, _) <- solveWantedsTcM (lhs_wanted `andWC` rhs_wanted)
                              -- Post: these are zonked and unflattened

       ; zonked_lhs_simples <- TcM.zonkSimples (wc_simple lhs_wanted)
       ; let (q_cts, non_q_cts) = partitionBag quantify_me zonked_lhs_simples
             quantify_me  -- Note [RULE quantification over equalities]
               | insolubleWC resid_wanted = quantify_insol
               | otherwise                = quantify_normal

             quantify_insol ct = not (isEqPred (ctPred ct))

             quantify_normal ct
               | EqPred NomEq t1 t2 <- classifyPredType (ctPred ct)
               = not (t1 `tcEqType` t2)
               | otherwise
               = True

       ; traceTc "simplifyRule" $
         vcat [ ptext (sLit "LHS of rule") <+> doubleQuotes (ftext name)
              , text "zonked_lhs_simples" <+> ppr zonked_lhs_simples
              , text "q_cts"      <+> ppr q_cts
              , text "non_q_cts"  <+> ppr non_q_cts ]

       ; return ( map (ctEvId . ctEvidence) (bagToList q_cts)
                , lhs_wanted { wc_simple = non_q_cts }) }

{-
*********************************************************************************
*                                                                                 *
*                                 Main Simplifier                                 *
*                                                                                 *
***********************************************************************************

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
in TcErrors (with ErrEnv). TcErrors.reportTidyWanteds does not print the errors
and does not fail if -fdefer-type-errors is on, so that we can continue
compilation. The errors are turned into warnings in `reportUnsolved`.

Note [Zonk after solving]
~~~~~~~~~~~~~~~~~~~~~~~~~
We zonk the result immediately after constraint solving, for two reasons:

a) because zonkWC generates evidence, and this is the moment when we
   have a suitable evidence variable to hand.

Note that *after* solving the constraints are typically small, so the
overhead is not great.
-}

solveWantedsTcMWithEvBinds :: EvBindsVar
                           -> WantedConstraints
                           -> (WantedConstraints -> TcS WantedConstraints)
                           -> TcM WantedConstraints
-- Returns a *zonked* result
-- We zonk when we finish primarily to un-flatten out any
-- flatten-skolems etc introduced by canonicalisation of
-- types involving type functions.  Happily the result
-- is typically much smaller than the input, indeed it is
-- often empty.
solveWantedsTcMWithEvBinds ev_binds_var wc tcs_action
  = do { traceTc "solveWantedsTcMWithEvBinds" $ text "wanted=" <+> ppr wc
       ; wc2 <- runTcSWithEvBinds ev_binds_var (tcs_action wc)
       ; zonkWC wc2 }
         -- See Note [Zonk after solving]

solveWantedsTcM :: WantedConstraints -> TcM (WantedConstraints, Bag EvBind)
-- Zonk the input constraints, and simplify them
-- Return the evidence binds in the BagEvBinds result
-- Discards all Derived stuff in result
-- Postcondition: fully zonked and unflattened constraints
solveWantedsTcM wanted
  = do { ev_binds_var <- TcM.newTcEvBinds
       ; wanteds' <- solveWantedsTcMWithEvBinds ev_binds_var wanted solveWantedsAndDrop
       ; binds <- TcRnMonad.getTcEvBinds ev_binds_var
       ; return (wanteds', binds) }

simplifyWantedsTcM :: [Ct] -> TcM WantedConstraints
-- Solve the specified Wanted constraints
-- Discard the evidence binds
-- Discards all Derived stuff in result
-- Postcondition: fully zonked and unflattened constraints
simplifyWantedsTcM wanted
 = do { traceTc "simplifyWantedsTcM {" (ppr wanted)
      ; (result, _) <- runTcS (solveWantedsAndDrop (mkSimpleWC wanted))
      ; result <- TcM.zonkWC result
      ; traceTc "simplifyWantedsTcM }" (ppr result)
      ; return result }

solveWantedsAndDrop :: WantedConstraints -> TcS (WantedConstraints)
-- Since solveWanteds returns the residual WantedConstraints,
-- it should always be called within a runTcS or something similar,
solveWantedsAndDrop wanted = do { wc <- solveWanteds wanted
                                ; return (dropDerivedWC wc) }

solveWanteds :: WantedConstraints -> TcS WantedConstraints
-- so that the inert set doesn't mindlessly propagate.
-- NB: wc_simples may be wanted /or/ derived now
solveWanteds wanteds
  = do { traceTcS "solveWanteds {" (ppr wanteds)

         -- Try the simple bit, including insolubles. Solving insolubles a
         -- second time round is a bit of a waste; but the code is simple
         -- and the program is wrong anyway, and we don't run the danger
         -- of adding Derived insolubles twice; see
         -- TcSMonad Note [Do not add duplicate derived insolubles]
       ; traceTcS "solveSimples {" empty
       ; solved_simples_wanteds <- solveSimples wanteds
       ; traceTcS "solveSimples end }" (ppr solved_simples_wanteds)

       -- solveWanteds iterates when it is able to float equalities
       -- equalities out of one or more of the implications.
       ; final_wanteds <- simpl_loop 1 solved_simples_wanteds

       ; bb <- getTcEvBindsMap
       ; traceTcS "solveWanteds }" $
                 vcat [ text "final wc =" <+> ppr final_wanteds
                      , text "current evbinds  =" <+> ppr (evBindMapBinds bb) ]

       ; return final_wanteds }

solveSimples :: WantedConstraints -> TcS WantedConstraints
-- Solve the wc_simple and wc_insol components of the WantedConstraints
-- Do not affect the inerts
solveSimples (WC { wc_simple = simples, wc_insol = insols, wc_impl = implics })
  = nestTcS $
    do { let all_simples = simples `unionBags` filterBag (not . isDerivedCt) insols
                     -- See Note [Dropping derived constraints] in TcRnTypes for
                     -- why the insolubles may have derived constraints
       ; wc <- solveSimpleWanteds all_simples
       ; return ( wc { wc_impl = implics `unionBags` wc_impl wc } ) }

simpl_loop :: Int
           -> WantedConstraints
           -> TcS WantedConstraints
simpl_loop n wanteds@(WC { wc_simple = simples, wc_insol = insols, wc_impl = implics })
  | n > 10
  = do { traceTcS "solveWanteds: loop!" empty
       ; return wanteds }

  | otherwise
  = do { traceTcS "simpl_loop, iteration" (int n)
       ; (floated_eqs, unsolved_implics) <- solveNestedImplications implics

       ; if isEmptyBag floated_eqs
         then return (wanteds { wc_impl = unsolved_implics })
         else

    do {   -- Put floated_eqs into the current inert set before looping
         (unifs_happened, solve_simple_res)
             <- reportUnifications $
                solveSimples (WC { wc_simple = floated_eqs `unionBags` simples
                                 -- Put floated_eqs first so they get solved first
                                 , wc_insol = emptyBag, wc_impl = emptyBag })

       ; let new_wanteds = solve_simple_res `andWC`
                           WC { wc_simple = emptyBag
                              , wc_insol  = insols
                              , wc_impl   = unsolved_implics }

       ; if   not unifs_happened   -- See Note [Cutting off simpl_loop]
           && isEmptyBag (wc_impl solve_simple_res)
         then return new_wanteds
         else simpl_loop (n+1) new_wanteds } }

solveNestedImplications :: Bag Implication
                        -> TcS (Cts, Bag Implication)
-- Precondition: the TcS inerts may contain unsolved simples which have
-- to be converted to givens before we go inside a nested implication.
solveNestedImplications implics
  | isEmptyBag implics
  = return (emptyBag, emptyBag)
  | otherwise
  = do {
--         inerts <- getTcSInerts
--       ; let thinner_inerts = prepareInertsForImplications inerts
--                 -- See Note [Preparing inert set for implications]
--
           traceTcS "solveNestedImplications starting {" empty
--           vcat [ text "original inerts = " <+> ppr inerts
--                , text "thinner_inerts  = " <+> ppr thinner_inerts ]

       ; (floated_eqs, unsolved_implics)
           <- flatMapBagPairM solveImplication implics

       -- ... and we are back in the original TcS inerts
       -- Notice that the original includes the _insoluble_simples so it was safe to ignore
       -- them in the beginning of this function.
       ; traceTcS "solveNestedImplications end }" $
                  vcat [ text "all floated_eqs ="  <+> ppr floated_eqs
                       , text "unsolved_implics =" <+> ppr unsolved_implics ]

       ; return (floated_eqs, unsolved_implics) }

solveImplication :: Implication    -- Wanted
                 -> TcS (Cts,      -- All wanted or derived floated equalities: var = type
                         Bag Implication) -- Unsolved rest (always empty or singleton)
-- Precondition: The TcS monad contains an empty worklist and given-only inerts
-- which after trying to solve this implication we must restore to their original value
solveImplication imp@(Implic { ic_tclvl  = tclvl
                             , ic_binds  = ev_binds
                             , ic_skols  = skols
                             , ic_given  = givens
                             , ic_wanted = wanteds
                             , ic_info   = info
                             , ic_env    = env })
  = do { inerts <- getTcSInerts
       ; traceTcS "solveImplication {" (ppr imp $$ text "Inerts" <+> ppr inerts)

         -- Solve the nested constraints
       ; (no_given_eqs, residual_wanted)
             <- nestImplicTcS ev_binds tclvl $
               do { solveSimpleGivens (mkGivenLoc tclvl info env) givens

                  ; residual_wanted <- solveWanteds wanteds
                        -- solveWanteds, *not* solveWantedsAndDrop, because
                        -- we want to retain derived equalities so we can float
                        -- them out in floatEqualities

                  ; no_eqs <- getNoGivenEqs tclvl skols

                  ; return (no_eqs, residual_wanted) }

       ; (floated_eqs, final_wanted)
             <- floatEqualities skols no_given_eqs residual_wanted

       ; let res_implic | isEmptyWC final_wanted -- && no_given_eqs
                        = emptyBag  -- Reason for the no_given_eqs: we don't want to
                                    -- lose the "inaccessible code" error message
                                    -- BUT: final_wanted still has the derived insolubles
                                    --      so it should be fine
                        | otherwise
                        = unitBag (imp { ic_no_eqs = no_given_eqs
                                       , ic_wanted = dropDerivedWC final_wanted
                                       , ic_insol  = insolubleWC final_wanted })

       ; evbinds <- getTcEvBindsMap
       ; traceTcS "solveImplication end }" $ vcat
             [ text "no_given_eqs =" <+> ppr no_given_eqs
             , text "floated_eqs =" <+> ppr floated_eqs
             , text "res_implic =" <+> ppr res_implic
             , text "implication evbinds = " <+> ppr (evBindMapBinds evbinds) ]

       ; return (floated_eqs, res_implic) }

{-
Note [Cutting off simpl_loop]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is very important not to iterate in simpl_loop unless there is a chance
of progress.  Trac #8474 is a classic example:

  * There's a deeply-nested chain of implication constraints.
       ?x:alpha => ?y1:beta1 => ... ?yn:betan => [W] ?x:Int

  * From the innermost one we get a [D] alpha ~ Int,
    but alpha is untouchable until we get out to the outermost one

  * We float [D] alpha~Int out (it is in floated_eqs), but since alpha
    is untouchable, the solveInteract in simpl_loop makes no progress

  * So there is no point in attempting to re-solve
       ?yn:betan => [W] ?x:Int
    because we'll just get the same [D] again

  * If we *do* re-solve, we'll get an infinite loop. It is cut off by
    the fixed bound of 10, but solving the next takes 10*10*...*10 (ie
    exponentially many) iterations!

Conclusion: we should iterate simpl_loop iff we will get more 'givens'
in the inert set when solving the nested implications.  That is the
result of prepareInertsForImplications is larger.  How can we tell
this?

Consider floated_eqs (all wanted or derived):

(a) [W/D] CTyEqCan (a ~ ty).  This can give rise to a new given only by causing
    a unification. So we count those unifications.

(b) [W] CFunEqCan (F tys ~ xi).  Even though these are wanted, they
    are pushed in as givens by prepareInertsForImplications.  See Note
    [Preparing inert set for implications] in TcSMonad.  But because
    of that very fact, we won't generate another copy if we iterate
    simpl_loop.  So we iterate if there any of these
-}

------------------
tcCheckSatisfiability :: Bag EvVar -> TcM Bool
-- Return True if satisfiable, False if definitely contradictory
tcCheckSatisfiability givens
  = do { lcl_env <- TcM.getLclEnv
       ; let given_loc = mkGivenLoc topTcLevel UnkSkol lcl_env
       ; traceTc "checkSatisfiability {" (ppr givens)
       ; (res, _ev_binds) <- runTcS $
             do { solveSimpleGivens given_loc (bagToList givens)
                ; insols <- getInertInsols
                ; return (not (isEmptyBag insols)) }
       ; traceTc "checkSatisfiability }" (ppr res)
       ; return (not res) }

promoteTyVar :: TcLevel -> TcTyVar  -> TcS ()
-- When we float a constraint out of an implication we must restore
-- invariant (MetaTvInv) in Note [TcLevel and untouchable type variables] in TcType
-- See Note [Promoting unification variables]
promoteTyVar tclvl tv
  | isFloatedTouchableMetaTyVar tclvl tv
  = do { cloned_tv <- TcS.cloneMetaTyVar tv
       ; let rhs_tv = setMetaTyVarTcLevel cloned_tv tclvl
       ; setWantedTyBind tv (mkTyVarTy rhs_tv) }
  | otherwise
  = return ()

promoteAndDefaultTyVar :: TcLevel -> TcTyVarSet -> TyVar -> TcS ()
-- See Note [Promote _and_ default when inferring]
promoteAndDefaultTyVar tclvl gbl_tvs tv
  = do { tv1 <- if tv `elemVarSet` gbl_tvs
                then return tv
                else defaultTyVar tv
       ; promoteTyVar tclvl tv1 }

defaultTyVar :: TcTyVar -> TcS TcTyVar
-- Precondition: MetaTyVars only
-- See Note [DefaultTyVar]
defaultTyVar the_tv
  | Just default_k <- defaultKind_maybe (tyVarKind the_tv)
  = do { tv' <- TcS.cloneMetaTyVar the_tv
       ; let new_tv = setTyVarKind tv' default_k
       ; traceTcS "defaultTyVar" (ppr the_tv <+> ppr new_tv)
       ; setWantedTyBind the_tv (mkTyVarTy new_tv)
       ; return new_tv }
             -- Why not directly derived_pred = mkTcEqPred k default_k?
             -- See Note [DefaultTyVar]
             -- We keep the same TcLevel on tv'

  | otherwise = return the_tv    -- The common case

approximateWC :: WantedConstraints -> Cts
-- Postcondition: Wanted or Derived Cts
-- See Note [ApproximateWC]
approximateWC wc
  = float_wc emptyVarSet wc
  where
    float_wc :: TcTyVarSet -> WantedConstraints -> Cts
    float_wc trapping_tvs (WC { wc_simple = simples, wc_impl = implics })
      = filterBag is_floatable simples `unionBags`
        do_bag (float_implic new_trapping_tvs) implics
      where
        new_trapping_tvs = fixVarSet grow trapping_tvs
        is_floatable ct = tyVarsOfCt ct `disjointVarSet` new_trapping_tvs

        grow tvs = foldrBag grow_one tvs simples
        grow_one ct tvs | ct_tvs `intersectsVarSet` tvs = tvs `unionVarSet` ct_tvs
                        | otherwise                     = tvs
                        where
                          ct_tvs = tyVarsOfCt ct

    float_implic :: TcTyVarSet -> Implication -> Cts
    float_implic trapping_tvs imp
      | ic_no_eqs imp                 -- No equalities, so float
      = float_wc new_trapping_tvs (ic_wanted imp)
      | otherwise                     -- Don't float out of equalities
      = emptyCts                      -- See Note [ApproximateWC]
      where
        new_trapping_tvs = trapping_tvs `extendVarSetList` ic_skols imp
    do_bag :: (a -> Bag c) -> Bag a -> Bag c
    do_bag f = foldrBag (unionBags.f) emptyBag

{-
Note [ApproximateWC]
~~~~~~~~~~~~~~~~~~~~
approximateWC takes a constraint, typically arising from the RHS of a
let-binding whose type we are *inferring*, and extracts from it some
*simple* constraints that we might plausibly abstract over.  Of course
the top-level simple constraints are plausible, but we also float constraints
out from inside, if they are not captured by skolems.

The same function is used when doing type-class defaulting (see the call
to applyDefaultingRules) to extract constraints that might be defaulted.

There are two caveats:

1.  We do *not* float anything out if the implication binds equality
    constraints, because that defeats the OutsideIn story.  Consider
       data T a where
         TInt :: T Int
         MkT :: T a

       f TInt = 3::Int

    We get the implication (a ~ Int => res ~ Int), where so far we've decided
      f :: T a -> res
    We don't want to float (res~Int) out because then we'll infer
      f :: T a -> Int
    which is only on of the possible types. (GHC 7.6 accidentally *did*
    float out of such implications, which meant it would happily infer
    non-principal types.)

2. We do not float out an inner constraint that shares a type variable
   (transitively) with one that is trapped by a skolem.  Eg
       forall a.  F a ~ beta, Integral beta
   We don't want to float out (Integral beta).  Doing so would be bad
   when defaulting, because then we'll default beta:=Integer, and that
   makes the error message much worse; we'd get
       Can't solve  F a ~ Integer
   rather than
       Can't solve  Integral (F a)

   Moreover, floating out these "contaminated" constraints doesn't help
   when generalising either. If we generalise over (Integral b), we still
   can't solve the retained implication (forall a. F a ~ b).  Indeed,
   arguably that too would be a harder error to understand.

Note [DefaultTyVar]
~~~~~~~~~~~~~~~~~~~
defaultTyVar is used on any un-instantiated meta type variables to
default the kind of OpenKind and ArgKind etc to *.  This is important
to ensure that instance declarations match.  For example consider

     instance Show (a->b)
     foo x = show (\_ -> True)

Then we'll get a constraint (Show (p ->q)) where p has kind ArgKind,
and that won't match the typeKind (*) in the instance decl.  See tests
tc217 and tc175.

We look only at touchable type variables. No further constraints
are going to affect these type variables, so it's time to do it by
hand.  However we aren't ready to default them fully to () or
whatever, because the type-class defaulting rules have yet to run.

An important point is that if the type variable tv has kind k and the
default is default_k we do not simply generate [D] (k ~ default_k) because:

   (1) k may be ArgKind and default_k may be * so we will fail

   (2) We need to rewrite all occurrences of the tv to be a type
       variable with the right kind and we choose to do this by rewriting
       the type variable /itself/ by a new variable which does have the
       right kind.

Note [Promote _and_ default when inferring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we are inferring a type, we simplify the constraint, and then use
approximateWC to produce a list of candidate constraints.  Then we MUST

  a) Promote any meta-tyvars that have been floated out by
     approximateWC, to restore invariant (MetaTvInv) described in
     Note [TcLevel and untouchable type variables] in TcType.

  b) Default the kind of any meta-tyyvars that are not mentioned in
     in the environment.

To see (b), suppose the constraint is (C ((a :: OpenKind) -> Int)), and we
have an instance (C ((x:*) -> Int)).  The instance doesn't match -- but it
should!  If we don't solve the constraint, we'll stupidly quantify over
(C (a->Int)) and, worse, in doing so zonkQuantifiedTyVar will quantify over
(b:*) instead of (a:OpenKind), which can lead to disaster; see Trac #7332.
Trac #7641 is a simpler example.

Note [Promoting unification variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we float an equality out of an implication we must "promote" free
unification variables of the equality, in order to maintain Invariant
(MetaTvInv) from Note [TcLevel and untouchable type variables] in TcType.  for the
leftover implication.

This is absolutely necessary. Consider the following example. We start
with two implications and a class with a functional dependency.

    class C x y | x -> y
    instance C [a] [a]

    (I1)      [untch=beta]forall b. 0 => F Int ~ [beta]
    (I2)      [untch=beta]forall c. 0 => F Int ~ [[alpha]] /\ C beta [c]

We float (F Int ~ [beta]) out of I1, and we float (F Int ~ [[alpha]]) out of I2.
They may react to yield that (beta := [alpha]) which can then be pushed inwards
the leftover of I2 to get (C [alpha] [a]) which, using the FunDep, will mean that
(alpha := a). In the end we will have the skolem 'b' escaping in the untouchable
beta! Concrete example is in indexed_types/should_fail/ExtraTcsUntch.hs:

    class C x y | x -> y where
     op :: x -> y -> ()

    instance C [a] [a]

    type family F a :: *

    h :: F Int -> ()
    h = undefined

    data TEx where
      TEx :: a -> TEx

    f (x::beta) =
        let g1 :: forall b. b -> ()
            g1 _ = h [x]
            g2 z = case z of TEx y -> (h [[undefined]], op x [y])
        in (g1 '3', g2 undefined)


Note [Solving Family Equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
After we are done with simplification we may be left with constraints of the form:
     [Wanted] F xis ~ beta
If 'beta' is a touchable unification variable not already bound in the TyBinds
then we'd like to create a binding for it, effectively "defaulting" it to be 'F xis'.

When is it ok to do so?
    1) 'beta' must not already be defaulted to something. Example:

           [Wanted] F Int  ~ beta   <~ Will default [beta := F Int]
           [Wanted] F Char ~ beta   <~ Already defaulted, can't default again. We
                                       have to report this as unsolved.

    2) However, we must still do an occurs check when defaulting (F xis ~ beta), to
       set [beta := F xis] only if beta is not among the free variables of xis.

    3) Notice that 'beta' can't be bound in ty binds already because we rewrite RHS
       of type family equations. See Inert Set invariants in TcInteract.

This solving is now happening during zonking, see Note [Unflattening while zonking]
in TcMType.


*********************************************************************************
*                                                                               *
*                          Floating equalities                                  *
*                                                                               *
*********************************************************************************

Note [Float Equalities out of Implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For ordinary pattern matches (including existentials) we float
equalities out of implications, for instance:
     data T where
       MkT :: Eq a => a -> T
     f x y = case x of MkT _ -> (y::Int)
We get the implication constraint (x::T) (y::alpha):
     forall a. [untouchable=alpha] Eq a => alpha ~ Int
We want to float out the equality into a scope where alpha is no
longer untouchable, to solve the implication!

But we cannot float equalities out of implications whose givens may
yield or contain equalities:

      data T a where
        T1 :: T Int
        T2 :: T Bool
        T3 :: T a

      h :: T a -> a -> Int

      f x y = case x of
                T1 -> y::Int
                T2 -> y::Bool
                T3 -> h x y

We generate constraint, for (x::T alpha) and (y :: beta):
   [untouchables = beta] (alpha ~ Int => beta ~ Int)   -- From 1st branch
   [untouchables = beta] (alpha ~ Bool => beta ~ Bool) -- From 2nd branch
   (alpha ~ beta)                                      -- From 3rd branch

If we float the equality (beta ~ Int) outside of the first implication and
the equality (beta ~ Bool) out of the second we get an insoluble constraint.
But if we just leave them inside the implications we unify alpha := beta and
solve everything.

Principle:
    We do not want to float equalities out which may
    need the given *evidence* to become soluble.

Consequence: classes with functional dependencies don't matter (since there is
no evidence for a fundep equality), but equality superclasses do matter (since
they carry evidence).
-}

floatEqualities :: [TcTyVar] -> Bool
                -> WantedConstraints
                -> TcS (Cts, WantedConstraints)
-- Main idea: see Note [Float Equalities out of Implications]
--
-- Precondition: the wc_simple of the incoming WantedConstraints are
--               fully zonked, so that we can see their free variables
--
-- Postcondition: The returned floated constraints (Cts) are only
--                Wanted or Derived and come from the input wanted
--                ev vars or deriveds
--
-- Also performs some unifications (via promoteTyVar), adding to
-- monadically-carried ty_binds. These will be used when processing
-- floated_eqs later
--
-- Subtleties: Note [Float equalities from under a skolem binding]
--             Note [Skolem escape]
floatEqualities skols no_given_eqs wanteds@(WC { wc_simple = simples })
  | not no_given_eqs  -- There are some given equalities, so don't float
  = return (emptyBag, wanteds)   -- Note [Float Equalities out of Implications]
  | otherwise
  = do { outer_tclvl <- TcS.getTcLevel
       ; mapM_ (promoteTyVar outer_tclvl) (varSetElems (tyVarsOfCts float_eqs))
             -- See Note [Promoting unification variables]
       ; traceTcS "floatEqualities" (vcat [ text "Skols =" <+> ppr skols
                                          , text "Simples =" <+> ppr simples
                                          , text "Floated eqs =" <+> ppr float_eqs ])
       ; return (float_eqs, wanteds { wc_simple = remaining_simples }) }
  where
    skol_set = mkVarSet skols
    (float_eqs, remaining_simples) = partitionBag float_me simples

    float_me :: Ct -> Bool
    float_me ct   -- The constraint is un-flattened and de-canonicalised
       | let pred = ctPred ct
       , EqPred NomEq ty1 ty2 <- classifyPredType pred
       , tyVarsOfType pred `disjointVarSet` skol_set
       , useful_to_float ty1 ty2
       = True
       | otherwise
       = False

      -- Float out alpha ~ ty, or ty ~ alpha
      -- which might be unified outside
      -- See Note [Do not float kind-incompatible equalities]
    useful_to_float ty1 ty2
      = case (tcGetTyVar_maybe ty1, tcGetTyVar_maybe ty2) of
          (Just tv1, _) | isMetaTyVar tv1
                        , k2 `isSubKind` k1
                        -> True
          (_, Just tv2) | isMetaTyVar tv2
                        , k1 `isSubKind` k2
                        -> True
          _ -> False
      where
        k1 = typeKind ty1
        k2 = typeKind ty2

{-
Note [Do not float kind-incompatible equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have (t::* ~ s::*->*), we'll get a Derived insoluble equality.
If we float the equality outwards, we'll get *another* Derived
insoluble equality one level out, so the same error will be reported
twice.  So we refrain from floating such equalities

Note [Float equalities from under a skolem binding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Which of the simple equalities can we float out?  Obviously, only
ones that don't mention the skolem-bound variables.  But that is
over-eager. Consider
   [2] forall a. F a beta[1] ~ gamma[2], G beta[1] gamma[2] ~ Int
The second constraint doesn't mention 'a'.  But if we float it
we'll promote gamma[2] to gamma'[1].  Now suppose that we learn that
beta := Bool, and F a Bool = a, and G Bool _ = Int.  Then we'll
we left with the constraint
   [2] forall a. a ~ gamma'[1]
which is insoluble because gamma became untouchable.

Solution: float only constraints that stand a jolly good chance of
being soluble simply by being floated, namely ones of form
      a ~ ty
where 'a' is a currently-untouchable unification variable, but may
become touchable by being floated (perhaps by more than one level).

We had a very complicated rule previously, but this is nice and
simple.  (To see the notes, look at this Note in a version of
TcSimplify prior to Oct 2014).

Note [Skolem escape]
~~~~~~~~~~~~~~~~~~~~
You might worry about skolem escape with all this floating.
For example, consider
    [2] forall a. (a ~ F beta[2] delta,
                   Maybe beta[2] ~ gamma[1])

The (Maybe beta ~ gamma) doesn't mention 'a', so we float it, and
solve with gamma := beta. But what if later delta:=Int, and
  F b Int = b.
Then we'd get a ~ beta[2], and solve to get beta:=a, and now the
skolem has escaped!

But it's ok: when we float (Maybe beta[2] ~ gamma[1]), we promote beta[2]
to beta[1], and that means the (a ~ beta[1]) will be stuck, as it should be.


*********************************************************************************
*                                                                               *
*                          Defaulting and disambiguation                        *
*                                                                               *
*********************************************************************************
-}

applyDefaultingRules :: Cts -> TcS Bool
  -- True <=> I did some defaulting, reflected in ty_binds

-- Return some extra derived equalities, which express the
-- type-class default choice.
applyDefaultingRules wanteds
  | isEmptyBag wanteds
  = return False
  | otherwise
  = do { traceTcS "applyDefaultingRules { " $
                  text "wanteds =" <+> ppr wanteds

       ; info@(default_tys, _) <- getDefaultInfo
       ; let groups = findDefaultableGroups info wanteds
       ; traceTcS "findDefaultableGroups" $ vcat [ text "groups=" <+> ppr groups
                                                 , text "info=" <+> ppr info ]
       ; something_happeneds <- mapM (disambigGroup default_tys) groups

       ; traceTcS "applyDefaultingRules }" (ppr something_happeneds)

       ; return (or something_happeneds) }

findDefaultableGroups
    :: ( [Type]
       , (Bool,Bool) )  -- (Overloaded strings, extended default rules)
    -> Cts              -- Unsolved (wanted or derived)
    -> [[(Ct,Class,TcTyVar)]]
findDefaultableGroups (default_tys, (ovl_strings, extended_defaults)) wanteds
  | null default_tys = []
  | otherwise        = defaultable_groups
  where
    defaultable_groups = filter is_defaultable_group groups
    groups             = equivClasses cmp_tv unaries
    unaries     :: [(Ct, Class, TcTyVar)]  -- (C tv) constraints
    non_unaries :: [Ct]             -- and *other* constraints

    (unaries, non_unaries) = partitionWith find_unary (bagToList wanteds)
        -- Finds unary type-class constraints
        -- But take account of polykinded classes like Typeable,
        -- which may look like (Typeable * (a:*))   (Trac #8931)
    find_unary cc
        | Just (cls,tys)   <- getClassPredTys_maybe (ctPred cc)
        , Just (kinds, ty) <- snocView tys
        , all isKind kinds
        , Just tv <- tcGetTyVar_maybe ty
        , isMetaTyVar tv  -- We might have runtime-skolems in GHCi, and
                          -- we definitely don't want to try to assign to those!
        = Left (cc, cls, tv)
    find_unary cc = Right cc  -- Non unary or non dictionary

    bad_tvs :: TcTyVarSet  -- TyVars mentioned by non-unaries
    bad_tvs = mapUnionVarSet tyVarsOfCt non_unaries

    cmp_tv (_,_,tv1) (_,_,tv2) = tv1 `compare` tv2

    is_defaultable_group ds@((_,_,tv):_)
        = let b1 = isTyConableTyVar tv  -- Note [Avoiding spurious errors]
              b2 = not (tv `elemVarSet` bad_tvs)
              b4 = defaultable_classes [cls | (_,cls,_) <- ds]
          in (b1 && b2 && b4)
    is_defaultable_group [] = panic "defaultable_group"

    defaultable_classes clss
        | extended_defaults = any isInteractiveClass clss
        | otherwise         = all is_std_class clss && (any is_num_class clss)

    -- In interactive mode, or with -XExtendedDefaultRules,
    -- we default Show a to Show () to avoid gratuitous errors on "show []"
    isInteractiveClass cls
        = is_num_class cls || (classKey cls `elem` [showClassKey, eqClassKey, ordClassKey])

    is_num_class cls = isNumericClass cls || (ovl_strings && (cls `hasKey` isStringClassKey))
    -- is_num_class adds IsString to the standard numeric classes,
    -- when -foverloaded-strings is enabled

    is_std_class cls = isStandardClass cls || (ovl_strings && (cls `hasKey` isStringClassKey))
    -- Similarly is_std_class

------------------------------
disambigGroup :: [Type]                  -- The default types
              -> [(Ct, Class, TcTyVar)]  -- All classes of the form (C a)
                                         --  sharing same type variable
              -> TcS Bool   -- True <=> something happened, reflected in ty_binds

disambigGroup []  _grp
  = return False
disambigGroup (default_ty:default_tys) group
  = do { traceTcS "disambigGroup {" (ppr group $$ ppr default_ty)
       ; fake_ev_binds_var <- TcS.newTcEvBinds
       ; given_ev_var      <- TcS.newEvVar (mkTcEqPred (mkTyVarTy the_tv) default_ty)
       ; tclvl             <- TcS.getTcLevel
       ; success <- nestImplicTcS fake_ev_binds_var (pushTcLevel tclvl) $
                    do { solveSimpleGivens loc [given_ev_var]
                       ; residual_wanted <- solveSimpleWanteds wanteds
                       ; return (isEmptyWC residual_wanted) }

       ; if success then
             -- Success: record the type variable binding, and return
             do { setWantedTyBind the_tv default_ty
                ; wrapWarnTcS $ warnDefaulting wanteds default_ty
                ; traceTcS "disambigGroup succeeded }" (ppr default_ty)
                ; return True }
         else
             -- Failure: try with the next type
             do { traceTcS "disambigGroup failed, will try other default types }"
                           (ppr default_ty)
                ; disambigGroup default_tys group } }
  where
    wanteds          = listToBag (map fstOf3 group)
    ((_,_,the_tv):_) = group
    loc = CtLoc { ctl_origin = GivenOrigin UnkSkol
                , ctl_env = panic "disambigGroup:env"
                , ctl_depth = initialSubGoalDepth }

{-
Note [Avoiding spurious errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When doing the unification for defaulting, we check for skolem
type variables, and simply don't default them.  For example:
   f = (*)      -- Monomorphic
   g :: Num a => a -> a
   g x = f x x
Here, we get a complaint when checking the type signature for g,
that g isn't polymorphic enough; but then we get another one when
dealing with the (Num a) context arising from f's definition;
we try to unify a with Int (to default it), but find that it's
already been unified with the rigid variable from g's type sig
-}
