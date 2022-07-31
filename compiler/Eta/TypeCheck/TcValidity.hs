{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}
{-# LANGUAGE CPP #-}

module Eta.TypeCheck.TcValidity (
  Rank, UserTypeCtxt(..), checkValidType, checkValidMonoType,
  expectedKindInCtxt,
  checkValidTheta, checkValidFamPats,
  checkValidInstance, validDerivPred,
  checkInstTermination, checkValidTyFamInst, checkTyFamFreeness,
  checkValidTyFamEqn, checkConsistentFamInst,
  arityErr, badATErr, ClsInfo
  ) where

-- friends:
import Eta.TypeCheck.TcUnify    ( tcSubType_NC )
import Eta.TypeCheck.TcSimplify ( simplifyAmbiguityCheck )
import Eta.Types.TypeRep
import Eta.TypeCheck.TcType
import Eta.TypeCheck.TcMType
import Eta.Prelude.TysWiredIn ( coercibleClass )
import Eta.Types.Type
import Eta.Types.Unify( tcMatchTyX )
import Eta.Types.Kind
import Eta.Types.CoAxiom
import Eta.Types.Class
import Eta.Types.TyCon
-- import Eta.BasicTypes.Unique ( hasKey )

-- others:
import Eta.HsSyn.HsSyn            -- HsType
import Eta.TypeCheck.TcRnMonad        -- TcType, amongst others
import Eta.TypeCheck.FunDeps
import Eta.BasicTypes.Name
import Eta.BasicTypes.VarEnv
import Eta.BasicTypes.VarSet
import Eta.Main.ErrUtils
import Eta.Main.DynFlags
import Eta.Utils.Util
import Eta.Utils.ListSetOps
import Eta.BasicTypes.SrcLoc
import Eta.Utils.Outputable
import qualified Eta.Utils.Outputable as Outputable
import Eta.Utils.FastString
import qualified Eta.LanguageExtensions as LangExt
import Control.Monad
import Data.Maybe
import Data.List        ( (\\) )

#include "HsVersions.h"

{-
************************************************************************
*                                                                      *
          Checking for ambiguity
*                                                                      *
************************************************************************
-}

checkAmbiguity :: UserTypeCtxt -> Type -> TcM ()
checkAmbiguity ctxt ty
  | GhciCtxt <- ctxt    -- Allow ambiguous types in GHCi's :kind command
  = return ()           -- E.g.   type family T a :: *  -- T :: forall k. k -> *
                        -- Then :k T should work in GHCi, not complain that
                        -- (T k) is ambiguous!

  | InfSigCtxt {} <- ctxt  -- See Note [Validity of inferred types] in TcBinds
  = return ()

  | otherwise
  = do { traceTc "Ambiguity check for" (ppr ty)
       ; let free_tkvs = varSetElemsKvsFirst (closeOverKinds (tyVarsOfType ty))
       ; (subst, _tvs) <- tcInstSkolTyVars free_tkvs
       ; let ty' = substTy subst ty
              -- The type might have free TyVars, esp when the ambiguity check
              -- happens during a call to checkValidType,
              -- so we skolemise them as TcTyVars.
              -- Tiresome; but the type inference engine expects TcTyVars
              -- NB: The free tyvar might be (a::k), so k is also free
              --     and we must skolemise it as well. Hence closeOverKinds.
              --     (Trac #9222)

         -- Solve the constraints eagerly because an ambiguous type
         -- can cause a cascade of further errors.  Since the free
         -- tyvars are skolemised, we can safely use tcSimplifyTop
       ; (_wrap, wanted) <- addErrCtxtM (mk_msg ty') $
                            captureConstraints $
                            tcSubType_NC ctxt ty' ty'
       ; simplifyAmbiguityCheck ty wanted

       ; traceTc "Done ambiguity check for" (ppr ty) }
 where
   mk_msg ty tidy_env
     = do { allow_ambiguous <- xoptM LangExt.AllowAmbiguousTypes
          ; (tidy_env', tidy_ty) <- zonkTidyTcType tidy_env ty
          ; return (tidy_env', mk_msg tidy_ty $$ ppWhen (not allow_ambiguous) ambig_msg) }
     where
       mk_msg ty = pprSigCtxt ctxt (ptext (sLit "the ambiguity check for")) (ppr ty)
       ambig_msg = ptext (sLit "To defer the ambiguity check to use sites, enable AllowAmbiguousTypes")

checkUserTypeError :: Type -> TcM ()
checkUserTypeError = check
 where
 check ty
   | Just (_,msg) <- isUserErrorTy ty = failWithTc (pprUserTypeErrorTy msg)
   | Just (_,ts)  <- splitTyConApp_maybe ty  = mapM_ check ts
   | Just (t1,t2) <- splitAppTy_maybe ty     = check t1 >> check t2
   | otherwise                               = return ()


{-
************************************************************************
*                                                                      *
          Checking validity of a user-defined type
*                                                                      *
************************************************************************

When dealing with a user-written type, we first translate it from an HsType
to a Type, performing kind checking, and then check various things that should
be true about it.  We don't want to perform these checks at the same time
as the initial translation because (a) they are unnecessary for interface-file
types and (b) when checking a mutually recursive group of type and class decls,
we can't "look" at the tycons/classes yet.  Also, the checks are rather
diverse, and used to really mess up the other code.

One thing we check for is 'rank'.

        Rank 0:         monotypes (no foralls)
        Rank 1:         foralls at the front only, Rank 0 inside
        Rank 2:         foralls at the front, Rank 1 on left of fn arrow,

        basic ::= tyvar | T basic ... basic

        r2  ::= forall tvs. cxt => r2a
        r2a ::= r1 -> r2a | basic
        r1  ::= forall tvs. cxt => r0
        r0  ::= r0 -> r0 | basic

Another thing is to check that type synonyms are saturated.
This might not necessarily show up in kind checking.
        type A i = i
        data T k = MkT (k Int)
        f :: T A        -- BAD!
-}

checkValidType :: UserTypeCtxt -> Type -> TcM ()
-- Checks that the type is valid for the given context
-- Not used for instance decls; checkValidInstance instead
checkValidType ctxt ty
  = do { traceTc "checkValidType" (ppr ty <+> text "::" <+> ppr (typeKind ty))
       ; rankn_flag  <- xoptM LangExt.RankNTypes
       ; let gen_rank :: Rank -> Rank
             gen_rank r | rankn_flag = ArbitraryRank
                        | otherwise  = r

             rank1 = gen_rank r1
             rank0 = gen_rank r0

             r0 = rankZeroMonoType
             r1 = LimitedRank True r0

             rank
               = case ctxt of
                 DefaultDeclCtxt-> MustBeMonoType
                 ResSigCtxt     -> MustBeMonoType
                 PatSigCtxt     -> rank0
                 RuleSigCtxt _  -> rank1
                 TySynCtxt _    -> rank0

                 ExprSigCtxt    -> rank1
                 FunSigCtxt _   -> rank1
                 InfSigCtxt _   -> ArbitraryRank        -- Inferred type
                 ConArgCtxt _   -> rank1 -- We are given the type of the entire
                                         -- constructor, hence rank 1

                 ForSigCtxt _   -> rank1
                 SpecInstCtxt   -> rank1
                 ThBrackCtxt    -> rank1
                 GhciCtxt       -> ArbitraryRank
                 _              -> panic "checkValidType"
                                          -- Can't happen; not used for *user* sigs

        -- Check the internal validity of the type itself
       ; check_type ctxt rank ty

        -- Check that the thing has kind Type, and is lifted if necessary.
        -- Do this *after* check_type, because we can't usefully take
        -- the kind of an ill-formed type such as (a~Int)
       ; check_kind ctxt ty

       ; checkUserTypeError ty

       ; traceTc "checkValidType done" (ppr ty <+> text "::" <+> ppr (typeKind ty)) }

checkValidMonoType :: Type -> TcM ()
checkValidMonoType ty = check_mono_type SigmaCtxt MustBeMonoType ty


check_kind :: UserTypeCtxt -> TcType -> TcM ()
-- Check that the type's kind is acceptable for the context
check_kind ctxt ty
  | TySynCtxt {} <- ctxt
  , returnsConstraintKind actual_kind
  = do { ck <- xoptM LangExt.ConstraintKinds
       ; if ck
         then  when (isConstraintKind actual_kind)
                    (do { dflags <- getDynFlags
                        ; check_pred_ty dflags ctxt ty })
         else addErrTc (constraintSynErr actual_kind) }

  | Just k <- expectedKindInCtxt ctxt
  = checkTc (tcIsSubKind actual_kind k) (kindErr actual_kind)

  | otherwise
  = return ()   -- Any kind will do
  where
    actual_kind = typeKind ty

-- Depending on the context, we might accept any kind (for instance, in a TH
-- splice), or only certain kinds (like in type signatures).
expectedKindInCtxt :: UserTypeCtxt -> Maybe Kind
expectedKindInCtxt (TySynCtxt _)  = Nothing -- Any kind will do
expectedKindInCtxt ThBrackCtxt    = Nothing
expectedKindInCtxt GhciCtxt       = Nothing
expectedKindInCtxt (ForSigCtxt _) = Just liftedTypeKind
expectedKindInCtxt InstDeclCtxt   = Just constraintKind
expectedKindInCtxt SpecInstCtxt   = Just constraintKind
expectedKindInCtxt _              = Just openTypeKind

{-
Note [Higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~
Technically
            Int -> forall a. a->a
is still a rank-1 type, but it's not Haskell 98 (Trac #5957).  So the
validity checker allow a forall after an arrow only if we allow it
before -- that is, with Rank2Types or RankNTypes
-}

data Rank = ArbitraryRank         -- Any rank ok

          | LimitedRank   -- Note [Higher rank types]
                 Bool     -- Forall ok at top
                 Rank     -- Use for function arguments

          | MonoType SDoc   -- Monotype, with a suggestion of how it could be a polytype

          | MustBeMonoType  -- Monotype regardless of flags

rankZeroMonoType, tyConArgMonoType, synArgMonoType :: Rank
rankZeroMonoType = MonoType (ptext (sLit "Perhaps you intended to use RankNTypes or Rank2Types"))
tyConArgMonoType = MonoType (ptext (sLit "Perhaps you intended to use ImpredicativeTypes"))
synArgMonoType   = MonoType (ptext (sLit "Perhaps you intended to use LiberalTypeSynonyms"))

funArgResRank :: Rank -> (Rank, Rank)             -- Function argument and result
funArgResRank (LimitedRank _ arg_rank) = (arg_rank, LimitedRank (forAllAllowed arg_rank) arg_rank)
funArgResRank other_rank               = (other_rank, other_rank)

forAllAllowed :: Rank -> Bool
forAllAllowed ArbitraryRank             = True
forAllAllowed (LimitedRank forall_ok _) = forall_ok
forAllAllowed _                         = False

----------------------------------------
check_mono_type :: UserTypeCtxt -> Rank
                -> KindOrType -> TcM () -- No foralls anywhere
                                        -- No unlifted types of any kind
check_mono_type ctxt rank ty
  | isKind ty = return ()  -- IA0_NOTE: Do we need to check kinds?
  | otherwise
   = do { check_type ctxt rank ty
        ; checkTc (not (isUnLiftedType ty)) (unliftedArgErr ty) }

check_type :: UserTypeCtxt -> Rank -> Type -> TcM ()
-- The args say what the *type context* requires, independent
-- of *flag* settings.  You test the flag settings at usage sites.
--
-- Rank is allowed rank for function args
-- Rank 0 means no for-alls anywhere

check_type ctxt rank ty
  | not (null tvs && null theta)
  = do  { checkTc (forAllAllowed rank) (forAllTyErr rank ty)
                -- Reject e.g. (Maybe (?x::Int => Int)),
                -- with a decent error message
        ; check_valid_theta ctxt theta
        ; check_type ctxt rank tau      -- Allow foralls to right of arrow
        ; checkAmbiguity ctxt ty }
  where
    (tvs, theta, tau) = tcSplitSigmaTy ty

check_type _ _ (TyVarTy _) = return ()

check_type ctxt rank (FunTy arg_ty res_ty)
  = do  { check_type ctxt arg_rank arg_ty
        ; check_type ctxt res_rank res_ty }
  where
    (arg_rank, res_rank) = funArgResRank rank

check_type ctxt rank (AppTy ty1 ty2)
  = do  { check_arg_type ctxt rank ty1
        ; check_arg_type ctxt rank ty2 }

check_type ctxt rank ty@(TyConApp tc tys)
  | isTypeSynonymTyCon tc || isTypeFamilyTyCon tc
  = check_syn_tc_app ctxt rank ty tc tys
  | isUnboxedTupleTyCon tc = check_ubx_tuple  ctxt      ty    tys
  | otherwise              = mapM_ (check_arg_type ctxt rank) tys

check_type _ _ (LitTy {}) = return ()

check_type _ _ ty = pprPanic "check_type" (ppr ty)

----------------------------------------
check_syn_tc_app :: UserTypeCtxt -> Rank -> KindOrType
                 -> TyCon -> [KindOrType] -> TcM ()
-- Used for type synonyms and type synonym families,
-- which must be saturated,
-- but not data families, which need not be saturated
check_syn_tc_app ctxt rank ty tc tys
  | tc_arity <= n_args   -- Saturated
       -- Check that the synonym has enough args
       -- This applies equally to open and closed synonyms
       -- It's OK to have an *over-applied* type synonym
       --      data Tree a b = ...
       --      type Foo a = Tree [a]
       --      f :: Foo a b -> ...
  = do  { -- See Note [Liberal type synonyms]
        ; liberal <- xoptM LangExt.LiberalTypeSynonyms
        ; if not liberal || isTypeFamilyTyCon tc then
                -- For H98 and synonym families, do check the type args
                mapM_ check_arg tys

          else  -- In the liberal case (only for closed syns), expand then check
          case tcView ty of
             Just ty' -> check_type ctxt rank ty'
             Nothing  -> pprPanic "check_tau_type" (ppr ty)  }

  | GhciCtxt <- ctxt  -- Accept under-saturated type synonyms in
                      -- GHCi :kind commands; see Trac #7586
  = mapM_ check_arg tys

  | otherwise
  = failWithTc (arityErr flavour (tyConName tc) tc_arity n_args)
  where
    flavour | isTypeFamilyTyCon tc = "Type family"
            | otherwise            = "Type synonym"
    n_args = length tys
    tc_arity  = tyConArity tc
    check_arg | isTypeFamilyTyCon tc = check_arg_type ctxt rank
              | otherwise            = check_mono_type ctxt synArgMonoType

----------------------------------------
check_ubx_tuple :: UserTypeCtxt -> KindOrType
                -> [KindOrType] -> TcM ()
check_ubx_tuple ctxt ty tys
  = do  { ub_tuples_allowed <- xoptM LangExt.UnboxedTuples
        ; checkTc ub_tuples_allowed (ubxArgTyErr ty)

        ; impred <- xoptM LangExt.ImpredicativeTypes
        ; let rank' = if impred then ArbitraryRank else tyConArgMonoType
                -- c.f. check_arg_type
                -- However, args are allowed to be unlifted, or
                -- more unboxed tuples, so can't use check_arg_ty
        ; mapM_ (check_type ctxt rank') tys }

----------------------------------------
check_arg_type :: UserTypeCtxt -> Rank -> KindOrType -> TcM ()
-- The sort of type that can instantiate a type variable,
-- or be the argument of a type constructor.
-- Not an unboxed tuple, but now *can* be a forall (since impredicativity)
-- Other unboxed types are very occasionally allowed as type
-- arguments depending on the kind of the type constructor
--
-- For example, we want to reject things like:
--
--      instance Ord a => Ord (forall s. T s a)
-- and
--      g :: T s (forall b.b)
--
-- NB: unboxed tuples can have polymorphic or unboxed args.
--     This happens in the workers for functions returning
--     product types with polymorphic components.
--     But not in user code.
-- Anyway, they are dealt with by a special case in check_tau_type

check_arg_type ctxt rank ty
  | isKind ty = return ()  -- IA0_NOTE: Do we need to check a kind?
  | otherwise
  = do  { impred <- xoptM LangExt.ImpredicativeTypes
        ; let rank' = case rank of          -- Predictive => must be monotype
                        MustBeMonoType     -> MustBeMonoType  -- Monotype, regardless
                        _other | impred    -> ArbitraryRank
                               | otherwise -> tyConArgMonoType
                        -- Make sure that MustBeMonoType is propagated,
                        -- so that we don't suggest -XImpredicativeTypes in
                        --    (Ord (forall a.a)) => a -> a
                        -- and so that if it Must be a monotype, we check that it is!

        ; check_type ctxt rank' ty
        ; checkTc (not (isUnLiftedType ty)) (unliftedArgErr ty) }
             -- NB the isUnLiftedType test also checks for
             --    T State#
             -- where there is an illegal partial application of State# (which has
             -- kind * -> #); see Note [The kind invariant] in TypeRep

----------------------------------------
forAllTyErr :: Rank -> Type -> SDoc
forAllTyErr rank ty
   = vcat [ hang (ptext (sLit "Illegal polymorphic or qualified type:")) 2 (ppr ty)
          , suggestion ]
  where
    suggestion = case rank of
                   LimitedRank {} -> ptext (sLit "Perhaps you intended to use RankNTypes or Rank2Types")
                   MonoType d     -> d
                   _              -> Outputable.empty -- Polytype is always illegal

unliftedArgErr, ubxArgTyErr :: Type -> SDoc
unliftedArgErr  ty = sep [ptext (sLit "Illegal unlifted type:"), ppr ty]
ubxArgTyErr     ty = sep [ptext (sLit "Illegal unboxed tuple type as function argument:"), ppr ty]

kindErr :: Kind -> SDoc
kindErr kind = sep [ptext (sLit "Expecting an ordinary type, but found a type of kind"), ppr kind]

{-
Note [Liberal type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If -XLiberalTypeSynonyms is on, expand closed type synonyms *before*
doing validity checking.  This allows us to instantiate a synonym defn
with a for-all type, or with a partially-applied type synonym.
        e.g.   type T a b = a
               type S m   = m ()
               f :: S (T Int)
Here, T is partially applied, so it's illegal in H98.  But if you
expand S first, then T we get just
               f :: Int
which is fine.

IMPORTANT: suppose T is a type synonym.  Then we must do validity
checking on an application (T ty1 ty2)

        *either* before expansion (i.e. check ty1, ty2)
        *or* after expansion (i.e. expand T ty1 ty2, and then check)
        BUT NOT BOTH

If we do both, we get exponential behaviour!!

  data TIACons1 i r c = c i ::: r c
  type TIACons2 t x = TIACons1 t (TIACons1 t x)
  type TIACons3 t x = TIACons2 t (TIACons1 t x)
  type TIACons4 t x = TIACons2 t (TIACons2 t x)
  type TIACons7 t x = TIACons4 t (TIACons3 t x)


************************************************************************
*                                                                      *
\subsection{Checking a theta or source type}
*                                                                      *
************************************************************************

Note [Implicit parameters in instance decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Implicit parameters _only_ allowed in type signatures; not in instance
decls, superclasses etc. The reason for not allowing implicit params in
instances is a bit subtle.  If we allowed
  instance (?x::Int, Eq a) => Foo [a] where ...
then when we saw
     (e :: (?x::Int) => t)
it would be unclear how to discharge all the potential uses of the ?x
in e.  For example, a constraint Foo [Int] might come out of e, and
applying the instance decl would show up two uses of ?x.  Trac #8912.
-}

checkValidTheta :: UserTypeCtxt -> ThetaType -> TcM ()
checkValidTheta ctxt theta
  = addErrCtxt (checkThetaCtxt ctxt theta) (check_valid_theta ctxt theta)

-------------------------
check_valid_theta :: UserTypeCtxt -> [PredType] -> TcM ()
check_valid_theta _ []
  = return ()
check_valid_theta ctxt theta
  = do { dflags <- getDynFlags
       ; warnTc (Reason Opt_WarnDuplicateConstraints)
                (wopt Opt_WarnDuplicateConstraints dflags &&
                 notNull dups) (dupPredWarn dups)
       ; mapM_ (check_pred_ty dflags ctxt) theta }
  where
    (_,dups) = removeDups cmpPred theta

-------------------------
check_pred_ty :: DynFlags -> UserTypeCtxt -> PredType -> TcM ()
-- Check the validity of a predicate in a signature
-- Do not look through any type synonyms; any constraint kinded
-- type synonyms have been checked at their definition site
-- C.f. Trac #9838

check_pred_ty dflags ctxt pred
  = do { checkValidMonoType pred
       ; check_pred_help False dflags ctxt pred }

check_pred_help :: Bool    -- True <=> under a type synonym
                -> DynFlags -> UserTypeCtxt
                -> PredType -> TcM ()
check_pred_help under_syn dflags ctxt pred
  | Just pred' <- coreView pred
  = check_pred_help True dflags ctxt pred'
  | otherwise
  = case classifyPredType pred of
      ClassPred cls tys     -> check_class_pred dflags ctxt pred cls tys
      EqPred NomEq _ _      -> check_eq_pred    dflags pred
      EqPred ReprEq ty1 ty2 -> check_repr_eq_pred dflags ctxt pred ty1 ty2
      TuplePred tys         -> check_tuple_pred under_syn dflags ctxt pred tys
      IrredPred _           -> check_irred_pred under_syn dflags ctxt pred

check_class_pred :: DynFlags -> UserTypeCtxt -> PredType -> Class -> [TcType] -> TcM ()
check_class_pred dflags ctxt pred cls tys
  = do {        -- Class predicates are valid in all contexts
       ; checkTc (arity == n_tys) arity_err

       ; checkTc (not (isIPClass cls) || okIPCtxt ctxt)
                 (badIPPred pred)

                -- Check the form of the argument types
       ; check_class_pred_tys dflags ctxt pred tys
       }
  where
    class_name = className cls
    arity      = classArity cls
    n_tys      = length tys
    arity_err  = arityErr "Class" class_name arity n_tys

check_eq_pred :: DynFlags -> PredType -> TcM ()
check_eq_pred dflags pred
  =         -- Equational constraints are valid in all contexts if type
            -- families are permitted
    checkTc (xopt LangExt.TypeFamilies dflags || xopt LangExt.GADTs dflags)
            (eqPredTyErr pred)

check_repr_eq_pred :: DynFlags -> UserTypeCtxt -> PredType
                   -> TcType -> TcType -> TcM ()
check_repr_eq_pred dflags ctxt pred ty1 ty2
  = check_class_pred_tys dflags ctxt pred tys
  where
    tys = [ty1, ty2]

check_tuple_pred :: Bool -> DynFlags -> UserTypeCtxt -> PredType -> [PredType] -> TcM ()
check_tuple_pred under_syn dflags ctxt pred ts
  = do { -- See Note [ConstraintKinds in predicates]
         checkTc (under_syn || xopt LangExt.ConstraintKinds dflags)
                 (predTupleErr pred)
       ; mapM_ (check_pred_help under_syn dflags ctxt) ts }
    -- This case will not normally be executed because without
    -- -XConstraintKinds tuple types are only kind-checked as *

check_irred_pred :: Bool -> DynFlags -> UserTypeCtxt -> PredType -> TcM ()
check_irred_pred under_syn dflags ctxt pred
    -- The predicate looks like (X t1 t2) or (x t1 t2) :: Constraint
    -- where X is a type function
  = do { -- If it looks like (x t1 t2), require ConstraintKinds
         --   see Note [ConstraintKinds in predicates]
         -- But (X t1 t2) is always ok because we just require ConstraintKinds
         -- at the definition site (Trac #9838)
        checkTc (under_syn || xopt LangExt.ConstraintKinds dflags || not (tyvar_head pred))
                (predIrredErr pred)

         -- Make sure it is OK to have an irred pred in this context
         -- See Note [Irreducible predicates in superclasses]
       ; checkTc (xopt LangExt.UndecidableInstances dflags || not (dodgy_superclass ctxt))
                 (predIrredBadCtxtErr pred) }
  where
    dodgy_superclass ctxt
       = case ctxt of { ClassSCCtxt _ -> True; InstDeclCtxt -> True; _ -> False }

{- Note [ConstraintKinds in predicates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Don't check for -XConstraintKinds under a type synonym, because that
was done at the type synonym definition site; see Trac #9838
e.g.   module A where
          type C a = (Eq a, Ix a)   -- Needs -XConstraintKinds
       module B where
          import A
          f :: C a => a -> a        -- Does *not* need -XConstraintKinds

Note [Irreducible predicates in superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Allowing irreducible predicates in class superclasses is somewhat dangerous
because we can write:

 type family Fooish x :: * -> Constraint
 type instance Fooish () = Foo
 class Fooish () a => Foo a where

This will cause the constraint simplifier to loop because every time we canonicalise a
(Foo a) class constraint we add a (Fooish () a) constraint which will be immediately
solved to add+canonicalise another (Foo a) constraint.

It is equally dangerous to allow them in instance heads because in that case the
Paterson conditions may not detect duplication of a type variable or size change. -}

-------------------------
check_class_pred_tys :: DynFlags -> UserTypeCtxt
                     -> PredType -> [KindOrType] -> TcM ()
check_class_pred_tys dflags ctxt pred kts
  = checkTc pred_ok (predTyVarErr pred $$ how_to_allow)
  where
    (_, tys) = span isKind kts  -- see Note [Kind polymorphic type classes]
    flexible_contexts = xopt LangExt.FlexibleContexts dflags
    undecidable_ok = xopt LangExt.UndecidableInstances dflags

    pred_ok = case ctxt of
        SpecInstCtxt -> True    -- {-# SPECIALISE instance Eq (T Int) #-} is fine
        InstDeclCtxt -> flexible_contexts || undecidable_ok || all tcIsTyVarTy tys
                                -- Further checks on head and theta in
                                -- checkInstTermination
        _             -> flexible_contexts || all tyvar_head tys
    how_to_allow = parens (ptext (sLit "Use FlexibleContexts to permit this"))


-------------------------
tyvar_head :: Type -> Bool
tyvar_head ty                   -- Haskell 98 allows predicates of form
  | tcIsTyVarTy ty = True       --      C (a ty1 .. tyn)
  | otherwise                   -- where a is a type variable
  = case tcSplitAppTy_maybe ty of
        Just (ty, _) -> tyvar_head ty
        Nothing      -> False

-------------------------
okIPCtxt :: UserTypeCtxt -> Bool
  -- See Note [Implicit parameters in instance decls]
okIPCtxt (ClassSCCtxt {})  = False
okIPCtxt (InstDeclCtxt {}) = False
okIPCtxt (SpecInstCtxt {}) = False
okIPCtxt _                 = True

badIPPred :: PredType -> SDoc
badIPPred pred = ptext (sLit "Illegal implicit parameter") <+> quotes (ppr pred)

{-
Note [Kind polymorphic type classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MultiParam check:

    class C f where...   -- C :: forall k. k -> Constraint
    instance C Maybe where...

  The dictionary gets type [C * Maybe] even if it's not a MultiParam
  type class.

Flexibility check:

    class C f where...   -- C :: forall k. k -> Constraint
    data D a = D a
    instance C D where

  The dictionary gets type [C * (D *)]. IA0_TODO it should be
  generalized actually.

Note [The ambiguity check for type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
checkAmbiguity is a check on user-supplied type signatures.  It is
*purely* there to report functions that cannot possibly be called.  So for
example we want to reject:
   f :: C a => Int
The idea is there can be no legal calls to 'f' because every call will
give rise to an ambiguous constraint.  We could soundly omit the
ambiguity check on type signatures entirely, at the expense of
delaying ambiguity errors to call sites.  Indeed, the flag
-XAllowAmbiguousTypes switches off the ambiguity check.

What about things like this:
   class D a b | a -> b where ..
   h :: D Int b => Int
The Int may well fix 'b' at the call site, so that signature should
not be rejected.  Moreover, using *visible* fundeps is too
conservative.  Consider
   class X a b where ...
   class D a b | a -> b where ...
   instance D a b => X [a] b where...
   h :: X a b => a -> a
Here h's type looks ambiguous in 'b', but here's a legal call:
   ...(h [True])...
That gives rise to a (X [Bool] beta) constraint, and using the
instance means we need (D Bool beta) and that fixes 'beta' via D's
fundep!

Behind all these special cases there is a simple guiding principle.
Consider

  f :: <type>
  f = ...blah...

  g :: <type>
  g = f

You would think that the definition of g would surely typecheck!
After all f has exactly the same type, and g=f. But in fact f's type
is instantiated and the instantiated constraints are solved against
the originals, so in the case an ambiguous type it won't work.
Consider our earlier example f :: C a => Int.  Then in g's definition,
we'll instantiate to (C alpha) and try to deduce (C alpha) from (C a),
and fail.

So in fact we use this as our *definition* of ambiguity.  We use a
very similar test for *inferred* types, to ensure that they are
unambiguous. See Note [Impedance matching] in TcBinds.

This test is very conveniently implemented by calling
    tcSubType <type> <type>
This neatly takes account of the functional dependency stuff above,
and implicit parameter (see Note [Implicit parameters and ambiguity]).

What about this, though?
   g :: C [a] => Int
Is every call to 'g' ambiguous?  After all, we might have
   instance C [a] where ...
at the call site.  So maybe that type is ok!  Indeed even f's
quintessentially ambiguous type might, just possibly be callable:
with -XFlexibleInstances we could have
  instance C a where ...
and now a call could be legal after all!  Well, we'll reject this
unless the instance is available *here*.

Side note: the ambiguity check is only used for *user* types, not for
types coming from interface files.  The latter can legitimately have
ambiguous types. Example

   class S a where s :: a -> (Int,Int)
   instance S Char where s _ = (1,1)
   f:: S a => [a] -> Int -> (Int,Int)
   f (_::[a]) x = (a*x,b)
        where (a,b) = s (undefined::a)

Here the worker for f gets the type
        fw :: forall a. S a => Int -> (# Int, Int #)

Note [Implicit parameters and ambiguity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Only a *class* predicate can give rise to ambiguity
An *implicit parameter* cannot.  For example:
        foo :: (?x :: [a]) => Int
        foo = length ?x
is fine.  The call site will supply a particular 'x'

Furthermore, the type variables fixed by an implicit parameter
propagate to the others.  E.g.
        foo :: (Show a, ?x::[a]) => Int
        foo = show (?x++?x)
The type of foo looks ambiguous.  But it isn't, because at a call site
we might have
        let ?x = 5::Int in foo
and all is well.  In effect, implicit parameters are, well, parameters,
so we can take their type variables into account as part of the
"tau-tvs" stuff.  This is done in the function 'FunDeps.grow'.
-}

checkThetaCtxt :: UserTypeCtxt -> ThetaType -> SDoc
checkThetaCtxt ctxt theta
  = vcat [ptext (sLit "In the context:") <+> pprTheta theta,
          ptext (sLit "While checking") <+> pprUserTypeCtxt ctxt ]

eqPredTyErr, predTyVarErr, predTupleErr, predIrredErr, predIrredBadCtxtErr :: PredType -> SDoc
eqPredTyErr  pred = ptext (sLit "Illegal equational constraint") <+> pprType pred
                    $$
                    parens (ptext (sLit "Use GADTs or TypeFamilies to permit this"))
predTyVarErr pred  = hang (ptext (sLit "Non type-variable argument"))
                        2 (ptext (sLit "in the constraint:") <+> pprType pred)
predTupleErr pred  = hang (ptext (sLit "Illegal tuple constraint:") <+> pprType pred)
                        2 (parens constraintKindsMsg)
predIrredErr pred  = hang (ptext (sLit "Illegal constraint:") <+> pprType pred)
                        2 (parens constraintKindsMsg)
predIrredBadCtxtErr pred = hang (ptext (sLit "Illegal constraint") <+> quotes (pprType pred)
                                 <+> ptext (sLit "in a superclass/instance context"))
                               2 (parens undecidableMsg)

constraintSynErr :: Type -> SDoc
constraintSynErr kind = hang (ptext (sLit "Illegal constraint synonym of kind:") <+> quotes (ppr kind))
                           2 (parens constraintKindsMsg)

dupPredWarn :: [[PredType]] -> SDoc
dupPredWarn dups   = ptext (sLit "Duplicate constraint(s):") <+> pprWithCommas pprType (map head dups)

arityErr :: Outputable a => String -> a -> Int -> Int -> SDoc
arityErr kind name n m
  = hsep [ text kind, quotes (ppr name), ptext (sLit "should have"),
           n_arguments <> comma, text "but has been given",
           if m==0 then text "none" else int m]
    where
        n_arguments | n == 0 = ptext (sLit "no arguments")
                    | n == 1 = ptext (sLit "1 argument")
                    | True   = hsep [int n, ptext (sLit "arguments")]

{-
************************************************************************
*                                                                      *
\subsection{Checking for a decent instance head type}
*                                                                      *
************************************************************************

@checkValidInstHead@ checks the type {\em and} its syntactic constraints:
it must normally look like: @instance Foo (Tycon a b c ...) ...@

The exceptions to this syntactic checking: (1)~if the @GlasgowExts@
flag is on, or (2)~the instance is imported (they must have been
compiled elsewhere). In these cases, we let them go through anyway.

We can also have instances for functions: @instance Foo (a -> b) ...@.
-}

checkValidInstHead :: UserTypeCtxt -> Class -> [Type] -> TcM ()
checkValidInstHead ctxt clas cls_args
  = do { dflags <- getDynFlags

       ; checkTc (clas `notElem` abstractClasses)
                 (instTypeErr clas cls_args abstract_class_msg)

           -- Check language restrictions;
           -- but not for SPECIALISE instance pragmas
       ; let ty_args = dropWhile isKind cls_args
       ; unless spec_inst_prag $
         do { checkTc (xopt LangExt.TypeSynonymInstances dflags ||
                       all tcInstHeadTyNotSynonym ty_args)
                 (instTypeErr clas cls_args head_type_synonym_msg)
            ; checkTc (xopt LangExt.FlexibleInstances dflags ||
                       all tcInstHeadTyAppAllTyVars ty_args)
                 (instTypeErr clas cls_args head_type_args_tyvars_msg)
            ; checkTc (xopt LangExt.MultiParamTypeClasses dflags ||
                       length ty_args == 1 ||  -- Only count type arguments
                       (xopt LangExt.NullaryTypeClasses dflags &&
                        null ty_args))
                 (instTypeErr clas cls_args head_one_type_msg) }

         -- May not contain type family applications
       ; mapM_ checkTyFamFreeness ty_args

       ; mapM_ checkValidMonoType ty_args
        -- For now, I only allow tau-types (not polytypes) in
        -- the head of an instance decl.
        --      E.g.  instance C (forall a. a->a) is rejected
        -- One could imagine generalising that, but I'm not sure
        -- what all the consequences might be
       }

  where
    spec_inst_prag = case ctxt of { SpecInstCtxt -> True; _ -> False }

    head_type_synonym_msg = parens (
                text "All instance types must be of the form (T t1 ... tn)" $$
                text "where T is not a synonym." $$
                text "Use TypeSynonymInstances if you want to disable this.")

    head_type_args_tyvars_msg = parens (vcat [
                text "All instance types must be of the form (T a1 ... an)",
                text "where a1 ... an are *distinct type variables*,",
                text "and each type variable appears at most once in the instance head.",
                text "Use FlexibleInstances if you want to disable this."])

    head_one_type_msg = parens (
                text "Only one type can be given in an instance head." $$
                text "Use MultiParamTypeClasses if you want to allow more, or zero.")

    abstract_class_msg =
                text "The class is abstract, manual instances are not permitted."

abstractClasses :: [ Class ]
abstractClasses = [ coercibleClass ] -- See Note [Coercible Instances]

instTypeErr :: Class -> [Type] -> SDoc -> SDoc
instTypeErr cls tys msg
  = hang (hang (ptext (sLit "Illegal instance declaration for"))
             2 (quotes (pprClassPred cls tys)))
       2 msg

{-
validDeivPred checks for OK 'deriving' context.  See Note [Exotic
derived instance contexts] in TcDeriv.  However the predicate is
here because it uses sizeTypes, fvTypes.

Also check for a bizarre corner case, when the derived instance decl
would look like
    instance C a b => D (T a) where ...
Note that 'b' isn't a parameter of T.  This gives rise to all sorts of
problems; in particular, it's hard to compare solutions for equality
when finding the fixpoint, and that means the inferContext loop does
not converge.  See Trac #5287.
-}

validDerivPred :: TyVarSet -> PredType -> Bool
validDerivPred tv_set pred
  = case classifyPredType pred of
       ClassPred _ tys       -> check_tys tys
       TuplePred ps          -> all (validDerivPred tv_set) ps
       EqPred {}             -> False  -- reject equality constraints
       _                     -> True   -- Non-class predicates are ok
  where
    check_tys tys = hasNoDups fvs
                    && sizeTypes tys == length fvs
                    && all (`elemVarSet` tv_set) fvs
    fvs = fvType pred

{-
************************************************************************
*                                                                      *
\subsection{Checking instance for termination}
*                                                                      *
************************************************************************
-}

checkValidInstance :: UserTypeCtxt -> LHsType Name -> Type
                   -> TcM ([TyVar], ThetaType, Class, [Type])
checkValidInstance ctxt hs_type ty
  | Just (clas,inst_tys) <- getClassPredTys_maybe tau
  , inst_tys `lengthIs` classArity clas
  = do  { setSrcSpan head_loc (checkValidInstHead ctxt clas inst_tys)
        ; checkValidTheta ctxt theta

        -- The Termination and Coverage Conditions
        -- Check that instance inference will terminate (if we care)
        -- For Haskell 98 this will already have been done by checkValidTheta,
        -- but as we may be using other extensions we need to check.
        --
        -- Note that the Termination Condition is *more conservative* than
        -- the checkAmbiguity test we do on other type signatures
        --   e.g.  Bar a => Bar Int is ambiguous, but it also fails
        --   the termination condition, because 'a' appears more often
        --   in the constraint than in the head
        ; undecidable_ok <- xoptM LangExt.UndecidableInstances
        ; if undecidable_ok
          then checkAmbiguity ctxt ty
          else checkInstTermination inst_tys theta

        ; case (checkInstCoverage undecidable_ok clas theta inst_tys) of
            IsValid  -> return ()   -- Check succeeded
            NotValid msg -> addErrTc (instTypeErr clas inst_tys msg)

        ; return (tvs, theta, clas, inst_tys) }

  | otherwise
  = failWithTc (ptext (sLit "Malformed instance head:") <+> ppr tau)
  where
    (tvs, theta, tau) = tcSplitSigmaTy ty

        -- The location of the "head" of the instance
    head_loc = case hs_type of
                 L _ (HsForAllTy _ _ _ _ (L loc _)) -> loc
                 L loc _                            -> loc

{-
Note [Paterson conditions]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Termination test: the so-called "Paterson conditions" (see Section 5 of
"Understanding functional dependencies via Constraint Handling Rules,
JFP Jan 2007).

We check that each assertion in the context satisfies:
 (1) no variable has more occurrences in the assertion than in the head, and
 (2) the assertion has fewer constructors and variables (taken together
     and counting repetitions) than the head.
This is only needed with -fglasgow-exts, as Haskell 98 restrictions
(which have already been checked) guarantee termination.

The underlying idea is that

    for any ground substitution, each assertion in the
    context has fewer type constructors than the head.
-}

checkInstTermination :: [TcType] -> ThetaType -> TcM ()
-- See Note [Paterson conditions]
checkInstTermination tys theta
  = check_preds theta
  where
   fvs  = fvTypes tys
   size = sizeTypes tys

   check_preds :: [PredType] -> TcM ()
   check_preds preds = mapM_ check preds

   check :: PredType -> TcM ()
   check pred
     = case classifyPredType pred of
         TuplePred preds -> check_preds preds  -- Look inside tuple predicates; Trac #8359
         EqPred {}       -> return ()          -- You can't get from equalities
                                               -- to class predicates, so this is safe
         _other      -- ClassPred, IrredPred
           | not (null bad_tvs)
           -> addErrTc (predUndecErr pred (nomoreMsg bad_tvs) $$ parens undecidableMsg)
           | sizePred pred >= size
           -> addErrTc (predUndecErr pred smallerMsg $$ parens undecidableMsg)
           | otherwise
           -> return ()
     where
        bad_tvs = filterOut isKindVar (fvType pred \\ fvs)
             -- Rightly or wrongly, we only check for
             -- excessive occurrences of *type* variables.
             -- e.g. type instance Demote {T k} a = T (Demote {k} (Any {k}))

predUndecErr :: PredType -> SDoc -> SDoc
predUndecErr pred msg = sep [msg,
                        nest 2 (ptext (sLit "in the constraint:") <+> pprType pred)]

nomoreMsg :: [TcTyVar] -> SDoc
nomoreMsg tvs
  = sep [ ptext (sLit "Variable") <> plural tvs <+> quotes (pprWithCommas ppr tvs)
        , (if isSingleton tvs then ptext (sLit "occurs")
                                  else ptext (sLit "occur"))
          <+> ptext (sLit "more often than in the instance head") ]

smallerMsg, undecidableMsg, constraintKindsMsg :: SDoc
smallerMsg         = ptext (sLit "Constraint is no smaller than the instance head")
undecidableMsg     = ptext (sLit "Use UndecidableInstances to permit this")
constraintKindsMsg = ptext (sLit "Use ConstraintKinds to permit this")

{-
Note [Associated type instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We allow this:
  class C a where
    type T x a
  instance C Int where
    type T (S y) Int = y
    type T Z     Int = Char

Note that
  a) The variable 'x' is not bound by the class decl
  b) 'x' is instantiated to a non-type-variable in the instance
  c) There are several type instance decls for T in the instance

All this is fine.  Of course, you can't give any *more* instances
for (T ty Int) elsewhere, because it's an *associated* type.

Note [Checking consistent instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  class C a b where
    type T a x b

  instance C [p] Int
    type T [p] y Int = (p,y,y)  -- Induces the family instance TyCon
                                --    type TR p y = (p,y,y)

So we
  * Form the mini-envt from the class type variables a,b
    to the instance decl types [p],Int:   [a->[p], b->Int]

  * Look at the tyvars a,x,b of the type family constructor T
    (it shares tyvars with the class C)

  * Apply the mini-event to them, and check that the result is
    consistent with the instance types [p] y Int

We do *not* assume (at this point) the bound variables of
the associated type instance decl are the same as for the parent
instance decl. So, for example,

  instance C [p] Int
    type T [q] y Int = ...

would work equally well. Reason: making the *kind* variables line
up is much harder. Example (Trac #7282):
  class Foo (xs :: [k]) where
     type Bar xs :: *

   instance Foo '[] where
     type Bar '[] = Int
Here the instance decl really looks like
   instance Foo k ('[] k) where
     type Bar k ('[] k) = Int
but the k's are not scoped, and hence won't match Uniques.

So instead we just match structure, with tcMatchTyX, and check
that distinct type variables match 1-1 with distinct type variables.

HOWEVER, we *still* make the instance type variables scope over the
type instances, to pick up non-obvious kinds.  Eg
   class Foo (a :: k) where
      type F a
   instance Foo (b :: k -> k) where
      type F b = Int
Here the instance is kind-indexed and really looks like
      type F (k->k) (b::k->k) = Int
But if the 'b' didn't scope, we would make F's instance too
poly-kinded.
-}

-- | Extra information needed when type-checking associated types. The 'Class' is
-- the enclosing class, and the @VarEnv Type@ maps class variables to their
-- instance types.
type ClsInfo       = (Class, VarEnv Type)

checkConsistentFamInst
               :: Maybe ( Class
                        , VarEnv Type )  -- ^ Class of associated type
                                         -- and instantiation of class TyVars
               -> TyCon              -- ^ Family tycon
               -> [TyVar]            -- ^ Type variables of the family instance
               -> [Type]             -- ^ Type patterns from instance
               -> TcM ()
-- See Note [Checking consistent instantiation]

checkConsistentFamInst Nothing _ _ _ = return ()
checkConsistentFamInst (Just (clas, mini_env)) fam_tc at_tvs at_tys
  = do { -- Check that the associated type indeed comes from this class
         checkTc (Just clas == tyConAssoc_maybe fam_tc)
                 (badATErr (className clas) (tyConName fam_tc))

         -- See Note [Checking consistent instantiation] in TcTyClsDecls
         -- Check right to left, so that we spot type variable
         -- inconsistencies before (more confusing) kind variables
       ; discardResult $ foldrM check_arg emptyTvSubst $
                         tyConTyVars fam_tc `zip` at_tys }
  where
    at_tv_set = mkVarSet at_tvs

    check_arg :: (TyVar, Type) -> TvSubst -> TcM TvSubst
    check_arg (fam_tc_tv, at_ty) subst
      | Just inst_ty <- lookupVarEnv mini_env fam_tc_tv
      = case tcMatchTyX at_tv_set subst at_ty inst_ty of
           Just subst | all_distinct subst -> return subst
           _ -> failWithTc $ wrongATArgErr at_ty inst_ty
                -- No need to instantiate here, because the axiom
                -- uses the same type variables as the associated class
      | otherwise
      = return subst   -- Allow non-type-variable instantiation
                       -- See Note [Associated type instances]

    all_distinct :: TvSubst -> Bool
    -- True if all the variables mapped the substitution
    -- map to *distinct* type *variables*
    all_distinct subst = go [] at_tvs
       where
         go _   []       = True
         go acc (tv:tvs) = case lookupTyVar subst tv of
                             Nothing -> go acc tvs
                             Just ty | Just tv' <- tcGetTyVar_maybe ty
                                     , tv' `notElem` acc
                                     -> go (tv' : acc) tvs
                             _other -> False

badATErr :: Name -> Name -> SDoc
badATErr clas op
  = hsep [ptext (sLit "Class"), quotes (ppr clas),
          ptext (sLit "does not have an associated type"), quotes (ppr op)]

wrongATArgErr :: Type -> Type -> SDoc
wrongATArgErr ty instTy =
  sep [ ptext (sLit "Type indexes must match class instance head")
      , ptext (sLit "Found") <+> quotes (ppr ty)
        <+> ptext (sLit "but expected") <+> quotes (ppr instTy)
      ]

{-
************************************************************************
*                                                                      *
        Checking type instance well-formedness and termination
*                                                                      *
************************************************************************
-}

-- Check that a "type instance" is well-formed (which includes decidability
-- unless -XUndecidableInstances is given).
--
checkValidTyFamInst :: Maybe ( Class, VarEnv Type )
                    -> TyCon -> CoAxBranch -> TcM ()
checkValidTyFamInst mb_clsinfo fam_tc
                    (CoAxBranch { cab_tvs = tvs, cab_lhs = typats
                                , cab_rhs = rhs, cab_loc = loc })
  = checkValidTyFamEqn mb_clsinfo fam_tc tvs typats rhs loc

-- | Do validity checks on a type family equation, including consistency
-- with any enclosing class instance head, termination, and lack of
-- polytypes.
checkValidTyFamEqn :: Maybe ClsInfo
                   -> TyCon   -- ^ of the type family
                   -> [TyVar] -- ^ bound tyvars in the equation
                   -> [Type]  -- ^ type patterns
                   -> Type    -- ^ rhs
                   -> SrcSpan
                   -> TcM ()
checkValidTyFamEqn mb_clsinfo fam_tc tvs typats rhs loc
  = setSrcSpan loc $
    do { checkValidFamPats fam_tc tvs typats

         -- The argument patterns, and RHS, are all boxed tau types
         -- E.g  Reject type family F (a :: k1) :: k2
         --             type instance F (forall a. a->a) = ...
         --             type instance F Int#             = ...
         --             type instance F Int              = forall a. a->a
         --             type instance F Int              = Int#
         -- See Trac #9357
       ; mapM_ checkValidMonoType typats
       ; checkValidMonoType rhs

         -- We have a decidable instance unless otherwise permitted
       ; undecidable_ok <- xoptM LangExt.UndecidableInstances
       ; unless undecidable_ok $
           mapM_ addErrTc (checkFamInstRhs typats (tcTyFamInsts rhs))

         -- Check that type patterns match the class instance head
       ; checkConsistentFamInst mb_clsinfo fam_tc tvs typats }

-- Make sure that each type family application is
--   (1) strictly smaller than the lhs,
--   (2) mentions no type variable more often than the lhs, and
--   (3) does not contain any further type family instances.
--
checkFamInstRhs :: [Type]                  -- lhs
                -> [(TyCon, [Type])]       -- type family instances
                -> [MsgDoc]
checkFamInstRhs lhsTys famInsts
  = mapMaybe check famInsts
  where
   size = sizeTypes lhsTys
   fvs  = fvTypes lhsTys
   check (tc, tys)
      | not (all isTyFamFree tys)
      = Just (famInstUndecErr famInst nestedMsg $$ parens undecidableMsg)
      | not (null bad_tvs)
      = Just (famInstUndecErr famInst (nomoreMsg bad_tvs) $$ parens undecidableMsg)
      | size <= sizeTypes tys
      = Just (famInstUndecErr famInst smallerAppMsg $$ parens undecidableMsg)
      | otherwise
      = Nothing
      where
        famInst = TyConApp tc tys
        bad_tvs = filterOut isKindVar (fvTypes tys \\ fvs)
             -- Rightly or wrongly, we only check for
             -- excessive occurrences of *type* variables.
             -- e.g. type instance Demote {T k} a = T (Demote {k} (Any {k}))

checkValidFamPats :: TyCon -> [TyVar] -> [Type] -> TcM ()
-- Patterns in a 'type instance' or 'data instance' decl should
-- a) contain no type family applications
--    (vanilla synonyms are fine, though)
-- b) properly bind all their free type variables
--    e.g. we disallow (Trac #7536)
--         type T a = Int
--         type instance F (T a) = a
-- c) Have the right number of patterns
checkValidFamPats fam_tc tvs ty_pats
  = ASSERT( length ty_pats == tyConArity fam_tc )
      -- A family instance must have exactly the same number of type
      -- parameters as the family declaration.  You can't write
      --    type family F a :: * -> *
      --    type instance F Int y = y
      -- because then the type (F Int) would be like (\y.y)
      -- But this is checked at the time the axiom is created
    do { mapM_ checkTyFamFreeness ty_pats
       ; let unbound_tvs = filterOut (`elemVarSet` exactTyVarsOfTypes ty_pats) tvs
       ; checkTc (null unbound_tvs) (famPatErr fam_tc unbound_tvs ty_pats) }

-- Ensure that no type family instances occur in a type.
checkTyFamFreeness :: Type -> TcM ()
checkTyFamFreeness ty
  = checkTc (isTyFamFree ty) $
    tyFamInstIllegalErr ty

-- Check that a type does not contain any type family applications.
--
isTyFamFree :: Type -> Bool
isTyFamFree = null . tcTyFamInsts

-- Error messages

tyFamInstIllegalErr :: Type -> SDoc
tyFamInstIllegalErr ty
  = hang (ptext (sLit "Illegal type synonym family application in instance") <>
         colon) 2 $
      ppr ty

famInstUndecErr :: Type -> SDoc -> SDoc
famInstUndecErr ty msg
  = sep [msg,
         nest 2 (ptext (sLit "in the type family application:") <+>
                 pprType ty)]

famPatErr :: TyCon -> [TyVar] -> [Type] -> SDoc
famPatErr fam_tc tvs pats
  = hang (ptext (sLit "Family instance purports to bind type variable") <> plural tvs
          <+> pprQuotedList tvs)
       2 (hang (ptext (sLit "but the real LHS (expanding synonyms) is:"))
             2 (pprTypeApp fam_tc (map expandTypeSynonyms pats) <+> ptext (sLit "= ...")))

nestedMsg, smallerAppMsg :: SDoc
nestedMsg     = ptext (sLit "Nested type family application")
smallerAppMsg = ptext (sLit "Application is no smaller than the instance head")

{-
************************************************************************
*                                                                      *
\subsection{Auxiliary functions}
*                                                                      *
************************************************************************
-}

-- Free variables of a type, retaining repetitions, and expanding synonyms
fvType :: Type -> [TyVar]
fvType ty | Just exp_ty <- tcView ty = fvType exp_ty
fvType (TyVarTy tv)        = [tv]
fvType (TyConApp _ tys)    = fvTypes tys
fvType (LitTy {})          = []
fvType (FunTy arg res)     = fvType arg ++ fvType res
fvType (AppTy fun arg)     = fvType fun ++ fvType arg
fvType (ForAllTy tyvar ty) = filter (/= tyvar) (fvType ty)

fvTypes :: [Type] -> [TyVar]
fvTypes tys                = concat (map fvType tys)

sizeType :: Type -> Int
-- Size of a type: the number of variables and constructors
sizeType ty | Just exp_ty <- tcView ty = sizeType exp_ty
sizeType (TyVarTy {})      = 1
sizeType (TyConApp _ tys)  = sizeTypes tys + 1
sizeType (LitTy {})        = 1
sizeType (FunTy arg res)   = sizeType arg + sizeType res + 1
sizeType (AppTy fun arg)   = sizeType fun + sizeType arg
sizeType (ForAllTy _ ty)   = sizeType ty

sizeTypes :: [Type] -> Int
-- IA0_NOTE: Avoid kinds.
sizeTypes xs = sum (map sizeType tys)
  where tys = filter (not . isKind) xs

-- Size of a predicate
--
-- We are considering whether class constraints terminate.
-- Equality constraints and constraints for the implicit
-- parameter class always terminate so it is safe to say "size 0".
-- (Implicit parameter constraints always terminate because
-- there are no instances for them---they are only solved by
-- "local instances" in expressions).
-- See Trac #4200.
sizePred :: PredType -> Int
sizePred ty = goClass ty
  where
    goClass p | isIPPred p = 0
              | otherwise  = go (classifyPredType p)

    go (ClassPred _ tys') = sizeTypes tys'
    go (EqPred {})        = 0
    go (TuplePred ts)     = sum (map goClass ts)
    go (IrredPred ty)     = sizeType ty

{-
Note [Paterson conditions on PredTypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We are considering whether *class* constraints terminate
(see Note [Paterson conditions]). Precisely, the Paterson conditions
would have us check that "the constraint has fewer constructors and variables
(taken together and counting repetitions) than the head.".

However, we can be a bit more refined by looking at which kind of constraint
this actually is. There are two main tricks:

 1. It seems like it should be OK not to count the tuple type constructor
    for a PredType like (Show a, Eq a) :: Constraint, since we don't
    count the "implicit" tuple in the ThetaType itself.

    In fact, the Paterson test just checks *each component* of the top level
    ThetaType against the size bound, one at a time. By analogy, it should be
    OK to return the size of the *largest* tuple component as the size of the
    whole tuple.

 2. Once we get into an implicit parameter or equality we
    can't get back to a class constraint, so it's safe
    to say "size 0".  See Trac #4200.

NB: we don't want to detect PredTypes in sizeType (and then call
sizePred on them), or we might get an infinite loop if that PredType
is irreducible. See Trac #5581.
-}
