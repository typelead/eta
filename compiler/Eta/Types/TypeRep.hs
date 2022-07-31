{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998

\section[TypeRep]{Type - friends' interface}

Note [The Type-related module hierarchy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Class
  TyCon    imports Class
  TypeRep
  TysPrim  imports TypeRep ( including mkTyConTy )
  Kind     imports TysPrim ( mainly for primitive kinds )
  Type     imports Kind
  Coercion imports Type
-}

{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_HADDOCK hide #-}
-- We expose the relevant stuff from this module via the Type module

module Eta.Types.TypeRep (
        TyThing(..),
        Type(..),
        TyLit(..),
        KindOrType, Kind, SuperKind,
        PredType, ThetaType,      -- Synonyms

        -- Functions over types
        mkTyConTy, mkTyVarTy, mkTyVarTys,
        isLiftedTypeKind, isSuperKind, isTypeVar, isKindVar,

        -- Pretty-printing
        pprType, pprParendType, pprTypeApp, pprTvBndr, pprTvBndrs,
        pprTyThing, pprTyThingCategory, pprSigmaType, pprSigmaTypeExtraCts,
        pprTheta, pprForAll, pprUserForAll,
        pprThetaArrowTy, pprClassPred,
        pprKind, pprParendKind, pprTyLit, suppressKinds,
        TyPrec(..), maybeParen, pprTcApp,
        pprPrefixApp, pprArrowChain, ppr_type,

        -- Free variables
        tyVarsOfType, tyVarsOfTypes, closeOverKinds, varSetElemsKvsFirst,
        tyVarsOfTypeAcc, tyVarsOfTypeList, tyVarsOfTypesAcc, tyVarsOfTypesList,

        -- * Tidying type related things up for printing
        tidyType,      tidyTypes,
        tidyOpenType,  tidyOpenTypes,
        tidyOpenKind,
        tidyTyVarBndr, tidyTyVarBndrs, tidyFreeTyVars,
        tidyOpenTyVar, tidyOpenTyVars,
        tidyTyVarOcc,
        tidyTopType,
        tidyKind, noFreeVarsOfType,

        -- Substitutions
        TvSubst(..), TvSubstEnv
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Eta.BasicTypes.DataCon( dataConTyCon )
import Eta.BasicTypes.ConLike ( ConLike(..) )
import {-# SOURCE #-} Eta.Types.Type( isPredTy ) -- Transitively pulls in a LOT of stuff, better to break the loop

-- friends:
import Eta.BasicTypes.Var
import Eta.BasicTypes.VarEnv
import Eta.BasicTypes.VarSet
import Eta.BasicTypes.Name
import Eta.BasicTypes.BasicTypes
import Eta.Types.TyCon
import Eta.Types.Class
import Eta.Types.CoAxiom
import Eta.Utils.FV
-- others
import Eta.Prelude.PrelNames
import Eta.Utils.Outputable
import Eta.Utils.FastString
import Eta.Utils.Util
import Eta.Main.DynFlags

-- libraries
import Data.List( mapAccumL, partition )
import qualified Data.Data        as Data hiding ( TyCon )

{-
************************************************************************
*                                                                      *
\subsection{The data type}
*                                                                      *
************************************************************************
-}

-- | The key representation of types within the compiler

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
data Type
  = TyVarTy Var -- ^ Vanilla type or kind variable (*never* a coercion variable)

  | AppTy         -- See Note [AppTy invariant]
        Type
        Type            -- ^ Type application to something other than a 'TyCon'. Parameters:
                        --
                        --  1) Function: must /not/ be a 'TyConApp',
                        --     must be another 'AppTy', or 'TyVarTy'
                        --
                        --  2) Argument type

  | TyConApp      -- See Note [AppTy invariant]
        TyCon
        [KindOrType]    -- ^ Application of a 'TyCon', including newtypes /and/ synonyms.
                        -- Invariant: saturated applications of 'FunTyCon' must
                        -- use 'FunTy' and saturated synonyms must use their own
                        -- constructors. However, /unsaturated/ 'FunTyCon's
                        -- do appear as 'TyConApp's.
                        -- Parameters:
                        --
                        -- 1) Type constructor being applied to.
                        --
                        -- 2) Type arguments. Might not have enough type arguments
                        --    here to saturate the constructor.
                        --    Even type synonyms are not necessarily saturated;
                        --    for example unsaturated type synonyms
                        --    can appear as the right hand side of a type synonym.

  | FunTy
        Type
        Type            -- ^ Special case of 'TyConApp': @TyConApp FunTyCon [t1, t2]@
                        -- See Note [Equality-constrained types]

  | ForAllTy
        Var         -- Type or kind variable
        Type            -- ^ A polymorphic type

  | LitTy TyLit     -- ^ Type literals are similar to type constructors.

  deriving (Data.Data, Data.Typeable)


-- NOTE:  Other parts of the code assume that type literals do not contain
-- types or type variables.
data TyLit
  = NumTyLit Integer
  | StrTyLit FastString
  deriving (Eq, Ord, Data.Data, Data.Typeable)

type KindOrType = Type -- See Note [Arguments to type constructors]

-- | The key type representing kinds in the compiler.
-- Invariant: a kind is always in one of these forms:
--
-- > FunTy k1 k2
-- > TyConApp PrimTyCon [...]
-- > TyVar kv   -- (during inference only)
-- > ForAll ... -- (for top-level coercions)
type Kind = Type

-- | "Super kinds", used to help encode 'Kind's as types.
-- Invariant: a super kind is always of this form:
--
-- > TyConApp SuperKindTyCon ...
type SuperKind = Type

{-
Note [The kind invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~
The kinds
   #          UnliftedTypeKind
   OpenKind   super-kind of *, #

can never appear under an arrow or type constructor in a kind; they
can only be at the top level of a kind.  It follows that primitive TyCons,
which have a naughty pseudo-kind
   State# :: * -> #
must always be saturated, so that we can never get a type whose kind
has a UnliftedTypeKind or ArgTypeKind underneath an arrow.

Nor can we abstract over a type variable with any of these kinds.

    k :: = kk | # | ArgKind | (#) | OpenKind
    kk :: = * | kk -> kk | T kk1 ... kkn

So a type variable can only be abstracted kk.

Note [Arguments to type constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because of kind polymorphism, in addition to type application we now
have kind instantiation. We reuse the same notations to do so.

For example:

  Just (* -> *) Maybe
  Right * Nat Zero

are represented by:

  TyConApp (PromotedDataCon Just) [* -> *, Maybe]
  TyConApp (PromotedDataCon Right) [*, Nat, (PromotedDataCon Zero)]

Important note: Nat is used as a *kind* and not as a type. This can be
confusing, since type-level Nat and kind-level Nat are identical. We
use the kind of (PromotedDataCon Right) to know if its arguments are
kinds or types.

This kind instantiation only happens in TyConApp currently.


Note [Equality-constrained types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type   forall ab. (a ~ [b]) => blah
is encoded like this:

   ForAllTy (a:*) $ ForAllTy (b:*) $
   FunTy (TyConApp (~) [a, [b]]) $
   blah

-------------------------------------
                Note [PredTy]
-}

-- | A type of the form @p@ of kind @Constraint@ represents a value whose type is
-- the Haskell predicate @p@, where a predicate is what occurs before
-- the @=>@ in a Haskell type.
--
-- We use 'PredType' as documentation to mark those types that we guarantee to have
-- this kind.
--
-- It can be expanded into its representation, but:
--
-- * The type checker must treat it as opaque
--
-- * The rest of the compiler treats it as transparent
--
-- Consider these examples:
--
-- > f :: (Eq a) => a -> Int
-- > g :: (?x :: Int -> Int) => a -> Int
-- > h :: (r\l) => {r} => {l::Int | r}
--
-- Here the @Eq a@ and @?x :: Int -> Int@ and @r\l@ are all called \"predicates\"
type PredType = Type

-- | A collection of 'PredType's
type ThetaType = [PredType]

{-
(We don't support TREX records yet, but the setup is designed
to expand to allow them.)

A Haskell qualified type, such as that for f,g,h above, is
represented using
        * a FunTy for the double arrow
        * with a type of kind Constraint as the function argument

The predicate really does turn into a real extra argument to the
function.  If the argument has type (p :: Constraint) then the predicate p is
represented by evidence of type p.

************************************************************************
*                                                                      *
            Simple constructors
*                                                                      *
************************************************************************

These functions are here so that they can be used by TysPrim,
which in turn is imported by Type
-}

mkTyVarTy  :: TyVar   -> Type
mkTyVarTy  = TyVarTy

mkTyVarTys :: [TyVar] -> [Type]
mkTyVarTys = map mkTyVarTy -- a common use of mkTyVarTy

-- | Create the plain type constructor type which has been applied to no type arguments at all.
mkTyConTy :: TyCon -> Type
mkTyConTy tycon = TyConApp tycon []

-- Some basic functions, put here to break loops eg with the pretty printer

isLiftedTypeKind :: Kind -> Bool
isLiftedTypeKind (TyConApp tc []) = tc `hasKey` liftedTypeKindTyConKey
isLiftedTypeKind _                = False

-- | Is this a super-kind (i.e. a type-of-kinds)?
isSuperKind :: Type -> Bool
isSuperKind (TyConApp skc []) = skc `hasKey` superKindTyConKey
isSuperKind _                 = False

isTypeVar :: Var -> Bool
isTypeVar v = isTKVar v && not (isSuperKind (varType v))

isKindVar :: Var -> Bool
isKindVar v = isTKVar v && isSuperKind (varType v)

{-
************************************************************************
*                                                                      *
                        Free variables of types and coercions
*                                                                      *
************************************************************************
-}

tyVarsOfType :: Type -> VarSet
-- ^ NB: for type synonyms tyVarsOfType does /not/ expand the synonym
-- tyVarsOfType returns only the free variables of a type
-- For example, tyVarsOfType (a::k) returns {a}, not including the
-- kind variable {k}
tyVarsOfType ty = fvVarSet $ tyVarsOfTypeAcc ty

-- | `tyVarsOfType` that returns free variables of a type in deterministic
-- order. For explanation of why using `VarSet` is not deterministic see
-- Note [Deterministic UniqFM] in UniqDFM.
tyVarsOfTypeList :: Type -> [Var]
tyVarsOfTypeList ty = fvVarList $ tyVarsOfTypeAcc ty

tyVarsOfTypes :: [Type] -> TyVarSet
tyVarsOfTypes tys = fvVarSet $ tyVarsOfTypesAcc tys

tyVarsOfTypesList :: [Type] -> [Var]
tyVarsOfTypesList tys = fvVarList $ tyVarsOfTypesAcc tys


-- | The worker for `tyVarsOfType` and `tyVarsOfTypeList`.
-- The previous implementation used `unionVarSet` which is O(n+m) and can
-- make the function quadratic.
-- It's exported, so that it can be composed with other functions that compute
-- free variables.
tyVarsOfTypeAcc :: Type -> FV
tyVarsOfTypeAcc (TyVarTy v) fv_cand in_scope acc = oneVar v fv_cand in_scope acc
tyVarsOfTypeAcc (TyConApp _ tys) fv_cand in_scope acc =
  tyVarsOfTypesAcc tys fv_cand in_scope acc
tyVarsOfTypeAcc (LitTy {}) fv_cand in_scope acc = emptyFV fv_cand in_scope acc
tyVarsOfTypeAcc (FunTy arg res) fv_cand in_scope acc =
  (tyVarsOfTypeAcc arg `unionFV` tyVarsOfTypeAcc res) fv_cand in_scope acc
tyVarsOfTypeAcc (AppTy fun arg) fv_cand in_scope acc =
  (tyVarsOfTypeAcc fun `unionFV` tyVarsOfTypeAcc arg) fv_cand in_scope acc
tyVarsOfTypeAcc (ForAllTy tyvar ty) fv_cand in_scope acc =
  (delFV tyvar (tyVarsOfTypeAcc ty) `unionFV`
    tyVarsOfTypeAcc (tyVarKind tyvar)) fv_cand in_scope acc

tyVarsOfTypesAcc :: [Type] -> FV
tyVarsOfTypesAcc (ty:tys) fv_cand in_scope acc =
  (tyVarsOfTypeAcc ty `unionFV` tyVarsOfTypesAcc tys) fv_cand in_scope acc
tyVarsOfTypesAcc [] fv_cand in_scope acc = emptyFV fv_cand in_scope acc

closeOverKinds :: TyVarSet -> TyVarSet
-- Add the kind variables free in the kinds
-- of the tyvars in the given set
closeOverKinds tvs
  = foldVarSet (\tv ktvs -> tyVarsOfType (tyVarKind tv) `unionVarSet` ktvs)
               tvs tvs

varSetElemsKvsFirst :: VarSet -> [TyVar]
-- {k1,a,k2,b} --> [k1,k2,a,b]
varSetElemsKvsFirst set
  = kvs ++ tvs
  where
    (kvs, tvs) = partition isKindVar (varSetElems set)

{-
************************************************************************
*                                                                      *
                        TyThing
*                                                                      *
************************************************************************

Despite the fact that DataCon has to be imported via a hi-boot route,
this module seems the right place for TyThing, because it's needed for
funTyCon and all the types in TysPrim.

Note [ATyCon for classes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Both classes and type constructors are represented in the type environment
as ATyCon.  You can tell the difference, and get to the class, with
   isClassTyCon :: TyCon -> Bool
   tyConClass_maybe :: TyCon -> Maybe Class
The Class and its associated TyCon have the same Name.
-}

-- | A typechecked-thing, essentially anything that has a name
data TyThing
  = AnId     Id
  | AConLike ConLike
  | ATyCon   TyCon       -- TyCons and classes; see Note [ATyCon for classes]
  | ACoAxiom (CoAxiom Branched)
  deriving (Eq, Ord)

instance Outputable TyThing where
  ppr = pprTyThing

pprTyThing :: TyThing -> SDoc
pprTyThing thing = pprTyThingCategory thing <+> quotes (ppr (getName thing))

pprTyThingCategory :: TyThing -> SDoc
pprTyThingCategory (ATyCon tc)
  | isClassTyCon tc = ptext (sLit "Class")
  | otherwise       = ptext (sLit "Type constructor")
pprTyThingCategory (ACoAxiom _) = ptext (sLit "Coercion axiom")
pprTyThingCategory (AnId   _)   = ptext (sLit "Identifier")
pprTyThingCategory (AConLike (RealDataCon _)) = ptext (sLit "Data constructor")
pprTyThingCategory (AConLike (PatSynCon _))  = ptext (sLit "Pattern synonym")


instance NamedThing TyThing where       -- Can't put this with the type
  getName (AnId id)     = getName id    -- decl, because the DataCon instance
  getName (ATyCon tc)   = getName tc    -- isn't visible there
  getName (ACoAxiom cc) = getName cc
  getName (AConLike cl) = getName cl

{-
************************************************************************
*                                                                      *
                        Substitutions
      Data type defined here to avoid unnecessary mutual recursion
*                                                                      *
************************************************************************
-}

-- | Type substitution
--
-- #tvsubst_invariant#
-- The following invariants must hold of a 'TvSubst':
--
-- 1. The in-scope set is needed /only/ to
-- guide the generation of fresh uniques
--
-- 2. In particular, the /kind/ of the type variables in
-- the in-scope set is not relevant
--
-- 3. The substitution is only applied ONCE! This is because
-- in general such application will not reached a fixed point.
data TvSubst
  = TvSubst InScopeSet  -- The in-scope type and kind variables
            TvSubstEnv  -- Substitutes both type and kind variables
        -- See Note [Apply Once]
        -- and Note [Extending the TvSubstEnv]

-- | A substitution of 'Type's for 'TyVar's
--                 and 'Kind's for 'KindVar's
type TvSubstEnv = TyVarEnv Type
        -- A TvSubstEnv is used both inside a TvSubst (with the apply-once
        -- invariant discussed in Note [Apply Once]), and also independently
        -- in the middle of matching, and unification (see Types.Unify)
        -- So you have to look at the context to know if it's idempotent or
        -- apply-once or whatever

{-
Note [Apply Once]
~~~~~~~~~~~~~~~~~
We use TvSubsts to instantiate things, and we might instantiate
        forall a b. ty
\with the types
        [a, b], or [b, a].
So the substitution might go [a->b, b->a].  A similar situation arises in Core
when we find a beta redex like
        (/\ a /\ b -> e) b a
Then we also end up with a substitution that permutes type variables. Other
variations happen to; for example [a -> (a, b)].

        ***************************************************
        *** So a TvSubst must be applied precisely once ***
        ***************************************************

A TvSubst is not idempotent, but, unlike the non-idempotent substitution
we use during unifications, it must not be repeatedly applied.

Note [Extending the TvSubst]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #tvsubst_invariant# for the invariants that must hold.

This invariant allows a short-cut when the TvSubstEnv is empty:
if the TvSubstEnv is empty --- i.e. (isEmptyTvSubt subst) holds ---
then (substTy subst ty) does nothing.

For example, consider:
        (/\a. /\b:(a~Int). ...b..) Int
We substitute Int for 'a'.  The Unique of 'b' does not change, but
nevertheless we add 'b' to the TvSubstEnv, because b's kind does change

This invariant has several crucial consequences:

* In substTyVarBndr, we need extend the TvSubstEnv
        - if the unique has changed
        - or if the kind has changed

* In substTyVar, we do not need to consult the in-scope set;
  the TvSubstEnv is enough

* In substTy, substTheta, we can short-circuit when the TvSubstEnv is empty



************************************************************************
*                                                                      *
                   Pretty-printing types

       Defined very early because of debug printing in assertions
*                                                                      *
************************************************************************

@pprType@ is the standard @Type@ printer; the overloaded @ppr@ function is
defined to use this.  @pprParendType@ is the same, except it puts
parens around the type, except for the atomic cases.  @pprParendType@
works just by setting the initial context precedence very high.

Note [Precedence in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't keep the fixity of type operators in the operator. So the pretty printer
operates the following precedence structure:
   Type constructor application   binds more tightly than
   Operator applications           which bind more tightly than
   Function arrow

So we might see  a :+: T b -> c
meaning          (a :+: (T b)) -> c

Maybe operator applications should bind a bit less tightly?

Anyway, that's the current story, and it is used consistently for Type and HsType
-}

data TyPrec   -- See Note [Precedence in types]

  = TopPrec         -- No parens
  | FunPrec         -- Function args; no parens for tycon apps
  | TyOpPrec        -- Infix operator
  | TyConPrec       -- Tycon args; no parens for atomic
  deriving( Eq, Ord )

maybeParen :: TyPrec -> TyPrec -> SDoc -> SDoc
maybeParen ctxt_prec inner_prec pretty
  | ctxt_prec < inner_prec = pretty
  | otherwise              = parens pretty

------------------
pprType, pprParendType :: Type -> SDoc
pprType       ty = ppr_type TopPrec ty
pprParendType ty = ppr_type TyConPrec ty

pprTyLit :: TyLit -> SDoc
pprTyLit = ppr_tylit TopPrec

pprKind, pprParendKind :: Kind -> SDoc
pprKind       = pprType
pprParendKind = pprParendType

------------
pprClassPred :: Class -> [Type] -> SDoc
pprClassPred clas tys = pprTypeApp (classTyCon clas) tys

------------
pprTheta :: ThetaType -> SDoc
-- pprTheta [pred] = pprPred pred        -- I'm in two minds about this
pprTheta theta  = parens (sep (punctuate comma (map (ppr_type TopPrec) theta)))

pprThetaArrowTy :: ThetaType -> SDoc
pprThetaArrowTy []     = empty
pprThetaArrowTy [pred] = ppr_type TyOpPrec pred <+> darrow
                         -- TyOpPrec:  Num a     => a -> a  does not need parens
                         --      bug   (a :~: b) => a -> b  currently does
                         -- Trac # 9658
pprThetaArrowTy preds  = parens (fsep (punctuate comma (map (ppr_type TopPrec) preds)))
                            <+> darrow
    -- Notice 'fsep' here rather that 'sep', so that
    -- type contexts don't get displayed in a giant column
    -- Rather than
    --  instance (Eq a,
    --            Eq b,
    --            Eq c,
    --            Eq d,
    --            Eq e,
    --            Eq f,
    --            Eq g,
    --            Eq h,
    --            Eq i,
    --            Eq j,
    --            Eq k,
    --            Eq l) =>
    --           Eq (a, b, c, d, e, f, g, h, i, j, k, l)
    -- we get
    --
    --  instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
    --            Eq j, Eq k, Eq l) =>
    --           Eq (a, b, c, d, e, f, g, h, i, j, k, l)

pprThetaArrowTyExtra :: ThetaType -> SDoc
pprThetaArrowTyExtra []    = text "_" <+> darrow
pprThetaArrowTyExtra preds = parens (fsep (punctuate comma xs)) <+> darrow
  where xs = (map (ppr_type TopPrec) preds) ++ [text "_"]
------------------
instance Outputable Type where
    ppr ty = pprType ty

instance Outputable TyLit where
   ppr = pprTyLit

------------------
        -- OK, here's the main printer

ppr_type :: TyPrec -> Type -> SDoc
ppr_type _ (TyVarTy tv)       = ppr_tvar tv
ppr_type p (TyConApp tc tys)  = pprTyTcApp p tc tys
ppr_type p (LitTy l)          = ppr_tylit p l
ppr_type p ty@(ForAllTy {})   = ppr_forall_type p ty

ppr_type p (AppTy t1 t2) = maybeParen p TyConPrec $
                           ppr_type FunPrec t1 <+> ppr_type TyConPrec t2

ppr_type p fun_ty@(FunTy ty1 ty2)
  | isPredTy ty1
  = ppr_forall_type p fun_ty
  | otherwise
  = pprArrowChain p (ppr_type FunPrec ty1 : ppr_fun_tail ty2)
  where
    -- We don't want to lose synonyms, so we mustn't use splitFunTys here.
    ppr_fun_tail (FunTy ty1 ty2)
      | not (isPredTy ty1) = ppr_type FunPrec ty1 : ppr_fun_tail ty2
    ppr_fun_tail other_ty = [ppr_type TopPrec other_ty]


ppr_forall_type :: TyPrec -> Type -> SDoc
ppr_forall_type p ty
  = maybeParen p FunPrec $ ppr_sigma_type True False ty
    -- True <=> we always print the foralls on *nested* quantifiers
    -- Opt_PrintExplicitForalls only affects top-level quantifiers
    -- False <=> we don't print an extra-constraints wildcard

ppr_tvar :: TyVar -> SDoc
ppr_tvar tv  -- Note [Infix type variables]
  = parenSymOcc (getOccName tv) (ppr tv)

ppr_tylit :: TyPrec -> TyLit -> SDoc
ppr_tylit _ tl =
  case tl of
    NumTyLit n -> integer n
    StrTyLit s -> text (show s)

-------------------
ppr_sigma_type :: Bool -> Bool -> Type -> SDoc
-- First Bool <=> Show the foralls unconditionally
-- Second Bool <=> Show an extra-constraints wildcard
ppr_sigma_type show_foralls_unconditionally extra_cts ty
  = sep [ if   show_foralls_unconditionally
          then pprForAll tvs
          else pprUserForAll tvs
        , if extra_cts
          then pprThetaArrowTyExtra ctxt
          else pprThetaArrowTy ctxt
        , pprType tau ]
  where
    (tvs,  rho) = split1 [] ty
    (ctxt, tau) = split2 [] rho

    split1 tvs (ForAllTy tv ty) = split1 (tv:tvs) ty
    split1 tvs ty               = (reverse tvs, ty)

    split2 ps (ty1 `FunTy` ty2) | isPredTy ty1 = split2 (ty1:ps) ty2
    split2 ps ty                               = (reverse ps, ty)

pprSigmaType :: Type -> SDoc
pprSigmaType ty = ppr_sigma_type False False ty

pprSigmaTypeExtraCts :: Bool -> Type -> SDoc
pprSigmaTypeExtraCts = ppr_sigma_type False

pprUserForAll :: [TyVar] -> SDoc
-- Print a user-level forall; see Note [WHen to print foralls]
pprUserForAll tvs
  = sdocWithDynFlags $ \dflags ->
    ppWhen (any tv_has_kind_var tvs || gopt Opt_PrintExplicitForalls dflags) $
    pprForAll tvs
  where
    tv_has_kind_var tv = not (isEmptyVarSet (tyVarsOfType (tyVarKind tv)))

pprForAll :: [TyVar] -> SDoc
pprForAll []  = empty
pprForAll tvs = forAllLit <+> pprTvBndrs tvs <> dot

pprTvBndrs :: [TyVar] -> SDoc
pprTvBndrs tvs = sep (map pprTvBndr tvs)

pprTvBndr :: TyVar -> SDoc
pprTvBndr tv
  | isLiftedTypeKind kind = ppr_tvar tv
  | otherwise             = parens (ppr_tvar tv <+> dcolon <+> pprKind kind)
             where
               kind = tyVarKind tv

{-
Note [When to print foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Mostly we want to print top-level foralls when (and only when) the user specifies
-fprint-explicit-foralls.  But when kind polymorphism is at work, that suppresses
too much information; see Trac #9018.

So I'm trying out this rule: print explicit foralls if
  a) User specifies -fprint-explicit-foralls, or
  b) Any of the quantified type variables has a kind
     that mentions a kind variable

This catches common situations, such as a type signature
     f :: m a
which means
      f :: forall k. forall (m :: k->*) (a :: k). m a
We really want to see both the "forall k" and the kind signatures
on m and a.  The latter comes from pprTvBndr.

Note [Infix type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
With TypeOperators you can say

   f :: (a ~> b) -> b

and the (~>) is considered a type variable.  However, the type
pretty-printer in this module will just see (a ~> b) as

   App (App (TyVarTy "~>") (TyVarTy "a")) (TyVarTy "b")

So it'll print the type in prefix form.  To avoid confusion we must
remember to parenthesise the operator, thus

   (~>) a b -> b

See Trac #2766.
-}

pprTypeApp :: TyCon -> [Type] -> SDoc
pprTypeApp tc tys = pprTyTcApp TopPrec tc tys
        -- We have to use ppr on the TyCon (not its name)
        -- so that we get promotion quotes in the right place

pprTyTcApp :: TyPrec -> TyCon -> [Type] -> SDoc
-- Used for types only; so that we can make a
-- special case for type-level lists
pprTyTcApp p tc tys
  | tc `hasKey` ipTyConKey
  , [LitTy (StrTyLit n),ty] <- tys
  = maybeParen p FunPrec $
    char '?' <> ftext n <> ptext (sLit "::") <> ppr_type TopPrec ty

  | tc `hasKey` consDataConKey
  , [_kind,ty1,ty2] <- tys
  = sdocWithDynFlags $ \dflags ->
    if gopt Opt_PrintExplicitKinds dflags then pprTcApp  p ppr_type tc tys
                                   else pprTyList p ty1 ty2

  | tc `hasKey` errorMessageTypeErrorFamKey = text "(TypeError ...)"

  | otherwise
  = pprTcApp p ppr_type tc tys

pprTcApp :: TyPrec -> (TyPrec -> a -> SDoc) -> TyCon -> [a] -> SDoc
-- Used for both types and coercions, hence polymorphism
pprTcApp _ pp tc [ty]
  | tc `hasKey` listTyConKey = pprPromotionQuote tc <> brackets   (pp TopPrec ty)
  | tc `hasKey` parrTyConKey = pprPromotionQuote tc <> paBrackets (pp TopPrec ty)

pprTcApp p pp tc tys
  | isTupleTyCon tc && tyConArity tc == length tys
  = pprTupleApp p pp tc tys

  | Just dc <- isPromotedDataCon_maybe tc
  , let dc_tc = dataConTyCon dc
  , isTupleTyCon dc_tc
  , let arity = tyConArity dc_tc    -- E.g. 3 for (,,) k1 k2 k3 t1 t2 t3
        ty_args = drop arity tys    -- Drop the kind args
  , ty_args `lengthIs` arity        -- Result is saturated
  = pprPromotionQuote tc <>
    (tupleParens (tupleTyConSort dc_tc) $
     sep (punctuate comma (map (pp TopPrec) ty_args)))

  | otherwise
  = sdocWithDynFlags (pprTcApp_help p pp tc tys)

pprTupleApp :: TyPrec -> (TyPrec -> a -> SDoc) -> TyCon -> [a] -> SDoc
-- Print a saturated tuple
pprTupleApp p pp tc tys
  | null tys
  , ConstraintTuple <- tupleTyConSort tc
  = maybeParen p TopPrec $
    ppr tc <+> dcolon <+> ppr (tyConKind tc)
  | otherwise
  = pprPromotionQuote tc <>
    tupleParens (tupleTyConSort tc) (sep (punctuate comma (map (pp TopPrec) tys)))

pprTcApp_help :: TyPrec -> (TyPrec -> a -> SDoc) -> TyCon -> [a] -> DynFlags -> SDoc
-- This one has access to the DynFlags
pprTcApp_help p pp tc tys dflags
  | not (isSymOcc (nameOccName (tyConName tc)))
  = pprPrefixApp p (ppr tc) (map (pp TyConPrec) tys_wo_kinds)

  | [ty1,ty2] <- tys_wo_kinds  -- Infix, two arguments;
                               -- we know nothing of precedence though
  = pprInfixApp p pp (ppr tc) ty1 ty2

  |  tc `hasKey` liftedTypeKindTyConKey
  || tc `hasKey` unliftedTypeKindTyConKey
  = ASSERT( null tys ) ppr tc   -- Do not wrap *, # in parens

  | otherwise
  = pprPrefixApp p (parens (ppr tc)) (map (pp TyConPrec) tys_wo_kinds)
  where
    tys_wo_kinds = suppressKinds dflags (tyConKind tc) tys

------------------
suppressKinds :: DynFlags -> Kind -> [a] -> [a]
-- Given the kind of a TyCon, and the args to which it is applied,
-- suppress the args that are kind args
-- C.f. Note [Suppressing kinds] in IfaceType
suppressKinds dflags kind xs
  | gopt Opt_PrintExplicitKinds dflags = xs
  | otherwise                          = suppress kind xs
  where
    suppress (ForAllTy _ kind) (_ : xs) = suppress kind xs
    suppress (FunTy _ res)     (x:xs)   = x : suppress res xs
    suppress _                 xs       = xs

----------------
pprTyList :: TyPrec -> Type -> Type -> SDoc
-- Given a type-level list (t1 ': t2), see if we can print
-- it in list notation [t1, ...].
pprTyList p ty1 ty2
  = case gather ty2 of
      (arg_tys, Nothing) -> char '\'' <> brackets (fsep (punctuate comma
                                            (map (ppr_type TopPrec) (ty1:arg_tys))))
      (arg_tys, Just tl) -> maybeParen p FunPrec $
                            hang (ppr_type FunPrec ty1)
                               2 (fsep [ colon <+> ppr_type FunPrec ty | ty <- arg_tys ++ [tl]])
  where
    gather :: Type -> ([Type], Maybe Type)
     -- (gather ty) = (tys, Nothing) means ty is a list [t1, .., tn]
     --             = (tys, Just tl) means ty is of form t1:t2:...tn:tl
    gather (TyConApp tc tys)
      | tc `hasKey` consDataConKey
      , [_kind, ty1,ty2] <- tys
      , (args, tl) <- gather ty2
      = (ty1:args, tl)
      | tc `hasKey` nilDataConKey
      = ([], Nothing)
    gather ty = ([], Just ty)

----------------
pprInfixApp :: TyPrec -> (TyPrec -> a -> SDoc) -> SDoc -> a -> a -> SDoc
pprInfixApp p pp pp_tc ty1 ty2
  = maybeParen p TyOpPrec $
    sep [pp TyOpPrec ty1, pprInfixVar True pp_tc <+> pp TyOpPrec ty2]

pprPrefixApp :: TyPrec -> SDoc -> [SDoc] -> SDoc
pprPrefixApp p pp_fun pp_tys
  | null pp_tys = pp_fun
  | otherwise   = maybeParen p TyConPrec $
                  hang pp_fun 2 (sep pp_tys)

----------------
pprArrowChain :: TyPrec -> [SDoc] -> SDoc
-- pprArrowChain p [a,b,c]  generates   a -> b -> c
pprArrowChain _ []         = empty
pprArrowChain p (arg:args) = maybeParen p FunPrec $
                             sep [arg, sep (map (arrow <+>) args)]

{-
************************************************************************
*                                                                      *
\subsection{TidyType}
*                                                                      *
************************************************************************

Tidying is here because it has a special case for FlatSkol
-}

-- | This tidies up a type for printing in an error message, or in
-- an interface file.
--
-- It doesn't change the uniques at all, just the print names.
tidyTyVarBndrs :: TidyEnv -> [TyVar] -> (TidyEnv, [TyVar])
tidyTyVarBndrs env tvs = mapAccumL tidyTyVarBndr env tvs

tidyTyVarBndr :: TidyEnv -> TyVar -> (TidyEnv, TyVar)
tidyTyVarBndr tidy_env@(occ_env, subst) tyvar
  = case tidyOccName occ_env occ1 of
      (tidy', occ') -> ((tidy', subst'), tyvar')
        where
          subst' = extendVarEnv subst tyvar tyvar'
          tyvar' = setTyVarKind (setTyVarName tyvar name') kind'
          name'  = tidyNameOcc name occ'
          kind'  = tidyKind tidy_env (tyVarKind tyvar)
  where
    name = tyVarName tyvar
    occ  = getOccName name
    -- System Names are for unification variables;
    -- when we tidy them we give them a trailing "0" (or 1 etc)
    -- so that they don't take precedence for the un-modified name
    -- Plus, indicating a unification variable in this way is a
    -- helpful clue for users
    occ1 | isSystemName name = mkTyVarOcc (occNameString occ ++ "0")
         | otherwise         = occ


---------------
tidyFreeTyVars :: TidyEnv -> TyVarSet -> TidyEnv
-- ^ Add the free 'TyVar's to the env in tidy form,
-- so that we can tidy the type they are free in
tidyFreeTyVars (full_occ_env, var_env) tyvars
  = fst (tidyOpenTyVars (full_occ_env, var_env) (varSetElems tyvars))

        ---------------
tidyOpenTyVars :: TidyEnv -> [TyVar] -> (TidyEnv, [TyVar])
tidyOpenTyVars env tyvars = mapAccumL tidyOpenTyVar env tyvars

---------------
tidyOpenTyVar :: TidyEnv -> TyVar -> (TidyEnv, TyVar)
-- ^ Treat a new 'TyVar' as a binder, and give it a fresh tidy name
-- using the environment if one has not already been allocated. See
-- also 'tidyTyVarBndr'
tidyOpenTyVar env@(_, subst) tyvar
  = case lookupVarEnv subst tyvar of
        Just tyvar' -> (env, tyvar')            -- Already substituted
        Nothing     -> tidyTyVarBndr env tyvar  -- Treat it as a binder

---------------
tidyTyVarOcc :: TidyEnv -> TyVar -> TyVar
tidyTyVarOcc (_, subst) tv
  = case lookupVarEnv subst tv of
        Nothing  -> tv
        Just tv' -> tv'

---------------
tidyTypes :: TidyEnv -> [Type] -> [Type]
tidyTypes env tys = map (tidyType env) tys

---------------
tidyType :: TidyEnv -> Type -> Type
tidyType _   (LitTy n)            = LitTy n
tidyType env (TyVarTy tv)         = TyVarTy (tidyTyVarOcc env tv)
tidyType env (TyConApp tycon tys) = let args = tidyTypes env tys
                                    in args `seqList` TyConApp tycon args
tidyType env (AppTy fun arg)      = (AppTy $! (tidyType env fun)) $! (tidyType env arg)
tidyType env (FunTy fun arg)      = (FunTy $! (tidyType env fun)) $! (tidyType env arg)
tidyType env (ForAllTy tv ty)     = ForAllTy tvp $! (tidyType envp ty)
                                  where
                                    (envp, tvp) = tidyTyVarBndr env tv

---------------
-- | Grabs the free type variables, tidies them
-- and then uses 'tidyType' to work over the type itself
tidyOpenType :: TidyEnv -> Type -> (TidyEnv, Type)
tidyOpenType env ty
  = (env', tidyType (trimmed_occ_env, var_env) ty)
  where
    (env'@(_, var_env), tvs') = tidyOpenTyVars env (tyVarsOfTypeList ty)
    trimmed_occ_env = initTidyOccEnv (map getOccName tvs')
      -- The idea here was that we restrict the new TidyEnv to the
      -- _free_ vars of the type, so that we don't gratuitously rename
      -- the _bound_ variables of the type.

---------------
tidyOpenTypes :: TidyEnv -> [Type] -> (TidyEnv, [Type])
tidyOpenTypes env tys = mapAccumL tidyOpenType env tys

---------------
-- | Calls 'tidyType' on a top-level type (i.e. with an empty tidying environment)
tidyTopType :: Type -> Type
tidyTopType ty = tidyType emptyTidyEnv ty

---------------
tidyOpenKind :: TidyEnv -> Kind -> (TidyEnv, Kind)
tidyOpenKind = tidyOpenType

tidyKind :: TidyEnv -> Kind -> Kind
tidyKind = tidyType

-- | Returns True if this type has no free variables. Should be the same as
-- isEmptyVarSet . tyCoVarsOfType, but faster in the non-forall case.
noFreeVarsOfType :: Type -> Bool
noFreeVarsOfType (TyVarTy _)      = False
noFreeVarsOfType (AppTy t1 t2)    = noFreeVarsOfType t1 && noFreeVarsOfType t2
noFreeVarsOfType (TyConApp _ tys) = all noFreeVarsOfType tys
noFreeVarsOfType ty@(ForAllTy {}) = isEmptyVarSet (tyVarsOfType ty)
noFreeVarsOfType (FunTy t1 t2)    = noFreeVarsOfType t1 && noFreeVarsOfType t2
noFreeVarsOfType (LitTy _)        = True
-- noFreeVarsOfType (CastTy ty co)   = noFreeVarsOfType ty && noFreeVarsOfCo co
-- noFreeVarsOfType (CoercionTy co)  = noFreeVarsOfCo co
