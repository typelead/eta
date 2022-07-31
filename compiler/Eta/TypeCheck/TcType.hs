{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[TcType]{Types used in the typechecker}

This module provides the Type interface for front-end parts of the
compiler.  These parts

        * treat "source types" as opaque:
                newtypes, and predicates are meaningful.
        * look through usage types

The "tc" prefix is for "TypeChecker", because the type checker
is the principal client.
-}

{-# LANGUAGE CPP #-}

module Eta.TypeCheck.TcType (
  --------------------------------
  -- Types
  TcType, TcSigmaType, TcRhoType, TcTauType, TcPredType, TcThetaType,
  TcTyVar, TcTyVarSet, TcKind, TcCoVar,

  -- TcLevel
  TcLevel(..), topTcLevel, pushTcLevel,
  strictlyDeeperThan, sameDepthAs, fskTcLevel,

  --------------------------------
  -- MetaDetails
  UserTypeCtxt(..), pprUserTypeCtxt, pprSigCtxt,
  TcTyVarDetails(..), pprTcTyVarDetails, vanillaSkolemTv, superSkolemTv,
  MetaDetails(Flexi, Indirect), MetaInfo(..),
  isImmutableTyVar, isSkolemTyVar, isMetaTyVar,  isMetaTyVarTy, isTyVarTy,
  isSigTyVar, isOverlappableTyVar,  isTyConableTyVar,
  isFskTyVar, isFmvTyVar, isFlattenTyVar, isReturnTyVar,
  isAmbiguousTyVar, metaTvRef, metaTyVarInfo,
  isFlexi, isIndirect, isRuntimeUnkSkol,
  isTypeVar, isKindVar,
  metaTyVarTcLevel, setMetaTyVarTcLevel, metaTyVarTcLevel_maybe,
  isTouchableMetaTyVar, isTouchableOrFmv,
  isFloatedTouchableMetaTyVar,
  canUnifyWithPolyType,

  --------------------------------
  -- Builders
  mkPhiTy, mkSigmaTy, mkTcEqPred, mkTcReprEqPred, mkTcEqPredRole,

  --------------------------------
  -- Splitters
  -- These are important because they do not look through newtypes
  tcView,
  tcSplitForAllTys, tcSplitPhiTy, tcSplitPredFunTy_maybe,
  tcSplitFunTy_maybe, tcSplitFunTys, tcFunArgTy, tcFunResultTy, tcSplitFunTysN,
  tcSplitTyConApp, tcSplitTyConApp_maybe, tcTyConAppTyCon, tcTyConAppArgs,
  tcSplitAppTy_maybe, tcSplitAppTy, tcSplitAppTys, repSplitAppTy_maybe,
  tcInstHeadTyNotSynonym, tcInstHeadTyAppAllTyVars,
  tcGetTyVar_maybe, tcGetTyVar, nextRole,
  tcSplitSigmaTy, tcDeepSplitSigmaTy_maybe,

  ---------------------------------
  -- Predicates.
  -- Again, newtypes are opaque
  eqType, eqTypes, eqPred, cmpType, cmpTypes, cmpPred, eqTypeX,
  pickyEqType, tcEqType, tcEqKind,
  isSigmaTy, isRhoTy, isOverloadedTy,
  isFloatingTy, isDoubleTy, isFloatTy, isIntTy, isWordTy, isStringTy,
  isIntegerTy, isBoolTy, isUnitTy, isCharTy, isCallStackTy, isCallStackPred,
  isTauTy, isTauTyCon, tcIsTyVarTy, tcIsForAllTy,
  isPredTy, isTyVarClassPred, isTyVarExposed, isTyVarUnderDatatype,

  ---------------------------------
  -- Misc type manipulators
  deNoteType, occurCheckExpand, OccCheckResult(..),
  orphNamesOfType, orphNamesOfDFunHead, orphNamesOfCo,
  orphNamesOfTypes, orphNamesOfCoCon,
  getDFunTyKey,
  evVarPred_maybe, evVarPred,

  ---------------------------------
  -- Predicate types
  mkMinimalBySCs, transSuperClasses, immSuperClasses,

  -- * Finding type instances
  tcTyFamInsts,

  -- * Finding "exact" (non-dead) type variables
  exactTyVarsOfType, exactTyVarsOfTypes,

  ---------------------------------
  -- Foreign import and export
  isFFIArgumentTy,     -- :: DynFlags -> Safety -> Type -> Bool
  isFFIImportResultTy, -- :: DynFlags -> Type -> Bool
  isFFIExportResultTy, -- :: Type -> Bool
  isFFIExternalTy,     -- :: Type -> Bool
  --isFFIDynTy,          -- :: Type -> Type -> Bool
  isFFIPrimArgumentTy, -- :: DynFlags -> Type -> Bool
  isFFIPrimResultTy,   -- :: DynFlags -> Type -> Bool
  --isFFILabelTy,        -- :: Type -> Bool
  --isFFITy,             -- :: Type -> Bool
  isFunPtrTy,          -- :: Type -> Bool
  tcSplitIOType_maybe, -- :: Type -> Maybe Type
  tcSplitJavaType_maybe, -- :: Type -> Maybe Type
  tcSplitExtendsType_maybe, -- :: Type -> Maybe Type
  tcSplitExtendsType, -- :: Type -> Maybe Type
  extendsVars,

  --------------------------------
  -- Rexported from Kind
  Kind, typeKind,
  unliftedTypeKind, liftedTypeKind,
  openTypeKind, constraintKind, mkArrowKind, mkArrowKinds,
  isLiftedTypeKind, isUnliftedTypeKind, isSubOpenTypeKind,
  tcIsSubKind, splitKindFunTys, defaultKind,

  --------------------------------
  -- Rexported from Type
  Type, PredType, ThetaType,
  mkForAllTy, mkForAllTys,
  mkFunTy, mkFunTys, zipFunTys,
  mkTyConApp, mkAppTy, mkAppTys, applyTy, applyTys,
  mkTyVarTy, mkTyVarTys, mkTyConTy,

  isClassPred, isEqPred, isIPPred,
  mkClassPred,
  isDictLikeTy,
  tcSplitDFunTy, tcSplitDFunHead,
  mkEqPred,

  -- Type substitutions
  TvSubst(..),  -- Representation visible to a few friends
  TvSubstEnv, emptyTvSubst,
  mkOpenTvSubst, zipOpenTvSubst, zipTopTvSubst,
  mkTopTvSubst, notElemTvSubst, unionTvSubst,
  getTvSubstEnv, setTvSubstEnv, getTvInScope, extendTvInScope,
  Type.lookupTyVar, Type.extendTvSubst, Type.substTyVarBndr,
  extendTvSubstList, isInScope, mkTvSubst, zipTyEnv,
  Type.substTy, substTys, substTyWith, substTheta, substTyVar, substTyVars,

  isUnLiftedType,       -- Source types are always lifted
  isUnboxedTupleType,   -- Ditto
  isPrimitiveType,

  tyVarsOfType, tyVarsOfTypes, closeOverKinds,
  tyVarsOfTypeList, tyVarsOfTypesList,
  tcTyVarsOfType, tcTyVarsOfTypes,

  --------------------------------
  -- Transforming Types to TcTypes
  toTcType,    -- :: Type -> TcType
  toTcTyVar,   -- :: TyVar -> TcTyVar
  toTcTypeBag, -- :: Bag EvVar -> Bag EvVar


  pprKind, pprParendKind, pprSigmaType,
  pprType, pprParendType, pprTypeApp, pprTyThingCategory,
  pprTheta, pprThetaArrowTy, pprClassPred

  ) where

#include "HsVersions.h"

-- friends:
import Eta.Types.Kind
import Eta.Types.TypeRep
import Eta.Types.Class
import Eta.BasicTypes.Var
import Eta.Prelude.ForeignCall
import Eta.BasicTypes.VarSet
import Eta.Types.Coercion
import Eta.Types.Type
import qualified Eta.Types.Type as Type
import Eta.Types.TyCon
import Eta.Types.CoAxiom
import Eta.Utils.Bag
-- others:
import Eta.Main.DynFlags
import Eta.BasicTypes.Name
import qualified Eta.BasicTypes.Name as Name
  -- hiding (varName)
  -- We use this to make dictionaries for type literals.
  -- Perhaps there's a better way to do this?
import Eta.BasicTypes.NameSet
import Eta.BasicTypes.VarEnv
import Eta.BasicTypes.DataCon
import Eta.Prelude.PrelNames
import Eta.Prelude.TysWiredIn
import Eta.BasicTypes.BasicTypes
import Eta.Utils.Util
import Eta.Utils.Maybes
import Eta.Utils.ListSetOps
import Eta.Utils.Outputable
import Eta.Utils.FastString
import Eta.Main.ErrUtils( Validity(..), isValid )
import Data.IORef
import Control.Monad (liftM, ap)
import qualified Eta.LanguageExtensions as LangExt

{-
************************************************************************
*                                                                      *
\subsection{Types}
*                                                                      *
************************************************************************

The type checker divides the generic Type world into the
following more structured beasts:

sigma ::= forall tyvars. phi
        -- A sigma type is a qualified type
        --
        -- Note that even if 'tyvars' is empty, theta
        -- may not be: e.g.   (?x::Int) => Int

        -- Note that 'sigma' is in prenex form:
        -- all the foralls are at the front.
        -- A 'phi' type has no foralls to the right of
        -- an arrow

phi :: theta => rho

rho ::= sigma -> rho
     |  tau

-- A 'tau' type has no quantification anywhere
-- Note that the args of a type constructor must be taus
tau ::= tyvar
     |  tycon tau_1 .. tau_n
     |  tau_1 tau_2
     |  tau_1 -> tau_2

-- In all cases, a (saturated) type synonym application is legal,
-- provided it expands to the required form.
-}

type TcTyVar = TyVar    -- Used only during type inference
type TcCoVar = CoVar    -- Used only during type inference; mutable
type TcType = Type      -- A TcType can have mutable type variables
        -- Invariant on ForAllTy in TcTypes:
        --      forall a. T
        -- a cannot occur inside a MutTyVar in T; that is,
        -- T is "flattened" before quantifying over a

-- These types do not have boxy type variables in them
type TcPredType     = PredType
type TcThetaType    = ThetaType
type TcSigmaType    = TcType
type TcRhoType      = TcType  -- Note [TcRhoType]
type TcTauType      = TcType
type TcKind         = Kind
type TcTyVarSet     = TyVarSet

{-
Note [TcRhoType]
~~~~~~~~~~~~~~~~
A TcRhoType has no foralls or contexts at the top, or to the right of an arrow
  YES    (forall a. a->a) -> Int
  NO     forall a. a ->  Int
  NO     Eq a => a -> a
  NO     Int -> forall a. a -> Int


************************************************************************
*                                                                      *
\subsection{TyVarDetails}
*                                                                      *
************************************************************************

TyVarDetails gives extra info about type variables, used during type
checking.  It's attached to mutable type variables only.
It's knot-tied back to Var.lhs.  There is no reason in principle
why Var.lhs shouldn't actually have the definition, but it "belongs" here.

Note [Signature skolems]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider this

  f :: forall a. [a] -> Int
  f (x::b : xs) = 3

Here 'b' is a lexically scoped type variable, but it turns out to be
the same as the skolem 'a'.  So we have a special kind of skolem
constant, SigTv, which can unify with other SigTvs. They are used
*only* for pattern type signatures.

Similarly consider
  data T (a:k1) = MkT (S a)
  data S (b:k2) = MkS (T b)
When doing kind inference on {S,T} we don't want *skolems* for k1,k2,
because they end up unifying; we want those SigTvs again.

Note [ReturnTv]
~~~~~~~~~~~~~~~
We sometimes want to convert a checking algorithm into an inference
algorithm. An easy way to do this is to "check" that a term has a
metavariable as a type. But, we must be careful to allow that metavariable
to unify with *anything*. (Well, anything that doesn't fail an occurs-check.)
This is what ReturnTv means.

For example, if we have

  (undefined :: (forall a. TF1 a ~ TF2 a => a)) x

we'll call (tcInfer . tcExpr) on the function expression. tcInfer will
create a ReturnTv to represent the expression's type. We really need this
ReturnTv to become set to (forall a. TF1 a ~ TF2 a => a) despite the fact
that this type mentions type families and is a polytype.

However, we must also be careful to make sure that the ReturnTvs really
always do get unified with something -- we don't want these floating
around in the solver. So, we check after running the checker to make
sure the ReturnTv is filled. If it's not, we set it to a TauTv.

We can't ASSERT that no ReturnTvs hit the solver, because they
can if there's, say, a kind error that stops checkTauTvUpdate from
working. This happens in test case typecheck/should_fail/T5570, for
example.

See also the commentary on #9404.
-}

-- A TyVarDetails is inside a TyVar
data TcTyVarDetails
  = SkolemTv      -- A skolem
       Bool       -- True <=> this skolem type variable can be overlapped
                  --          when looking up instances
                  -- See Note [Binding when looking up instances] in InstEnv

  | FlatSkol      -- A flatten-skolem.  It stands for the TcType, and zonking
       TcType     -- will replace it by that type.
                  -- See Note [The flattening story] in TcFlatten

  | RuntimeUnk    -- Stands for an as-yet-unknown type in the GHCi
                  -- interactive context

  | MetaTv { mtv_info  :: MetaInfo
           , mtv_ref   :: IORef MetaDetails
           , mtv_tclvl :: TcLevel }  -- See Note [TcLevel and untouchable type variables]

vanillaSkolemTv, superSkolemTv :: TcTyVarDetails
-- See Note [Binding when looking up instances] in InstEnv
vanillaSkolemTv = SkolemTv False  -- Might be instantiated
superSkolemTv   = SkolemTv True   -- Treat this as a completely distinct type

-----------------------------
data MetaDetails
  = Flexi  -- Flexi type variables unify to become Indirects
  | Indirect TcType

instance Outputable MetaDetails where
  ppr Flexi         = ptext (sLit "Flexi")
  ppr (Indirect ty) = ptext (sLit "Indirect") <+> ppr ty

data MetaInfo
   = TauTv Bool    -- This MetaTv is an ordinary unification variable
                   -- A TauTv is always filled in with a tau-type, which
                   -- never contains any ForAlls.
                   -- The boolean is true when the meta var originates
                   -- from a wildcard.

   | ReturnTv      -- Can unify with *anything*. Used to convert a
                   -- type "checking" algorithm into a type inference algorithm.
                   -- See Note [ReturnTv]

   | SigTv         -- A variant of TauTv, except that it should not be
                   -- unified with a type, only with a type variable
                   -- SigTvs are only distinguished to improve error messages
                   --      see Note [Signature skolems]
                   --      The MetaDetails, if filled in, will
                   --      always be another SigTv or a SkolemTv

   | FlatMetaTv    -- A flatten meta-tyvar
                   -- It is a meta-tyvar, but it is always untouchable, with level 0
                   -- See Note [The flattening story] in TcFlatten

-------------------------------------
-- UserTypeCtxt describes the origin of the polymorphic type
-- in the places where we need to an expression has that type

data UserTypeCtxt
  = FunSigCtxt Name     -- Function type signature
                        -- Also used for types in SPECIALISE pragmas
  | InfSigCtxt Name     -- Inferred type for function
  | ExprSigCtxt         -- Expression type signature
  | ConArgCtxt Name     -- Data constructor argument
  | TySynCtxt Name      -- RHS of a type synonym decl
  | PatSigCtxt          -- Type sig in pattern
                        --   eg  f (x::t) = ...
                        --   or  (x::t, y) = e
  | RuleSigCtxt Name    -- LHS of a RULE forall
                        --    RULE "foo" forall (x :: a -> a). f (Just x) = ...
  | ResSigCtxt          -- Result type sig
                        --      f x :: t = ....
  | ForSigCtxt Name     -- Foreign import or export signature
  | DefaultDeclCtxt     -- Types in a default declaration
  | InstDeclCtxt        -- An instance declaration
  | SpecInstCtxt        -- SPECIALISE instance pragma
  | ThBrackCtxt         -- Template Haskell type brackets [t| ... |]
  | GenSigCtxt          -- Higher-rank or impredicative situations
                        -- e.g. (f e) where f has a higher-rank type
                        -- We might want to elaborate this
  | GhciCtxt            -- GHCi command :kind <type>

  | ClassSCCtxt Name    -- Superclasses of a class
  | SigmaCtxt           -- Theta part of a normal for-all type
                        --      f :: <S> => a -> a
  | DataTyCtxt Name     -- Theta part of a data decl
                        --      data <S> => T a = MkT a

{-
-- Notes re TySynCtxt
-- We allow type synonyms that aren't types; e.g.  type List = []
--
-- If the RHS mentions tyvars that aren't in scope, we'll
-- quantify over them:
--      e.g.    type T = a->a
-- will become  type T = forall a. a->a
--
-- With gla-exts that's right, but for H98 we should complain.


************************************************************************
*                                                                      *
                Untouchable type variables
*                                                                      *
************************************************************************
-}

newtype TcLevel = TcLevel Int deriving( Eq )
  -- See Note [TcLevel and untouchable type variables] for what this Int is

{-
Note [TcLevel and untouchable type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Each unification variable (MetaTv)
  and each Implication
  has a level number (of type TcLevel)

* INVARIANTS.  In a tree of Implications,

    (ImplicInv) The level number of an Implication is
                STRICTLY GREATER THAN that of its parent

    (MetaTvInv) The level number of a unification variable is
                LESS THAN OR EQUAL TO that of its parent
                implication

* A unification variable is *touchable* if its level number
  is EQUAL TO that of its immediate parent implication.

* INVARIANT
    (GivenInv)  The free variables of the ic_given of an
                implication are all untouchable; ie their level
                numbers are LESS THAN the ic_tclvl of the implication


Note [Skolem escape prevention]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We only unify touchable unification variables.  Because of
(MetaTvInv), there can be no occurrences of the variable further out,
so the unification can't cause the skolems to escape. Example:
     data T = forall a. MkT a (a->Int)
     f x (MkT v f) = length [v,x]
We decide (x::alpha), and generate an implication like
      [1]forall a. (a ~ alpha[0])
But we must not unify alpha:=a, because the skolem would escape.

For the cases where we DO want to unify, we rely on floating the
equality.   Example (with same T)
     g x (MkT v f) = x && True
We decide (x::alpha), and generate an implication like
      [1]forall a. (Bool ~ alpha[0])
We do NOT unify directly, bur rather float out (if the constraint
does not mention 'a') to get
      (Bool ~ alpha[0]) /\ [1]forall a.()
and NOW we can unify alpha.

The same idea of only unifying touchables solves another problem.
Suppose we had
   (F Int ~ uf[0])  /\  [1](forall a. C a => F Int ~ beta[1])
In this example, beta is touchable inside the implication. The
first solveSimpleWanteds step leaves 'uf' un-unified. Then we move inside
the implication where a new constraint
       uf  ~  beta
emerges. If we (wrongly) spontaneously solved it to get uf := beta,
the whole implication disappears but when we pop out again we are left with
(F Int ~ uf) which will be unified by our final zonking stage and
uf will get unified *once more* to (F Int).
-}

fskTcLevel :: TcLevel
fskTcLevel = TcLevel 0  -- 0 = Outside the outermost level:
                                  --     flatten skolems

topTcLevel :: TcLevel
topTcLevel = TcLevel 1   -- 1 = outermost level

pushTcLevel :: TcLevel -> TcLevel
pushTcLevel (TcLevel us) = TcLevel (us+1)

strictlyDeeperThan :: TcLevel -> TcLevel -> Bool
strictlyDeeperThan (TcLevel tv_tclvl) (TcLevel ctxt_tclvl)
  = tv_tclvl > ctxt_tclvl

sameDepthAs :: TcLevel -> TcLevel -> Bool
sameDepthAs (TcLevel ctxt_tclvl) (TcLevel tv_tclvl)
  = ctxt_tclvl == tv_tclvl   -- NB: invariant ctxt_tclvl >= tv_tclvl
                             --     So <= would be equivalent

checkTcLevelInvariant :: TcLevel -> TcLevel -> Bool
-- Checks (MetaTvInv) from Note [TcLevel and untouchable type variables]
checkTcLevelInvariant (TcLevel ctxt_tclvl) (TcLevel tv_tclvl)
  = ctxt_tclvl >= tv_tclvl

instance Outputable TcLevel where
  ppr (TcLevel us) = ppr us

{-
************************************************************************
*                                                                      *
                Pretty-printing
*                                                                      *
************************************************************************
-}

pprTcTyVarDetails :: TcTyVarDetails -> SDoc
-- For debugging
pprTcTyVarDetails (SkolemTv True)  = ptext (sLit "ssk")
pprTcTyVarDetails (SkolemTv False) = ptext (sLit "sk")
pprTcTyVarDetails (RuntimeUnk {})  = ptext (sLit "rt")
pprTcTyVarDetails (FlatSkol {})    = ptext (sLit "fsk")
pprTcTyVarDetails (MetaTv { mtv_info = info, mtv_tclvl = tclvl })
  = pp_info <> colon <> ppr tclvl
  where
    pp_info = case info of
                ReturnTv    -> ptext (sLit "ret")
                TauTv True  -> ptext (sLit "twc")
                TauTv False -> ptext (sLit "tau")
                SigTv       -> ptext (sLit "sig")
                FlatMetaTv  -> ptext (sLit "fuv")

pprUserTypeCtxt :: UserTypeCtxt -> SDoc
pprUserTypeCtxt (InfSigCtxt n)    = ptext (sLit "the inferred type for") <+> quotes (ppr n)
pprUserTypeCtxt (FunSigCtxt n)    = ptext (sLit "the type signature for") <+> quotes (ppr n)
pprUserTypeCtxt (RuleSigCtxt n)   = ptext (sLit "a RULE for") <+> quotes (ppr n)
pprUserTypeCtxt ExprSigCtxt       = ptext (sLit "an expression type signature")
pprUserTypeCtxt (ConArgCtxt c)    = ptext (sLit "the type of the constructor") <+> quotes (ppr c)
pprUserTypeCtxt (TySynCtxt c)     = ptext (sLit "the RHS of the type synonym") <+> quotes (ppr c)
pprUserTypeCtxt ThBrackCtxt       = ptext (sLit "a Template Haskell quotation [t|...|]")
pprUserTypeCtxt PatSigCtxt        = ptext (sLit "a pattern type signature")
pprUserTypeCtxt ResSigCtxt        = ptext (sLit "a result type signature")
pprUserTypeCtxt (ForSigCtxt n)    = ptext (sLit "the foreign declaration for") <+> quotes (ppr n)
pprUserTypeCtxt DefaultDeclCtxt   = ptext (sLit "a type in a `default' declaration")
pprUserTypeCtxt InstDeclCtxt      = ptext (sLit "an instance declaration")
pprUserTypeCtxt SpecInstCtxt      = ptext (sLit "a SPECIALISE instance pragma")
pprUserTypeCtxt GenSigCtxt        = ptext (sLit "a type expected by the context")
pprUserTypeCtxt GhciCtxt          = ptext (sLit "a type in a Eta REPL command")
pprUserTypeCtxt (ClassSCCtxt c)   = ptext (sLit "the super-classes of class") <+> quotes (ppr c)
pprUserTypeCtxt SigmaCtxt         = ptext (sLit "the context of a polymorphic type")
pprUserTypeCtxt (DataTyCtxt tc)   = ptext (sLit "the context of the data type declaration for") <+> quotes (ppr tc)

pprSigCtxt :: UserTypeCtxt -> SDoc -> SDoc -> SDoc
-- (pprSigCtxt ctxt <extra> <type>)
-- prints    In <extra> the type signature for 'f':
--              f :: <type>
-- The <extra> is either empty or "the ambiguity check for"
pprSigCtxt ctxt extra pp_ty
  = sep [ ptext (sLit "In") <+> extra <+> pprUserTypeCtxt ctxt <> colon
        , nest 2 (pp_sig ctxt) ]
  where
    pp_sig (FunSigCtxt n)  = pp_n_colon n
    pp_sig (ConArgCtxt n)  = pp_n_colon n
    pp_sig (ForSigCtxt n)  = pp_n_colon n
    pp_sig _               = pp_ty

    pp_n_colon n = pprPrefixOcc n <+> dcolon <+> pp_ty

{-
************************************************************************
*                  *
    Finding type family instances
*                  *
************************************************************************
-}

-- | Finds outermost type-family applications occurring in a type,
-- after expanding synonyms.
tcTyFamInsts :: Type -> [(TyCon, [Type])]
tcTyFamInsts ty
  | Just exp_ty <- tcView ty    = tcTyFamInsts exp_ty
tcTyFamInsts (TyVarTy _)        = []
tcTyFamInsts (TyConApp tc tys)
  | isTypeFamilyTyCon tc        = [(tc, tys)]
  | otherwise                   = concat (map tcTyFamInsts tys)
tcTyFamInsts (LitTy {})         = []
tcTyFamInsts (FunTy ty1 ty2)    = tcTyFamInsts ty1 ++ tcTyFamInsts ty2
tcTyFamInsts (AppTy ty1 ty2)    = tcTyFamInsts ty1 ++ tcTyFamInsts ty2
tcTyFamInsts (ForAllTy _ ty)    = tcTyFamInsts ty

{-
************************************************************************
*                  *
          The "exact" free variables of a type
*                  *
************************************************************************

Note [Silly type synonym]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  type T a = Int
What are the free tyvars of (T x)?  Empty, of course!
Here's the example that Ralf Laemmel showed me:
  foo :: (forall a. C u a -> C u a) -> u
  mappend :: Monoid u => u -> u -> u

  bar :: Monoid u => u
  bar = foo (\t -> t `mappend` t)
We have to generalise at the arg to f, and we don't
want to capture the constraint (Monad (C u a)) because
it appears to mention a.  Pretty silly, but it was useful to him.

exactTyVarsOfType is used by the type checker to figure out exactly
which type variables are mentioned in a type.  It's also used in the
smart-app checking code --- see TcExpr.tcIdApp

On the other hand, consider a *top-level* definition
  f = (\x -> x) :: T a -> T a
If we don't abstract over 'a' it'll get fixed to GHC.Prim.Any, and then
if we have an application like (f "x") we get a confusing error message
involving Any.  So the conclusion is this: when generalising
  - at top level use tyVarsOfType
  - in nested bindings use exactTyVarsOfType
See Trac #1813 for example.
-}

exactTyVarsOfType :: Type -> TyVarSet
-- Find the free type variables (of any kind)
-- but *expand* type synonyms.  See Note [Silly type synonym] above.
exactTyVarsOfType ty
  = go ty
  where
    go ty | Just ty' <- tcView ty = go ty'  -- This is the key line
    go (TyVarTy tv)         = unitVarSet tv
    go (TyConApp _ tys)     = exactTyVarsOfTypes tys
    go (LitTy {})           = emptyVarSet
    go (FunTy arg res)      = go arg `unionVarSet` go res
    go (AppTy fun arg)      = go fun `unionVarSet` go arg
    go (ForAllTy tyvar ty)  = delVarSet (go ty) tyvar

exactTyVarsOfTypes :: [Type] -> TyVarSet
exactTyVarsOfTypes = mapUnionVarSet exactTyVarsOfType

{-
************************************************************************
*                                                                      *
                Predicates
*                                                                      *
************************************************************************
-}

isTouchableOrFmv :: TcLevel -> TcTyVar -> Bool
isTouchableOrFmv ctxt_tclvl tv
  = ASSERT2( isTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
      MetaTv { mtv_tclvl = tv_tclvl, mtv_info = info }
        -> ASSERT2( checkTcLevelInvariant ctxt_tclvl tv_tclvl,
                    ppr tv $$ ppr tv_tclvl $$ ppr ctxt_tclvl )
           case info of
             FlatMetaTv -> True
             _          -> tv_tclvl `sameDepthAs` ctxt_tclvl
      _          -> False

isTouchableMetaTyVar :: TcLevel -> TcTyVar -> Bool
isTouchableMetaTyVar ctxt_tclvl tv
  = ASSERT2( isTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
      MetaTv { mtv_tclvl = tv_tclvl }
        -> ASSERT2( checkTcLevelInvariant ctxt_tclvl tv_tclvl,
                    ppr tv $$ ppr tv_tclvl $$ ppr ctxt_tclvl )
           tv_tclvl `sameDepthAs` ctxt_tclvl
      _ -> False

isFloatedTouchableMetaTyVar :: TcLevel -> TcTyVar -> Bool
isFloatedTouchableMetaTyVar ctxt_tclvl tv
  = ASSERT2( isTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
      MetaTv { mtv_tclvl = tv_tclvl } -> tv_tclvl `strictlyDeeperThan` ctxt_tclvl
      _ -> False

isImmutableTyVar :: TyVar -> Bool
isImmutableTyVar tv
  | isTcTyVar tv = isSkolemTyVar tv
  | otherwise    = True

isTyConableTyVar, isSkolemTyVar, isOverlappableTyVar,
  isMetaTyVar, isAmbiguousTyVar,
  isFmvTyVar, isFskTyVar, isFlattenTyVar, isReturnTyVar :: TcTyVar -> Bool

isTyConableTyVar tv
        -- True of a meta-type variable that can be filled in
        -- with a type constructor application; in particular,
        -- not a SigTv
  = ASSERT( isTcTyVar tv)
    case tcTyVarDetails tv of
        MetaTv { mtv_info = SigTv } -> False
        _                           -> True

isFmvTyVar tv
  = ASSERT2( isTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
        MetaTv { mtv_info = FlatMetaTv } -> True
        _                                -> False

-- | True of both given and wanted flatten-skolems (fak and usk)
isFlattenTyVar tv
  = ASSERT2( isTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
        FlatSkol {}                      -> True
        MetaTv { mtv_info = FlatMetaTv } -> True
        _                                -> False

-- | True of FlatSkol skolems only
isFskTyVar tv
  = ASSERT2( isTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
        FlatSkol {} -> True
        _           -> False

isSkolemTyVar tv
  = ASSERT2( isTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
        MetaTv {} -> False
        _other    -> True

isOverlappableTyVar tv
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
        SkolemTv overlappable -> overlappable
        _                     -> False

isMetaTyVar tv
  = ASSERT2( isTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
        MetaTv {} -> True
        _         -> False

isReturnTyVar tv
  = ASSERT2( isTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
      MetaTv { mtv_info = ReturnTv } -> True
      _                              -> False

-- isAmbiguousTyVar is used only when reporting type errors
-- It picks out variables that are unbound, namely meta
-- type variables and the RuntimeUnk variables created by
-- RtClosureInspect.zonkRTTIType.  These are "ambiguous" in
-- the sense that they stand for an as-yet-unknown type
isAmbiguousTyVar tv
  = ASSERT2( isTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
        MetaTv {}     -> True
        RuntimeUnk {} -> True
        _             -> False

isMetaTyVarTy :: TcType -> Bool
isMetaTyVarTy (TyVarTy tv) = isMetaTyVar tv
isMetaTyVarTy _            = False

metaTyVarInfo :: TcTyVar -> MetaInfo
metaTyVarInfo tv
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
      MetaTv { mtv_info = info } -> info
      _ -> pprPanic "metaTyVarInfo" (ppr tv)

metaTyVarTcLevel :: TcTyVar -> TcLevel
metaTyVarTcLevel tv
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
      MetaTv { mtv_tclvl = tclvl } -> tclvl
      _ -> pprPanic "metaTyVarTcLevel" (ppr tv)

metaTyVarTcLevel_maybe :: TcTyVar -> Maybe TcLevel
metaTyVarTcLevel_maybe tv
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
      MetaTv { mtv_tclvl = tclvl } -> Just tclvl
      _                            -> Nothing

setMetaTyVarTcLevel :: TcTyVar -> TcLevel -> TcTyVar
setMetaTyVarTcLevel tv tclvl
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
      details@(MetaTv {}) -> setTcTyVarDetails tv (details { mtv_tclvl = tclvl })
      _ -> pprPanic "metaTyVarTcLevel" (ppr tv)

isSigTyVar :: Var -> Bool
isSigTyVar tv
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
        MetaTv { mtv_info = SigTv } -> True
        _                           -> False

metaTvRef :: TyVar -> IORef MetaDetails
metaTvRef tv
  = ASSERT2( isTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
        MetaTv { mtv_ref = ref } -> ref
        _ -> pprPanic "metaTvRef" (ppr tv)

isFlexi, isIndirect :: MetaDetails -> Bool
isFlexi Flexi = True
isFlexi _     = False

isIndirect (Indirect _) = True
isIndirect _            = False

isRuntimeUnkSkol :: TyVar -> Bool
-- Called only in TcErrors; see Note [Runtime skolems] there
isRuntimeUnkSkol x
  | isTcTyVar x, RuntimeUnk <- tcTyVarDetails x = True
  | otherwise                                   = False

{-
************************************************************************
*                                                                      *
\subsection{Tau, sigma and rho}
*                                                                      *
************************************************************************
-}

mkSigmaTy :: [TyVar] -> [PredType] -> Type -> Type
mkSigmaTy tyvars theta tau = mkForAllTys tyvars (mkPhiTy theta tau)

mkPhiTy :: [PredType] -> Type -> Type
mkPhiTy theta ty = foldr mkFunTy ty theta

mkTcEqPred :: TcType -> TcType -> Type
-- During type checking we build equalities between
-- type variables with OpenKind or ArgKind.  Ultimately
-- they will all settle, but we want the equality predicate
-- itself to have kind '*'.  I think.
--
-- But for now we call mkTyConApp, not mkEqPred, because the invariants
-- of the latter might not be satisfied during type checking.
-- Notably when we form an equality   (a : OpenKind) ~ (Int : *)
--
-- But this is horribly delicate: what about type variables
-- that turn out to be bound to Int#?
mkTcEqPred ty1 ty2
  = mkTyConApp eqTyCon [k, ty1, ty2]
  where
    k = typeKind ty1

-- | Make a representational equality predicate
mkTcReprEqPred :: TcType -> TcType -> Type
mkTcReprEqPred ty1 ty2
  = mkTyConApp coercibleTyCon [k, ty1, ty2]
  where
    k = typeKind ty1

-- | Make an equality predicate at a given role. The role must not be Phantom.
mkTcEqPredRole :: Role -> TcType -> TcType -> Type
mkTcEqPredRole Nominal          = mkTcEqPred
mkTcEqPredRole Representational = mkTcReprEqPred
mkTcEqPredRole Phantom          = panic "mkTcEqPredRole Phantom"

-- @isTauTy@ tests for nested for-alls.  It should not be called on a boxy type.

isTauTy :: Type -> Bool
isTauTy ty | Just ty' <- tcView ty = isTauTy ty'
isTauTy (TyVarTy _)       = True
isTauTy (LitTy {})        = True
isTauTy (TyConApp tc tys) = all isTauTy tys && isTauTyCon tc
isTauTy (AppTy a b)       = isTauTy a && isTauTy b
isTauTy (FunTy a b)       = isTauTy a && isTauTy b
isTauTy (ForAllTy {})     = False

isTauTyCon :: TyCon -> Bool
-- Returns False for type synonyms whose expansion is a polytype
isTauTyCon tc
  | Just (_, rhs) <- synTyConDefn_maybe tc = isTauTy rhs
  | otherwise                              = True

---------------
getDFunTyKey :: Type -> OccName -- Get some string from a type, to be used to
                                -- construct a dictionary function name
getDFunTyKey ty | Just ty' <- tcView ty = getDFunTyKey ty'
getDFunTyKey (TyVarTy tv)    = getOccName tv
getDFunTyKey (TyConApp tc _) = getOccName tc
getDFunTyKey (LitTy x)       = getDFunTyLitKey x
getDFunTyKey (AppTy fun _)   = getDFunTyKey fun
getDFunTyKey (FunTy _ _)     = getOccName funTyCon
getDFunTyKey (ForAllTy _ t)  = getDFunTyKey t

getDFunTyLitKey :: TyLit -> OccName
getDFunTyLitKey (NumTyLit n) = mkOccName Name.varName (show n)
getDFunTyLitKey (StrTyLit n) = mkOccName Name.varName (show n)  -- hm

{-
************************************************************************
*                                                                      *
\subsection{Expanding and splitting}
*                                                                      *
************************************************************************

These tcSplit functions are like their non-Tc analogues, but
        *) they do not look through newtypes

However, they are non-monadic and do not follow through mutable type
variables.  It's up to you to make sure this doesn't matter.
-}

tcSplitForAllTys :: Type -> ([TyVar], Type)
tcSplitForAllTys ty = split ty ty []
   where
     split orig_ty ty tvs | Just ty' <- tcView ty = split orig_ty ty' tvs
     split _ (ForAllTy tv ty) tvs = split ty ty (tv:tvs)
     split orig_ty _          tvs = (reverse tvs, orig_ty)

tcIsForAllTy :: Type -> Bool
tcIsForAllTy ty | Just ty' <- tcView ty = tcIsForAllTy ty'
tcIsForAllTy (ForAllTy {}) = True
tcIsForAllTy _             = False

tcSplitPredFunTy_maybe :: Type -> Maybe (PredType, Type)
-- Split off the first predicate argument from a type
tcSplitPredFunTy_maybe ty
  | Just ty' <- tcView ty = tcSplitPredFunTy_maybe ty'
tcSplitPredFunTy_maybe (FunTy arg res)
  | isPredTy arg = Just (arg, res)
tcSplitPredFunTy_maybe _
  = Nothing

tcSplitPhiTy :: Type -> (ThetaType, Type)
tcSplitPhiTy ty
  = split ty []
  where
    split ty ts
      = case tcSplitPredFunTy_maybe ty of
          Just (pred, ty) -> split ty (pred:ts)
          Nothing         -> (reverse ts, ty)

tcSplitSigmaTy :: Type -> ([TyVar], ThetaType, Type)
tcSplitSigmaTy ty = case tcSplitForAllTys ty of
                        (tvs, rho) -> case tcSplitPhiTy rho of
                                        (theta, tau) -> (tvs, theta, tau)

-----------------------
tcDeepSplitSigmaTy_maybe
  :: TcSigmaType -> Maybe ([TcType], [TyVar], ThetaType, TcSigmaType)
-- Looks for a *non-trivial* quantified type, under zero or more function arrows
-- By "non-trivial" we mean either tyvars or constraints are non-empty

tcDeepSplitSigmaTy_maybe ty
  | Just (arg_ty, res_ty)           <- tcSplitFunTy_maybe ty
  , Just (arg_tys, tvs, theta, rho) <- tcDeepSplitSigmaTy_maybe res_ty
  = Just (arg_ty:arg_tys, tvs, theta, rho)

  | (tvs, theta, rho) <- tcSplitSigmaTy ty
  , not (null tvs && null theta)
  = Just ([], tvs, theta, rho)

  | otherwise = Nothing

-----------------------
tcTyConAppTyCon :: Type -> TyCon
tcTyConAppTyCon ty = case tcSplitTyConApp_maybe ty of
                        Just (tc, _) -> tc
                        Nothing      -> pprPanic "tcTyConAppTyCon" (pprType ty)

tcTyConAppArgs :: Type -> [Type]
tcTyConAppArgs ty = case tcSplitTyConApp_maybe ty of
                        Just (_, args) -> args
                        Nothing        -> pprPanic "tcTyConAppArgs" (pprType ty)

tcSplitTyConApp :: Type -> (TyCon, [Type])
tcSplitTyConApp ty = case tcSplitTyConApp_maybe ty of
                        Just stuff -> stuff
                        Nothing    -> pprPanic "tcSplitTyConApp" (pprType ty)

tcSplitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
tcSplitTyConApp_maybe ty | Just ty' <- tcView ty = tcSplitTyConApp_maybe ty'
tcSplitTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
tcSplitTyConApp_maybe (FunTy arg res)   = Just (funTyCon, [arg,res])
        -- Newtypes are opaque, so they may be split
        -- However, predicates are not treated
        -- as tycon applications by the type checker
tcSplitTyConApp_maybe _                 = Nothing

-----------------------
tcSplitFunTys :: Type -> ([Type], Type)
tcSplitFunTys ty = case tcSplitFunTy_maybe ty of
                        Nothing        -> ([], ty)
                        Just (arg,res) -> (arg:args, res')
                                       where
                                          (args,res') = tcSplitFunTys res

tcSplitFunTy_maybe :: Type -> Maybe (Type, Type)
tcSplitFunTy_maybe ty | Just ty' <- tcView ty           = tcSplitFunTy_maybe ty'
tcSplitFunTy_maybe (FunTy arg res) | not (isPredTy arg) = Just (arg, res)
tcSplitFunTy_maybe _                                    = Nothing
        -- Note the typeKind guard
        -- Consider     (?x::Int) => Bool
        -- We don't want to treat this as a function type!
        -- A concrete example is test tc230:
        --      f :: () -> (?p :: ()) => () -> ()
        --
        --      g = f () ()

tcSplitFunTysN
        :: TcRhoType
        -> Arity                -- N: Number of desired args
        -> ([TcSigmaType],      -- Arg types (N or fewer)
            TcSigmaType)        -- The rest of the type

tcSplitFunTysN ty n_args
  | n_args == 0
  = ([], ty)
  | Just (arg,res) <- tcSplitFunTy_maybe ty
  = case tcSplitFunTysN res (n_args - 1) of
        (args, res) -> (arg:args, res)
  | otherwise
  = ([], ty)

tcSplitFunTy :: Type -> (Type, Type)
tcSplitFunTy  ty = expectJust "tcSplitFunTy" (tcSplitFunTy_maybe ty)

tcFunArgTy :: Type -> Type
tcFunArgTy    ty = fst (tcSplitFunTy ty)

tcFunResultTy :: Type -> Type
tcFunResultTy ty = snd (tcSplitFunTy ty)

-----------------------
tcSplitAppTy_maybe :: Type -> Maybe (Type, Type)
tcSplitAppTy_maybe ty | Just ty' <- tcView ty = tcSplitAppTy_maybe ty'
tcSplitAppTy_maybe ty = repSplitAppTy_maybe ty

tcSplitAppTy :: Type -> (Type, Type)
tcSplitAppTy ty = case tcSplitAppTy_maybe ty of
                    Just stuff -> stuff
                    Nothing    -> pprPanic "tcSplitAppTy" (pprType ty)

tcSplitAppTys :: Type -> (Type, [Type])
tcSplitAppTys ty
  = go ty []
  where
    go ty args = case tcSplitAppTy_maybe ty of
                   Just (ty', arg) -> go ty' (arg:args)
                   Nothing         -> (ty,args)

-----------------------
tcGetTyVar_maybe :: Type -> Maybe TyVar
tcGetTyVar_maybe ty | Just ty' <- tcView ty = tcGetTyVar_maybe ty'
tcGetTyVar_maybe (TyVarTy tv)   = Just tv
tcGetTyVar_maybe _              = Nothing

tcGetTyVar :: String -> Type -> TyVar
tcGetTyVar msg ty = expectJust msg (tcGetTyVar_maybe ty)

tcIsTyVarTy :: Type -> Bool
tcIsTyVarTy ty = isJust (tcGetTyVar_maybe ty)

-----------------------
tcSplitDFunTy :: Type -> ([TyVar], [Type], Class, [Type])
-- Split the type of a dictionary function
-- We don't use tcSplitSigmaTy,  because a DFun may (with NDP)
-- have non-Pred arguments, such as
--     df :: forall m. (forall b. Eq b => Eq (m b)) -> C m
--
-- Also NB splitFunTys, not tcSplitFunTys;
-- the latter  specifically stops at PredTy arguments,
-- and we don't want to do that here
tcSplitDFunTy ty
  = case tcSplitForAllTys ty   of { (tvs, rho)   ->
    case splitFunTys rho       of { (theta, tau) ->
    case tcSplitDFunHead tau   of { (clas, tys)  ->
    (tvs, theta, clas, tys) }}}

tcSplitDFunHead :: Type -> (Class, [Type])
tcSplitDFunHead = getClassPredTys

tcInstHeadTyNotSynonym :: Type -> Bool
-- Used in Haskell-98 mode, for the argument types of an instance head
-- These must not be type synonyms, but everywhere else type synonyms
-- are transparent, so we need a special function here
tcInstHeadTyNotSynonym ty
  = case ty of
        TyConApp tc _ -> not (isTypeSynonymTyCon tc)
        _ -> True

tcInstHeadTyAppAllTyVars :: Type -> Bool
-- Used in Haskell-98 mode, for the argument types of an instance head
-- These must be a constructor applied to type variable arguments.
-- But we allow kind instantiations.
tcInstHeadTyAppAllTyVars ty
  | Just ty' <- tcView ty       -- Look through synonyms
  = tcInstHeadTyAppAllTyVars ty'
  | otherwise
  = case ty of
        TyConApp _ tys  -> ok (filter (not . isKind) tys)  -- avoid kinds
        FunTy arg res   -> ok [arg, res]
        _               -> False
  where
        -- Check that all the types are type variables,
        -- and that each is distinct
    ok tys = equalLength tvs tys && hasNoDups tvs
           where
             tvs = mapMaybe get_tv tys

    get_tv (TyVarTy tv)  = Just tv      -- through synonyms
    get_tv _             = Nothing

tcEqKind :: TcKind -> TcKind -> Bool
tcEqKind = tcEqType

tcEqType :: TcType -> TcType -> Bool
-- tcEqType is a proper, sensible type-equality function, that does
-- just what you'd expect The function Type.eqType (currently) has a
-- grotesque hack that makes OpenKind = *, and that is NOT what we
-- want in the type checker!  Otherwise, for example, TcCanonical.reOrient
-- thinks the LHS and RHS have the same kinds, when they don't, and
-- fails to re-orient.  That in turn caused Trac #8553.

tcEqType ty1 ty2
  = go init_env ty1 ty2
  where
    init_env = mkRnEnv2 (mkInScopeSet (tyVarsOfType ty1 `unionVarSet` tyVarsOfType ty2))
    go env t1 t2 | Just t1' <- tcView t1 = go env t1' t2
                 | Just t2' <- tcView t2 = go env t1 t2'
    go env (TyVarTy tv1)       (TyVarTy tv2)     = rnOccL env tv1 == rnOccR env tv2
    go _   (LitTy lit1)        (LitTy lit2)      = lit1 == lit2
    go env (ForAllTy tv1 t1)   (ForAllTy tv2 t2) = go env (tyVarKind tv1) (tyVarKind tv2)
                                                && go (rnBndr2 env tv1 tv2) t1 t2
    go env (AppTy s1 t1)       (AppTy s2 t2)     = go env s1 s2 && go env t1 t2
    go env (FunTy s1 t1)       (FunTy s2 t2)     = go env s1 s2 && go env t1 t2
    go env (TyConApp tc1 ts1) (TyConApp tc2 ts2) = (tc1 == tc2) && gos env ts1 ts2
    go _ _ _ = False

    gos _   []       []       = True
    gos env (t1:ts1) (t2:ts2) = go env t1 t2 && gos env ts1 ts2
    gos _ _ _ = False

pickyEqType :: TcType -> TcType -> Bool
-- Check when two types _look_ the same, _including_ synonyms.
-- So (pickyEqType String [Char]) returns False
pickyEqType ty1 ty2
  = go init_env ty1 ty2
  where
    init_env = mkRnEnv2 (mkInScopeSet (tyVarsOfType ty1 `unionVarSet` tyVarsOfType ty2))
    go env (TyVarTy tv1)       (TyVarTy tv2)     = rnOccL env tv1 == rnOccR env tv2
    go _   (LitTy lit1)        (LitTy lit2)      = lit1 == lit2
    go env (ForAllTy tv1 t1)   (ForAllTy tv2 t2) = go env (tyVarKind tv1) (tyVarKind tv2)
                                                && go (rnBndr2 env tv1 tv2) t1 t2
    go env (AppTy s1 t1)       (AppTy s2 t2)     = go env s1 s2 && go env t1 t2
    go env (FunTy s1 t1)       (FunTy s2 t2)     = go env s1 s2 && go env t1 t2
    go env (TyConApp tc1 ts1) (TyConApp tc2 ts2) = (tc1 == tc2) && gos env ts1 ts2
    go _ _ _ = False

    gos _   []       []       = True
    gos env (t1:ts1) (t2:ts2) = go env t1 t2 && gos env ts1 ts2
    gos _ _ _ = False

{-
Note [Occurs check expansion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(occurCheckExpand tv xi) expands synonyms in xi just enough to get rid
of occurrences of tv outside type function arguments, if that is
possible; otherwise, it returns Nothing.

For example, suppose we have
  type F a b = [a]
Then
  occurCheckExpand b (F Int b) = Just [Int]
but
  occurCheckExpand a (F a Int) = Nothing

We don't promise to do the absolute minimum amount of expanding
necessary, but we try not to do expansions we don't need to.  We
prefer doing inner expansions first.  For example,
  type F a b = (a, Int, a, [a])
  type G b   = Char
We have
  occurCheckExpand b (F (G b)) = F Char
even though we could also expand F to get rid of b.

See also Note [occurCheckExpand] in TcCanonical
-}

data OccCheckResult a
  = OC_OK a
  | OC_Forall
  | OC_NonTyVar
  | OC_Occurs

instance Functor OccCheckResult where
      fmap = liftM

instance Applicative OccCheckResult where
      pure = return
      (<*>) = ap

instance Monad OccCheckResult where
  return x = OC_OK x
  OC_OK x     >>= k = k x
  OC_Forall   >>= _ = OC_Forall
  OC_NonTyVar >>= _ = OC_NonTyVar
  OC_Occurs   >>= _ = OC_Occurs

occurCheckExpand :: DynFlags -> TcTyVar -> Type -> OccCheckResult Type
-- See Note [Occurs check expansion]
-- Check whether
--   a) the given variable occurs in the given type.
--   b) there is a forall in the type (unless we have -XImpredicativeTypes
--                                     or it's a ReturnTv
--   c) if it's a SigTv, ty should be a tyvar
--
-- We may have needed to do some type synonym unfolding in order to
-- get rid of the variable (or forall), so we also return the unfolded
-- version of the type, which is guaranteed to be syntactically free
-- of the given type variable.  If the type is already syntactically
-- free of the variable, then the same type is returned.

occurCheckExpand dflags tv ty
  | MetaTv { mtv_info = SigTv } <- details
                  = go_sig_tv ty
  | fast_check ty = return ty
  | otherwise     = go ty
  where
    details = ASSERT2( isTcTyVar tv, ppr tv ) tcTyVarDetails tv

    impredicative = canUnifyWithPolyType dflags details (tyVarKind tv)

    -- Check 'ty' is a tyvar, or can be expanded into one
    go_sig_tv ty@(TyVarTy {})            = OC_OK ty
    go_sig_tv ty | Just ty' <- tcView ty = go_sig_tv ty'
    go_sig_tv _                          = OC_NonTyVar

    -- True => fine
    fast_check (LitTy {})        = True
    fast_check (TyVarTy tv')     = tv /= tv'
    fast_check (TyConApp _ tys)  = all fast_check tys
    fast_check (FunTy arg res)   = fast_check arg && fast_check res
    fast_check (AppTy fun arg)   = fast_check fun && fast_check arg
    fast_check (ForAllTy tv' ty) = impredicative
                                && fast_check (tyVarKind tv')
                                && (tv == tv' || fast_check ty)

    go t@(TyVarTy tv') | tv == tv' = OC_Occurs
                       | otherwise = return t
    go ty@(LitTy {}) = return ty
    go (AppTy ty1 ty2) = do { ty1' <- go ty1
                            ; ty2' <- go ty2
                            ; return (mkAppTy ty1' ty2') }
    go (FunTy ty1 ty2) = do { ty1' <- go ty1
                            ; ty2' <- go ty2
                            ; return (mkFunTy ty1' ty2') }
    go ty@(ForAllTy tv' body_ty)
       | not impredicative                = OC_Forall
       | not (fast_check (tyVarKind tv')) = OC_Occurs
           -- Can't expand away the kinds unless we create
           -- fresh variables which we don't want to do at this point.
           -- In principle fast_check might fail because of a for-all
           -- but we don't yet have poly-kinded tyvars so I'm not
           -- going to worry about that now
       | tv == tv' = return ty
       | otherwise = do { body' <- go body_ty
                        ; return (ForAllTy tv' body') }

    -- For a type constructor application, first try expanding away the
    -- offending variable from the arguments.  If that doesn't work, next
    -- see if the type constructor is a type synonym, and if so, expand
    -- it and try again.
    go ty@(TyConApp tc tys)
      = case do { tys <- mapM go tys; return (mkTyConApp tc tys) } of
          OC_OK ty -> return ty  -- First try to eliminate the tyvar from the args
          bad | Just ty' <- tcView ty -> go ty'
              | otherwise             -> bad
                      -- Failing that, try to expand a synonym

canUnifyWithPolyType :: DynFlags -> TcTyVarDetails -> TcKind -> Bool
canUnifyWithPolyType dflags details kind
  = case details of
      MetaTv { mtv_info = ReturnTv } -> True      -- See Note [ReturnTv]
      MetaTv { mtv_info = SigTv }    -> False
      MetaTv { mtv_info = TauTv _ }  -> xopt LangExt.ImpredicativeTypes dflags
                                     || isOpenTypeKind kind
                                          -- Note [OpenTypeKind accepts foralls]
      _other                         -> True
          -- We can have non-meta tyvars in given constraints

{-
Note [OpenTypeKind accepts foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is a common paradigm:
   foo :: (forall a. a -> a) -> Int
   foo = error "urk"
To make this work we need to instantiate 'error' with a polytype.
A similar case is
   bar :: Bool -> (forall a. a->a) -> Int
   bar True = \x. (x 3)
   bar False = error "urk"
Here we need to instantiate 'error' with a polytype.

But 'error' has an OpenTypeKind type variable, precisely so that
we can instantiate it with Int#.  So we also allow such type variables
to be instantiated with foralls.  It's a bit of a hack, but seems
straightforward.

************************************************************************
*                                                                      *
\subsection{Predicate types}
*                                                                      *
************************************************************************

Deconstructors and tests on predicate types
-}

isTyVarClassPred :: PredType -> Bool
isTyVarClassPred ty = case getClassPredTys_maybe ty of
    Just (_, tys) -> all isTyVarTy tys
    _             -> False

evVarPred_maybe :: EvVar -> Maybe PredType
evVarPred_maybe v = if isPredTy ty then Just ty else Nothing
  where ty = varType v

evVarPred :: EvVar -> PredType
evVarPred var
 | debugIsOn
  = case evVarPred_maybe var of
      Just pred -> pred
      Nothing   -> pprPanic "tcEvVarPred" (ppr var <+> ppr (varType var))
 | otherwise
  = varType var

-- Superclasses

mkMinimalBySCs :: [PredType] -> [PredType]
-- Remove predicates that can be deduced from others by superclasses
mkMinimalBySCs ptys = [ ploc |  ploc <- ptys
                             ,  ploc `not_in_preds` rec_scs ]
 where
   rec_scs = concatMap trans_super_classes ptys
   not_in_preds p ps = not (any (eqPred p) ps)

   trans_super_classes pred   -- Superclasses of pred, excluding pred itself
     = case classifyPredType pred of
         ClassPred cls tys -> transSuperClasses cls tys
         TuplePred ts      -> concatMap trans_super_classes ts
         _                 -> []

transSuperClasses :: Class -> [Type] -> [PredType]
transSuperClasses cls tys    -- Superclasses of (cls tys),
                             -- excluding (cls tys) itself
  = concatMap trans_sc (immSuperClasses cls tys)
  where
    trans_sc :: PredType -> [PredType]
    -- (trans_sc p) returns (p : p's superclasses)
    trans_sc p = case classifyPredType p of
                   ClassPred cls tys -> p : transSuperClasses cls tys
                   TuplePred ps      -> concatMap trans_sc ps
                   _                 -> [p]

immSuperClasses :: Class -> [Type] -> [PredType]
immSuperClasses cls tys
  = substTheta (zipTopTvSubst tyvars tys) sc_theta
  where
    (tyvars,sc_theta,_,_) = classBigSig cls

{-
************************************************************************
*                                                                      *
\subsection{Predicates}
*                                                                      *
************************************************************************
-}

isSigmaTy :: TcType -> Bool
-- isSigmaTy returns true of any qualified type.  It doesn't
-- *necessarily* have any foralls.  E.g
--        f :: (?x::Int) => Int -> Int
isSigmaTy ty | Just ty' <- tcView ty = isSigmaTy ty'
isSigmaTy (ForAllTy _ _) = True
isSigmaTy (FunTy a _)    = isPredTy a
isSigmaTy _              = False

isRhoTy :: TcType -> Bool   -- True of TcRhoTypes; see Note [TcRhoType]
isRhoTy ty | Just ty' <- tcView ty = isRhoTy ty'
isRhoTy (ForAllTy {}) = False
isRhoTy (FunTy a r)   = not (isPredTy a) && isRhoTy r
isRhoTy _             = True

isOverloadedTy :: Type -> Bool
-- Yes for a type of a function that might require evidence-passing
-- Used only by bindLocalMethods
isOverloadedTy ty | Just ty' <- tcView ty = isOverloadedTy ty'
isOverloadedTy (ForAllTy _ ty) = isOverloadedTy ty
isOverloadedTy (FunTy a _)     = isPredTy a
isOverloadedTy _               = False

isFloatTy, isDoubleTy, isIntegerTy, isIntTy, isWordTy, isBoolTy,
    isUnitTy, isCharTy, isAnyTy :: Type -> Bool
isFloatTy      = is_tc floatTyConKey
isDoubleTy     = is_tc doubleTyConKey
isIntegerTy    = is_tc integerTyConKey
isIntTy        = is_tc intTyConKey
isWordTy       = is_tc wordTyConKey
isBoolTy       = is_tc boolTyConKey
isUnitTy       = is_tc unitTyConKey
isCharTy       = is_tc charTyConKey
isAnyTy        = is_tc anyTyConKey

-- | Does a type represent a floating-point number?
isFloatingTy :: Type -> Bool
isFloatingTy ty = isFloatTy ty || isDoubleTy ty

-- | Is a type 'String'?
isStringTy :: Type -> Bool
isStringTy ty
  = case tcSplitTyConApp_maybe ty of
      Just (tc, [arg_ty]) -> tc == listTyCon && isCharTy arg_ty
      _                   -> False

-- | Is a type a 'CallStack'?
isCallStackTy :: Type -> Bool
isCallStackTy ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` callStackTyConKey
  | otherwise
  = False

-- | Is a 'PredType' a 'CallStack' implicit parameter?
--
-- If so, return the name of the parameter.
isCallStackPred :: PredType -> Maybe FastString
isCallStackPred pred
  | Just (str, ty) <- isIPPred_maybe pred
  , isCallStackTy ty
  = Just str
  | otherwise
  = Nothing

is_tc :: Unique -> Type -> Bool
-- Newtypes are opaque to this
is_tc uniq ty = case tcSplitTyConApp_maybe ty of
                        Just (tc, _) -> uniq == getUnique tc
                        Nothing      -> False

-- | Does the given tyvar appear in the given type outside of any
-- non-newtypes? Assume we're looking for @a@. Says "yes" for
-- @a@, @N a@, @b a@, @a b@, @b (N a)@. Says "no" for
-- @[a]@, @Maybe a@, @T a@, where @N@ is a newtype and @T@ is a datatype.
isTyVarExposed :: TcTyVar -> TcType -> Bool
isTyVarExposed tv (TyVarTy tv')   = tv == tv'
isTyVarExposed tv (TyConApp tc tys)
  | isNewTyCon tc                 = any (isTyVarExposed tv) tys
  | otherwise                     = False
isTyVarExposed _  (LitTy {})      = False
isTyVarExposed _  (FunTy {})      = False
isTyVarExposed tv (AppTy fun arg) = isTyVarExposed tv fun
                                 || isTyVarExposed tv arg
isTyVarExposed _  (ForAllTy {})   = False

-- | Does the given tyvar appear under a type generative w.r.t.
-- representational equality? See Note [Occurs check error] in
-- TcCanonical for the motivation for this function.
isTyVarUnderDatatype :: TcTyVar -> TcType -> Bool
isTyVarUnderDatatype tv = go False
  where
    go under_dt ty | Just ty' <- tcView ty = go under_dt ty'
    go under_dt (TyVarTy tv') = under_dt && (tv == tv')
    go under_dt (TyConApp tc tys) = let under_dt' = under_dt ||
                                                    isGenerativeTyCon tc
                                                      Representational
                                    in any (go under_dt') tys
    go _        (LitTy {}) = False
    go _        (FunTy arg res) = go True arg || go True res
    go under_dt (AppTy fun arg) = go under_dt fun || go under_dt arg
    go under_dt (ForAllTy tv' inner_ty)
      | tv' == tv = False
      | otherwise = go under_dt inner_ty

{-
************************************************************************
*                                                                      *
\subsection{Misc}
*                                                                      *
************************************************************************
-}

deNoteType :: Type -> Type
-- Remove all *outermost* type synonyms and other notes
deNoteType ty | Just ty' <- tcView ty = deNoteType ty'
deNoteType ty = ty

tcTyVarsOfType :: Type -> TcTyVarSet
-- Just the *TcTyVars* free in the type
-- (Types.tyVarsOfTypes finds all free TyVars)
tcTyVarsOfType (TyVarTy tv)         = if isTcTyVar tv then unitVarSet tv
                                                      else emptyVarSet
tcTyVarsOfType (TyConApp _ tys)     = tcTyVarsOfTypes tys
tcTyVarsOfType (LitTy {})           = emptyVarSet
tcTyVarsOfType (FunTy arg res)      = tcTyVarsOfType arg `unionVarSet` tcTyVarsOfType res
tcTyVarsOfType (AppTy fun arg)      = tcTyVarsOfType fun `unionVarSet` tcTyVarsOfType arg
tcTyVarsOfType (ForAllTy tyvar ty)  = tcTyVarsOfType ty `delVarSet` tyvar
        -- We do sometimes quantify over skolem TcTyVars

tcTyVarsOfTypes :: [Type] -> TyVarSet
tcTyVarsOfTypes = mapUnionVarSet tcTyVarsOfType

{-
Find the free tycons and classes of a type.  This is used in the front
end of the compiler.
-}

orphNamesOfTyCon :: TyCon -> NameSet
orphNamesOfTyCon tycon = unitNameSet (getName tycon) `unionNameSet` case tyConClass_maybe tycon of
    Nothing  -> emptyNameSet
    Just cls -> unitNameSet (getName cls)

orphNamesOfType :: Type -> NameSet
orphNamesOfType ty | Just ty' <- tcView ty = orphNamesOfType ty'
                -- Look through type synonyms (Trac #4912)
orphNamesOfType (TyVarTy _)          = emptyNameSet
orphNamesOfType (LitTy {})           = emptyNameSet
orphNamesOfType (TyConApp tycon tys) = orphNamesOfTyCon tycon
                                       `unionNameSet` orphNamesOfTypes tys
orphNamesOfType (FunTy arg res)      = orphNamesOfTyCon funTyCon   -- NB!  See Trac #8535
                                       `unionNameSet` orphNamesOfType arg
                                       `unionNameSet` orphNamesOfType res
orphNamesOfType (AppTy fun arg)      = orphNamesOfType fun `unionNameSet` orphNamesOfType arg
orphNamesOfType (ForAllTy _ ty)      = orphNamesOfType ty

orphNamesOfThings :: (a -> NameSet) -> [a] -> NameSet
orphNamesOfThings f = foldr (unionNameSet . f) emptyNameSet

orphNamesOfTypes :: [Type] -> NameSet
orphNamesOfTypes = orphNamesOfThings orphNamesOfType

orphNamesOfDFunHead :: Type -> NameSet
-- Find the free type constructors and classes
-- of the head of the dfun instance type
-- The 'dfun_head_type' is because of
--      instance Foo a => Baz T where ...
-- The decl is an orphan if Baz and T are both not locally defined,
--      even if Foo *is* locally defined
orphNamesOfDFunHead dfun_ty
  = case tcSplitSigmaTy dfun_ty of
        (_, _, head_ty) -> orphNamesOfType head_ty

orphNamesOfCo :: Coercion -> NameSet
orphNamesOfCo (Refl _ ty)           = orphNamesOfType ty
orphNamesOfCo (TyConAppCo _ tc cos) = unitNameSet (getName tc) `unionNameSet` orphNamesOfCos cos
orphNamesOfCo (AppCo co1 co2)       = orphNamesOfCo co1 `unionNameSet` orphNamesOfCo co2
orphNamesOfCo (ForAllCo _ co)       = orphNamesOfCo co
orphNamesOfCo (CoVarCo _)           = emptyNameSet
orphNamesOfCo (AxiomInstCo con _ cos) = orphNamesOfCoCon con `unionNameSet` orphNamesOfCos cos
orphNamesOfCo (UnivCo _ _ ty1 ty2)  = orphNamesOfType ty1 `unionNameSet` orphNamesOfType ty2
orphNamesOfCo (SymCo co)            = orphNamesOfCo co
orphNamesOfCo (TransCo co1 co2)     = orphNamesOfCo co1 `unionNameSet` orphNamesOfCo co2
orphNamesOfCo (NthCo _ co)          = orphNamesOfCo co
orphNamesOfCo (LRCo  _ co)          = orphNamesOfCo co
orphNamesOfCo (InstCo co ty)        = orphNamesOfCo co `unionNameSet` orphNamesOfType ty
orphNamesOfCo (SubCo co)            = orphNamesOfCo co
orphNamesOfCo (AxiomRuleCo _ ts cs) = orphNamesOfTypes ts `unionNameSet`
                                      orphNamesOfCos cs

orphNamesOfCos :: [Coercion] -> NameSet
orphNamesOfCos = orphNamesOfThings orphNamesOfCo

orphNamesOfCoCon :: CoAxiom br -> NameSet
orphNamesOfCoCon (CoAxiom { co_ax_tc = tc, co_ax_branches = branches })
  = orphNamesOfTyCon tc `unionNameSet` orphNamesOfCoAxBranches branches

orphNamesOfCoAxBranches :: BranchList CoAxBranch br -> NameSet
orphNamesOfCoAxBranches = brListFoldr (unionNameSet . orphNamesOfCoAxBranch) emptyNameSet

orphNamesOfCoAxBranch :: CoAxBranch -> NameSet
orphNamesOfCoAxBranch (CoAxBranch { cab_lhs = lhs, cab_rhs = rhs })
  = orphNamesOfTypes lhs `unionNameSet` orphNamesOfType rhs

{-
************************************************************************
*                                                                      *
\subsection[TysWiredIn-ext-type]{External types}
*                                                                      *
************************************************************************

The compiler's foreign function interface supports the passing of a
restricted set of types as arguments and results (the restricting factor
being the )
-}

tcSplitIOType_maybe :: Type -> Maybe (TyCon, Type)
-- (tcSplitIOType_maybe t) returns Just (IO,t',co)
--              if co : t ~ IO t'
--              returns Nothing otherwise
tcSplitIOType_maybe ty
  = case tcSplitTyConApp_maybe ty of
        Just (io_tycon, [io_res_ty])
         | io_tycon `hasKey` ioTyConKey ->
            Just (io_tycon, io_res_ty)
        _ ->
            Nothing

tcSplitJavaType_maybe :: Type -> Maybe (TyCon, Type, Type)
tcSplitJavaType_maybe ty
  = case tcSplitTyConApp_maybe ty of
        Just (javaTyCon, [javaTagType, javaResType])
         | javaTyCon `hasKey` javaTyConKey  ->
            Just (javaTyCon, javaTagType, javaResType)
        _ ->
            Nothing

tcSplitExtendsType_maybe :: Type -> Maybe (Type, Type)
tcSplitExtendsType_maybe ty
  = case tcSplitTyConApp_maybe ty of
        Just (extendsTyCon, [extendsVarType, extendsTagType])
         | extendsTyCon `hasKey` extendsClassKey  ->
            Just ( extendsVarType
                 , extendsTagType )
        _ ->
            Nothing

tcSplitExtendsType :: Type -> (Type, Type)
tcSplitExtendsType ty = expectJust "tcSplitExtendsType" $ tcSplitExtendsType_maybe ty

extendsVars :: ThetaType -> VarSet
extendsVars = mkVarSet . mapMaybe ( fmap ( getTyVar "extendsVars: Not type variable!"
                                         . fst) . tcSplitExtendsType_maybe )

-- isFFITy :: Type -> Bool
-- -- True for any TyCon that can possibly be an arg or result of an FFI call
-- isFFITy ty = isValid (checkRepTyCon legalFFITyCon ty empty)

isFFIArgumentTy :: DynFlags -> Safety -> VarSet -> Type -> Validity
-- Checks for valid argument type for a 'foreign import'
isFFIArgumentTy dflags safety vs ty
  | checkValidTyVar vs ty = IsValid
  | otherwise = checkRepTyCon (legalOutgoingTyCon dflags safety) ty empty

isFFIExternalTy :: VarSet -> Type -> Validity
-- Types that are allowed as arguments of a 'foreign export'
isFFIExternalTy vs ty
  | checkValidTyVar vs ty = IsValid
  | otherwise = checkRepTyCon legalFEArgTyCon ty empty

isFFIImportResultTy :: DynFlags -> Type -> Validity
isFFIImportResultTy dflags = isFFIResultTy (legalFIResultTyCon dflags) 

isFFIExportResultTy :: Type -> Validity
isFFIExportResultTy = isFFIResultTy legalFEResultTyCon

isFFIResultTy :: (TyCon -> Type -> Bool) -> Type -> Validity
isFFIResultTy isLegalResultTyCon ty
  | isTyVarTy ty = IsValid
  | otherwise = checkRepTyCon isLegalResultTyCon ty empty

-- isFFIDynTy :: Type -> Type -> Validity
-- -- The type in a foreign import dynamic must be Ptr, FunPtr, or a newtype of
-- -- either, and the wrapped function type must be equal to the given type.
-- -- We assume that all types have been run through normaliseFfiType, so we don't
-- -- need to worry about expanding newtypes here.
-- isFFIDynTy expected ty
--     -- Note [Foreign import dynamic]
--     -- In the example below, expected would be 'CInt -> IO ()', while ty would
--     -- be 'FunPtr (CDouble -> IO ())'.
--     | Just (tc, [ty']) <- splitTyConApp_maybe ty
--     , tyConUnique tc `elem` [ptrTyConKey, funPtrTyConKey]
--     , eqType ty' expected
--     = IsValid
--     | otherwise
--     = NotValid (vcat [ ptext (sLit "Expected: Ptr/FunPtr") <+> pprParendType expected <> comma
--                      , ptext (sLit "  Actual:") <+> ppr ty ])

-- isFFILabelTy :: Type -> Validity
-- -- The type of a foreign label must be Ptr, FunPtr, or a newtype of either.
-- isFFILabelTy ty = checkRepTyCon ok ty extra
--   where
--     ok tc _ = tc `hasKey` funPtrTyConKey || tc `hasKey` ptrTyConKey
--     extra = ptext (sLit "A foreign-imported address (via &foo) must have type (Ptr a) or (FunPtr a)")

isFFIPrimArgumentTy :: DynFlags -> Type -> Validity
-- Checks for valid argument type for a 'foreign import prim'
-- Currently they must all be simple unlifted types, or the well-known type
-- Any, which can be used to pass the address to a Haskell object on the heap to
-- the foreign function.
isFFIPrimArgumentTy dflags ty
  | isAnyTy ty = IsValid
  | otherwise  = checkRepTyCon (legalFIPrimArgTyCon dflags) ty empty

isFFIPrimResultTy :: DynFlags -> Type -> Validity
-- Checks for valid result type for a 'foreign import prim'
-- Currently it must be an unlifted type, including unboxed tuples.
isFFIPrimResultTy dflags ty
   = checkRepTyCon (legalFIPrimResultTyCon dflags) ty empty

isFunPtrTy :: Type -> Bool
isFunPtrTy ty = isValid (checkRepTyCon (\tc _ -> tc `hasKey` funPtrTyConKey) ty empty)

-- normaliseFfiType gets run before checkRepTyCon, so we don't
-- need to worry about looking through newtypes or type functions
-- here; that's already been taken care of.
checkRepTyCon :: (TyCon -> Type -> Bool) -> Type -> SDoc -> Validity
checkRepTyCon checkTc ty extra
  = case splitTyConApp_maybe ty of
        Just (tc, tys)
          | isNewTyCon tc           -> NotValid (hang msg 2 (mk_nt_reason tc tys $$ nt_fix))
          | checkTc tc ty           -> IsValid
          | null (tyConDataCons tc) -> NotValid (nullDataConstructorMessage $$ extra)
          | otherwise               -> IsValid
        Nothing -> NotValid (quotes (ppr ty) <+> ptext (sLit "is not a data type") $$ extra)
  where
    msg = quotes (ppr ty) <+> ptext (sLit "cannot be marshalled in a foreign call")
    nullDataConstructorMessage =
      quotes (ppr ty) <+> ptext (sLit "cannot be marshalled in a foreign call, as it has no data constructors.")
    mk_nt_reason tc tys
     | null tys  = ptext (sLit "because its data construtor is not in scope")
     | otherwise = ptext (sLit "because the data construtor for")
                  <+> quotes (ppr tc) <+> ptext (sLit "is not in scope")
    nt_fix = ptext (sLit "Possible fix: import the data constructor to bring it into scope")

checkValidTyVar :: VarSet -> Type -> Bool
checkValidTyVar vs ty
  | Just var <- getTyVar_maybe ty
  , var `elemVarSet` vs
  = True
  | otherwise
  = False

{-
Note [Foreign import dynamic]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A dynamic stub must be of the form 'FunPtr ft -> ft' where ft is any foreign
type.  Similarly, a wrapper stub must be of the form 'ft -> IO (FunPtr ft)'.

We use isFFIDynTy to check whether a signature is well-formed. For example,
given a (illegal) declaration like:

foreign import ccall "dynamic"
  foo :: FunPtr (CDouble -> IO ()) -> CInt -> IO ()

isFFIDynTy will compare the 'FunPtr' type 'CDouble -> IO ()' with the curried
result type 'CInt -> IO ()', and return False, as they are not equal.


----------------------------------------------
These chaps do the work; they are not exported
----------------------------------------------
-}

legalFEArgTyCon :: TyCon -> Type -> Bool
legalFEArgTyCon tc ty
  -- It's illegal to make foreign exports that take unboxed
  -- arguments.  The RTS API currently can't invoke such things.  --SDM 7/2000
  = boxedMarshalableTyCon tc ty

legalFIResultTyCon :: DynFlags -> TyCon -> Type -> Bool
legalFIResultTyCon dflags tc ty
  | tc == unitTyCon         = True
  | otherwise               = marshalableTyCon dflags tc ty

legalFEResultTyCon :: TyCon -> Type -> Bool
legalFEResultTyCon tc ty
  | tc == unitTyCon         = True
  | otherwise               = boxedMarshalableTyCon tc ty

legalOutgoingTyCon :: DynFlags -> Safety -> TyCon -> Type -> Bool
-- Checks validity of types going from Haskell -> external world
legalOutgoingTyCon dflags _ tc ty
  = marshalableTyCon dflags tc ty

-- legalFFITyCon :: VarSet -> TyCon -> Type -> Bool
-- -- True for any TyCon that can possibly be an arg or result of an FFI call
-- legalFFITyCon vs tc ty
--   | isUnLiftedTyCon tc = True
--   | tc == unitTyCon    = True
--   | otherwise          = boxedMarshalableTyCon vs tc ty

marshalableTyCon :: DynFlags -> TyCon -> Type -> Bool
marshalableTyCon dflags tc ty
  |  (xopt LangExt.UnliftedFFITypes dflags
      && isUnLiftedTyCon tc
      && not (isUnboxedTupleTyCon tc))
      -- && not (isVoidRep (typePrimRep ty)))
  = True
  | otherwise
  = boxedMarshalableTyCon tc ty

boxedMarshalableTyCon :: TyCon -> Type -> Bool
boxedMarshalableTyCon tc ty
   | getUnique tc `elem` [ intTyConKey, int8TyConKey, int16TyConKey
                         , int32TyConKey, int64TyConKey
                         , wordTyConKey, word8TyConKey, word16TyConKey
                         , word32TyConKey, word64TyConKey
                         , floatTyConKey, doubleTyConKey
                         , ptrTyConKey, funPtrTyConKey
                         , charTyConKey
                         , stablePtrTyConKey
                         , boolTyConKey
                         , maybeTyConKey
                         , listTyConKey ]
  = True
   -- TODO: Optimize this to add just raw key checks like above.
   --       Can be done once the GHC source in integrated.
  | Just (_, _, _, [primTy]) <- splitDataProductType_maybe ty
  , isPrimitiveType primTy
  = True
  | otherwise = False

legalFIPrimArgTyCon :: DynFlags -> TyCon -> Type -> Bool
-- Check args of 'foreign import prim', only allow simple unlifted types.
-- Strictly speaking it is unnecessary to ban unboxed tuples here since
-- currently they're of the wrong kind to use in function args anyway.
legalFIPrimArgTyCon dflags tc _
  | xopt LangExt.UnliftedFFITypes dflags
    && isUnLiftedTyCon tc
    && not (isUnboxedTupleTyCon tc)
  = True
  | otherwise
  = False

legalFIPrimResultTyCon :: DynFlags -> TyCon -> Type -> Bool
-- Check result type of 'foreign import prim'. Allow simple unlifted
-- types and also unboxed tuple result types '... -> (# , , #)'
legalFIPrimResultTyCon dflags tc _ty
  | xopt LangExt.UnliftedFFITypes dflags
    && isUnLiftedTyCon tc
  = True
  | otherwise
  = False

{-
Note [Marshalling VoidRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't treat State# (whose PrimRep is VoidRep) as marshalable.
In turn that means you can't write
        foreign import foo :: Int -> State# RealWorld

Reason: the back end falls over with panic "primRepHint:VoidRep";
        and there is no compelling reason to permit it
-}

{-
************************************************************************
*                                                                      *
\subsection{Transformation of Types to TcTypes}
*                                                                      *
************************************************************************
-}

toTcType :: Type -> TcType
toTcType ty = to_tc_type emptyVarSet ty
   where
    to_tc_type :: VarSet -> Type -> TcType
    -- The constraint solver expects EvVars to have TcType, in which the
    -- free type variables are TcTyVars. So we convert from Type to TcType here
    -- A bit tiresome; but one day I expect the two types to be entirely separate
    -- in which case we'll definitely need to do this
    to_tc_type forall_tvs (TyVarTy tv)
      | Just var <- lookupVarSet forall_tvs tv = TyVarTy var
      | otherwise = TyVarTy (toTcTyVar tv)
    to_tc_type  ftvs (FunTy t1 t2)     = FunTy (to_tc_type ftvs t1) (to_tc_type ftvs t2)
    to_tc_type  ftvs (AppTy t1 t2)     = AppTy (to_tc_type ftvs t1) (to_tc_type ftvs t2)
    to_tc_type  ftvs (TyConApp tc tys) = TyConApp tc (map (to_tc_type ftvs) tys)
    to_tc_type  ftvs (ForAllTy tv ty)  = let tv' = toTcTyVar tv
                                         in ForAllTy tv' (to_tc_type (ftvs `extendVarSet` tv') ty)
    to_tc_type _ftvs (LitTy l)         = LitTy l

toTcTyVar :: TyVar -> TcTyVar
toTcTyVar tv
  | isTcTyVar tv = setVarType tv (toTcType (tyVarKind tv))
  | isId tv      = pprPanic "toTcTyVar: Id:" (ppr tv)
  | otherwise    = mkTcTyVar (tyVarName tv) (toTcType (tyVarKind tv)) vanillaSkolemTv

toTcTypeBag :: Bag EvVar -> Bag EvVar -- All TyVars are transformed to TcTyVars
toTcTypeBag evvars = mapBag (\tv -> setTyVarKind tv (toTcType (tyVarKind tv))) evvars
