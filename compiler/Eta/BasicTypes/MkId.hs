{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1998


This module contains definitions for the IdInfo for things that
have a standard form, namely:

- data constructors
- record selectors
- method and superclass selectors
- primitive operations
-}

{-# LANGUAGE CPP #-}

module Eta.BasicTypes.MkId (
        mkDictFunId, mkDictFunTy, mkDictSelId, mkDictSelRhs,

        mkPrimOpId, mkFCallId,

        wrapNewTypeBody, unwrapNewTypeBody,
        wrapFamInstBody, unwrapFamInstScrut,
        wrapTypeFamInstBody, wrapTypeUnbranchedFamInstBody, unwrapTypeFamInstScrut,
        unwrapTypeUnbranchedFamInstScrut,

        DataConBoxer(..), mkDataConRep, mkDataConWorkId,

        -- And some particular Ids; see below for why they are wired in
        wiredInIds, ghcPrimIds,
        unsafeCoerceName, unsafeCoerceId, realWorldPrimId,
        voidPrimId, voidArgId,
        nullAddrId, seqId, lazyId, lazyIdKey, runRWId,
        coercionTokenId, magicDictId, coerceId,
        proxyHashId, noinlineId, noinlineIdName,

        -- Re-export error Ids
        module Eta.Prelude.PrelRules
    ) where

#include "HsVersions.h"

import Eta.Specialise.Rules
import Eta.Prelude.TysPrim
import Eta.Prelude.TysWiredIn
import Eta.Prelude.PrelRules
import Eta.Types.Type
import Eta.Types.FamInstEnv
import Eta.Types.Coercion
import Eta.TypeCheck.TcType
import qualified Eta.TypeCheck.TcType as TcType
import Eta.Core.MkCore
import Eta.Core.CoreUtils        ( exprType, mkCast )
import Eta.Core.CoreUnfold
import Eta.BasicTypes.Literal
import Eta.Types.TyCon
import Eta.Types.CoAxiom
import Eta.Types.Class
import Eta.BasicTypes.NameSet
import Eta.BasicTypes.VarSet
import Eta.BasicTypes.Name
import Eta.Prelude.PrimOp
import Eta.Prelude.ForeignCall
import Eta.BasicTypes.DataCon
import Eta.BasicTypes.Id
import Eta.BasicTypes.IdInfo
import Eta.BasicTypes.Demand
import Eta.Core.CoreSyn
import Eta.BasicTypes.Unique
import Eta.BasicTypes.UniqSupply
import Eta.Prelude.PrelNames
import Eta.BasicTypes.BasicTypes       hiding ( SuccessFlag(..) )
import Eta.Utils.Util
import Eta.Utils.Pair
import Eta.Main.DynFlags
import Eta.Utils.Outputable
import Eta.Utils.FastString
import Eta.Utils.ListSetOps
import qualified Eta.LanguageExtensions as LangExt

import Data.Maybe       ( maybeToList )

{-
************************************************************************
*                                                                      *
\subsection{Wired in Ids}
*                                                                      *
************************************************************************

Note [Wired-in Ids]
~~~~~~~~~~~~~~~~~~~
There are several reasons why an Id might appear in the wiredInIds:

(1) The ghcPrimIds are wired in because they can't be defined in
    Haskell at all, although the can be defined in Core.  They have
    compulsory unfoldings, so they are always inlined and they  have
    no definition site.  Their home module is GHC.Prim, so they
    also have a description in primops.txt.pp, where they are called
    'pseudoops'.

(2) The 'error' function, eRROR_ID, is wired in because we don't yet have
    a way to express in an interface file that the result type variable
    is 'open'; that is can be unified with an unboxed type

    [The interface file format now carry such information, but there's
    no way yet of expressing at the definition site for these
    error-reporting functions that they have an 'open'
    result type. -- sof 1/99]

(3) Other error functions (rUNTIME_ERROR_ID) are wired in (a) because
    the desugarer generates code that mentions them directly, and
    (b) for the same reason as eRROR_ID

(4) lazyId is wired in because the wired-in version overrides the
    strictness of the version defined in GHC.Base

(5) noinlineId is wired in because when we serialize to interfaces
    we may insert noinline statements.

In cases (2-4), the function has a definition in a library module, and
can be called; but the wired-in version means that the details are
never read from that module's interface file; instead, the full definition
is right here.
-}

wiredInIds :: [Id]
wiredInIds
  =  [lazyId, dollarId, oneShotId, runRWId, noinlineId]
  ++ errorIds           -- Defined in MkCore
  ++ ghcPrimIds

-- These Ids are exported from GHC.Prim
ghcPrimIds :: [Id]
ghcPrimIds
  = [   -- These can't be defined in Haskell, but they have
        -- perfectly reasonable unfoldings in Core
    realWorldPrimId,
    voidPrimId,
    unsafeCoerceId,
    nullAddrId,
    seqId,
    magicDictId,
    coerceId,
    proxyHashId
    ]

{-
************************************************************************
*                                                                      *
\subsection{Data constructors}
*                                                                      *
************************************************************************

The wrapper for a constructor is an ordinary top-level binding that evaluates
any strict args, unboxes any args that are going to be flattened, and calls
the worker.

We're going to build a constructor that looks like:

        data (Data a, C b) =>  T a b = T1 !a !Int b

        T1 = /\ a b ->
             \d1::Data a, d2::C b ->
             \p q r -> case p of { p ->
                       case q of { q ->
                       Con T1 [a,b] [p,q,r]}}

Notice that

* d2 is thrown away --- a context in a data decl is used to make sure
  one *could* construct dictionaries at the site the constructor
  is used, but the dictionary isn't actually used.

* We have to check that we can construct Data dictionaries for
  the types a and Int.  Once we've done that we can throw d1 away too.

* We use (case p of q -> ...) to evaluate p, rather than "seq" because
  all that matters is that the arguments are evaluated.  "seq" is
  very careful to preserve evaluation order, which we don't need
  to be here.

  You might think that we could simply give constructors some strictness
  info, like PrimOps, and let CoreToStg do the let-to-case transformation.
  But we don't do that because in the case of primops and functions strictness
  is a *property* not a *requirement*.  In the case of constructors we need to
  do something active to evaluate the argument.

  Making an explicit case expression allows the simplifier to eliminate
  it in the (common) case where the constructor arg is already evaluated.

Note [Wrappers for data instance tycons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the case of data instances, the wrapper also applies the coercion turning
the representation type into the family instance type to cast the result of
the wrapper.  For example, consider the declarations

  data family Map k :: * -> *
  data instance Map (a, b) v = MapPair (Map a (Pair b v))

The tycon to which the datacon MapPair belongs gets a unique internal
name of the form :R123Map, and we call it the representation tycon.
In contrast, Map is the family tycon (accessible via
tyConFamInst_maybe). A coercion allows you to move between
representation and family type.  It is accessible from :R123Map via
tyConFamilyCoercion_maybe and has kind

  Co123Map a b v :: {Map (a, b) v ~ :R123Map a b v}

The wrapper and worker of MapPair get the types

        -- Wrapper
  $WMapPair :: forall a b v. Map a (Map a b v) -> Map (a, b) v
  $WMapPair a b v = MapPair a b v `cast` sym (Co123Map a b v)

        -- Worker
  MapPair :: forall a b v. Map a (Map a b v) -> :R123Map a b v

This coercion is conditionally applied by wrapFamInstBody.

It's a bit more complicated if the data instance is a GADT as well!

   data instance T [a] where
        T1 :: forall b. b -> T [Maybe b]

Hence we translate to

        -- Wrapper
  $WT1 :: forall b. b -> T [Maybe b]
  $WT1 b v = T1 (Maybe b) b (Maybe b) v
                        `cast` sym (Co7T (Maybe b))

        -- Worker
  T1 :: forall c b. (c ~ Maybe b) => b -> :R7T c

        -- Coercion from family type to representation type
  Co7T a :: T [a] ~ :R7T a

Note [Newtype datacons]
~~~~~~~~~~~~~~~~~~~~~~~
The "data constructor" for a newtype should always be vanilla.  At one
point this wasn't true, because the newtype arising from
     class C a => D a
looked like
       newtype T:D a = D:D (C a)
so the data constructor for T:C had a single argument, namely the
predicate (C a).  But now we treat that as an ordinary argument, not
part of the theta-type, so all is well.


************************************************************************
*                                                                      *
\subsection{Dictionary selectors}
*                                                                      *
************************************************************************

Selecting a field for a dictionary.  If there is just one field, then
there's nothing to do.

Dictionary selectors may get nested forall-types.  Thus:

        class Foo a where
          op :: forall b. Ord b => a -> b -> b

Then the top-level type for op is

        op :: forall a. Foo a =>
              forall b. Ord b =>
              a -> b -> b

This is unlike ordinary record selectors, which have all the for-alls
at the outside.  When dealing with classes it's very convenient to
recover the original type signature from the class op selector.
-}

mkDictSelId :: Name          -- Name of one of the *value* selectors
                             -- (dictionary superclass or method)
            -> Class -> Id
mkDictSelId name clas
  = mkGlobalId (ClassOpId clas) name sel_ty info
  where
    tycon          = classTyCon clas
    sel_names      = map idName (classAllSelIds clas)
    new_tycon      = isNewTyCon tycon
    [data_con]     = tyConDataCons tycon
    tyvars         = dataConUnivTyVars data_con
    arg_tys        = dataConRepArgTys data_con  -- Includes the dictionary superclasses
    val_index      = assoc "MkId.mkDictSelId" (sel_names `zip` [0..]) name

    sel_ty = mkForAllTys tyvars (mkFunTy (mkClassPred clas (mkTyVarTys tyvars))
                                         (getNth arg_tys val_index))

    base_info = noCafIdInfo
                `setArityInfo`         1
                `setStrictnessInfo`    strict_sig

    info | new_tycon
         = base_info `setInlinePragInfo` alwaysInlinePragma
                     `setUnfoldingInfo`  mkInlineUnfolding (Just 1) (mkDictSelRhs clas val_index)
                   -- See Note [Single-method classes] in TcInstDcls
                   -- for why alwaysInlinePragma

         | otherwise
         = base_info `setRuleInfo` mkRuleInfo [rule]
                   -- Add a magic BuiltinRule, but no unfolding
                   -- so that the rule is always available to fire.
                   -- See Note [ClassOp/DFun selection] in TcInstDcls

    n_ty_args = length tyvars

    -- This is the built-in rule that goes
    --      op (dfT d1 d2) --->  opT d1 d2
    rule = BuiltinRule { ru_name = fsLit "Class op " `appendFS`
                                     occNameFS (getOccName name)
                       , ru_fn    = name
                       , ru_nargs = n_ty_args + 1
                       , ru_try   = dictSelRule val_index n_ty_args }

        -- The strictness signature is of the form U(AAAVAAAA) -> T
        -- where the V depends on which item we are selecting
        -- It's worth giving one, so that absence info etc is generated
        -- even if the selector isn't inlined

    strict_sig = mkClosedStrictSig [arg_dmd] topRes
    arg_dmd | new_tycon = evalDmd
            | otherwise = mkManyUsedDmd $
                          mkProdDmd [ if name == sel_name then evalDmd else absDmd
                                    | sel_name <- sel_names ]

mkDictSelRhs :: Class
             -> Int         -- 0-indexed selector among (superclasses ++ methods)
             -> CoreExpr
mkDictSelRhs clas val_index
  = mkLams tyvars (Lam dict_id rhs_body)
  where
    tycon          = classTyCon clas
    new_tycon      = isNewTyCon tycon
    [data_con]     = tyConDataCons tycon
    tyvars         = dataConUnivTyVars data_con
    arg_tys        = dataConRepArgTys data_con  -- Includes the dictionary superclasses

    the_arg_id     = getNth arg_ids val_index
    pred           = mkClassPred clas (mkTyVarTys tyvars)
    dict_id        = mkTemplateLocal 1 pred
    arg_ids        = mkTemplateLocalsNum 2 arg_tys

    rhs_body | new_tycon = unwrapNewTypeBody tycon (map mkTyVarTy tyvars) (Var dict_id)
             | otherwise = Case (Var dict_id) dict_id (idType the_arg_id)
                                [(DataAlt data_con, arg_ids, varToCoreExpr the_arg_id)]
                                -- varToCoreExpr needed for equality superclass selectors
                                --   sel a b d = case x of { MkC _ (g:a~b) _ -> CO g }

dictSelRule :: Int -> Arity -> RuleFun
-- Tries to persuade the argument to look like a constructor
-- application, using exprIsConApp_maybe, and then selects
-- from it
--       sel_i t1..tk (D t1..tk op1 ... opm) = opi
--
dictSelRule val_index n_ty_args _ id_unf _ args
  | (dict_arg : _) <- drop n_ty_args args
  , Just (_, _, con_args) <- exprIsConApp_maybe id_unf dict_arg
  = Just (getNth con_args val_index)
  | otherwise
  = Nothing

{-
************************************************************************
*                                                                      *
        Data constructors
*                                                                      *
************************************************************************
-}

mkDataConWorkId :: Name -> DataCon -> Id
mkDataConWorkId wkr_name data_con
  | isNewTyCon tycon
  = mkGlobalId (DataConWrapId data_con) wkr_name nt_wrap_ty nt_work_info
  | otherwise
  = mkGlobalId (DataConWorkId data_con) wkr_name alg_wkr_ty wkr_info

  where
    tycon = dataConTyCon data_con

        ----------- Workers for data types --------------
    alg_wkr_ty = dataConRepType data_con
    wkr_arity = dataConRepArity data_con
    wkr_info  = noCafIdInfo
                `setArityInfo`       wkr_arity
                `setStrictnessInfo`  wkr_sig
                `setUnfoldingInfo`   evaldUnfolding  -- Record that it's evaluated,
                                                     -- even if arity = 0

    wkr_sig = mkClosedStrictSig (replicate wkr_arity topDmd) (dataConCPR data_con)
        --      Note [Data-con worker strictness]
        -- Notice that we do *not* say the worker is strict
        -- even if the data constructor is declared strict
        --      e.g.    data T = MkT !(Int,Int)
        -- Why?  Because the *wrapper* is strict (and its unfolding has case
        -- expressions that do the evals) but the *worker* itself is not.
        -- If we pretend it is strict then when we see
        --      case x of y -> $wMkT y
        -- the simplifier thinks that y is "sure to be evaluated" (because
        --  $wMkT is strict) and drops the case.  No, $wMkT is not strict.
        --
        -- When the simplifier sees a pattern
        --      case e of MkT x -> ...
        -- it uses the dataConRepStrictness of MkT to mark x as evaluated;
        -- but that's fine... dataConRepStrictness comes from the data con
        -- not from the worker Id.

        ----------- Workers for newtypes --------------
    (nt_tvs, _, nt_arg_tys, _) = dataConSig data_con
    res_ty_args  = mkTyVarTys nt_tvs
    nt_wrap_ty   = dataConUserType data_con
    nt_work_info = noCafIdInfo          -- The NoCaf-ness is set by noCafIdInfo
                  `setArityInfo` 1      -- Arity 1
                  `setInlinePragInfo`    alwaysInlinePragma
                  `setUnfoldingInfo`     newtype_unf
    id_arg1      = mkTemplateLocal 1 (head nt_arg_tys)
    newtype_unf  = ASSERT2( isVanillaDataCon data_con &&
                            isSingleton nt_arg_tys, ppr data_con  )
                              -- Note [Newtype datacons]
                   mkCompulsoryUnfolding $
                   mkLams nt_tvs $ Lam id_arg1 $
                   wrapNewTypeBody tycon res_ty_args (Var id_arg1)

dataConCPR :: DataCon -> DmdResult
dataConCPR con
  | isDataTyCon tycon     -- Real data types only; that is,
                          -- not unboxed tuples or newtypes
  , isVanillaDataCon con  -- No existentials
  , wkr_arity > 0
  , wkr_arity <= mAX_CPR_SIZE
  = if is_prod then vanillaCprProdRes (dataConRepArity con)
               else cprSumRes (dataConTag con)
  | otherwise
  = topRes
  where
    is_prod = isProductTyCon tycon
    tycon = dataConTyCon con
    wkr_arity = dataConRepArity con

    mAX_CPR_SIZE :: Arity
    mAX_CPR_SIZE = 10
    -- We do not treat very big tuples as CPR-ish:
    --      a) for a start we get into trouble because there aren't
    --         "enough" unboxed tuple types (a tiresome restriction,
    --         but hard to fix),
    --      b) more importantly, big unboxed tuples get returned mainly
    --         on the stack, and are often then allocated in the heap
    --         by the caller.  So doing CPR for them may in fact make
    --         things worse.

{-
-------------------------------------------------
--         Data constructor representation
--
-- This is where we decide how to wrap/unwrap the
-- constructor fields
--
--------------------------------------------------
-}

type Unboxer = Var -> UniqSM ([Var], CoreExpr -> CoreExpr)
  -- Unbox: bind rep vars by decomposing src var

data Boxer = UnitBox | Boxer (TvSubst -> UniqSM ([Var], CoreExpr))
  -- Box:   build src arg using these rep vars

newtype DataConBoxer = DCB ([Type] -> [Var] -> UniqSM ([Var], [CoreBind]))
                       -- Bind these src-level vars, returning the
                       -- rep-level vars to bind in the pattern

mkDataConRep :: DynFlags
             -> FamInstEnvs
             -> Name
             -> Maybe [HsImplBang]
             -> DataCon
             -> UniqSM DataConRep
mkDataConRep dflags fam_envs wrap_name mb_bangs data_con
  | not wrapper_reqd
  = return NoDataConRep

  | otherwise
  = do { wrap_args <- mapM newLocal wrap_arg_tys
       ; wrap_body <- mk_rep_app (wrap_args `zip` dropList eq_spec unboxers)
                                 initial_wrap_app

       ; let wrap_id = mkGlobalId (DataConWrapId data_con) wrap_name wrap_ty wrap_info
             wrap_info = noCafIdInfo
                         `setArityInfo`         wrap_arity
                             -- It's important to specify the arity, so that partial
                             -- applications are treated as values
                         `setInlinePragInfo`    alwaysInlinePragma
                         `setUnfoldingInfo`     wrap_unf
                         `setStrictnessInfo`    wrap_sig
                             -- We need to get the CAF info right here because TidyPgm
                             -- does not tidy the IdInfo of implicit bindings (like the wrapper)
                             -- so it not make sure that the CAF info is sane

             wrap_sig = mkClosedStrictSig wrap_arg_dmds (dataConCPR data_con)
             wrap_arg_dmds = map mk_dmd arg_ibangs
             mk_dmd str | isBanged str = evalDmd
                        | otherwise    = topDmd
                 -- The Cpr info can be important inside INLINE rhss, where the
                 -- wrapper constructor isn't inlined.
                 -- And the argument strictness can be important too; we
                 -- may not inline a constructor when it is partially applied.
                 -- For example:
                 --      data W = C !Int !Int !Int
                 --      ...(let w = C x in ...(w p q)...)...
                 -- we want to see that w is strict in its two arguments

             wrap_unf = mkInlineUnfolding (Just wrap_arity) wrap_rhs
             wrap_tvs = (univ_tvs `minusList` map fst eq_spec) ++ ex_tvs
             wrap_rhs = mkLams wrap_tvs $
                        mkLams wrap_args $
                        wrapFamInstBody tycon res_ty_args $
                        wrap_body

       ; return (DCR { dcr_wrap_id = wrap_id
                     , dcr_boxer   = mk_boxer boxers
                     , dcr_arg_tys = rep_tys
                     , dcr_stricts = rep_strs
                     , dcr_bangs   = arg_ibangs }) }

  where
    (univ_tvs, ex_tvs, eq_spec, theta, orig_arg_tys, _) = dataConFullSig data_con
    res_ty_args  = substTyVars (mkTopTvSubst eq_spec) univ_tvs
    tycon        = dataConTyCon data_con       -- The representation TyCon (not family)
    wrap_ty      = dataConUserType data_con
    ev_tys       = eqSpecPreds eq_spec ++ theta
    all_arg_tys  = ev_tys ++ orig_arg_tys
    ev_ibangs    = map mk_pred_strict_mark ev_tys
    orig_bangs   = dataConSrcBangs data_con

    wrap_arg_tys = theta ++ orig_arg_tys
    wrap_arity   = length wrap_arg_tys
             -- The wrap_args are the arguments *other than* the eq_spec
             -- Because we are going to apply the eq_spec args manually in the
             -- wrapper

    arg_ibangs =
      case mb_bangs of
        Nothing    -> zipWith (dataConSrcToImplBang dflags fam_envs)
                              orig_arg_tys orig_bangs
        Just bangs -> bangs

    (rep_tys_w_strs, wrappers)
       = unzip (zipWith dataConArgRep all_arg_tys (ev_ibangs ++ arg_ibangs))
    (unboxers, boxers) = unzip wrappers
    (rep_tys, rep_strs) = unzip (concat rep_tys_w_strs)

    wrapper_reqd = not (isNewTyCon tycon)  -- Newtypes have only a worker
                && (any isBanged (ev_ibangs ++ arg_ibangs)
                         -- Some forcing/unboxing (includes eq_spec)
                    || isFamInstTyCon tycon)  -- Cast result

    initial_wrap_app = Var (dataConWorkId data_con)
                      `mkTyApps`  res_ty_args
                      `mkVarApps` ex_tvs
                      `mkCoApps`  map (mkReflCo Nominal . snd) eq_spec
                        -- Dont box the eq_spec coercions since they are
                        -- marked as HsUnpack by mk_dict_strict_mark

    mk_boxer :: [Boxer] -> DataConBoxer
    mk_boxer boxers = DCB (\ ty_args src_vars ->
                      do { let ex_vars = takeList ex_tvs src_vars
                               subst1 = mkTopTvSubst (univ_tvs `zip` ty_args)
                               subst2 = extendTvSubstList subst1 ex_tvs
                                                          (mkTyVarTys ex_vars)
                         ; (rep_ids, binds) <- go subst2 boxers (dropList ex_tvs src_vars)
                         ; return (ex_vars ++ rep_ids, binds) } )

    go _ [] src_vars = ASSERT2( null src_vars, ppr data_con ) return ([], [])
    go subst (UnitBox : boxers) (src_var : src_vars)
      = do { (rep_ids2, binds) <- go subst boxers src_vars
           ; return (src_var : rep_ids2, binds) }
    go subst (Boxer boxer : boxers) (src_var : src_vars)
      = do { (rep_ids1, arg)  <- boxer subst
           ; (rep_ids2, binds) <- go subst boxers src_vars
           ; return (rep_ids1 ++ rep_ids2, NonRec src_var arg : binds) }
    go _ (_:_) [] = pprPanic "mk_boxer" (ppr data_con)

    mk_rep_app :: [(Id,Unboxer)] -> CoreExpr -> UniqSM CoreExpr
    mk_rep_app [] con_app
      = return con_app
    mk_rep_app ((wrap_arg, unboxer) : prs) con_app
      = do { (rep_ids, unbox_fn) <- unboxer wrap_arg
           ; expr <- mk_rep_app prs (mkVarApps con_app rep_ids)
           ; return (unbox_fn expr) }

{-
Note [Bangs on imported data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We pass Maybe [HsImplBang] to mkDataConRep to make use of HsImplBangs
from imported modules.

- Nothing <=> use HsSrcBangs
- Just bangs <=> use HsImplBangs

For imported types we can't work it all out from the HsSrcBangs,
because we want to be very sure to follow what the original module
(where the data type was declared) decided, and that depends on what
flags were enabled when it was compiled. So we record the decisions in
the interface file.

The HsImplBangs passed are in 1-1 correspondence with the
dataConOrigArgTys of the DataCon.

-}

-------------------------
newLocal :: Type -> UniqSM Var
newLocal ty = do { uniq <- getUniqueM
                 ; return (mkSysLocal (fsLit "dt") uniq ty) }

-------------------------
dataConSrcToImplBang
   :: DynFlags
   -> FamInstEnvs
   -> Type
   -> HsSrcBang
   -> HsImplBang

dataConSrcToImplBang dflags fam_envs arg_ty
             (HsSrcBang ann unpk NoSrcStrict)
 | xopt LangExt.StrictData dflags -- StrictData => strict field
 = dataConSrcToImplBang dflags fam_envs arg_ty
                 (HsSrcBang ann unpk SrcStrict)
 | otherwise -- no StrictData => lazy field
 = HsLazy

dataConSrcToImplBang _ _ _ (HsSrcBang _ _ SrcLazy)
   = HsLazy

dataConSrcToImplBang dflags fam_envs arg_ty
    (HsSrcBang _ unpk_prag SrcStrict)  -- {-# UNPACK #-} !
  | not (gopt Opt_OmitInterfacePragmas dflags) -- Don't unpack if -fomit-iface-pragmas
          -- Don't unpack if we aren't optimising; rather arbitrarily,
          -- we use -fomit-iface-pragmas as the indication
  , let mb_co   = topNormaliseType_maybe fam_envs arg_ty
                     -- Unwrap type families and newtypes
        arg_ty' = case mb_co of { Just (_,ty) -> ty; Nothing -> arg_ty }
  , isUnpackableType dflags fam_envs arg_ty'
  , (rep_tys, _) <- dataConArgUnpack arg_ty'
  , case unpk_prag of
      NoSrcUnpack ->
        gopt Opt_UnboxStrictFields dflags
            || (gopt Opt_UnboxSmallStrictFields dflags
                && length rep_tys <= 1) -- See Note [Unpack one-wide fields]
      srcUnpack -> isSrcUnpacked srcUnpack
  = case mb_co of
      Nothing     -> HsUnpack Nothing
      Just (co,_) -> HsUnpack (Just co)

  | otherwise  -- Record the strict-but-no-unpack decision
  = HsStrict

-- | Wrappers/Workser and representation following Unpack/Strictness
-- decisions
dataConArgRep
  :: Type
  -> HsImplBang
  -> ([(Type,StrictnessMark)] -- Rep types
     ,(Unboxer,Boxer))

dataConArgRep arg_ty HsLazy
 = ([(arg_ty, NotMarkedStrict)], (unitUnboxer, unitBoxer))

dataConArgRep arg_ty HsStrict
 = ([(arg_ty, MarkedStrict)], (seqUnboxer, unitBoxer))

dataConArgRep arg_ty (HsUnpack Nothing)
 | (rep_tys, wrappers) <- dataConArgUnpack arg_ty
 = (rep_tys, wrappers)

dataConArgRep _ (HsUnpack (Just co))
 | let co_rep_ty = pSnd (coercionKind co)
 , (rep_tys, wrappers) <- dataConArgUnpack co_rep_ty
 = (rep_tys, wrapCo co co_rep_ty wrappers)

-------------------------
wrapCo :: Coercion -> Type -> (Unboxer, Boxer) -> (Unboxer, Boxer)
wrapCo co rep_ty (unbox_rep, box_rep)  -- co :: arg_ty ~ rep_ty
  = (unboxer, boxer)
  where
    unboxer arg_id = do { rep_id <- newLocal rep_ty
                        ; (rep_ids, rep_fn) <- unbox_rep rep_id
                        ; let co_bind = NonRec rep_id (Var arg_id `Cast` co)
                        ; return (rep_ids, Let co_bind . rep_fn) }
    boxer = Boxer $ \ subst ->
            do { (rep_ids, rep_expr)
                    <- case box_rep of
                         UnitBox -> do { rep_id <- newLocal (TcType.substTy subst rep_ty)
                                       ; return ([rep_id], Var rep_id) }
                         Boxer boxer -> boxer subst
               ; let sco = substCo (tvCvSubst subst) co
               ; return (rep_ids, rep_expr `Cast` mkSymCo sco) }

------------------------
seqUnboxer :: Unboxer
seqUnboxer v = return ([v], \e -> Case (Var v) v (exprType e) [(DEFAULT, [], e)])

unitUnboxer :: Unboxer
unitUnboxer v = return ([v], \e -> e)

unitBoxer :: Boxer
unitBoxer = UnitBox

-------------------------
dataConArgUnpack
   :: Type
   ->  ( [(Type, StrictnessMark)]   -- Rep types
       , (Unboxer, Boxer) )

dataConArgUnpack arg_ty
  | Just (tc, tc_args) <- splitTyConApp_maybe arg_ty
  , Just con <- tyConSingleAlgDataCon_maybe tc
      -- NB: check for an *algebraic* data type
      -- A recursive newtype might mean that
      -- 'arg_ty' is a newtype
  , let rep_tys = dataConInstArgTys con tc_args
  = ASSERT( isVanillaDataCon con )
    ( rep_tys `zip` dataConRepStrictness con
    ,( \ arg_id ->
       do { rep_ids <- mapM newLocal rep_tys
          ; let unbox_fn body
                  = Case (Var arg_id) arg_id (exprType body)
                         [(DataAlt con, rep_ids, body)]
          ; return (rep_ids, unbox_fn) }
     , Boxer $ \ subst ->
       do { rep_ids <- mapM (newLocal . TcType.substTy subst) rep_tys
          ; return (rep_ids, Var (dataConWorkId con)
                             `mkTyApps` (substTys subst tc_args)
                             `mkVarApps` rep_ids ) } ) )
  | otherwise
  = pprPanic "dataConArgUnpack" (ppr arg_ty)
    -- An interface file specified Unpacked, but we couldn't unpack it

isUnpackableType :: DynFlags -> FamInstEnvs -> Type -> Bool
-- True if we can unpack the UNPACK the argument type
-- See Note [Recursive unboxing]
-- We look "deeply" inside rather than relying on the DataCons
-- we encounter on the way, because otherwise we might well
-- end up relying on ourselves!
isUnpackableType dflags fam_envs ty
  | Just (tc, _) <- splitTyConApp_maybe ty
  , Just con <- tyConSingleAlgDataCon_maybe tc
  , isVanillaDataCon con
  = ok_con_args (unitNameSet (getName tc)) con
  | otherwise
  = False
  where
    ok_arg tcs (ty, bang) = not (attempt_unpack bang) || ok_ty tcs norm_ty
        where
          norm_ty = topNormaliseType fam_envs ty
    ok_ty tcs ty
      | Just (tc, _) <- splitTyConApp_maybe ty
      , let tc_name = getName tc
      =  not (tc_name `elemNameSet` tcs)
      && case tyConSingleAlgDataCon_maybe tc of
            Just con | isVanillaDataCon con
                    -> ok_con_args (tcs `extendNameSet` getName tc) con
            _ -> True
      | otherwise
      = True

    ok_con_args tcs con
       = all (ok_arg tcs) (dataConOrigArgTys con `zip` dataConSrcBangs con)
         -- NB: dataConSrcBangs gives the *user* request;
         -- We'd get a black hole if we used dataConImplBangs

    attempt_unpack (HsSrcBang _ SrcUnpack NoSrcStrict)
       = xopt LangExt.StrictData dflags
    attempt_unpack (HsSrcBang _ SrcUnpack SrcStrict)
       = True
    attempt_unpack (HsSrcBang _  NoSrcUnpack SrcStrict)
       = True  -- Be conservative
    attempt_unpack (HsSrcBang _  NoSrcUnpack NoSrcStrict)
       = xopt LangExt.StrictData dflags -- Be conservative
    attempt_unpack _ = False

{-
Note [Unpack one-wide fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The flag UnboxSmallStrictFields ensures that any field that can
(safely) be unboxed to a word-sized unboxed field, should be so unboxed.
For example:

    data A = A Int#
    newtype B = B A
    data C = C !B
    data D = D !C
    data E = E !()
    data F = F !D
    data G = G !F !F

All of these should have an Int# as their representation, except
G which should have two Int#s.

However

    data T = T !(S Int)
    data S = S !a

Here we can represent T with an Int#.

Note [Recursive unboxing]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data R = MkR {-# UNPACK #-} !S Int
  data S = MkS {-# UNPACK #-} !Int
The representation arguments of MkR are the *representation* arguments
of S (plus Int); the rep args of MkS are Int#.  This is all fine.

But be careful not to try to unbox this!
        data T = MkT {-# UNPACK #-} !T Int
Because then we'd get an infinite number of arguments.

Here is a more complicated case:
        data S = MkS {-# UNPACK #-} !T Int
        data T = MkT {-# UNPACK #-} !S Int
Each of S and T must decide independently whether to unpack
and they had better not both say yes. So they must both say no.

Also behave conservatively when there is no UNPACK pragma
        data T = MkS !T Int
with -funbox-strict-fields or -funbox-small-strict-fields
we need to behave as if there was an UNPACK pragma there.

But it's the *argument* type that matters. This is fine:
        data S = MkS S !Int
because Int is non-recursive.


Note [Unpack equality predicates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have a GADT with a constructor C :: (a~[b]) => b -> T a
we definitely want that equality predicate *unboxed* so that it
takes no space at all.  This is easily done: just give it
an UNPACK pragma. The rest of the unpack/repack code does the
heavy lifting.  This one line makes every GADT take a word less
space for each equality predicate, so it's pretty important!
-}

mk_pred_strict_mark :: PredType -> HsImplBang
mk_pred_strict_mark pred
  | isEqPred pred = HsUnpack Nothing
  -- Note [Unpack equality predicates]
  | otherwise     = HsLazy

{-
************************************************************************
*                                                                      *
        Wrapping and unwrapping newtypes and type families
*                                                                      *
************************************************************************
-}

wrapNewTypeBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
-- The wrapper for the data constructor for a newtype looks like this:
--      newtype T a = MkT (a,Int)
--      MkT :: forall a. (a,Int) -> T a
--      MkT = /\a. \(x:(a,Int)). x `cast` sym (CoT a)
-- where CoT is the coercion TyCon associated with the newtype
--
-- The call (wrapNewTypeBody T [a] e) returns the
-- body of the wrapper, namely
--      e `cast` (CoT [a])
--
-- If a coercion constructor is provided in the newtype, then we use
-- it, otherwise the wrap/unwrap are both no-ops
--
-- If the we are dealing with a newtype *instance*, we have a second coercion
-- identifying the family instance with the constructor of the newtype
-- instance.  This coercion is applied in any case (ie, composed with the
-- coercion constructor of the newtype or applied by itself).

wrapNewTypeBody tycon args result_expr
  = ASSERT( isNewTyCon tycon )
    wrapFamInstBody tycon args $
    mkCast result_expr (mkSymCo co)
  where
    co = mkUnbranchedAxInstCo Representational (newTyConCo tycon) args

-- When unwrapping, we do *not* apply any family coercion, because this will
-- be done via a CoPat by the type checker.  We have to do it this way as
-- computing the right type arguments for the coercion requires more than just
-- a splitting operation (cf, TcPat.tcConPat).

unwrapNewTypeBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
unwrapNewTypeBody tycon args result_expr
  = ASSERT( isNewTyCon tycon )
    mkCast result_expr (mkUnbranchedAxInstCo Representational (newTyConCo tycon) args)

-- If the type constructor is a representation type of a data instance, wrap
-- the expression into a cast adjusting the expression type, which is an
-- instance of the representation type, to the corresponding instance of the
-- family instance type.
-- See Note [Wrappers for data instance tycons]
wrapFamInstBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
wrapFamInstBody tycon args body
  | Just co_con <- tyConFamilyCoercion_maybe tycon
  = mkCast body (mkSymCo (mkUnbranchedAxInstCo Representational co_con args))
  | otherwise
  = body

-- Same as `wrapFamInstBody`, but for type family instances, which are
-- represented by a `CoAxiom`, and not a `TyCon`
wrapTypeFamInstBody :: CoAxiom br -> Int -> [Type] -> CoreExpr -> CoreExpr
wrapTypeFamInstBody axiom ind args body
  = mkCast body (mkSymCo (mkAxInstCo Representational axiom ind args))

wrapTypeUnbranchedFamInstBody :: CoAxiom Unbranched -> [Type] -> CoreExpr -> CoreExpr
wrapTypeUnbranchedFamInstBody axiom
  = wrapTypeFamInstBody axiom 0

unwrapFamInstScrut :: TyCon -> [Type] -> CoreExpr -> CoreExpr
unwrapFamInstScrut tycon args scrut
  | Just co_con <- tyConFamilyCoercion_maybe tycon
  = mkCast scrut (mkUnbranchedAxInstCo Representational co_con args) -- data instances only
  | otherwise
  = scrut

unwrapTypeFamInstScrut :: CoAxiom br -> Int -> [Type] -> CoreExpr -> CoreExpr
unwrapTypeFamInstScrut axiom ind args scrut
  = mkCast scrut (mkAxInstCo Representational axiom ind args)

unwrapTypeUnbranchedFamInstScrut :: CoAxiom Unbranched -> [Type] -> CoreExpr -> CoreExpr
unwrapTypeUnbranchedFamInstScrut axiom
  = unwrapTypeFamInstScrut axiom 0

{-
************************************************************************
*                                                                      *
\subsection{Primitive operations}
*                                                                      *
************************************************************************
-}

mkPrimOpId :: PrimOp -> Id
mkPrimOpId prim_op
  = id
  where
    (tyvars,arg_tys,res_ty, arity, strict_sig) = primOpSig prim_op
    ty   = mkForAllTys tyvars (mkFunTys arg_tys res_ty)
    name = mkWiredInName gHC_PRIM (primOpOcc prim_op)
                         (mkPrimOpIdUnique (primOpTag prim_op))
                         (AnId id) UserSyntax
    id   = mkGlobalId (PrimOpId prim_op) name ty info

    info = noCafIdInfo
           `setRuleInfo`          mkRuleInfo (maybeToList $ primOpRules name prim_op)
           `setArityInfo`         arity
           `setStrictnessInfo`    strict_sig
           `setInlinePragInfo`    neverInlinePragma
               -- We give PrimOps a NOINLINE pragma so that we don't
               -- get silly warnings from Desugar.dsRule (the inline_shadows_rule
               -- test) about a RULE conflicting with a possible inlining
               -- cf Trac #7287

-- For each ccall we manufacture a separate CCallOpId, giving it
-- a fresh unique, a type that is correct for this particular ccall,
-- and a CCall structure that gives the correct details about calling
-- convention etc.
--
-- The *name* of this Id is a local name whose OccName gives the full
-- details of the ccall, type and all.  This means that the interface
-- file reader can reconstruct a suitable Id

mkFCallId :: DynFlags -> Unique -> ForeignCall -> Type -> Id
mkFCallId dflags uniq fcall ty
  = ASSERT( isEmptyVarSet (tyVarsOfType ty) )
    -- A CCallOpId should have no free type variables;
    -- when doing substitutions won't substitute over it
    mkGlobalId (FCallId fcall) name ty info
  where
    occ_str = showSDoc dflags (braces (ppr fcall <+> ppr ty))
    -- The "occurrence name" of a ccall is the full info about the
    -- ccall; it is encoded, but may have embedded spaces etc!

    name = mkFCallName uniq occ_str

    info = noCafIdInfo
           `setArityInfo`         arity
           `setStrictnessInfo`    strict_sig

    (_, tau)        = tcSplitForAllTys ty
    (arg_tys, _)    = tcSplitFunTys tau
    arity           = length arg_tys

    strict_sig      = mkClosedStrictSig (replicate arity topDmd) topRes
    -- the call does not claim to be strict in its arguments, since they
    -- may be lifted (foreign import prim) and the called code doesn't
    -- necessarily force them. See Trac #11076.
{-
************************************************************************
*                                                                      *
\subsection{DictFuns and default methods}
*                                                                      *
************************************************************************

Note [Dict funs and default methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Dict funs and default methods are *not* ImplicitIds.  Their definition
involves user-written code, so we can't figure out their strictness etc
based on fixed info, as we can for constructors and record selectors (say).

NB: See also Note [Exported LocalIds] in Id
-}

mkDictFunId :: Name      -- Name to use for the dict fun;
            -> [TyVar]
            -> ThetaType
            -> Class
            -> [Type]
            -> Id
-- Implements the DFun Superclass Invariant (see TcInstDcls)
-- See Note [Dict funs and default methods]

mkDictFunId dfun_name tvs theta clas tys
  = mkExportedLocalId (DFunId n_silent is_nt)
                      dfun_name
                      dfun_ty
  where
    is_nt = isNewTyCon (classTyCon clas)
    (n_silent, dfun_ty) = mkDictFunTy tvs theta clas tys

mkDictFunTy :: [TyVar] -> ThetaType -> Class -> [Type] -> (Int, Type)
mkDictFunTy tvs theta clas tys
  = (length silent_theta, dfun_ty)
  where
    dfun_ty = mkSigmaTy tvs (silent_theta ++ theta) (mkClassPred clas tys)
    silent_theta
      | null tvs, null theta
      = []
      | otherwise
      = filterOut discard $
        substTheta (zipTopTvSubst (classTyVars clas) tys)
                   (classSCTheta clas)
                   -- See Note [Silent Superclass Arguments]
    discard pred = any (`eqPred` pred) theta
                 -- See the DFun Superclass Invariant in TcInstDcls

{-
************************************************************************
*                                                                      *
\subsection{Un-definable}
*                                                                      *
************************************************************************

These Ids can't be defined in Haskell.  They could be defined in
unfoldings in the wired-in GHC.Prim interface file, but we'd have to
ensure that they were definitely, definitely inlined, because there is
no curried identifier for them.  That's what mkCompulsoryUnfolding
does.  If we had a way to get a compulsory unfolding from an interface
file, we could do that, but we don't right now.

unsafeCoerce# isn't so much a PrimOp as a phantom identifier, that
just gets expanded into a type coercion wherever it occurs.  Hence we
add it as a built-in Id with an unfolding here.

The type variables we use here are "open" type variables: this means
they can unify with both unlifted and lifted types.  Hence we provide
another gun with which to shoot yourself in the foot.
-}

lazyIdName, unsafeCoerceName, nullAddrName, seqName,
   realWorldName, voidPrimIdName, coercionTokenName,
   magicDictName, coerceName, proxyName, dollarName, oneShotName,
   runRWName, noinlineIdName :: Name
unsafeCoerceName  = mkWiredInIdName gHC_PRIM  (fsLit "unsafeCoerce#")  unsafeCoerceIdKey  unsafeCoerceId
nullAddrName      = mkWiredInIdName gHC_PRIM  (fsLit "nullAddr#")      nullAddrIdKey      nullAddrId
seqName           = mkWiredInIdName gHC_PRIM  (fsLit "seq")            seqIdKey           seqId
realWorldName     = mkWiredInIdName gHC_PRIM  (fsLit "realWorld#")     realWorldPrimIdKey realWorldPrimId
voidPrimIdName    = mkWiredInIdName gHC_PRIM  (fsLit "void#")          voidPrimIdKey      voidPrimId
lazyIdName        = mkWiredInIdName gHC_MAGIC (fsLit "lazy")           lazyIdKey          lazyId
coercionTokenName = mkWiredInIdName gHC_PRIM  (fsLit "coercionToken#") coercionTokenIdKey coercionTokenId
magicDictName     = mkWiredInIdName gHC_PRIM  (fsLit "magicDict")      magicDictKey       magicDictId
coerceName        = mkWiredInIdName gHC_PRIM  (fsLit "coerce")         coerceKey          coerceId
proxyName         = mkWiredInIdName gHC_PRIM  (fsLit "proxy#")         proxyHashKey       proxyHashId
dollarName        = mkWiredInIdName gHC_BASE  (fsLit "$")              dollarIdKey        dollarId
oneShotName       = mkWiredInIdName gHC_MAGIC (fsLit "oneShot")        oneShotKey         oneShotId
runRWName         = mkWiredInIdName gHC_MAGIC (fsLit "runRW#")         runRWKey           runRWId
noinlineIdName    = mkWiredInIdName gHC_MAGIC (fsLit "noinline") noinlineIdKey noinlineId

dollarId :: Id  -- Note [dollarId magic]
dollarId = pcMiscPrelId dollarName ty
             (noCafIdInfo `setUnfoldingInfo` unf)
  where
    fun_ty = mkFunTy alphaTy openBetaTy
    ty     = mkForAllTys [alphaTyVar, openBetaTyVar] $
             mkFunTy fun_ty fun_ty
    unf    = mkInlineUnfolding (Just 2) rhs
    [f,x]  = mkTemplateLocals [fun_ty, alphaTy]
    rhs    = mkLams [alphaTyVar, openBetaTyVar, f, x] $
             App (Var f) (Var x)

------------------------------------------------
-- proxy# :: forall a. Proxy# a
proxyHashId :: Id
proxyHashId
  = pcMiscPrelId proxyName ty
       (noCafIdInfo `setUnfoldingInfo` evaldUnfolding) -- Note [evaldUnfoldings]
  where
    ty      = mkForAllTys [kv, tv] (mkProxyPrimTy k t)
    kv      = kKiVar
    k       = mkTyVarTy kv
    [tv]    = mkTemplateTyVars [k]
    t       = mkTyVarTy tv

------------------------------------------------
-- unsafeCoerce# :: forall a b. a -> b
unsafeCoerceId :: Id
unsafeCoerceId
  = pcMiscPrelId unsafeCoerceName ty info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs


    ty  = mkForAllTys [openAlphaTyVar,openBetaTyVar]
                      (mkFunTy openAlphaTy openBetaTy)
    [x] = mkTemplateLocals [openAlphaTy]
    rhs = mkLams [openAlphaTyVar,openBetaTyVar,x] $
          Cast (Var x) (mkUnsafeCo openAlphaTy openBetaTy)

------------------------------------------------
nullAddrId :: Id
-- nullAddr# :: Addr#
-- The reason is is here is because we don't provide
-- a way to write this literal in Haskell.
nullAddrId = pcMiscPrelId nullAddrName addrPrimTy info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding (Lit nullAddrLit)

------------------------------------------------
seqId :: Id     -- See Note [seqId magic]
seqId = pcMiscPrelId seqName ty info
  where
    info = noCafIdInfo `setInlinePragInfo` inline_prag
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
                       `setRuleInfo`       mkRuleInfo [seq_cast_rule]

    inline_prag = alwaysInlinePragma `setInlinePragmaActivation` ActiveAfter 0
                  -- Make 'seq' not inline-always, so that simpleOptExpr
                  -- (see CoreSubst.simple_app) won't inline 'seq' on the
                  -- LHS of rules.  That way we can have rules for 'seq';
                  -- see Note [seqId magic]

    ty  = mkForAllTys [alphaTyVar,betaTyVar]
                      (mkFunTy alphaTy (mkFunTy betaTy betaTy))
              -- NB argBetaTyVar; see Note [seqId magic]

    [x,y] = mkTemplateLocals [alphaTy, betaTy]
    rhs = mkLams [alphaTyVar,betaTyVar,x,y] (Case (Var x) x betaTy [(DEFAULT, [], Var y)])

    -- See Note [Built-in RULES for seq]
    -- NB: ru_nargs = 3, not 4, to match the code in
    --     Simplify.rebuildCase which tries to apply this rule
    seq_cast_rule = BuiltinRule { ru_name  = fsLit "seq of cast"
                                , ru_fn    = seqName
                                , ru_nargs = 3
                                , ru_try   = match_seq_of_cast }

match_seq_of_cast :: RuleFun
    -- See Note [Built-in RULES for seq]
match_seq_of_cast _ _ _ [Type _, Type res_ty, Cast scrut co]
  = Just (Var seqId `mkApps` [Type (pFst (coercionKind co)), Type res_ty,
                              scrut])
match_seq_of_cast _ _ _ _ = Nothing

------------------------------------------------
lazyId :: Id    -- See Note [lazyId magic]
lazyId = pcMiscPrelId lazyIdName ty info
  where
    info = noCafIdInfo
    ty  = mkForAllTys [alphaTyVar] (mkFunTy alphaTy alphaTy)

noinlineId :: Id -- See Note [noinlineId magic]
noinlineId = pcMiscPrelId noinlineIdName ty info
  where
    info = noCafIdInfo
    ty  = mkForAllTys [alphaTyVar] (mkFunTy alphaTy alphaTy)

oneShotId :: Id -- See Note [The oneShot function]
oneShotId = pcMiscPrelId oneShotName ty info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
    ty  = mkForAllTys [alphaTyVar, betaTyVar] (mkFunTy fun_ty fun_ty)
    fun_ty = mkFunTy alphaTy betaTy
    [body, x] = mkTemplateLocals [fun_ty, alphaTy]
    x' = setOneShotLambda x
    rhs = mkLams [alphaTyVar, betaTyVar, body, x'] $ Var body `App` Var x

runRWId :: Id -- See Note [runRW magic] in this module
runRWId = pcMiscPrelId runRWName ty info
  where
    info    = noCafIdInfo `setInlinePragInfo` neverInlinePragma
                          `setStrictnessInfo` strict_sig
                          `setArityInfo`      1

    strict_sig = mkClosedStrictSig [strictApply1Dmd] topRes
      -- Important to express its strictness,
      -- since it is not inlined until CorePrep
      -- Also see Note [runRW arg] in CorePrep

    -- State# RealWorld
    stateRW = mkTyConApp statePrimTyCon [realWorldTy]
    -- o
    ret_ty  = openAlphaTy
    -- State# RealWorld -> o
    arg_ty  = stateRW `mkFunTy` ret_ty
    -- (State# RealWorld -> o ) -> o
    ty      = mkForAllTys [openAlphaTyVar] (arg_ty `mkFunTy` ret_ty)

--------------------------------------------------------------------------------
magicDictId :: Id  -- See Note [magicDictId magic]
magicDictId = pcMiscPrelId magicDictName ty info
  where
  info = noCafIdInfo `setInlinePragInfo` neverInlinePragma
  ty   = mkForAllTys [alphaTyVar] alphaTy

--------------------------------------------------------------------------------

coerceId :: Id
coerceId = pcMiscPrelId coerceName ty info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
    eqRTy     = mkTyConApp coercibleTyCon  [liftedTypeKind, alphaTy, betaTy]
    eqRPrimTy = mkTyConApp eqReprPrimTyCon [liftedTypeKind, alphaTy, betaTy]
    ty        = mkForAllTys [alphaTyVar, betaTyVar] $
                mkFunTys [eqRTy, alphaTy] betaTy

    [eqR,x,eq] = mkTemplateLocals [eqRTy, alphaTy, eqRPrimTy]
    rhs = mkLams [alphaTyVar, betaTyVar, eqR, x] $
          mkWildCase (Var eqR) eqRTy betaTy $
          [(DataAlt coercibleDataCon, [eq], Cast (Var x) (CoVarCo eq))]

{-
Note [dollarId magic]
~~~~~~~~~~~~~~~~~~~~~
The only reason that ($) is wired in is so that its type can be
    forall (a:*, b:Open). (a->b) -> a -> b
That is, the return type can be unboxed.  E.g. this is OK
    foo $ True    where  foo :: Bool -> Int#
because ($) doesn't inspect or move the result of the call to foo.
See Trac #8739.

There is a special typing rule for ($) in TcExpr, so the type of ($)
isn't looked at there, BUT Lint subsequently (and rightly) complains
if sees ($) applied to Int# (say), unless we give it a wired-in type
as we do here.

Note [Unsafe coerce magic]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We define a *primitive*
   GHC.Prim.unsafeCoerce#
and then in the base library we define the ordinary function
   Unsafe.Coerce.unsafeCoerce :: forall (a:*) (b:*). a -> b
   unsafeCoerce x = unsafeCoerce# x

Notice that unsafeCoerce has a civilized (albeit still dangerous)
polymorphic type, whose type args have kind *.  So you can't use it on
unboxed values (unsafeCoerce 3#).

In contrast unsafeCoerce# is even more dangerous because you *can* use
it on unboxed things, (unsafeCoerce# 3#) :: Int. Its type is
   forall (a:OpenKind) (b:OpenKind). a -> b

Note [seqId magic]
~~~~~~~~~~~~~~~~~~
'GHC.Prim.seq' is special in several ways.

a) Its second arg can have an unboxed type
      x `seq` (v +# w)
   Hence its second type variable has ArgKind

b) Its fixity is set in LoadIface.ghcPrimIface

c) It has quite a bit of desugaring magic.
   See DsUtils.lhs Note [Desugaring seq (1)] and (2) and (3)

d) There is some special rule handing: Note [User-defined RULES for seq]

e) See Note [Typing rule for seq] in TcExpr.

Note [User-defined RULES for seq]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Roman found situations where he had
      case (f n) of _ -> e
where he knew that f (which was strict in n) would terminate if n did.
Notice that the result of (f n) is discarded. So it makes sense to
transform to
      case n of _ -> e

Rather than attempt some general analysis to support this, I've added
enough support that you can do this using a rewrite rule:

  RULE "f/seq" forall n.  seq (f n) = seq n

You write that rule.  When GHC sees a case expression that discards
its result, it mentally transforms it to a call to 'seq' and looks for
a RULE.  (This is done in Simplify.rebuildCase.)  As usual, the
correctness of the rule is up to you.

VERY IMPORTANT: to make this work, we give the RULE an arity of 1, not 2.
If we wrote
  RULE "f/seq" forall n e.  seq (f n) e = seq n e
with rule arity 2, then two bad things would happen:

  - The magical desugaring done in Note [seqId magic] item (c)
    for saturated application of 'seq' would turn the LHS into
    a case expression!

  - The code in Simplify.rebuildCase would need to actually supply
    the value argument, which turns out to be awkward.

Note [Built-in RULES for seq]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We also have the following built-in rule for seq

  seq (x `cast` co) y = seq x y

This eliminates unnecessary casts and also allows other seq rules to
match more often.  Notably,

   seq (f x `cast` co) y  -->  seq (f x) y

and now a user-defined rule for seq (see Note [User-defined RULES for seq])
may fire.


Note [lazyId magic]
~~~~~~~~~~~~~~~~~~~
    lazy :: forall a?. a? -> a?   (i.e. works for unboxed types too)

Used to lazify pseq:   pseq a b = a `seq` lazy b

Also, no strictness: by being a built-in Id, all the info about lazyId comes from here,
not from GHC.Base.hi.   This is important, because the strictness
analyser will spot it as strict!

Also no unfolding in lazyId: it gets "inlined" by a HACK in CorePrep.
It's very important to do this inlining *after* unfoldings are exposed
in the interface file.  Otherwise, the unfolding for (say) pseq in the
interface file will not mention 'lazy', so if we inline 'pseq' we'll totally
miss the very thing that 'lazy' was there for in the first place.
See Trac #3259 for a real world example.

lazyId is defined in GHC.Base, so we don't *have* to inline it.  If it
appears un-applied, we'll end up just calling it.

Note [noinlineId magic]
~~~~~~~~~~~~~~~~~~~~~~~
noinline :: forall a. a -> a

'noinline' is used to make sure that a function f is never inlined,
e.g., as in 'noinline f x'.  Ordinarily, the identity function with NOINLINE
could be used to achieve this effect; however, this has the unfortunate
result of leaving a (useless) call to noinline at runtime.  So we have
a little bit of magic to optimize away 'noinline' after we are done
running the simplifier.

'noinline' needs to be wired-in because it gets inserted automatically
when we serialize an expression to the interface format, and we DON'T
want use its fingerprints.

Note [runRW magic]
~~~~~~~~~~~~~~~~~~
Some definitions, for instance @runST@, must have careful control over float out
of the bindings in their body. Consider this use of @runST@,

    f x = runST ( \ s -> let (a, s')  = newArray# 100 [] s
                             (_, s'') = fill_in_array_or_something a x s'
                         in freezeArray# a s'' )

If we inline @runST@, we'll get:

    f x = let (a, s')  = newArray# 100 [] realWorld#{-NB-}
              (_, s'') = fill_in_array_or_something a x s'
          in freezeArray# a s''

And now if we allow the @newArray#@ binding to float out to become a CAF,
we end up with a result that is totally and utterly wrong:

    f = let (a, s')  = newArray# 100 [] realWorld#{-NB-} -- YIKES!!!
        in \ x ->
            let (_, s'') = fill_in_array_or_something a x s'
            in freezeArray# a s''

All calls to @f@ will share a {\em single} array! Clearly this is nonsense and
must be prevented.

This is what @runRW#@ gives us: by being inlined extremely late in the
optimization (right before lowering to STG, in CorePrep), we can ensure that
no further floating will occur. This allows us to safely inline things like
@runST@, which are otherwise needlessly expensive (see #10678 and #5916).

While the definition of @GHC.Magic.runRW#@, we override its type in @MkId@
to be open-kinded,

    runRW# :: (o :: OpenKind) => (State# RealWorld -> (# State# RealWorld, o #))
                              -> (# State# RealWorld, o #)

Note [The oneShot function]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the context of making left-folds fuse somewhat okish (see ticket #7994
and Note [Left folds via right fold]) it was determined that it would be useful
if library authors could explicitly tell the compiler that a certain lambda is
called at most once. The oneShot function allows that.

Like most magic functions it has a compulsory unfolding, so there is no need
for a real definition somewhere. We have one in GHC.Magic for the convenience
of putting the documentation there.

It uses `setOneShotLambda` on the lambda's binder. That is the whole magic:

A typical call looks like
     oneShot (\y. e)
after unfolding the definition `oneShot = \f \x[oneshot]. f x` we get
     (\f \x[oneshot]. f x) (\y. e)
 --> \x[oneshot]. ((\y.e) x)
 --> \x[oneshot] e[x/y]
which is what we want.

It is only effective if this bits survives as long as possible and makes it into
the interface in unfoldings (See Note [Preserve OneShotInfo]). Also see
https://ghc.haskell.org/trac/ghc/wiki/OneShot.


Note [magicDictId magic]
~~~~~~~~~~~~~~~~~~~~~~~~~

The identifier `magicDict` is just a place-holder, which is used to
implement a primitive that we cannot define in Haskell but we can write
in Core.  It is declared with a place-holder type:

    magicDict :: forall a. a

The intention is that the identifier will be used in a very specific way,
to create dictionaries for classes with a single method.  Consider a class
like this:

   class C a where
     f :: T a

We are going to use `magicDict`, in conjunction with a built-in Prelude
rule, to cast values of type `T a` into dictionaries for `C a`.  To do
this, we define a function like this in the library:

  data WrapC a b = WrapC (C a => Proxy a -> b)

  withT :: (C a => Proxy a -> b)
        ->  T a -> Proxy a -> b
  withT f x y = magicDict (WrapC f) x y

The purpose of `WrapC` is to avoid having `f` instantiated.
Also, it avoids impredicativity, because `magicDict`'s type
cannot be instantiated with a forall.  The field of `WrapC` contains
a `Proxy` parameter which is used to link the type of the constraint,
`C a`, with the type of the `Wrap` value being made.

Next, we add a built-in Prelude rule (see prelude/PrelRules.hs),
which will replace the RHS of this definition with the appropriate
definition in Core.  The rewrite rule works as follows:

magicDict@t (wrap@a@b f) x y
---->
f (x `cast` co a) y

The `co` coercion is the newtype-coercion extracted from the type-class.
The type class is obtain by looking at the type of wrap.



-------------------------------------------------------------
@realWorld#@ used to be a magic literal, \tr{void#}.  If things get
nasty as-is, change it back to a literal (@Literal@).

voidArgId is a Local Id used simply as an argument in functions
where we just want an arg to avoid having a thunk of unlifted type.
E.g.
        x = \ void :: Void# -> (# p, q #)

This comes up in strictness analysis

Note [evaldUnfoldings]
~~~~~~~~~~~~~~~~~~~~~~
The evaldUnfolding makes it look that some primitive value is
evaluated, which in turn makes Simplify.interestingArg return True,
which in turn makes INLINE things applied to said value likely to be
inlined.
-}

realWorldPrimId :: Id   -- :: State# RealWorld
realWorldPrimId = pcMiscPrelId realWorldName realWorldStatePrimTy
                     (noCafIdInfo `setUnfoldingInfo` evaldUnfolding    -- Note [evaldUnfoldings]
                                  `setOneShotInfo` stateHackOneShot)

voidPrimId :: Id     -- Global constant :: Void#
voidPrimId  = pcMiscPrelId voidPrimIdName voidPrimTy
                (noCafIdInfo `setUnfoldingInfo` evaldUnfolding)    -- Note [evaldUnfoldings]

voidArgId :: Id       -- Local lambda-bound :: Void#
voidArgId = mkSysLocal (fsLit "void") voidArgIdKey voidPrimTy

coercionTokenId :: Id         -- :: () ~ ()
coercionTokenId -- Used to replace Coercion terms when we go to STG
  = pcMiscPrelId coercionTokenName
                 (mkTyConApp eqPrimTyCon [liftedTypeKind, unitTy, unitTy])
                 noCafIdInfo

pcMiscPrelId :: Name -> Type -> IdInfo -> Id
pcMiscPrelId name ty info
  = mkVanillaGlobalWithInfo name ty info
    -- We lie and say the thing is imported; otherwise, we get into
    -- a mess with dependency analysis; e.g., core2stg may heave in
    -- random calls to GHCbase.unpackPS__.  If GHCbase is the module
    -- being compiled, then it's just a matter of luck if the definition
    -- will be in "the right place" to be in scope.
