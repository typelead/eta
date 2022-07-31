{-# LANGUAGE CPP, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2006
--
-- The purpose of this module is to transform an HsExpr into a CoreExpr which
-- when evaluated, returns a (Meta.Q Meta.Exp) computation analogous to the
-- input HsExpr. We do this in the DsM monad, which supplies access to
-- CoreExpr's of the "smart constructors" of the Meta.Exp datatype.
--
-- It also defines a bunch of knownKeyNames, in the same way as is done
-- in prelude/PrelNames.  It's much more convenient to do it here, because
-- otherwise we have to recompile PrelNames whenever we add a Name, which is
-- a Royal Pain (triggers other recompilation).
-----------------------------------------------------------------------------

module Eta.DeSugar.DsMeta( dsBracket ) where

#include "HsVersions.h"

import {-# SOURCE #-} Eta.DeSugar.DsExpr ( dsExpr )

import Eta.DeSugar.MatchLit
import Eta.DeSugar.DsMonad

import qualified Language.Eta.Meta as TH

import Eta.HsSyn.HsSyn
import Eta.Types.Class
import Eta.Prelude.PrelNames
-- To avoid clashes with DsMeta.varName we must make a local alias for
-- OccName.varName we do this by removing varName from the import of
-- OccName above, making a qualified instance of OccName and using
-- OccNameAlias.varName where varName ws previously used in this file.
import qualified Eta.BasicTypes.OccName as OccName ( isDataOcc, isVarOcc, isTcOcc )

import Eta.BasicTypes.Module
import Eta.BasicTypes.Id
import Eta.BasicTypes.Name hiding( isVarOcc, isTcOcc, varName, tcName )
import Eta.Prelude.THNames
import Eta.BasicTypes.NameEnv
-- import Eta.BasicTypes.NameSet
import Eta.TypeCheck.TcType
import Eta.Types.TyCon
import Eta.Prelude.TysWiredIn
import Eta.Prelude.TysPrim
import Eta.Core.CoreSyn
import Eta.Core.MkCore
import Eta.Core.CoreUtils
import Eta.BasicTypes.SrcLoc
import Eta.BasicTypes.Unique
import Eta.BasicTypes.BasicTypes
import Eta.Utils.Outputable
import Eta.Utils.Bag
import Eta.Main.DynFlags
import Eta.Utils.FastString
import Eta.Prelude.ForeignCall
import Eta.Utils.Util
import Eta.Utils.Maybes
import Eta.Utils.MonadUtils
import Data.ByteString ( unpack )
import Control.Monad
import Data.List

-----------------------------------------------------------------------------
dsBracket :: HsBracket Name -> [PendingTcSplice] -> DsM CoreExpr
-- Returns a CoreExpr of type TH.ExpQ
-- The quoted thing is parameterised over Name, even though it has
-- been type checked.  We don't want all those type decorations!

dsBracket brack splices
  = dsExtendMetaEnv new_bit (do_brack brack)
  where
    new_bit = mkNameEnv [(n, DsSplice (unLoc e)) | PendingTcSplice n e <- splices]

    do_brack (VarBr _ n) = do { MkC e1  <- lookupOcc n ; return e1 }
    do_brack (ExpBr e)   = do { MkC e1  <- repLE e     ; return e1 }
    do_brack (PatBr p)   = do { MkC p1  <- repTopP p   ; return p1 }
    do_brack (TypBr t)   = do { MkC t1  <- repLTy t    ; return t1 }
    do_brack (DecBrG gp) = do { MkC ds1 <- repTopDs gp ; return ds1 }
    do_brack (DecBrL {})   = panic "dsBracket: unexpected DecBrL"
    do_brack (TExpBr e)  = do { MkC e1  <- repLE e     ; return e1 }

{- -------------- Examples --------------------

  [| \x -> x |]
====>
  gensym (unpackString "x"#) `bindQ` \ x1::String ->
  lam (pvar x1) (var x1)


  [| \x -> $(f [| x |]) |]
====>
  gensym (unpackString "x"#) `bindQ` \ x1::String ->
  lam (pvar x1) (f (var x1))
-}


-------------------------------------------------------
--                      Declarations
-------------------------------------------------------

repTopP :: LPat Name -> DsM (Core TH.PatQ)
repTopP pat = do { ss <- mkGenSyms (collectPatBinders pat)
                 ; pat' <- addBinds ss (repLP pat)
                 ; wrapGenSyms ss pat' }

repTopDs :: HsGroup Name -> DsM (Core (TH.Q [TH.Dec]))
repTopDs group@(HsGroup { hs_valds   = valds
                        , hs_splcds  = splcds
                        , hs_tyclds  = tyclds
                        , hs_instds  = instds
                        , hs_derivds = derivds
                        , hs_fixds   = fixds
                        , hs_defds   = defds
                        , hs_fords   = fords
                        , hs_warnds  = warnds
                        , hs_annds   = annds
                        , hs_ruleds  = ruleds
                        , hs_vects   = vects
                        , hs_docs    = docs })
 = do { let { bndrs  = hsSigTvBinders valds
                       ++ hsGroupBinders group
                      --  ++ hsPatSynSelectors valds
            -- ; instds = tyclds >>= group_instds
            } ;
        ss <- mkGenSyms bndrs ;

        -- Bind all the names mainly to avoid repeated use of explicit strings.
        -- Thus we get
        --      do { t :: String <- genSym "T" ;
        --           return (Data t [] ...more t's... }
        -- The other important reason is that the output must mention
        -- only "T", not "Foo:T" where Foo is the current module

        decls <- addBinds ss (
                  do { val_ds   <- rep_val_binds valds
                     ; _        <- mapM no_splice splcds
                     ; tycl_ds  <- mapM repTyClD (tyClGroupConcat tyclds)
                     ; role_ds  <- mapM repRoleD (concatMap group_roles tyclds)
                     ; inst_ds  <- mapM repInstD instds
                     ; deriv_ds <- mapM repStandaloneDerivD derivds
                     ; fix_ds   <- mapM repFixD fixds
                     ; _        <- mapM no_default_decl defds
                     ; for_ds   <- mapM repForD fords
                     ; _        <- mapM no_warn (concatMap (wd_warnings . unLoc)
                                                           warnds)
                     ; ann_ds   <- mapM repAnnD annds
                     ; rule_ds  <- mapM repRuleD (concatMap (rds_rules . unLoc)
                                                            ruleds)
                     ; _        <- mapM no_vect vects
                     ; _        <- mapM no_doc docs

                        -- more needed
                     ;  return (de_loc $ sort_by_loc $
                                val_ds ++ catMaybes tycl_ds ++ role_ds
                                       ++ (concat fix_ds)
                                       ++ inst_ds ++ rule_ds ++ for_ds
                                       ++ ann_ds ++ deriv_ds) }) ;

        decl_ty <- lookupType decQTyConName ;
        let { core_list = coreList' decl_ty decls } ;

        dec_ty <- lookupType decTyConName ;
        q_decs  <- repSequenceQ dec_ty core_list ;

        wrapGenSyms ss q_decs
      }
  where
    no_splice (L loc _)
      = notHandledL loc "Splices within declaration brackets" empty
    no_default_decl (L loc decl)
      = notHandledL loc "Default declarations" (ppr decl)
    no_warn (L loc (Warning thing _))
      = notHandledL loc "WARNING and DEPRECATION pragmas" $
                    text "Pragma for declaration of" <+> ppr thing
    no_vect (L loc decl)
      = notHandledL loc "Vectorisation pragmas" (ppr decl)
    no_doc (L loc _)
      = notHandledL loc "Haddock documentation" empty

hsSigTvBinders :: HsValBinds Name -> [Name]
-- See Note [Scoped type variables in bindings]
hsSigTvBinders binds
    = concatMap get_scoped_tvs sigs
  where
    sigs = case binds of
             ValBindsIn  _ sigs -> sigs
             ValBindsOut _ sigs -> sigs

get_scoped_tvs :: LSig Name -> [Name]
get_scoped_tvs sig
  | L _ (TypeSig _ (L _ (HsForAllTy Explicit _ qtvs _ _)) _ _) <- sig
  = map hsLTyVarName $ hsQTvBndrs qtvs
  | L _ (GenericSig _ (L _ (HsForAllTy Explicit _ qtvs _ _))) <- sig
  = map hsLTyVarName $ hsQTvBndrs qtvs
  | otherwise = []
-- get_scoped_tvs (L _ signature)
--   | TypeSig _ _ sig <- signature
--   = get_scoped_tvs_from_sig (hswc_body sig)
--   | ClassOpSig _ _ _ sig <- signature
--   = get_scoped_tvs_from_sig sig
--   | PatSynSig _ _ sig <- signature
--   = get_scoped_tvs_from_sig sig
--   | otherwise
--   = []
--   where
--     get_scoped_tvs_from_sig sig
--       -- Both implicit and explicit quantified variables
--       -- We need the implicit ones for   f :: forall (a::k). blah
--       --    here 'k' scopes too
--       | HsIB { hsib_ext = HsIBRn { hsib_vars = implicit_vars }
--              , hsib_body = hs_ty } <- sig
--       , (explicit_vars, _) <- splitLHsForAllTy hs_ty
--       = implicit_vars ++ map hsLTyVarName explicit_vars
--     get_scoped_tvs_from_sig (XHsImplicitBndrs _)
--       = panic "get_scoped_tvs_from_sig"

{- Notes

Note [Scoped type variables in bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f :: forall a. a -> a
   f x = x::a
Here the 'forall a' brings 'a' into scope over the binding group.
To achieve this we

  a) Gensym a binding for 'a' at the same time as we do one for 'f'
     collecting the relevant binders with hsSigTvBinders

  b) When processing the 'forall', don't gensym

The relevant places are signposted with references to this Note

Note [Scoped type variables in class and instance declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Scoped type variables may occur in default methods and default
signatures. We need to bring the type variables in 'foralls'
into the scope of the method bindings.

Consider
   class Foo a where
     foo :: forall (b :: k). a -> Proxy b -> Proxy b
     foo _ x = (x :: Proxy b)

We want to ensure that the 'b' in the type signature and the default
implementation are the same, so we do the following:

  a) Before desugaring the signature and binding of 'foo', use
     get_scoped_tvs to collect type variables in 'forall' and
     create symbols for them.
  b) Use 'addBinds' to bring these symbols into the scope of the type
     signatures and bindings.
  c) Use these symbols to generate Core for the class/instance declaration.

Note that when desugaring the signatures, we lookup the type variables
from the scope rather than recreate symbols for them. See more details
in "rep_ty_sig" and in Trac#14885.

Note [Binders and occurrences]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we desugar [d| data T = MkT |]
we want to get
        Data "T" [] [Con "MkT" []] []
and *not*
        Data "Foo:T" [] [Con "Foo:MkT" []] []
That is, the new data decl should fit into whatever new module it is
asked to fit in.   We do *not* clone, though; no need for this:
        Data "T79" ....

But if we see this:
        data T = MkT
        foo = reifyDecl T

then we must desugar to
        foo = Data "Foo:T" [] [Con "Foo:MkT" []] []

So in repTopDs we bring the binders into scope with mkGenSyms and addBinds.
And we use lookupOcc, rather than lookupBinder
in repTyClD and repC.

Note [Don't quantify implicit type variables in quotes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If you're not careful, it's surprisingly easy to take this quoted declaration:

  [d| idProxy :: forall proxy (b :: k). proxy b -> proxy b
      idProxy x = x
    |]

and have Template Haskell turn it into this:

  idProxy :: forall k proxy (b :: k). proxy b -> proxy b
  idProxy x = x

Notice that we explicitly quantified the variable `k`! This is quite bad, as the
latter declaration requires -XTypeInType, while the former does not. Not to
mention that the latter declaration isn't even what the user wrote in the
first place.

Usually, the culprit behind these bugs is taking implicitly quantified type
variables (often from the hsib_vars field of HsImplicitBinders) and putting
them into a `ForallT` or `ForallC`. Doing so caused #13018 and #13123.
-}

-- represent associated family instances
--
repTyClD :: LTyClDecl Name -> DsM (Maybe (SrcSpan, Core TH.DecQ))

repTyClD (L loc (FamDecl { tcdFam = fam })) = liftM Just $ repFamilyDecl (L loc fam)

repTyClD (L loc (SynDecl { tcdLName = tc, tcdTyVars = tvs, tcdRhs = rhs }))
  = do { tc1 <- lookupLOcc tc           -- See note [Binders and occurrences]
       ; dec <- addTyClTyVarBinds tvs $ \bndrs ->
                repSynDecl tc1 bndrs rhs
       ; return (Just (loc, dec)) }

repTyClD (L loc (DataDecl { tcdLName = tc, tcdTyVars = tvs, tcdDataDefn = defn }))
  = do { tc1 <- lookupLOcc tc
             -- See note [Binders and occurrences]
       ; tc_tvs <- mk_extra_tvs tc tvs defn
       ; dec <- addTyClTyVarBinds tvs $ \bndrs ->
                repDataDefn tc1 bndrs Nothing (hsLTyVarNames tc_tvs) defn
       ; return (Just (loc, dec)) }

repTyClD (L loc (ClassDecl { tcdCtxt = cxt, tcdLName = cls,
                             tcdTyVars = tvs, tcdFDs = fds,
                             tcdSigs = sigs, tcdMeths = meth_binds,
                             tcdATs = ats, tcdATDefs = atds }))
  = do { cls1 <- lookupLOcc cls         -- See note [Binders and occurrences]
       ; dec  <- addTyVarBinds tvs $ \bndrs ->
           do { cxt1   <- repLContext cxt
              -- See Note [Scoped type variables in class and instance declarations]
              ; (ss, sigs_binds) <- rep_sigs_binds sigs meth_binds
              ; fds1   <- repLFunDeps fds
              ; ats1   <- repFamilyDecls ats
              ; atds1  <- repAssocTyFamDefaults atds
              ; decls1 <- coreList decQTyConName (ats1 ++ atds1 ++ sigs_binds)
              ; decls2 <- repClass cxt1 cls1 bndrs fds1 decls1
              ; wrapGenSyms ss decls2 }
       ; return $ Just (loc, dec)
       }

-- repTyClD (L loc d) = putSrcSpanDs loc $
--                     do { warnDs (hang ds_msg 4 (ppr d))
--                        ; return Nothing }

-------------------------
repRoleD :: LRoleAnnotDecl Name -> DsM (SrcSpan, Core TH.DecQ)
repRoleD (L loc (RoleAnnotDecl tycon roles))
  = do { tycon1 <- lookupLOcc tycon
       ; roles1 <- mapM repRole roles
       ; roles2 <- coreList roleTyConName roles1
       ; dec <- repRoleAnnotD tycon1 roles2
       ; return (loc, dec) }

-------------------------
repDataDefn :: Core TH.Name -> Core [TH.TyVarBndr]
            -> Maybe (Core [TH.TypeQ])
            -> [Name]
            -> HsDataDefn Name
            -> DsM (Core TH.DecQ)
repDataDefn tc bndrs opt_tys tv_names
          (HsDataDefn { dd_ND = new_or_data, dd_ctxt = cxt, dd_kindSig = ksig
                      , dd_cons = cons, dd_derivs = mb_derivs })
  = do { cxt1     <- repLContext cxt
       ; derivs1  <- repDerivs mb_derivs
       ; case (new_or_data, cons) of
           (NewType, [con])  -> do { con'  <- repC tv_names con
                                   ; ksig' <- repMaybeLTy ksig
                                   ; repNewtype cxt1 tc bndrs opt_tys ksig' con'
                                                derivs1 }
           (NewType, _) -> failWithDs (text "Multiple constructors for newtype:"
                                       <+> pprQuotedList
                                       (getConNames $ unLoc $ head cons))
           (DataType, _) -> do { ksig' <- repMaybeLTy ksig
                               ; consL <- mapM (repC tv_names) cons
                               ; cons1 <- coreList conQTyConName consL
                               ; repData cxt1 tc bndrs opt_tys ksig' cons1
                                         derivs1 }
       }

repSynDecl :: Core TH.Name -> Core [TH.TyVarBndr]
           -> LHsType Name
           -> DsM (Core TH.DecQ)
repSynDecl tc bndrs ty
  = do { ty1 <- repLTy ty
       ; repTySyn tc bndrs ty1 }

repFamilyDecl :: LFamilyDecl Name -> DsM (SrcSpan, Core TH.DecQ)
repFamilyDecl _decl@(L loc (FamilyDecl { fdInfo    = info,
                                        fdLName   = tc,
                                        fdTyVars  = tvs,
                                        fdKindSig = opt_kind }))
  = do { tc1 <- lookupLOcc tc           -- See note [Binders and occurrences]
       ; dec <- addTyClTyVarBinds tvs $ \bndrs ->
           case info of
             ClosedTypeFamily eqns ->
               do { eqns1  <- mapM repTyFamEqn eqns
                  ; eqns2  <- coreList tySynEqnQTyConName eqns1
                  ; result <- repFamilyResultSig opt_kind
                  ; inj    <- repInjectivityAnn
                  ; repClosedFamilyD tc1 bndrs result inj eqns2 }
             OpenTypeFamily ->
               do { result <- repFamilyResultSig opt_kind
                  ; inj    <- repInjectivityAnn
                  ; repOpenFamilyD tc1 bndrs result inj }
             DataFamily ->
               do { kind <- repFamilyResultSigToMaybeKind opt_kind
                  ; repDataFamilyD tc1 bndrs kind }
       ; return (loc, dec)
       }

-- | Represent result signature of a type family
repFamilyResultSig :: Maybe (LHsKind Name) -> DsM (Core TH.FamilyResultSig)
repFamilyResultSig Nothing      = repNoSig
repFamilyResultSig (Just ki)    = do { ki' <- repLKind ki
                                     ; repKindSig ki' }
-- repFamilyResultSig (TyVarSig _ bndr) =
--   pprPanic "repFamilyResultSig: TyVarSig unsupported"

-- | Represent result signature using a Maybe Kind. Used with data families,
-- where the result signature can be either missing or a kind but never a named
-- result variable.
repFamilyResultSigToMaybeKind :: Maybe (LHsKind Name)
                              -> DsM (Core (Maybe TH.Kind))
repFamilyResultSigToMaybeKind Nothing =
    do { coreNothing kindTyConName }
repFamilyResultSigToMaybeKind (Just ki) =
    do { ki' <- repLKind ki
       ; coreJust kindTyConName ki' }
-- repFamilyResultSigToMaybeKind _ = panic "repFamilyResultSigToMaybeKind"

-- | Represent injectivity annotation of a type family
repInjectivityAnn :: -- Maybe (LInjectivityAnn Name) ->
                     DsM (Core (Maybe TH.InjectivityAnn))
repInjectivityAnn = coreNothing injAnnTyConName
-- repInjectivityAnn (Just (L _ (InjectivityAnn lhs rhs))) =
--     do { lhs'   <- lookupBinder (unLoc lhs)
--        ; rhs1   <- mapM (lookupBinder . unLoc) rhs
--        ; rhs2   <- coreList nameTyConName rhs1
--        ; injAnn <- rep2 injectivityAnnName [unC lhs', unC rhs2]
--        ; coreJust injAnnTyConName injAnn }

repFamilyDecls :: [LFamilyDecl Name] -> DsM [Core TH.DecQ]
repFamilyDecls fds = liftM de_loc (mapM repFamilyDecl fds)

-------------------------
mk_extra_tvs :: Located Name -> LHsTyVarBndrs Name
             -> HsDataDefn Name -> DsM (LHsTyVarBndrs Name)
-- If there is a kind signature it must be of form
--    k1 -> .. -> kn -> *
-- Return type variables [tv1:k1, tv2:k2, .., tvn:kn]
mk_extra_tvs tc tvs defn
  | HsDataDefn { dd_kindSig = Just hs_kind } <- defn
  = do { extra_tvs <- go hs_kind
       ; return (tvs { hsq_tvs = hsq_tvs tvs ++ extra_tvs }) }
  | otherwise
  = return tvs
  where
    go :: LHsKind Name -> DsM [LHsTyVarBndr Name]
    go (L loc (HsFunTy kind rest))
      = do { uniq <- newUnique
           ; let { occ = mkTyVarOccFS (fsLit "t")
                 ; nm = mkInternalName uniq occ loc
                 ; hs_tv = L loc (KindedTyVar (noLoc nm) kind) }
           ; hs_tvs <- go rest
           ; return (hs_tv : hs_tvs) }

    go (L _ (HsTyVar n))
      | n == liftedTypeKindTyConName
      = return []

    go _ = failWithDs (ptext (sLit "Malformed kind signature for") <+> ppr tc)

repAssocTyFamDefaults :: [LTyFamDefltEqn Name] -> DsM [Core TH.DecQ]
repAssocTyFamDefaults = mapM rep_deflt
  where
     -- very like repTyFamEqn, but different in the details
    rep_deflt :: LTyFamDefltEqn Name -> DsM (Core TH.DecQ)
    rep_deflt (L _ (TyFamEqn { tfe_tycon = tc
                             , tfe_pats  = bndrs
                             , tfe_rhs   = rhs }))
      = addTyClTyVarBinds bndrs $ \ _ ->
        do { tc1  <- lookupLOcc tc
           ; tys1 <- repLTys (hsLTyVarBndrsToTypes bndrs)
           ; tys2 <- coreList typeQTyConName tys1
           ; rhs1 <- repLTy rhs
           ; eqn1 <- repTySynEqn tys2 rhs1
           ; repTySynInst tc1 eqn1 }

-------------------------
-- represent fundeps
--
repLFunDeps :: [Located (FunDep (Located Name))] -> DsM (Core [TH.FunDep])
repLFunDeps fds = repList funDepTyConName repLFunDep fds

repLFunDep :: Located (FunDep (Located Name)) -> DsM (Core TH.FunDep)
repLFunDep (L _ (xs, ys))
   = do xs' <- repList nameTyConName (lookupBinder . unLoc) xs
        ys' <- repList nameTyConName (lookupBinder . unLoc) ys
        repFunDep xs' ys'

-- Represent instance declarations
--
repInstD :: LInstDecl Name -> DsM (SrcSpan, Core TH.DecQ)
repInstD (L loc (TyFamInstD { tfid_inst = fi_decl }))
  = do { dec <- repTyFamInstD fi_decl
       ; return (loc, dec) }
repInstD (L loc (DataFamInstD { dfid_inst = fi_decl }))
  = do { dec <- repDataFamInstD fi_decl
       ; return (loc, dec) }
repInstD (L loc (ClsInstD { cid_inst = cls_decl }))
  = do { dec <- repClsInstD cls_decl
       ; return (loc, dec) }

repClsInstD :: ClsInstDecl Name -> DsM (Core TH.DecQ)
repClsInstD (ClsInstDecl { cid_poly_ty = ty, cid_binds = binds
                         , cid_sigs = sigs, cid_tyfam_insts = ats
                         , cid_datafam_insts = adts
                         , cid_overlap_mode = overlap
                         })
  = addTyVarBinds tvs $ \_ ->
            -- We must bring the type variables into scope, so their
            -- occurrences don't fail, even though the binders don't
            -- appear in the resulting data structure
            --
            -- But we do NOT bring the binders of 'binds' into scope
            -- because they are properly regarded as occurrences
            -- For example, the method names should be bound to
            -- the selector Ids, not to fresh names (Trac #5410)
            --
            do { cxt1     <- repContext cxt
               ; cls_tcon <- repTy (HsTyVar (unLoc cls))
               ; cls_tys  <- repLTys tys
               ; inst_ty1 <- repTapps cls_tcon cls_tys
               -- See Note [Scoped type variables in class and instance declarations]
               ; (ss, sigs_binds) <- rep_sigs_binds sigs binds
               ; ats1   <- mapM (repTyFamInstD . unLoc) ats
               ; adts1  <- mapM (repDataFamInstD . unLoc) adts
               ; decls1 <- coreList decQTyConName (ats1 ++ adts1 ++ sigs_binds)
               ; rOver  <- repOverlap (fmap unLoc overlap)
               ; decls2 <- repInst rOver cxt1 inst_ty1 decls1
               ; wrapGenSyms ss decls2 }
 where
   Just (tvs, cxt, cls, tys) = splitLHsInstDeclTy_maybe ty

repStandaloneDerivD :: LDerivDecl Name -> DsM (SrcSpan, Core TH.DecQ)
repStandaloneDerivD (L loc (DerivDecl { deriv_type = ty }))
  = do { dec <- addTyVarBinds tvs $ \_ ->
                do { cxt'     <- repContext cxt
                   ; cls_tcon <- repTy (HsTyVar (unLoc cls))
                   ; cls_tys  <- repLTys tys
                   ; inst_ty  <- repTapps cls_tcon cls_tys
                   ; repDeriv cxt' inst_ty }
       ; return (loc, dec) }
  where
    Just (tvs, cxt, cls, tys) = splitLHsInstDeclTy_maybe ty

repTyFamInstD :: TyFamInstDecl Name -> DsM (Core TH.DecQ)
repTyFamInstD decl@(TyFamInstDecl { tfid_eqn = eqn })
  = do { let tc_name = tyFamInstDeclLName decl
       ; tc <- lookupLOcc tc_name               -- See note [Binders and occurrences]
       ; eqn1 <- repTyFamEqn eqn
       ; repTySynInst tc eqn1 }

repTyFamEqn :: LTyFamInstEqn Name -> DsM (Core TH.TySynEqnQ)
repTyFamEqn (L loc (TyFamEqn { tfe_pats = HsWB { hswb_cts = tys
                                               , hswb_kvs = kv_names
                                               , hswb_tvs = tv_names }
                                 , tfe_rhs = rhs }))
  = do { let hs_tvs = HsQTvs { hsq_kvs = kv_names
                             , hsq_tvs = userHsTyVarBndrs loc tv_names }   -- Yuk
       ; addTyClTyVarBinds hs_tvs $ \ _ ->
         do { tys1 <- repLTys tys
            ; tys2 <- coreList typeQTyConName tys1
            ; rhs1 <- repLTy rhs
            ; repTySynEqn tys2 rhs1 } }

repDataFamInstD :: DataFamInstDecl Name -> DsM (Core TH.DecQ)
repDataFamInstD (DataFamInstDecl { dfid_tycon = tc_name
                                 , dfid_pats = HsWB { hswb_cts = tys
                                                    , hswb_kvs = kv_names
                                                    , hswb_tvs = tv_names }
                                 , dfid_defn = defn })
  = do { tc <- lookupLOcc tc_name               -- See note [Binders and occurrences]
       ; let loc = getLoc tc_name
             hs_tvs = HsQTvs { hsq_kvs = kv_names
                             , hsq_tvs = userHsTyVarBndrs loc tv_names }   -- Yuk
       ; addTyClTyVarBinds hs_tvs $ \ bndrs ->
         do { tys1 <- repList typeQTyConName repLTy tys
            ; repDataDefn tc bndrs (Just tys1) tv_names defn } }

repForD :: Located (ForeignDecl Name) -> DsM (SrcSpan, Core TH.DecQ)
repForD (L loc (ForeignImport name typ _ (CImport (L _ cc) (L _ s) mch cis _)))
 = do MkC name' <- lookupLOcc name
      MkC typ' <- repLTy typ
      MkC cc' <- repCCallConv cc
      MkC s' <- repSafety s
      cis' <- conv_cimportspec cis
      MkC str <- coreStringLit (static ++ chStr ++ cis')
      dec <- rep2 forImpDName [cc', s', str, name', typ']
      return (loc, dec)
 where
    conv_cimportspec (CLabel cls) = notHandled "Foreign label" (doubleQuotes (ppr cls))
    conv_cimportspec (CFunction DynamicTarget) = return "dynamic"
    conv_cimportspec (CFunction (StaticTarget fs _ True))
                            = return (unpackFS fs)
    conv_cimportspec (CFunction (StaticTarget _ _ False))
                            = panic "conv_cimportspec: values not supported yet"
    conv_cimportspec (CWrapper _ _) = return "wrapper"
    -- these calling conventions do not support headers and the static keyword
    raw_cconv = cc == PrimCallConv || cc == JavaScriptCallConv
    static = case cis of
                 CFunction (StaticTarget _ _ _) | not raw_cconv -> "static "
                 _ -> ""
    chStr = case mch of
            Just (Header h) | not raw_cconv -> unpackFS h ++ " "
            _ -> ""
repForD decl = notHandled "Foreign declaration" (ppr decl)

repCCallConv :: CCallConv -> DsM (Core TH.Callconv)
repCCallConv CCallConv          = rep2 cCallName []
repCCallConv StdCallConv        = rep2 stdCallName []
repCCallConv CApiConv           = rep2 cApiCallName []
repCCallConv PrimCallConv       = rep2 primCallName []
repCCallConv JavaScriptCallConv = rep2 javaScriptCallName []
repCCallConv JavaCallConv       = rep2 javaCallName []

repSafety :: Safety -> DsM (Core TH.Safety)
repSafety PlayRisky = rep2 unsafeName []
repSafety PlayInterruptible = rep2 interruptibleName []
repSafety PlaySafe = rep2 safeName []

repFixD :: LFixitySig Name -> DsM [(SrcSpan, Core TH.DecQ)]
repFixD (L loc (FixitySig names (Fixity prec dir)))
  = do { MkC prec' <- coreIntLit prec
       ; let rep_fn = case dir of
                        InfixL -> infixLDName
                        InfixR -> infixRDName
                        InfixN -> infixNDName
       ; let do_one name
              = do { MkC name' <- lookupLOcc name
                   ; dec <- rep2 rep_fn [prec', name']
                   ; return (loc,dec) }
       ; mapM do_one names }

repRuleD :: LRuleDecl Name -> DsM (SrcSpan, Core TH.DecQ)
repRuleD (L loc (HsRule n act bndrs lhs _ rhs _))
  = do { let bndr_names = concatMap ruleBndrNames bndrs
       ; ss <- mkGenSyms bndr_names
       ; rule1 <- addBinds ss $
                  do { bndrs' <- repList ruleBndrQTyConName repRuleBndr bndrs
                     ; n'   <- coreStringLit $ unpackFS $ unLoc n
                     ; act' <- repPhases act
                     ; lhs' <- repLE lhs
                     ; rhs' <- repLE rhs
                     ; repPragRule n' bndrs' lhs' rhs' act' }
       ; rule2 <- wrapGenSyms ss rule1
       ; return (loc, rule2) }

ruleBndrNames :: LRuleBndr Name -> [Name]
ruleBndrNames (L _ (RuleBndr n))      = [unLoc n]
ruleBndrNames (L _ (RuleBndrSig n (HsWB { hswb_kvs = kvs, hswb_tvs = tvs })))
  = unLoc n : kvs ++ tvs

repRuleBndr :: LRuleBndr Name -> DsM (Core TH.RuleBndrQ)
repRuleBndr (L _ (RuleBndr n))
  = do { MkC n' <- lookupLBinder n
       ; rep2 ruleVarName [n'] }
repRuleBndr (L _ (RuleBndrSig n sig))
  = do { MkC n'  <- lookupLBinder n
       ; MkC ty' <- repLTy (hswb_cts sig)
       ; rep2 typedRuleVarName [n', ty'] }

repAnnD :: LAnnDecl Name -> DsM (SrcSpan, Core TH.DecQ)
repAnnD (L loc (HsAnnotation _ ann_prov (L _ exp)))
  = do { target <- repAnnProv ann_prov
       ; exp'   <- repE exp
       ; dec    <- repPragAnn target exp'
       ; return (loc, dec) }

repAnnProv :: AnnProvenance Name -> DsM (Core TH.AnnTarget)
repAnnProv (ValueAnnProvenance (L _ n))
  = do { MkC n' <- globalVar n  -- ANNs are allowed only at top-level
       ; rep2 valueAnnotationName [ n' ] }
repAnnProv (TypeAnnProvenance (L _ n))
  = do { MkC n' <- globalVar n
       ; rep2 typeAnnotationName [ n' ] }
repAnnProv ModuleAnnProvenance
  = rep2 moduleAnnotationName []

-- ds_msg :: SDoc
-- ds_msg = text "Cannot desugar this Template Haskell declaration:"

-------------------------------------------------------
--                      Constructors
-------------------------------------------------------

repC :: [Name] -> LConDecl Name -> DsM (Core TH.ConQ)
repC _ (L _ (ConDecl { con_names = con, con_qvars = con_tvs, con_cxt = L _ []
                     , con_details = details, con_res = ResTyH98 }))
  | null (hsQTvBndrs con_tvs)
  = do { con1 <- mapM lookupLOcc con       -- See Note [Binders and occurrences]
       ; repConstr details Nothing con1  }

repC tvs (L _ (ConDecl { con_names = cons
                       , con_qvars = con_tvs, con_cxt = L _ ctxt
                       , con_details = details
                       , con_res = res_ty }))
  = do { (eq_ctxt, con_tv_subst) <- mkGadtCtxt tvs res_ty
       ; let ex_tvs = HsQTvs { hsq_kvs = filterOut (in_subst con_tv_subst) (hsq_kvs con_tvs)
                             , hsq_tvs = filterOut (in_subst con_tv_subst . hsLTyVarName) (hsq_tvs con_tvs) }

       ; binds <- mapM dupBinder con_tv_subst
       ; b <- dsExtendMetaEnv (mkNameEnv binds) $ -- Binds some of the con_tvs
         addTyVarBinds ex_tvs $ \ ex_bndrs ->   -- Binds the remaining con_tvs
    do { cons1     <- mapM lookupLOcc cons -- See Note [Binders and occurrences]
       ; let res_ty' = case res_ty of
                         ResTyH98       -> Nothing
                         ResTyGADT _ ty -> Just ty
       ; c'        <- repConstr details res_ty' cons1
       ; ctxt'     <- repContext (eq_ctxt ++ ctxt)
       ; rep2 forallCName [unC ex_bndrs, unC ctxt', unC c'] }
    ; return b
    }

in_subst :: [(Name,Name)] -> Name -> Bool
in_subst []          _ = False
in_subst ((n',_):ns) n = n==n' || in_subst ns n

mkGadtCtxt :: [Name]            -- Tyvars of the data type
           -> ResType (LHsType Name)
           -> DsM (HsContext Name, [(Name,Name)])
-- Given a data type in GADT syntax, figure out the equality
-- context, so that we can represent it with an explicit
-- equality context, because that is the only way to express
-- the GADT in TH syntax
--
-- Example:
-- data T a b c where { MkT :: forall d e. d -> e -> T d [e] e
--     mkGadtCtxt [a,b,c] [d,e] (T d [e] e)
--   returns
--     (b~[e], c~e), [d->a]
--
-- This function is fiddly, but not really hard
mkGadtCtxt _ ResTyH98
  = return ([], [])
mkGadtCtxt data_tvs (ResTyGADT _ res_ty)
  | Just (_, tys) <- hsTyGetAppHead_maybe res_ty
  , data_tvs `equalLength` tys
  = return (go [] [] (data_tvs `zip` tys))

  | otherwise
  = failWithDs (ptext (sLit "Malformed constructor result type:") <+> ppr res_ty)
  where
    go cxt subst [] = (cxt, subst)
    go cxt subst ((data_tv, ty) : rest)
       | Just con_tv <- is_hs_tyvar ty
       , isTyVarName con_tv
       , not (in_subst subst con_tv)
       = go cxt ((con_tv, data_tv) : subst) rest
       | otherwise
       = go (eq_pred : cxt) subst rest
       where
         loc = getLoc ty
         eq_pred = L loc (HsEqTy (L loc (HsTyVar data_tv)) ty)

    is_hs_tyvar (L _ (HsTyVar n))  = Just n   -- Type variables *and* tycons
    is_hs_tyvar (L _ (HsParTy ty)) = is_hs_tyvar ty
    is_hs_tyvar _                  = Nothing

-- repMbContext :: Maybe (LHsContext Name) -> DsM (Core TH.CxtQ)
-- repMbContext Nothing          = repContext []
-- repMbContext (Just (L _ cxt)) = repContext cxt

repSrcUnpackedness :: SrcUnpackedness -> DsM (Core TH.SourceUnpackednessQ)
repSrcUnpackedness SrcUnpack   = rep2 sourceUnpackName         []
repSrcUnpackedness SrcNoUnpack = rep2 sourceNoUnpackName       []
repSrcUnpackedness NoSrcUnpack = rep2 noSourceUnpackednessName []

repSrcStrictness :: SrcStrictness -> DsM (Core TH.SourceStrictnessQ)
repSrcStrictness SrcLazy     = rep2 sourceLazyName         []
repSrcStrictness SrcStrict   = rep2 sourceStrictName       []
repSrcStrictness NoSrcStrict = rep2 noSourceStrictnessName []

repBangTy :: LBangType Name -> DsM (Core (TH.BangTypeQ))
repBangTy ty = do
  MkC u <- repSrcUnpackedness su'
  MkC s <- repSrcStrictness ss'
  MkC b <- rep2 bangName [u, s]
  MkC t <- repLTy ty'
  rep2 bangTypeName [b, t]
  where
    (su', ss', ty') = case ty of
                L _ (HsBangTy (HsSrcBang _ su ss) ty) -> (su, ss, ty)
                _                                     -> (NoSrcUnpack, NoSrcStrict, ty)

-------------------------------------------------------
--                      Deriving clauses
-------------------------------------------------------

repDerivs :: Maybe (Located [LHsType Name]) -> DsM (Core TH.CxtQ)
repDerivs mCtxt = repContext ctxt'
  where ctxt' = case mCtxt of
                    Nothing -> []
                    Just (L _ ctxt) -> ctxt

rep_sigs_binds :: [LSig Name] -> LHsBinds Name
               -> DsM ([GenSymBind], [Core TH.DecQ])
-- Represent signatures and methods in class/instance declarations.
-- See Note [Scoped type variables in class and instance declarations]
--
-- Why not use 'repBinds': we have already created symbols for methods in
-- 'repTopDs' via 'hsGroupBinders'. However in 'repBinds', we recreate
-- these fun_id via 'collectHsValBinders decs', which would lead to the
-- instance declarations failing in TH.
rep_sigs_binds sigs binds
  = do { let tvs = concatMap get_scoped_tvs sigs
       ; ss <- mkGenSyms tvs
       ; sigs1 <- addBinds ss $ rep_sigs sigs
       ; binds1 <- addBinds ss $ rep_binds binds
       ; return (ss, de_loc (sort_by_loc (sigs1 ++ binds1))) }

-------------------------------------------------------
--   Signatures in a class decl, or a group of bindings
-------------------------------------------------------

rep_sigs :: [LSig Name] -> DsM [(SrcSpan, Core TH.DecQ)]
        -- We silently ignore ones we don't recognise
rep_sigs = concatMapM rep_sig

rep_sig :: LSig Name -> DsM [(SrcSpan, Core TH.DecQ)]
rep_sig (L loc (TypeSig nms ty _ _))    = mapM (rep_ty_sig sigDName loc ty) nms
-- rep_sig (L loc (PatSynSig _ _ _ nms ty))  = mapM (rep_patsyn_ty_sig loc ty) nms
rep_sig (L _loc (PatSynSig _ _ _ _nms _ty)) = notHandled "Pattern type signatures" empty
rep_sig (L loc (GenericSig nms ty))   = mapM (rep_ty_sig defaultSigDName loc ty) nms
-- rep_sig (L loc (ClassOpSig _ is_deflt nms ty))
--   | is_deflt                          = mapM (rep_ty_sig defaultSigDName loc ty) nms
--   | otherwise                         = mapM (rep_ty_sig sigDName loc ty) nms
rep_sig d@(L _ (IdSig {}))            = pprPanic "rep_sig IdSig" (ppr d)
rep_sig (L _   (FixSig {}))           = return [] -- fixity sigs at top level
rep_sig (L loc (InlineSig nm ispec))= rep_inline nm ispec loc
rep_sig (L loc (SpecSig nm tys ispec))
  = concatMapM (\t -> rep_specialise nm t ispec loc) tys
rep_sig (L loc (SpecInstSig _ ty))  = rep_specialiseInst ty loc
rep_sig (L _   (MinimalSig {}))       = notHandled "MINIMAL pragmas" empty
-- rep_sig (L _   (SCCFunSig {}))        = notHandled "SCC pragmas" empty
-- rep_sig (L loc (CompleteMatchSig _ _st cls mty)) = rep_complete_sig cls mty loc

rep_ty_sig :: Name -> SrcSpan -> LHsType Name -> Located Name
           -> DsM (SrcSpan, Core TH.DecQ)
-- Don't create the implicit and explicit variables when desugaring signatures,
-- see Note [Scoped type variables in class and instance declarations].
-- and Note [Don't quantify implicit type variables in quotes]
rep_ty_sig mk_sig loc hs_ty nm
  | (explicit_tvs, ctxt, ty) <- splitLHsSigmaTy hs_ty
  = do { nm1 <- lookupLOcc nm
       ; let rep_in_scope_tv tv = do { name <- lookupBinder (hsLTyVarName tv)
                                     ; repTyVarBndrWithKind tv name }
       ; th_explicit_tvs <- repList tyVarBndrTyConName rep_in_scope_tv
                                    explicit_tvs

         -- NB: Don't pass any implicit type variables to repList above
         -- See Note [Don't quantify implicit type variables in quotes]

       ; th_ctxt <- repLContext ctxt
       ; th_ty   <- repLTy ty
       ; ty1     <- if null explicit_tvs && null (unLoc ctxt)
                       then return th_ty
                       else repTForall th_explicit_tvs th_ctxt th_ty
       ; sig     <- repProto mk_sig nm1 ty1
       ; return (loc, sig) }

-- rep_patsyn_ty_sig :: SrcSpan -> LHsType Name -> Located Name
--                   -> DsM (SrcSpan, Core TH.DecQ)
-- -- represents a pattern synonym type signature;
-- -- see Note [Pattern synonym type signatures and Template Haskell] in Convert
-- --
-- -- Don't create the implicit and explicit variables when desugaring signatures,
-- -- see Note [Scoped type variables in class and instance declarations]
-- -- and Note [Don't quantify implicit type variables in quotes]
-- rep_patsyn_ty_sig loc hs_ty nm
--   | (univs, reqs, exis, provs, ty) <- splitLHsPatSynTy hs_ty
--   = do { nm1 <- lookupLOcc nm
--        ; let rep_in_scope_tv tv = do { name <- lookupBinder (hsLTyVarName tv)
--                                      ; repTyVarBndrWithKind tv name }
--        ; th_univs <- repList tyVarBndrQTyConName rep_in_scope_tv univs
--        ; th_exis  <- repList tyVarBndrQTyConName rep_in_scope_tv exis
--
          -- NB: Don't pass any implicit type variables to repList above
--          -- See Note [Don't quantify implicit type variables in quotes]
--
--        ; th_reqs  <- repLContext reqs
--        ; th_provs <- repLContext provs
--        ; th_ty    <- repLTy ty
--        ; ty1      <- repTForall th_univs th_reqs =<<
--                        repTForall th_exis th_provs th_ty
--        ; sig      <- repProto patSynSigDName nm1 ty1
--        ; return (loc, sig) }

-- rep_wc_ty_sig :: Name -> SrcSpan -> LHsSigWcType Name -> Located Name
--               -> DsM (SrcSpan, Core TH.DecQ)
-- rep_wc_ty_sig mk_sig loc sig_ty nm
--   = rep_ty_sig mk_sig loc (hswc_body sig_ty) nm

rep_inline :: Located Name
           -> InlinePragma      -- Never defaultInlinePragma
           -> SrcSpan
           -> DsM [(SrcSpan, Core TH.DecQ)]
rep_inline nm ispec loc
  = do { nm1    <- lookupLOcc nm
       ; inline <- repInline $ inl_inline ispec
       ; rm     <- repRuleMatch $ inl_rule ispec
       ; phases <- repPhases $ inl_act ispec
       ; pragma <- repPragInl nm1 inline rm phases
       ; return [(loc, pragma)]
       }

rep_specialise :: Located Name -> LHsType Name -> InlinePragma
               -> SrcSpan
               -> DsM [(SrcSpan, Core TH.DecQ)]
rep_specialise nm ty ispec loc
  = do { nm1 <- lookupLOcc nm
       ; ty1 <- repLTy ty
       ; phases <- repPhases $ inl_act ispec
       ; let inline = inl_inline ispec
       ; pragma <- if isEmptyInlineSpec inline
                   then -- SPECIALISE
                     repPragSpec nm1 ty1 phases
                   else -- SPECIALISE INLINE
                     do { inline1 <- repInline inline
                        ; repPragSpecInl nm1 ty1 inline1 phases }
       ; return [(loc, pragma)]
       }

rep_specialiseInst :: LHsType Name -> SrcSpan
                   -> DsM [(SrcSpan, Core TH.DecQ)]
rep_specialiseInst ty loc
  = do { ty1    <- repLTy ty
       ; pragma <- repPragSpecInst ty1
       ; return [(loc, pragma)] }

repInline :: InlineSpec -> DsM (Core TH.Inline)
repInline NoInline  = dataCon noInlineDataConName
repInline Inline    = dataCon inlineDataConName
repInline Inlinable = dataCon inlinableDataConName
repInline spec      = notHandled "repInline" (ppr spec)

repRuleMatch :: RuleMatchInfo -> DsM (Core TH.RuleMatch)
repRuleMatch ConLike = dataCon conLikeDataConName
repRuleMatch FunLike = dataCon funLikeDataConName

repPhases :: Activation -> DsM (Core TH.Phases)
repPhases (ActiveBefore i) = do { MkC arg <- coreIntLit i
                                  ; dataCon' beforePhaseDataConName [arg] }
repPhases (ActiveAfter i)  = do { MkC arg <- coreIntLit i
                                  ; dataCon' fromPhaseDataConName [arg] }
repPhases _                  = dataCon allPhasesDataConName

-- rep_complete_sig :: Located [Located Name]
--                  -> Maybe (Located Name)
--                  -> SrcSpan
--                  -> DsM [(SrcSpan, Core TH.DecQ)]
-- rep_complete_sig (L _ cls) mty loc
--   = do { mty' <- rep_maybe_name mty
--        ; cls' <- repList nameTyConName lookupLOcc cls
--        ; sig <- repPragComplete cls' mty'
--        ; return [(loc, sig)] }
--   where
--     rep_maybe_name Nothing = coreNothing nameTyConName
--     rep_maybe_name (Just n) = do
--       cn <- lookupLOcc n
--       coreJust nameTyConName cn

-------------------------------------------------------
--                      Types
-------------------------------------------------------

-- addSimpleTyVarBinds :: [Name]                -- the binders to be added
--                     -> DsM (Core (TH.Q a))   -- action in the ext env
--                     -> DsM (Core (TH.Q a))
-- addSimpleTyVarBinds names thing_inside
--   = do { fresh_names <- mkGenSyms names
--        ; term <- addBinds fresh_names thing_inside
--        ; wrapGenSyms fresh_names term }

addTyVarBinds :: LHsTyVarBndrs Name                    -- the binders to be added
              -> (Core [TH.TyVarBndr] -> DsM (Core (TH.Q a)))  -- action in the ext env
              -> DsM (Core (TH.Q a))
-- gensym a list of type variables and enter them into the meta environment;
-- the computations passed as the second argument is executed in that extended
-- meta environment and gets the *new* names on Core-level as an argument
addTyVarBinds (HsQTvs { hsq_kvs = kvs, hsq_tvs = tvs }) m
  = do { fresh_kv_names <- mkGenSyms kvs
       ; fresh_tv_names <- mkGenSyms (map hsLTyVarName tvs)
       ; let fresh_names = fresh_kv_names ++ fresh_tv_names
       ; term <- addBinds fresh_names $
                 do { kbs <- repList tyVarBndrTyConName mk_tv_bndr (tvs `zip` fresh_tv_names)
                    ; m kbs }
       ; wrapGenSyms fresh_names term }
  where
    mk_tv_bndr (tv, (_,v)) = repTyVarBndrWithKind tv (coreVar v)

addTyClTyVarBinds :: LHsTyVarBndrs Name
                  -> (Core [TH.TyVarBndr] -> DsM (Core (TH.Q a)))
                  -> DsM (Core (TH.Q a))

-- Used for data/newtype declarations, and family instances,
-- so that the nested type variables work right
--    instance C (T a) where
--      type W (T a) = blah
-- The 'a' in the type instance is the one bound by the instance decl
addTyClTyVarBinds tvs m
  = do { let tv_names = hsLKiTyVarNames tvs
       ; env <- dsGetMetaEnv
       ; freshNames <- mkGenSyms (filterOut (`elemNameEnv` env) tv_names)
            -- Make fresh names for the ones that are not already in scope
            -- This makes things work for family declarations

       ; term <- addBinds freshNames $
                 do { kbs <- repList tyVarBndrTyConName mk_tv_bndr
                                     (hsQTvBndrs tvs)
                    ; m kbs }

       ; wrapGenSyms freshNames term }
  where
    mk_tv_bndr :: LHsTyVarBndr Name -> DsM (Core TH.TyVarBndr)
    mk_tv_bndr tv = do { v <- lookupBinder (hsLTyVarName tv)
                       ; repTyVarBndrWithKind tv v }

-- Produce kinded binder constructors from the Haskell tyvar binders
--
repTyVarBndrWithKind :: LHsTyVarBndr Name
                     -> Core TH.Name -> DsM (Core TH.TyVarBndr)
repTyVarBndrWithKind (L _ (UserTyVar _)) nm
  = repPlainTV nm
repTyVarBndrWithKind (L _ (KindedTyVar _ ki)) nm
  = repLKind ki >>= repKindedTV nm

-- | Represent a type variable binder
-- repTyVarBndr :: LHsTyVarBndr Name -> DsM (Core TH.TyVarBndr)
-- repTyVarBndr (L _ (UserTyVar (L _ nm)) )= do { nm' <- lookupBinder nm
--                                                ; repPlainTV nm' }
-- repTyVarBndr (L _ (KindedTyVar (L _ nm) ki)) = do { nm' <- lookupBinder nm
--                                                     ; ki' <- repLTy ki
--                                                     ; repKindedTV nm' ki' }

-- represent a type context
--
repLContext :: LHsContext Name -> DsM (Core TH.CxtQ)
repLContext (L _ ctxt) = repContext ctxt

repContext :: HsContext Name -> DsM (Core TH.CxtQ)
repContext ctxt = do preds <- repList typeQTyConName repLTy ctxt
                     repCtxt preds

-- repHsSigType :: LHsType Name -> DsM (Core TH.TypeQ)
-- repHsSigType (HsIB { hsib_ext = HsIBRn { hsib_vars = implicit_tvs }
--                    , hsib_body = body })
--   | (explicit_tvs, ctxt, ty) <- splitLHsSigmaTy body
--   = addSimpleTyVarBinds implicit_tvs $
--       -- See Note [Don't quantify implicit type variables in quotes]
--     addHsTyVarBinds explicit_tvs $ \ th_explicit_tvs ->
--     do { th_ctxt <- repLContext ctxt
--        ; th_ty   <- repLTy ty
--        ; if null explicit_tvs && null (unLoc ctxt)
--          then return th_ty
--          else repTForall th_explicit_tvs th_ctxt th_ty }

-- repHsSigWcType :: LHsSigWcType Name -> DsM (Core TH.TypeQ)
-- repHsSigWcType (HsWC { hswc_body = sig1 })
--   = repHsSigType sig1

-- yield the representation of a list of types
repLTys :: [LHsType Name] -> DsM [Core TH.TypeQ]
repLTys tys = mapM repLTy tys

-- represent a type
repLTy :: LHsType Name -> DsM (Core TH.TypeQ)
repLTy (L _ ty) = repTy ty

-- repForall :: HsType Name -> DsM (Core TH.TypeQ)
-- -- Arg of repForall is always HsForAllTy or HsQualTy
-- repForall ty
--  | (tvs, ctxt, tau) <- splitLHsSigmaTy (noLoc ty)
--  = addHsTyVarBinds tvs $ \bndrs ->
--    do { ctxt1  <- repLContext ctxt
--       ; ty1    <- repLTy tau
--       ; repTForall bndrs ctxt1 ty1 }

repTy :: HsType Name -> DsM (Core TH.TypeQ)
repTy (HsForAllTy _ _ tvs ctxt ty)  =
  addTyVarBinds tvs $ \bndrs -> do
    ctxt1  <- repLContext ctxt
    ty1    <- repLTy ty
    repTForall bndrs ctxt1 ty1

repTy (HsTyVar n)
  | isLiftedTypeKindTyConName n       = repTStar
  | n `hasKey` constraintKindTyConKey = repTConstraint
  | n `hasKey` funTyConKey            = repArrowTyCon
  | isTvOcc occ   = do tv1 <- lookupOcc n
                       repTvar tv1
  | isDataOcc occ = do tc1 <- lookupOcc n
                       repPromotedDataCon tc1
  | n == eqTyConName = repTequality
  | otherwise     = do tc1 <- lookupOcc n
                       repNamedTyCon tc1
  where
    occ = nameOccName n

repTy (HsAppTy f a)       = do
                                f1 <- repLTy f
                                a1 <- repLTy a
                                repTapp f1 a1
repTy (HsFunTy f a)       = do
                                f1   <- repLTy f
                                a1   <- repLTy a
                                tcon <- repArrowTyCon
                                repTapps tcon [f1, a1]
repTy (HsListTy t)        = do
                                t1   <- repLTy t
                                tcon <- repListTyCon
                                repTapp tcon t1
repTy (HsPArrTy t)   = do
                           t1   <- repLTy t
                           tcon <- repTy (HsTyVar (tyConName parrTyCon))
                           repTapp tcon t1
repTy (HsTupleTy HsUnboxedTuple tys) = do
                                tys1 <- repLTys tys
                                tcon <- repUnboxedTupleTyCon (length tys)
                                repTapps tcon tys1
repTy (HsTupleTy _ tys)     = do tys1 <- repLTys tys
                                 tcon <- repTupleTyCon (length tys)
                                 repTapps tcon tys1
-- repTy (HsSumTy _ tys)       = do tys1 <- repLTys tys
--                                  tcon <- repUnboxedSumTyCon (length tys)
--                                  repTapps tcon tys1
repTy (HsOpTy ty1 (_, n) ty2)  = repLTy ((nlHsTyVar (unLoc n) `nlHsAppTy` ty1)
                                   `nlHsAppTy` ty2)
repTy (HsParTy t)         = repLTy t
repTy (HsEqTy t1 t2) = do
                         t1' <- repLTy t1
                         t2' <- repLTy t2
                         eq  <- repTequality
                         repTapps eq [t1', t2']
repTy (HsKindSig t k)     = do
                                t1 <- repLTy t
                                k1 <- repLKind k
                                repTSig t1 k1
repTy (HsSpliceTy splice _)      = repSplice splice
repTy (HsExplicitListTy _ tys) = do
                                    tys1 <- repLTys tys
                                    repTPromotedList tys1
repTy (HsExplicitTupleTy _ tys) = do
                                    tys1 <- repLTys tys
                                    tcon <- repPromotedTupleTyCon (length tys)
                                    repTapps tcon tys1
repTy (HsTyLit lit) = do
                          lit' <- repTyLit lit
                          repTLit lit'
repTy HsWildcardTy = repTWildCard

repTy ty                      = notHandled "Exotic form of type" (ppr ty)

repTyLit :: HsTyLit -> DsM (Core TH.TyLitQ)
repTyLit (HsNumTy _ i) = do iExpr <- mkIntegerExpr i
                            rep2 numTyLitName [iExpr]
repTyLit (HsStrTy _ s) = do { s' <- mkStringExprFS s
                            ; rep2 strTyLitName [s']
                            }

-- | Represent a type wrapped in a Maybe
repMaybeLTy :: Maybe (LHsKind Name)
            -> DsM (Core (Maybe TH.Kind))
repMaybeLTy Nothing =
    do { coreNothing kindTyConName }
repMaybeLTy (Just ki) =
    do { ki' <- repLKind ki
       ; coreJust kindTyConName ki' }

-- represent a kind

repLKind :: LHsKind Name -> DsM (Core TH.Kind)
repLKind ki
 = do { let (kis, ki') = splitHsFunType ki
      ; kis_rep <- mapM repLKind kis
      ; ki'_rep <- repNonArrowLKind ki'
      ; kcon <- repKArrow
      ; let f k1 k2 = repKApp kcon k1 >>= flip repKApp k2
      ; foldrM f ki'_rep kis_rep
      }

repNonArrowLKind :: LHsKind Name -> DsM (Core TH.Kind)
repNonArrowLKind (L _ ki) = repNonArrowKind ki

repNonArrowKind :: HsKind Name -> DsM (Core TH.Kind)
repNonArrowKind (HsTyVar name)
 | name == liftedTypeKindTyConName = repKStar
 | name == constraintKindTyConName = repKConstraint
 | isTvOcc (nameOccName name)      = lookupOcc name >>= repKVar
 | otherwise                       = lookupOcc name >>= repKCon
repNonArrowKind (HsAppTy f a)       = do  { f' <- repLKind f
                                         ; a' <- repLKind a
                                         ; repKApp f' a'
                                         }
repNonArrowKind (HsListTy k)        = do  { k' <- repLKind k
                                         ; kcon <- repKList
                                         ; repKApp kcon k'
                                         }
repNonArrowKind (HsTupleTy _ ks)    = do  { ks' <- mapM repLKind ks
                                         ; kcon <- repKTuple (length ks)
                                         ; repKApps kcon ks'
                                         }
repNonArrowKind k                   = notHandled "Exotic form of kind" (ppr k)

repRole :: Located (Maybe Role) -> DsM (Core TH.Role)
repRole (L _ (Just Nominal))          = rep2 nominalRName []
repRole (L _ (Just Representational)) = rep2 representationalRName []
repRole (L _ (Just Phantom))          = rep2 phantomRName []
repRole (L _ Nothing)                 = rep2 inferRName []

-----------------------------------------------------------------------------
--              Splices
-----------------------------------------------------------------------------

repSplice :: HsSplice Name -> DsM (Core a)
-- See Note [How brackets and nested splices are handled] in TcSplice
-- We return a CoreExpr of any old type; the context should know
repSplice (HsTypedSplice  _ n _)  = rep_splice n
repSplice (HsUntypedSplice _ n _) = rep_splice n
repSplice (HsQuasiQuote n _ _ _)  = rep_splice n
repSplice e@(HsSpliced {})        = pprPanic "repSplice" (ppr e)

rep_splice :: Name -> DsM (Core a)
rep_splice splice_name
 = do { mb_val <- dsLookupMetaEnv splice_name
       ; case mb_val of
           Just (DsSplice e) -> do { e' <- dsExpr e
                                   ; return (MkC e') }
           _ -> pprPanic "HsSplice" (ppr splice_name) }
                        -- Should not happen; statically checked

-----------------------------------------------------------------------------
--              Expressions
-----------------------------------------------------------------------------

repLEs :: [LHsExpr Name] -> DsM (Core [TH.ExpQ])
repLEs es = repList expQTyConName repLE es

-- FIXME: some of these panics should be converted into proper error messages
--        unless we can make sure that constructs, which are plainly not
--        supported in TH already lead to error messages at an earlier stage
repLE :: LHsExpr Name -> DsM (Core TH.ExpQ)
repLE (L loc e) = putSrcSpanDs loc (repE e)

repE :: HsExpr Name -> DsM (Core TH.ExpQ)
repE (HsVar x)            =
  do { mb_val <- dsLookupMetaEnv x
     ; case mb_val of
        Nothing            -> do { str <- globalVar x
                                 ; repVarOrCon x str }
        Just (DsBound y)   -> repVarOrCon x (coreVar y)
        Just (DsSplice e)  -> do { e' <- dsExpr e
                                 ; return (MkC e') } }
repE e@(HsIPVar {})    = notHandled "Implicit parameters" (ppr e)
repE e@(HsOverLabel _) = notHandled "Overloaded labels" (ppr e)
-- repE (HsOverLabel _ _ s) = repOverLabel s

-- repE e@(HsRecFld _ f) = case f of
--   Unambiguous x _ -> repE (HsVar x)
--   Ambiguous{}     -> notHandled "Ambiguous record selectors" (ppr e)

        -- Remember, we're desugaring renamer output here, so
        -- HsOverlit can definitely occur
repE (HsOverLit l) = do { a <- repOverloadedLiteral l; repLit a }
repE (HsLit l)     = do { a <- repLiteral l;           repLit a }
repE (HsLam (MG { mg_alts = [m] })) = repLambda m
repE (HsLamCase _ (MG { mg_alts = ms }))
                   = do { ms' <- mapM repMatchTup ms
                        ; core_ms <- coreList matchQTyConName ms'
                        ; repLamCase core_ms }
repE (HsApp x y)   = do {a <- repLE x; b <- repLE y; repApp a b}
-- repE (HsAppType t e) = do { a <- repLE e
--                           ; s <- repLTy (hswc_body t)
--                           ; repAppType a s }

repE (OpApp e1 op _ e2) =
  do { arg1 <- repLE e1;
       arg2 <- repLE e2;
       the_op <- repLE op ;
       repInfixApp arg1 the_op arg2 }
repE (NegApp x _)      = do
                              a         <- repLE x
                              negateVar <- lookupOcc negateName >>= repVar
                              negateVar `repApp` a
repE (HsPar x)            = repLE x
repE (SectionL x y)       = do { a <- repLE x; b <- repLE y; repSectionL a b }
repE (SectionR x y)       = do { a <- repLE x; b <- repLE y; repSectionR a b }
repE (HsCase e (MG { mg_alts = ms }))
                          = do { arg <- repLE e
                               ; ms2 <- mapM repMatchTup ms
                               ; core_ms2 <- coreList matchQTyConName ms2
                               ; repCaseE arg core_ms2 }
repE (HsIf _ x y z)       = do
                              a <- repLE x
                              b <- repLE y
                              c <- repLE z
                              repCond a b c
repE (HsMultiIf _ alts)
  = do { (binds, alts') <- liftM unzip $ mapM repLGRHS alts
       ; expr' <- repMultiIf (nonEmptyCoreList alts')
       ; wrapGenSyms (concat binds) expr' }
repE (HsLet bs e)       = do { (ss,ds) <- repBinds bs
                                     ; e2 <- addBinds ss (repLE e)
                                     ; z <- repLetE ds e2
                                     ; wrapGenSyms ss z }

-- FIXME: I haven't got the types here right yet
repE e@(HsDo ctxt sts _)
 | case ctxt of { DoExpr -> True; GhciStmtCtxt -> True; _ -> False }
 = do { (ss,zs) <- repLSts sts;
        e'      <- repDoE (nonEmptyCoreList zs);
        wrapGenSyms ss e' }

 | ListComp <- ctxt
 = do { (ss,zs) <- repLSts sts;
        e'      <- repComp (nonEmptyCoreList zs);
        wrapGenSyms ss e' }

  | otherwise
  = notHandled "mdo, monad comprehension and [: :]" (ppr e)

repE (ExplicitList _ _ es) = do { xs <- repLEs es; repListExp xs }
repE e@(ExplicitPArr _ _) = notHandled "Parallel arrays" (ppr e)
repE e@(ExplicitTuple es boxed)
  | not (all tupArgPresent es) = notHandled "Tuple sections" (ppr e)
  | isBoxed boxed = do { xs <- repLEs [e | L _ (Present e) <- es]; repTup xs }
  | otherwise     = do { xs <- repLEs [e | L _ (Present e) <- es]
                       ; repUnboxedTup xs }

-- repE (ExplicitSum _ alt arity e)
--  = do { e1 <- repLE e
--       ; repUnboxedSum e1 alt arity }

repE (RecordCon c _ flds)
 = do { x <- lookupLOcc c;
        fs <- repFields flds;
        repRecCon x fs }

repE (RecordUpd e flds _ _ _)
 = do { x <- repLE e;
        fs <- repFields flds;
        repRecUpd x fs }

repE (ExprWithTySig e ty _)
  = do { e1 <- repLE e
       ; t1 <- repLTy ty
       ; repSigExp e1 t1 }

repE (ArithSeq _ _ aseq) =
  case aseq of
    From e              -> do { ds1 <- repLE e; repFrom ds1 }
    FromThen e1 e2      -> do
                             ds1 <- repLE e1
                             ds2 <- repLE e2
                             repFromThen ds1 ds2
    FromTo   e1 e2      -> do
                             ds1 <- repLE e1
                             ds2 <- repLE e2
                             repFromTo ds1 ds2
    FromThenTo e1 e2 e3 -> do
                             ds1 <- repLE e1
                             ds2 <- repLE e2
                             ds3 <- repLE e3
                             repFromThenTo ds1 ds2 ds3

repE (HsSpliceE splice)  = repSplice splice
repE (HsStatic e)        = repLE e >>= rep2 staticEName . (:[]) . unC
repE e@(HsUnboundVar _)   = notHandled "Unbounded Vars" (ppr e)

repE e@(PArrSeq {})        = notHandled "Parallel arrays" (ppr e)
repE e@(HsCoreAnn {})      = notHandled "Core annotations" (ppr e)
repE e@(HsSCC {})          = notHandled "Cost centres" (ppr e)
repE e@(HsTickPragma {})   = notHandled "Tick Pragma" (ppr e)
repE e                     = notHandled "Expression form" (ppr e)

-----------------------------------------------------------------------------
-- Building representations of auxillary structures like Match, Clause, Stmt,

repMatchTup ::  LMatch Name (LHsExpr Name) -> DsM (Core TH.MatchQ)
repMatchTup (L _ (Match { m_pats = [p]
                        , m_grhss = GRHSs guards wheres })) =
  do { ss1 <- mkGenSyms (collectPatBinders p)
     ; addBinds ss1 $ do {
     ; p1 <- repLP p
     ; (ss2,ds) <- repBinds wheres
     ; addBinds ss2 $ do {
     ; gs    <- repGuards guards
     ; match <- repMatch p1 gs ds
     ; wrapGenSyms (ss1++ss2) match }}}
repMatchTup _ = panic "repMatchTup: case alt with more than one arg"

repClauseTup ::  LMatch Name (LHsExpr Name) -> DsM (Core TH.ClauseQ)
repClauseTup (L _ (Match { m_pats = ps
                         , m_grhss = GRHSs guards wheres })) =
  do { ss1 <- mkGenSyms (collectPatsBinders ps)
     ; addBinds ss1 $ do {
       ps1 <- repLPs ps
     ; (ss2,ds) <- repBinds wheres
     ; addBinds ss2 $ do {
       gs <- repGuards guards
     ; clause <- repClause ps1 gs ds
     ; wrapGenSyms (ss1++ss2) clause }}}

repGuards ::  [LGRHS Name (LHsExpr Name)] ->  DsM (Core TH.BodyQ)
repGuards [L _ (GRHS [] e)]
  = do {a <- repLE e; repNormal a }
repGuards other
  = do { zs <- mapM repLGRHS other
       ; let (xs, ys) = unzip zs
       ; gd <- repGuarded (nonEmptyCoreList ys)
       ; wrapGenSyms (concat xs) gd }

repLGRHS :: LGRHS Name (LHsExpr Name)
         -> DsM ([GenSymBind], (Core (TH.Q (TH.Guard, TH.Exp))))
repLGRHS (L _ (GRHS [L _ (BodyStmt e1 _ _ _)] e2))
  = do { guarded <- repLNormalGE e1 e2
       ; return ([], guarded) }
repLGRHS (L _ (GRHS ss rhs))
  = do { (gs, ss') <- repLSts ss
       ; rhs' <- addBinds gs $ repLE rhs
       ; guarded <- repPatGE (nonEmptyCoreList ss') rhs'
       ; return (gs, guarded) }

repFields :: HsRecordBinds Name -> DsM (Core [TH.Q TH.FieldExp])
repFields (HsRecFields { rec_flds = flds })
  = repList fieldExpQTyConName rep_fld flds
  where
    rep_fld :: LHsRecField Name (LHsExpr Name)
            -> DsM (Core (TH.Q TH.FieldExp))
    rep_fld (L _ fld) = do { fn <- lookupLOcc (hsRecFieldId fld)
                           ; e  <- repLE (hsRecFieldArg fld)
                           ; repFieldExp fn e }

-- repUpdFields :: [LHsRecUpdField Name] -> DsM (Core [TH.Q TH.FieldExp])
-- repUpdFields = repList fieldExpQTyConName rep_fld
--   where
--     rep_fld :: LHsRecUpdField Name -> DsM (Core (TH.Q TH.FieldExp))
--     rep_fld (L l fld) = case unLoc (hsRecFieldLbl fld) of
--       Unambiguous sel_name _ -> do { fn <- lookupLOcc (L l sel_name)
--                                    ; e  <- repLE (hsRecFieldArg fld)
--                                    ; repFieldExp fn e }
--       _                      -> notHandled "Ambiguous record updates" (ppr fld)



-----------------------------------------------------------------------------
-- Representing Stmt's is tricky, especially if bound variables
-- shadow each other. Consider:  [| do { x <- f 1; x <- f x; g x } |]
-- First gensym new names for every variable in any of the patterns.
-- both static (x'1 and x'2), and dynamic ((gensym "x") and (gensym "y"))
-- if variables didn't shadow, the static gensym wouldn't be necessary
-- and we could reuse the original names (x and x).
--
-- do { x'1 <- gensym "x"
--    ; x'2 <- gensym "x"
--    ; doE [ BindSt (pvar x'1) [| f 1 |]
--          , BindSt (pvar x'2) [| f x |]
--          , NoBindSt [| g x |]
--          ]
--    }

-- The strategy is to translate a whole list of do-bindings by building a
-- bigger environment, and a bigger set of meta bindings
-- (like:  x'1 <- gensym "x" ) and then combining these with the translations
-- of the expressions within the Do

-----------------------------------------------------------------------------
-- The helper function repSts computes the translation of each sub expression
-- and a bunch of prefix bindings denoting the dynamic renaming.

repLSts :: [LStmt Name (LHsExpr Name)] -> DsM ([GenSymBind], [Core TH.StmtQ])
repLSts stmts = repSts (map unLoc stmts)

repSts :: [Stmt Name (LHsExpr Name)] -> DsM ([GenSymBind], [Core TH.StmtQ])
repSts (BindStmt p e _ _ : ss) =
   do { e2 <- repLE e
      ; ss1 <- mkGenSyms (collectPatBinders p)
      ; addBinds ss1 $ do {
      ; p1 <- repLP p;
      ; (ss2,zs) <- repSts ss
      ; z <- repBindSt p1 e2
      ; return (ss1++ss2, z : zs) }}
repSts (LetStmt bs : ss) =
   do { (ss1,ds) <- repBinds bs
      ; z <- repLetSt ds
      ; (ss2,zs) <- addBinds ss1 (repSts ss)
      ; return (ss1++ss2, z : zs) }
repSts (BodyStmt e _ _ _ : ss) =
   do { e2 <- repLE e
      ; z <- repNoBindSt e2
      ; (ss2,zs) <- repSts ss
      ; return (ss2, z : zs) }
repSts (ParStmt stmt_blocks _ _ : ss) =
   do { (ss_s, stmt_blocks1) <- mapAndUnzipM rep_stmt_block stmt_blocks
      ; let stmt_blocks2 = nonEmptyCoreList stmt_blocks1
            ss1 = concat ss_s
      ; z <- repParSt stmt_blocks2
      ; (ss2, zs) <- addBinds ss1 (repSts ss)
      ; return (ss1++ss2, z : zs) }
   where
     rep_stmt_block :: ParStmtBlock Name Name
                    -> DsM ([GenSymBind], Core [TH.StmtQ])
     rep_stmt_block (ParStmtBlock stmts _ _) =
       do { (ss1, zs) <- repSts (map unLoc stmts)
          ; zs1 <- coreList stmtQTyConName zs
          ; return (ss1, zs1) }
repSts [LastStmt e _ _]
  = do { e2 <- repLE e
       ; z <- repNoBindSt e2
       ; return ([], [z]) }
repSts []    = return ([],[])
repSts other = notHandled "Exotic statement" (ppr other)


-----------------------------------------------------------
--                      Bindings
-----------------------------------------------------------

repBinds :: HsLocalBinds Name -> DsM ([GenSymBind], Core [TH.DecQ])
repBinds EmptyLocalBinds
  = do  { core_list <- coreList decQTyConName []
        ; return ([], core_list) }

repBinds b@(HsIPBinds {}) = notHandled "Implicit parameters" (ppr b)

repBinds (HsValBinds decs)
 = do   { let { bndrs = hsSigTvBinders decs ++ collectHsValBinders decs }
                -- No need to worry about detailed scopes within
                -- the binding group, because we are talking Names
                -- here, so we can safely treat it as a mutually
                -- recursive group
                -- For hsSigTvBinders see Note [Scoped type variables in bindings]
        ; ss        <- mkGenSyms bndrs
        ; prs       <- addBinds ss (rep_val_binds decs)
        ; core_list <- coreList decQTyConName
                                (de_loc (sort_by_loc prs))
        ; return (ss, core_list) }

rep_val_binds :: HsValBinds Name -> DsM [(SrcSpan, Core TH.DecQ)]
-- Assumes: all the binders of the binding are already in the meta-env
rep_val_binds (ValBindsOut binds sigs)
 = do { core1 <- rep_binds (unionManyBags (map snd binds))
      ; core2 <- rep_sigs sigs
      ; return (core1 ++ core2) }
rep_val_binds (ValBindsIn _ _)
 = panic "rep_val_binds: ValBinds"

rep_binds :: LHsBinds Name -> DsM [(SrcSpan, Core TH.DecQ)]
rep_binds = mapM rep_bind . bagToList

rep_bind :: LHsBind Name -> DsM (SrcSpan, Core TH.DecQ)
-- Assumes: all the binders of the binding are already in the meta-env

-- Note GHC treats declarations of a variable (not a pattern)
-- e.g.  x = g 5 as a Fun MonoBinds. This is indicated by a single match
-- with an empty list of patterns
rep_bind (L loc (FunBind
                 { fun_id = fn,
                   fun_matches = MG { mg_alts
                           = [L _ (Match
                                       { m_pats = []
                                       , m_grhss = GRHSs guards wheres }
                                      )] } }))
 = do { (ss,wherecore) <- repBinds wheres
        ; guardcore <- addBinds ss (repGuards guards)
        ; fn'  <- lookupLBinder fn
        ; p    <- repPvar fn'
        ; ans  <- repVal p guardcore wherecore
        ; ans' <- wrapGenSyms ss ans
        ; return (loc, ans') }

rep_bind (L loc (FunBind { fun_id = fn
                         , fun_matches = MG { mg_alts = ms } }))
 =   do { ms1 <- mapM repClauseTup ms
        ; fn' <- lookupLBinder fn
        ; ans <- repFun fn' (nonEmptyCoreList ms1)
        ; return (loc, ans) }

rep_bind (L loc (PatBind { pat_lhs = pat
                         , pat_rhs = GRHSs guards wheres }))
 =   do { patcore <- repLP pat
        ; (ss,wherecore) <- repBinds wheres
        ; guardcore <- addBinds ss (repGuards guards)
        ; ans  <- repVal patcore guardcore wherecore
        ; ans' <- wrapGenSyms ss ans
        ; return (loc, ans') }

rep_bind (L _ (VarBind { var_id = v, var_rhs = e}))
 =   do { v' <- lookupBinder v
        ; e2 <- repLE e
        ; x <- repNormal e2
        ; patcore <- repPvar v'
        ; empty_decls <- coreList decQTyConName []
        ; ans <- repVal patcore x empty_decls
        ; return (srcLocSpan (getSrcLoc v), ans) }

rep_bind (L _ (AbsBinds {}))  = panic "rep_bind: AbsBinds"
rep_bind (L _loc (PatSynBind (PSB { psb_id   = _syn
                                   , psb_args = _args
                                   , psb_def  = _pat
                                   , psb_dir  = _dir })))
  = panic "rep_bind: PatSynBind"
  -- = do { syn'      <- lookupLBinder syn
  --      ; dir'      <- repPatSynDir dir
  --      ; ss        <- mkGenArgSyms args
  --      ; patSynD'  <- addBinds ss (
  --        do { args'  <- repPatSynArgs args
  --           ; pat'   <- repLP pat
  --           ; repPatSynD syn' args' dir' pat' })
  --      ; patSynD'' <- wrapGenArgSyms args ss patSynD'
  --      ; return (loc, patSynD'') }
  -- where
  --   mkGenArgSyms :: HsPatSynDetails (Located Name) -> DsM [GenSymBind]
  --   -- for Record Pattern Synonyms we want to conflate the selector
  --   -- and the pattern-only names in order to provide a nicer TH
  --   -- API. Whereas inside GHC, record pattern synonym selectors and
  --   -- their pattern-only bound right hand sides have different names,
  --   -- we want to treat them the same in TH. This is the reason why we
  --   -- need an adjusted mkGenArgSyms in the `RecCon` case below.
  --   mkGenArgSyms (PrefixCon args)     = mkGenSyms (map unLoc args)
  --   mkGenArgSyms (InfixCon arg1 arg2) = mkGenSyms [unLoc arg1, unLoc arg2]
  --   mkGenArgSyms (RecCon fields)
  --     = do { let pats = map (unLoc . recordPatSynPatVar) fields
  --                sels = map (unLoc . recordPatSynSelectorId) fields
  --          ; ss <- mkGenSyms sels
  --          ; return $ replaceNames (zip sels pats) ss }
  --
  --   replaceNames selsPats genSyms
  --     = [ (pat, id) | (sel, id) <- genSyms, (sel', pat) <- selsPats
  --                   , sel == sel' ]
  --
  --   wrapGenArgSyms :: HsPatSynDetails (Located Name)
  --                  -> [GenSymBind] -> Core TH.DecQ -> DsM (Core TH.DecQ)
  --   wrapGenArgSyms (RecCon _) _  dec = return dec
  --   wrapGenArgSyms _          ss dec = wrapGenSyms ss dec

-- repPatSynD :: Core TH.Name
--           --  -> Core TH.PatSynArgsQ
--            -> Core TH.PatSynDirQ
--            -> Core TH.PatQ
--            -> DsM (Core TH.DecQ)
-- repPatSynD (MkC syn) (MkC args) (MkC dir) (MkC pat)
--   = rep2 patSynDName [syn, args, dir, pat]

-- repPatSynArgs :: HsPatSynDetails (Located Name) -> DsM (Core TH.PatSynArgsQ)
-- repPatSynArgs (PrefixCon args)
--   = do { args' <- repList nameTyConName lookupLOcc args
--        ; repPrefixPatSynArgs args' }
-- repPatSynArgs (InfixCon arg1 arg2)
--   = do { arg1' <- lookupLOcc arg1
--        ; arg2' <- lookupLOcc arg2
--        ; repInfixPatSynArgs arg1' arg2' }
-- repPatSynArgs (RecCon fields)
--   = do { sels' <- repList nameTyConName lookupLOcc sels
--        ; repRecordPatSynArgs sels' }
--   where sels = map recordPatSynSelectorId fields
--
-- repPrefixPatSynArgs :: Core [TH.Name] -> DsM (Core TH.PatSynArgsQ)
-- repPrefixPatSynArgs (MkC nms) = rep2 prefixPatSynName [nms]
--
-- repInfixPatSynArgs :: Core TH.Name -> Core TH.Name -> DsM (Core TH.PatSynArgsQ)
-- repInfixPatSynArgs (MkC nm1) (MkC nm2) = rep2 infixPatSynName [nm1, nm2]
--
-- repRecordPatSynArgs :: Core [TH.Name]
--                     -> DsM (Core TH.PatSynArgsQ)
-- repRecordPatSynArgs (MkC sels) = rep2 recordPatSynName [sels]
--
-- repPatSynDir :: HsPatSynDir Name -> DsM (Core TH.PatSynDirQ)
-- repPatSynDir Unidirectional        = rep2 unidirPatSynName []
-- repPatSynDir ImplicitBidirectional = rep2 implBidirPatSynName []
-- repPatSynDir (ExplicitBidirectional (MG { mg_alts = L _ clauses }))
--   = do { clauses' <- mapM repClauseTup clauses
--        ; repExplBidirPatSynDir (nonEmptyCoreList clauses') }
--
-- repExplBidirPatSynDir :: Core [TH.ClauseQ] -> DsM (Core TH.PatSynDirQ)
-- repExplBidirPatSynDir (MkC cls) = rep2 explBidirPatSynName [cls]
--

-----------------------------------------------------------------------------
-- Since everything in a Bind is mutually recursive we need rename all
-- all the variables simultaneously. For example:
-- [| AndMonoBinds (f x = x + g 2) (g x = f 1 + 2) |] would translate to
-- do { f'1 <- gensym "f"
--    ; g'2 <- gensym "g"
--    ; [ do { x'3 <- gensym "x"; fun f'1 [pvar x'3] [| x + g2 |]},
--        do { x'4 <- gensym "x"; fun g'2 [pvar x'4] [| f 1 + 2 |]}
--      ]}
-- This requires collecting the bindings (f'1 <- gensym "f"), and the
-- environment ( f |-> f'1 ) from each binding, and then unioning them
-- together. As we do this we collect GenSymBinds's which represent the renamed
-- variables bound by the Bindings. In order not to lose track of these
-- representations we build a shadow datatype MB with the same structure as
-- MonoBinds, but which has slots for the representations


-----------------------------------------------------------------------------
-- GHC allows a more general form of lambda abstraction than specified
-- by Haskell 98. In particular it allows guarded lambda's like :
-- (\  x | even x -> 0 | odd x -> 1) at the moment we can't represent this in
-- Haskell Template's Meta.Exp type so we punt if it isn't a simple thing like
-- (\ p1 .. pn -> exp) by causing an error.

repLambda :: LMatch Name (LHsExpr Name) -> DsM (Core TH.ExpQ)
repLambda (L _ (Match { m_pats = ps
                      , m_grhss = GRHSs [L _ (GRHS [] e)] EmptyLocalBinds } ))
 = do { let bndrs = collectPatsBinders ps ;
      ; ss  <- mkGenSyms bndrs
      ; lam <- addBinds ss (
                do { xs <- repLPs ps; body <- repLE e; repLam xs body })
      ; wrapGenSyms ss lam }

repLambda (L _ m) = notHandled "Guarded lambdas" (pprMatch (LambdaExpr :: HsMatchContext Name) m)


-----------------------------------------------------------------------------
--                      Patterns
-- repP deals with patterns.  It assumes that we have already
-- walked over the pattern(s) once to collect the binders, and
-- have extended the environment.  So every pattern-bound
-- variable should already appear in the environment.

-- Process a list of patterns
repLPs :: [LPat Name] -> DsM (Core [TH.PatQ])
repLPs ps = repList patQTyConName repLP ps

repLP :: LPat Name -> DsM (Core TH.PatQ)
repLP (L _ p) = repP p

repP :: Pat Name -> DsM (Core TH.PatQ)
repP (WildPat _)        = repPwild
repP (LitPat l)         = do { l2 <- repLiteral l; repPlit l2 }
repP (VarPat x)         = do { x' <- lookupBinder x; repPvar x' }
repP (LazyPat  p)       = do { p1 <- repLP p; repPtilde p1 }
repP (BangPat p)        = do { p1 <- repLP p; repPbang p1 }
repP (AsPat x p)        = do { x' <- lookupLBinder x; p1 <- repLP p
                             ; repPaspat x' p1 }
repP (ParPat p)         = repLP p
repP (ListPat ps _ Nothing)        = do { qs <- repLPs ps; repPlist qs }
repP (ListPat ps ty1 (Just (_,e))) = do { p <- repP (ListPat ps ty1 Nothing)
                                        ; e' <- repE e
                                        ; repPview e' p}
repP (TuplePat ps boxed _)
  | isBoxed boxed       = do { qs <- repLPs ps; repPtup qs }
  | otherwise           = do { qs <- repLPs ps; repPunboxedTup qs }
-- repP (SumPat _ p alt arity) = do { p1 <- repLP p
--                                  ; repPunboxedSum p1 alt arity }
repP (ConPatIn dc details)
 = do { con_str <- lookupLOcc dc
      ; case details of
         PrefixCon ps -> do { qs <- repLPs ps; repPcon con_str qs }
         RecCon rec   -> do { fps <- repList fieldPatQTyConName rep_fld (rec_flds rec)
                            ; repPrec con_str fps }
         InfixCon p1 p2 -> do { p1' <- repLP p1;
                                p2' <- repLP p2;
                                repPinfix p1' con_str p2' }
   }
 where
   rep_fld :: LHsRecField Name (LPat Name) -> DsM (Core (TH.Name,TH.PatQ))
   rep_fld (L _ fld) = do { MkC v <- lookupLOcc (hsRecFieldId fld)
                          ; MkC p <- repLP (hsRecFieldArg fld)
                          ; rep2 fieldPatName [v,p] }

repP (NPat (L _ l) Nothing _) = do { a <- repOverloadedLiteral l; repPlit a }
repP (ViewPat e p _) = do { e' <- repLE e; p' <- repLP p; repPview e' p' }
repP p@(NPat _ (Just _) _) = notHandled "Negative overloaded patterns" (ppr p)
-- repP (SigPat t p) = do { p' <- repLP p
--                        ; t' <- repLTy (hsSigWcType t)
--                        ; repPsig p' t' }
repP (SplicePat splice) = repSplice splice

repP other = notHandled "Exotic pattern" (ppr other)

----------------------------------------------------------
-- Declaration ordering helpers

sort_by_loc :: [(SrcSpan, a)] -> [(SrcSpan, a)]
sort_by_loc xs = sortBy comp xs
    where comp x y = compare (fst x) (fst y)

de_loc :: [(a, b)] -> [b]
de_loc = map snd

----------------------------------------------------------
--      The meta-environment

-- A name/identifier association for fresh names of locally bound entities
type GenSymBind = (Name, Id)    -- Gensym the string and bind it to the Id
                                -- I.e.         (x, x_id) means
                                --      let x_id = gensym "x" in ...

-- Generate a fresh name for a locally bound entity

mkGenSyms :: [Name] -> DsM [GenSymBind]
-- We can use the existing name.  For example:
--      [| \x_77 -> x_77 + x_77 |]
-- desugars to
--      do { x_77 <- genSym "x"; .... }
-- We use the same x_77 in the desugared program, but with the type Bndr
-- instead of Int
--
-- We do make it an Internal name, though (hence localiseName)
--
-- Nevertheless, it's monadic because we have to generate nameTy
mkGenSyms ns = do { var_ty <- lookupType nameTyConName
                  ; return [(nm, mkLocalId (localiseName nm) var_ty) | nm <- ns] }


addBinds :: [GenSymBind] -> DsM a -> DsM a
-- Add a list of fresh names for locally bound entities to the
-- meta environment (which is part of the state carried around
-- by the desugarer monad)
addBinds bs m = dsExtendMetaEnv (mkNameEnv [(n,DsBound id) | (n,id) <- bs]) m

dupBinder :: (Name, Name) -> DsM (Name, DsMetaVal)
dupBinder (new, old)
  = do { mb_val <- dsLookupMetaEnv old
       ; case mb_val of
           Just val -> return (new, val)
           Nothing  -> pprPanic "dupBinder" (ppr old) }

-- Look up a locally bound name
--
lookupLBinder :: Located Name -> DsM (Core TH.Name)
lookupLBinder (L _ n) = lookupBinder n

lookupBinder :: Name -> DsM (Core TH.Name)
lookupBinder = lookupOcc
  -- Binders are brought into scope before the pattern or what-not is
  -- desugared.  Moreover, in instance declaration the binder of a method
  -- will be the selector Id and hence a global; so we need the
  -- globalVar case of lookupOcc

-- Look up a name that is either locally bound or a global name
--
--  * If it is a global name, generate the "original name" representation (ie,
--   the <module>:<name> form) for the associated entity
--
lookupLOcc :: Located Name -> DsM (Core TH.Name)
-- Lookup an occurrence; it can't be a splice.
-- Use the in-scope bindings if they exist
lookupLOcc (L _ n) = lookupOcc n

lookupOcc :: Name -> DsM (Core TH.Name)
lookupOcc n
  = do {  mb_val <- dsLookupMetaEnv n ;
          case mb_val of
                Nothing           -> globalVar n
                Just (DsBound x)  -> return (coreVar x)
                Just (DsSplice _) -> pprPanic "repE:lookupOcc" (ppr n)
    }

globalVar :: Name -> DsM (Core TH.Name)
-- Not bound by the meta-env
-- Could be top-level; or could be local
--      f x = $(g [| x |])
-- Here the x will be local
globalVar name
  | isExternalName name
  = do  { MkC mod <- coreStringLit name_mod
        ; MkC pkg <- coreStringLit name_pkg
        ; MkC occ <- nameLit name
        ; rep2 mk_varg [pkg,mod,occ] }
  | otherwise
  = do  { MkC occ <- nameLit name
        ; MkC uni <- coreIntLit (getKey (getUnique name))
        ; rep2 mkNameLName [occ,uni] }
  where
      mod = ASSERT( isExternalName name) nameModule name
      name_mod = moduleNameString (moduleName mod)
      name_pkg = unitIdString (moduleUnitId mod)
      name_occ = nameOccName name
      mk_varg | OccName.isDataOcc name_occ = mkNameG_dName
              | OccName.isVarOcc  name_occ = mkNameG_vName
              | OccName.isTcOcc   name_occ = mkNameG_tcName
              | otherwise                  = pprPanic "DsMeta.globalVar" (ppr name)

lookupType :: Name      -- Name of type constructor (e.g. TH.ExpQ)
           -> DsM Type  -- The type
lookupType tc_name = do { tc <- dsLookupTyCon tc_name ;
                          return (mkTyConApp tc []) }

wrapGenSyms :: [GenSymBind]
            -> Core (TH.Q a) -> DsM (Core (TH.Q a))
-- wrapGenSyms [(nm1,id1), (nm2,id2)] y
--      --> bindQ (gensym nm1) (\ id1 ->
--          bindQ (gensym nm2 (\ id2 ->
--          y))

wrapGenSyms binds body@(MkC b)
  = do  { var_ty <- lookupType nameTyConName
        ; go var_ty binds }
  where
    [elt_ty] = tcTyConAppArgs (exprType b)
        -- b :: Q a, so we can get the type 'a' by looking at the
        -- argument type. NB: this relies on Q being a data/newtype,
        -- not a type synonym

    go _ [] = return body
    go var_ty ((name,id) : binds)
      = do { MkC body'  <- go var_ty binds
           ; lit_str    <- nameLit name
           ; gensym_app <- repGensym lit_str
           ; repBindQ var_ty elt_ty
                      gensym_app (MkC (Lam id body')) }

nameLit :: Name -> DsM (Core String)
nameLit n = coreStringLit (occNameString (nameOccName n))

-- occNameLit :: OccName -> DsM (Core String)
-- occNameLit name = coreStringLit (occNameString name)


-- %*********************************************************************
-- %*                                                                   *
--              Constructing code
-- %*                                                                   *
-- %*********************************************************************

-----------------------------------------------------------------------------
-- PHANTOM TYPES for consistency. In order to make sure we do this correct
-- we invent a new datatype which uses phantom types.

newtype Core a = MkC CoreExpr
unC :: Core a -> CoreExpr
unC (MkC x) = x

rep2 :: Name -> [ CoreExpr ] -> DsM (Core a)
rep2 n xs = do { id <- dsLookupGlobalId n
               ; return (MkC (foldl App (Var id) xs)) }

dataCon' :: Name -> [CoreExpr] -> DsM (Core a)
dataCon' n args = do { id <- dsLookupDataCon n
                     ; return $ MkC $ mkCoreConApps id args }

dataCon :: Name -> DsM (Core a)
dataCon n = dataCon' n []


-- %*********************************************************************
-- %*                                                                   *
--              The 'smart constructors'
-- %*                                                                   *
-- %*********************************************************************

--------------- Patterns -----------------
repPlit   :: Core TH.Lit -> DsM (Core TH.PatQ)
repPlit (MkC l) = rep2 litPName [l]

repPvar :: Core TH.Name -> DsM (Core TH.PatQ)
repPvar (MkC s) = rep2 varPName [s]

repPtup :: Core [TH.PatQ] -> DsM (Core TH.PatQ)
repPtup (MkC ps) = rep2 tupPName [ps]

repPunboxedTup :: Core [TH.PatQ] -> DsM (Core TH.PatQ)
repPunboxedTup (MkC ps) = rep2 unboxedTupPName [ps]

-- repPunboxedSum :: Core TH.PatQ -> TH.SumAlt -> TH.SumArity -> DsM (Core TH.PatQ)
-- -- Note: not Core TH.SumAlt or Core TH.SumArity; it's easier to be direct here
-- repPunboxedSum (MkC p) alt arity
--  = do { dflags <- getDynFlags
--       ; rep2 unboxedSumPName [ p
--                              , mkIntExprInt dflags alt
--                              , mkIntExprInt dflags arity ] }

repPcon   :: Core TH.Name -> Core [TH.PatQ] -> DsM (Core TH.PatQ)
repPcon (MkC s) (MkC ps) = rep2 conPName [s, ps]

repPrec   :: Core TH.Name -> Core [(TH.Name,TH.PatQ)] -> DsM (Core TH.PatQ)
repPrec (MkC c) (MkC rps) = rep2 recPName [c,rps]

repPinfix :: Core TH.PatQ -> Core TH.Name -> Core TH.PatQ -> DsM (Core TH.PatQ)
repPinfix (MkC p1) (MkC n) (MkC p2) = rep2 infixPName [p1, n, p2]

repPtilde :: Core TH.PatQ -> DsM (Core TH.PatQ)
repPtilde (MkC p) = rep2 tildePName [p]

repPbang :: Core TH.PatQ -> DsM (Core TH.PatQ)
repPbang (MkC p) = rep2 bangPName [p]

repPaspat :: Core TH.Name -> Core TH.PatQ -> DsM (Core TH.PatQ)
repPaspat (MkC s) (MkC p) = rep2 asPName [s, p]

repPwild  :: DsM (Core TH.PatQ)
repPwild = rep2 wildPName []

repPlist :: Core [TH.PatQ] -> DsM (Core TH.PatQ)
repPlist (MkC ps) = rep2 listPName [ps]

repPview :: Core TH.ExpQ -> Core TH.PatQ -> DsM (Core TH.PatQ)
repPview (MkC e) (MkC p) = rep2 viewPName [e,p]

-- repPsig :: Core TH.PatQ -> Core TH.TypeQ -> DsM (Core TH.PatQ)
-- repPsig (MkC p) (MkC t) = rep2 sigPName [p, t]

--------------- Expressions -----------------
repVarOrCon :: Name -> Core TH.Name -> DsM (Core TH.ExpQ)
repVarOrCon vc str | isDataOcc (nameOccName vc) = repCon str
                   | otherwise                  = repVar str

repVar :: Core TH.Name -> DsM (Core TH.ExpQ)
repVar (MkC s) = rep2 varEName [s]

repCon :: Core TH.Name -> DsM (Core TH.ExpQ)
repCon (MkC s) = rep2 conEName [s]

repLit :: Core TH.Lit -> DsM (Core TH.ExpQ)
repLit (MkC c) = rep2 litEName [c]

repApp :: Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repApp (MkC x) (MkC y) = rep2 appEName [x,y]

-- repAppType :: Core TH.ExpQ -> Core TH.TypeQ -> DsM (Core TH.ExpQ)
-- repAppType (MkC x) (MkC y) = rep2 appTypeEName [x,y]

repLam :: Core [TH.PatQ] -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repLam (MkC ps) (MkC e) = rep2 lamEName [ps, e]

repLamCase :: Core [TH.MatchQ] -> DsM (Core TH.ExpQ)
repLamCase (MkC ms) = rep2 lamCaseEName [ms]

repTup :: Core [TH.ExpQ] -> DsM (Core TH.ExpQ)
repTup (MkC es) = rep2 tupEName [es]

repUnboxedTup :: Core [TH.ExpQ] -> DsM (Core TH.ExpQ)
repUnboxedTup (MkC es) = rep2 unboxedTupEName [es]

-- repUnboxedSum :: Core TH.ExpQ -> TH.SumAlt -> TH.SumArity -> DsM (Core TH.ExpQ)
-- -- Note: not Core TH.SumAlt or Core TH.SumArity; it's easier to be direct here
-- repUnboxedSum (MkC e) alt arity
--  = do { dflags <- getDynFlags
--       ; rep2 unboxedSumEName [ e
--                              , mkIntExprInt dflags alt
--                              , mkIntExprInt dflags arity ] }

repCond :: Core TH.ExpQ -> Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repCond (MkC x) (MkC y) (MkC z) = rep2 condEName [x,y,z]

repMultiIf :: Core [TH.Q (TH.Guard, TH.Exp)] -> DsM (Core TH.ExpQ)
repMultiIf (MkC alts) = rep2 multiIfEName [alts]

repLetE :: Core [TH.DecQ] -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repLetE (MkC ds) (MkC e) = rep2 letEName [ds, e]

repCaseE :: Core TH.ExpQ -> Core [TH.MatchQ] -> DsM( Core TH.ExpQ)
repCaseE (MkC e) (MkC ms) = rep2 caseEName [e, ms]

repDoE :: Core [TH.StmtQ] -> DsM (Core TH.ExpQ)
repDoE (MkC ss) = rep2 doEName [ss]

repComp :: Core [TH.StmtQ] -> DsM (Core TH.ExpQ)
repComp (MkC ss) = rep2 compEName [ss]

repListExp :: Core [TH.ExpQ] -> DsM (Core TH.ExpQ)
repListExp (MkC es) = rep2 listEName [es]

repSigExp :: Core TH.ExpQ -> Core TH.TypeQ -> DsM (Core TH.ExpQ)
repSigExp (MkC e) (MkC t) = rep2 sigEName [e,t]

repRecCon :: Core TH.Name -> Core [TH.Q TH.FieldExp]-> DsM (Core TH.ExpQ)
repRecCon (MkC c) (MkC fs) = rep2 recConEName [c,fs]

repRecUpd :: Core TH.ExpQ -> Core [TH.Q TH.FieldExp] -> DsM (Core TH.ExpQ)
repRecUpd (MkC e) (MkC fs) = rep2 recUpdEName [e,fs]

repFieldExp :: Core TH.Name -> Core TH.ExpQ -> DsM (Core (TH.Q TH.FieldExp))
repFieldExp (MkC n) (MkC x) = rep2 fieldExpName [n,x]

repInfixApp :: Core TH.ExpQ -> Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repInfixApp (MkC x) (MkC y) (MkC z) = rep2 infixAppName [x,y,z]

repSectionL :: Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repSectionL (MkC x) (MkC y) = rep2 sectionLName [x,y]

repSectionR :: Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repSectionR (MkC x) (MkC y) = rep2 sectionRName [x,y]

------------ Right hand sides (guarded expressions) ----
repGuarded :: Core [TH.Q (TH.Guard, TH.Exp)] -> DsM (Core TH.BodyQ)
repGuarded (MkC pairs) = rep2 guardedBName [pairs]

repNormal :: Core TH.ExpQ -> DsM (Core TH.BodyQ)
repNormal (MkC e) = rep2 normalBName [e]

------------ Guards ----
repLNormalGE :: LHsExpr Name -> LHsExpr Name
             -> DsM (Core (TH.Q (TH.Guard, TH.Exp)))
repLNormalGE g e = do g' <- repLE g
                      e' <- repLE e
                      repNormalGE g' e'

repNormalGE :: Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core (TH.Q (TH.Guard, TH.Exp)))
repNormalGE (MkC g) (MkC e) = rep2 normalGEName [g, e]

repPatGE :: Core [TH.StmtQ] -> Core TH.ExpQ -> DsM (Core (TH.Q (TH.Guard, TH.Exp)))
repPatGE (MkC ss) (MkC e) = rep2 patGEName [ss, e]

------------- Stmts -------------------
repBindSt :: Core TH.PatQ -> Core TH.ExpQ -> DsM (Core TH.StmtQ)
repBindSt (MkC p) (MkC e) = rep2 bindSName [p,e]

repLetSt :: Core [TH.DecQ] -> DsM (Core TH.StmtQ)
repLetSt (MkC ds) = rep2 letSName [ds]

repNoBindSt :: Core TH.ExpQ -> DsM (Core TH.StmtQ)
repNoBindSt (MkC e) = rep2 noBindSName [e]

repParSt :: Core [[TH.StmtQ]] -> DsM (Core TH.StmtQ)
repParSt (MkC sss) = rep2 parSName [sss]

-------------- Range (Arithmetic sequences) -----------
repFrom :: Core TH.ExpQ -> DsM (Core TH.ExpQ)
repFrom (MkC x) = rep2 fromEName [x]

repFromThen :: Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repFromThen (MkC x) (MkC y) = rep2 fromThenEName [x,y]

repFromTo :: Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repFromTo (MkC x) (MkC y) = rep2 fromToEName [x,y]

repFromThenTo :: Core TH.ExpQ -> Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repFromThenTo (MkC x) (MkC y) (MkC z) = rep2 fromThenToEName [x,y,z]

------------ Match and Clause Tuples -----------
repMatch :: Core TH.PatQ -> Core TH.BodyQ -> Core [TH.DecQ] -> DsM (Core TH.MatchQ)
repMatch (MkC p) (MkC bod) (MkC ds) = rep2 matchName [p, bod, ds]

repClause :: Core [TH.PatQ] -> Core TH.BodyQ -> Core [TH.DecQ] -> DsM (Core TH.ClauseQ)
repClause (MkC ps) (MkC bod) (MkC ds) = rep2 clauseName [ps, bod, ds]

-------------- Dec -----------------------------
repVal :: Core TH.PatQ -> Core TH.BodyQ -> Core [TH.DecQ] -> DsM (Core TH.DecQ)
repVal (MkC p) (MkC b) (MkC ds) = rep2 valDName [p, b, ds]

repFun :: Core TH.Name -> Core [TH.ClauseQ] -> DsM (Core TH.DecQ)
repFun (MkC nm) (MkC b) = rep2 funDName [nm, b]

repData :: Core TH.CxtQ -> Core TH.Name -> Core [TH.TyVarBndr]
        -> Maybe (Core [TH.TypeQ]) -> Core (Maybe TH.Kind)
        -> Core [TH.ConQ] -> Core TH.CxtQ -> DsM (Core TH.DecQ)
repData (MkC cxt) (MkC nm) (MkC tvs) Nothing (MkC ksig) (MkC cons) (MkC derivs)
  = rep2 dataDName [cxt, nm, tvs, ksig, cons, derivs]
repData (MkC cxt) (MkC nm) (MkC _) (Just (MkC tys)) (MkC ksig) (MkC cons)
        (MkC derivs)
  = rep2 dataInstDName [cxt, nm, tys, ksig, cons, derivs]

repNewtype :: Core TH.CxtQ -> Core TH.Name -> Core [TH.TyVarBndr]
           -> Maybe (Core [TH.TypeQ]) -> Core (Maybe TH.Kind)
           -> Core TH.ConQ -> Core TH.CxtQ -> DsM (Core TH.DecQ)
repNewtype (MkC cxt) (MkC nm) (MkC tvs) Nothing (MkC ksig) (MkC con)
           (MkC derivs)
  = rep2 newtypeDName [cxt, nm, tvs, ksig, con, derivs]
repNewtype (MkC cxt) (MkC nm) (MkC _) (Just (MkC tys)) (MkC ksig) (MkC con)
           (MkC derivs)
  = rep2 newtypeInstDName [cxt, nm, tys, ksig, con, derivs]

repTySyn :: Core TH.Name -> Core [TH.TyVarBndr]
         -> Core TH.TypeQ -> DsM (Core TH.DecQ)
repTySyn (MkC nm) (MkC tvs) (MkC rhs)
  = rep2 tySynDName [nm, tvs, rhs]

repInst :: Core (Maybe TH.Overlap) ->
           Core TH.CxtQ -> Core TH.TypeQ -> Core [TH.DecQ] -> DsM (Core TH.DecQ)
repInst (MkC o) (MkC cxt) (MkC ty) (MkC ds) = rep2 instanceWithOverlapDName
                                                              [o, cxt, ty, ds]

-- repDerivStrategy ::  DsM (Core (Maybe TH.DerivStrategy))
-- repDerivStrategy = coreNothing derivStrategyTyConName

repOverlap :: Maybe OverlapMode -> DsM (Core (Maybe TH.Overlap))
repOverlap mb =
  case mb of
    Nothing -> nothing
    Just o ->
      case o of
        NoOverlap _    -> nothing
        Overlappable _ -> just =<< dataCon overlappableDataConName
        Overlapping _  -> just =<< dataCon overlappingDataConName
        Overlaps _     -> just =<< dataCon overlapsDataConName
        Incoherent _   -> just =<< dataCon incoherentDataConName
  where
  nothing = coreNothing overlapTyConName
  just    = coreJust overlapTyConName


repClass :: Core TH.CxtQ -> Core TH.Name -> Core [TH.TyVarBndr]
         -> Core [TH.FunDep] -> Core [TH.DecQ]
         -> DsM (Core TH.DecQ)
repClass (MkC cxt) (MkC cls) (MkC tvs) (MkC fds) (MkC ds)
  = rep2 classDName [cxt, cls, tvs, fds, ds]

repDeriv :: Core TH.CxtQ -> Core TH.TypeQ -> DsM (Core TH.DecQ)
repDeriv (MkC cxt) (MkC ty) = rep2 standaloneDerivDName [cxt, ty]

repPragInl :: Core TH.Name -> Core TH.Inline -> Core TH.RuleMatch
           -> Core TH.Phases -> DsM (Core TH.DecQ)
repPragInl (MkC nm) (MkC inline) (MkC rm) (MkC phases)
  = rep2 pragInlDName [nm, inline, rm, phases]

repPragSpec :: Core TH.Name -> Core TH.TypeQ -> Core TH.Phases
            -> DsM (Core TH.DecQ)
repPragSpec (MkC nm) (MkC ty) (MkC phases)
  = rep2 pragSpecDName [nm, ty, phases]

repPragSpecInl :: Core TH.Name -> Core TH.TypeQ -> Core TH.Inline
               -> Core TH.Phases -> DsM (Core TH.DecQ)
repPragSpecInl (MkC nm) (MkC ty) (MkC inline) (MkC phases)
  = rep2 pragSpecInlDName [nm, ty, inline, phases]

repPragSpecInst :: Core TH.TypeQ -> DsM (Core TH.DecQ)
repPragSpecInst (MkC ty) = rep2 pragSpecInstDName [ty]

-- repPragComplete :: Core [TH.Name] -> Core (Maybe TH.Name) -> DsM (Core TH.DecQ)
-- repPragComplete (MkC cls) (MkC mty) = rep2 pragCompleteDName [cls, mty]

repPragRule :: Core String -> Core [TH.RuleBndrQ] -> Core TH.ExpQ
            -> Core TH.ExpQ -> Core TH.Phases -> DsM (Core TH.DecQ)
repPragRule (MkC nm) (MkC bndrs) (MkC lhs) (MkC rhs) (MkC phases)
  = rep2 pragRuleDName [nm, bndrs, lhs, rhs, phases]

repPragAnn :: Core TH.AnnTarget -> Core TH.ExpQ -> DsM (Core TH.DecQ)
repPragAnn (MkC targ) (MkC e) = rep2 pragAnnDName [targ, e]

repTySynInst :: Core TH.Name -> Core TH.TySynEqnQ -> DsM (Core TH.DecQ)
repTySynInst (MkC nm) (MkC eqn)
    = rep2 tySynInstDName [nm, eqn]

-- repClosedFamilyKind :: Core TH.Name
--                     -> Core [TH.TyVarBndr]
--                     -> Core TH.Kind
--                     -> Core [TH.TySynEqnQ]
--                     -> DsM (Core TH.DecQ)
-- repClosedFamilyKind (MkC nm) (MkC tvs) (MkC ki) (MkC eqns)
--     = rep2 closedTypeFamilyKindDName [nm, tvs, ki, eqns]

-- repClosedFamilyNoKind :: Core TH.Name
--                       -> Core [TH.TyVarBndr]
--                       -> Core [TH.TySynEqnQ]
--                       -> DsM (Core TH.DecQ)
-- repClosedFamilyNoKind (MkC nm) (MkC tvs) (MkC eqns)
--     = rep2 closedTypeFamilyNoKindDName [nm, tvs, eqns]

repDataFamilyD :: Core TH.Name -> Core [TH.TyVarBndr]
               -> Core (Maybe TH.Kind) -> DsM (Core TH.DecQ)
repDataFamilyD (MkC nm) (MkC tvs) (MkC kind)
    = rep2 dataFamilyDName [nm, tvs, kind]

repOpenFamilyD :: Core TH.Name
               -> Core [TH.TyVarBndr]
               -> Core TH.FamilyResultSig
               -> Core (Maybe TH.InjectivityAnn)
               -> DsM (Core TH.DecQ)
repOpenFamilyD (MkC nm) (MkC tvs) (MkC result) (MkC inj)
    = rep2 openTypeFamilyDName [nm, tvs, result, inj]

repClosedFamilyD :: Core TH.Name
                 -> Core [TH.TyVarBndr]
                 -> Core TH.FamilyResultSig
                 -> Core (Maybe TH.InjectivityAnn)
                 -> Core [TH.TySynEqnQ]
                 -> DsM (Core TH.DecQ)
repClosedFamilyD (MkC nm) (MkC tvs) (MkC res) (MkC inj) (MkC eqns)
    = rep2 closedTypeFamilyDName [nm, tvs, res, inj, eqns]

repTySynEqn :: Core [TH.TypeQ] -> Core TH.TypeQ -> DsM (Core TH.TySynEqnQ)
repTySynEqn (MkC lhs) (MkC rhs)
  = rep2 tySynEqnName [lhs, rhs]

repRoleAnnotD :: Core TH.Name -> Core [TH.Role] -> DsM (Core TH.DecQ)
repRoleAnnotD (MkC n) (MkC roles) = rep2 roleAnnotDName [n, roles]

repFunDep :: Core [TH.Name] -> Core [TH.Name] -> DsM (Core TH.FunDep)
repFunDep (MkC xs) (MkC ys) = rep2 funDepName [xs, ys]

repProto :: Name -> Core TH.Name -> Core TH.TypeQ -> DsM (Core TH.DecQ)
repProto mk_sig (MkC s) (MkC ty) = rep2 mk_sig [s, ty]

repCtxt :: Core [TH.PredQ] -> DsM (Core TH.CxtQ)
repCtxt (MkC tys) = rep2 cxtName [tys]

-- repDataCon :: Located Name
--            -> HsConDeclDetails Name
--            -> DsM (Core TH.ConQ)
-- repDataCon con details
--     = do con' <- lookupLOcc con -- See Note [Binders and occurrences]
--          repConstr details Nothing [con']

-- repGadtDataCons :: [Located Name]
--                 -> HsConDeclDetails Name
--                 -> LHsType Name
--                 -> DsM (Core TH.ConQ)
-- repGadtDataCons cons details res_ty
--     = do cons' <- mapM lookupLOcc cons -- See Note [Binders and occurrences]
--          repConstr details (Just res_ty) cons'

-- Invariant:
--   * for plain H98 data constructors second argument is Nothing and third
--     argument is a singleton list
--   * for GADTs data constructors second argument is (Just return_type) and
--     third argument is a non-empty list
repConstr :: HsConDeclDetails Name
          -> Maybe (LHsType Name)
          -> [Core TH.Name]
          -> DsM (Core TH.ConQ)
repConstr (PrefixCon ps) Nothing [con]
    = do arg_tys  <- repList bangTypeQTyConName repBangTy ps
         rep2 normalCName [unC con, unC arg_tys]

repConstr (PrefixCon ps) (Just (L _ res_ty)) cons
    = do arg_tys     <- repList bangTypeQTyConName repBangTy ps
         res_ty' <- repTy res_ty
         rep2 gadtCName [ unC (nonEmptyCoreList cons), unC arg_tys, unC res_ty']

repConstr (RecCon (L _ ips)) resTy cons
    = do args     <- concatMapM rep_ip ips
         arg_vtys <- coreList varBangTypeQTyConName args
         case resTy of
           Nothing -> rep2 recCName [unC (head cons), unC arg_vtys]
           Just (L _ res_ty) -> do
             res_ty' <- repTy res_ty
             rep2 recGadtCName [unC (nonEmptyCoreList cons), unC arg_vtys,
                                unC res_ty']

    where
      rep_ip (L _ ip) = mapM (rep_one_ip (cd_fld_type ip)) (cd_fld_names ip)
      rep_one_ip t n = do { MkC v  <- lookupLOcc n
                          ; MkC ty <- repBangTy  t
                          ; rep2 varBangTypeName [v,ty] }

repConstr (InfixCon st1 st2) Nothing [con]
    = do arg1 <- repBangTy st1
         arg2 <- repBangTy st2
         rep2 infixCName [unC arg1, unC con, unC arg2]

repConstr (InfixCon {}) (Just _) _ =
    panic "repConstr: infix GADT constructor should be in a PrefixCon"
repConstr _ _ _ =
    panic "repConstr: invariant violated"

------------ Types -------------------

repTForall :: Core [TH.TyVarBndr] -> Core TH.CxtQ -> Core TH.TypeQ
           -> DsM (Core TH.TypeQ)
repTForall (MkC tvars) (MkC ctxt) (MkC ty)
    = rep2 forallTName [tvars, ctxt, ty]

repTvar :: Core TH.Name -> DsM (Core TH.TypeQ)
repTvar (MkC s) = rep2 varTName [s]

repTapp :: Core TH.TypeQ -> Core TH.TypeQ -> DsM (Core TH.TypeQ)
repTapp (MkC t1) (MkC t2) = rep2 appTName [t1, t2]

repTapps :: Core TH.TypeQ -> [Core TH.TypeQ] -> DsM (Core TH.TypeQ)
repTapps f []     = return f
repTapps f (t:ts) = do { f1 <- repTapp f t; repTapps f1 ts }

repTSig :: Core TH.TypeQ -> Core TH.Kind -> DsM (Core TH.TypeQ)
repTSig (MkC ty) (MkC ki) = rep2 sigTName [ty, ki]

repTequality :: DsM (Core TH.TypeQ)
repTequality = rep2 equalityTName []

repTPromotedList :: [Core TH.TypeQ] -> DsM (Core TH.TypeQ)
repTPromotedList []     = repPromotedNilTyCon
repTPromotedList (t:ts) = do  { tcon <- repPromotedConsTyCon
                              ; f <- repTapp tcon t
                              ; t' <- repTPromotedList ts
                              ; repTapp f t'
                              }

repTLit :: Core TH.TyLitQ -> DsM (Core TH.TypeQ)
repTLit (MkC lit) = rep2 litTName [lit]

repTWildCard :: DsM (Core TH.TypeQ)
repTWildCard = rep2 wildCardTName []

repTStar :: DsM (Core TH.TypeQ)
repTStar = rep2 starKName []

repTConstraint :: DsM (Core TH.TypeQ)
repTConstraint = rep2 constraintKName []

--------- Type constructors --------------

repNamedTyCon :: Core TH.Name -> DsM (Core TH.TypeQ)
repNamedTyCon (MkC s) = rep2 conTName [s]

repTupleTyCon :: Int -> DsM (Core TH.TypeQ)
-- Note: not Core Int; it's easier to be direct here
repTupleTyCon i = do dflags <- getDynFlags
                     rep2 tupleTName [mkIntExprInt dflags i]

repUnboxedTupleTyCon :: Int -> DsM (Core TH.TypeQ)
-- Note: not Core Int; it's easier to be direct here
repUnboxedTupleTyCon i = do dflags <- getDynFlags
                            rep2 unboxedTupleTName [mkIntExprInt dflags i]

-- repUnboxedSumTyCon :: TH.SumArity -> DsM (Core TH.TypeQ)
-- -- Note: not Core TH.SumArity; it's easier to be direct here
-- repUnboxedSumTyCon arity = do dflags <- getDynFlags
--                               rep2 unboxedSumTName [mkIntExprInt dflags arity]

repArrowTyCon :: DsM (Core TH.TypeQ)
repArrowTyCon = rep2 arrowTName []

repListTyCon :: DsM (Core TH.TypeQ)
repListTyCon = rep2 listTName []

repPromotedDataCon :: Core TH.Name -> DsM (Core TH.TypeQ)
repPromotedDataCon (MkC s) = rep2 promotedTName [s]

repPromotedTupleTyCon :: Int -> DsM (Core TH.TypeQ)
repPromotedTupleTyCon i = do dflags <- getDynFlags
                             rep2 promotedTupleTName [mkIntExprInt dflags i]

repPromotedNilTyCon :: DsM (Core TH.TypeQ)
repPromotedNilTyCon = rep2 promotedNilTName []

repPromotedConsTyCon :: DsM (Core TH.TypeQ)
repPromotedConsTyCon = rep2 promotedConsTName []

------------ TyVarBndrs -------------------

repPlainTV :: Core TH.Name -> DsM (Core TH.TyVarBndr)
repPlainTV (MkC nm) = rep2 plainTVName [nm]

repKindedTV :: Core TH.Name -> Core TH.Kind -> DsM (Core TH.TyVarBndr)
repKindedTV (MkC nm) (MkC ki) = rep2 kindedTVName [nm, ki]

repKVar :: Core TH.Name -> DsM (Core TH.Kind)
repKVar (MkC s) = rep2 varKName [s]

repKCon :: Core TH.Name -> DsM (Core TH.Kind)
repKCon (MkC s) = rep2 conKName [s]

repKTuple :: Int -> DsM (Core TH.Kind)
repKTuple i = do dflags <- getDynFlags
                 rep2 tupleKName [mkIntExprInt dflags i]

repKArrow :: DsM (Core TH.Kind)
repKArrow = rep2 arrowKName []

repKList :: DsM (Core TH.Kind)
repKList = rep2 listKName []

repKApp :: Core TH.Kind -> Core TH.Kind -> DsM (Core TH.Kind)
repKApp (MkC k1) (MkC k2) = rep2 appKName [k1, k2]

repKApps :: Core TH.Kind -> [Core TH.Kind] -> DsM (Core TH.Kind)
repKApps f []     = return f
repKApps f (k:ks) = do { f' <- repKApp f k; repKApps f' ks }

repKStar :: DsM (Core TH.Kind)
repKStar = rep2 starKName []

repKConstraint :: DsM (Core TH.Kind)
repKConstraint = rep2 constraintKName []

----------------------------------------------------------
--       Type family result signature

repNoSig :: DsM (Core TH.FamilyResultSig)
repNoSig = rep2 noSigName []

repKindSig :: Core TH.Kind -> DsM (Core TH.FamilyResultSig)
repKindSig (MkC ki) = rep2 kindSigName [ki]

-- repTyVarSig :: Core TH.TyVarBndr -> DsM (Core TH.FamilyResultSig)
-- repTyVarSig (MkC bndr) = rep2 tyVarSigName [bndr]

----------------------------------------------------------
--              Literals

repLiteral :: HsLit -> DsM (Core TH.Lit)
repLiteral (HsStringPrim _ bs)
  = do dflags   <- getDynFlags
       word8_ty <- lookupType word8TyConName
       let w8s = unpack bs
           w8s_expr = map (\w8 -> mkCoreConApps word8DataCon
                                  [mkWordLit dflags (toInteger w8)]) w8s
       rep2 stringPrimLName [mkListExpr word8_ty w8s_expr]
repLiteral lit
  = do lit' <- case lit of
                   HsIntPrim _ i    -> mk_integer i
                   HsWordPrim _ w   -> mk_integer w
                   HsInt   i        -> mk_integer (il_value i)
                   HsFloatPrim r    -> mk_rational r
                   HsDoublePrim r   -> mk_rational r
                   HsCharPrim _ c   -> mk_char c
                   _ -> return lit
       lit_expr <- dsLit lit'
       case mb_lit_name of
          Just lit_name -> rep2 lit_name [lit_expr]
          Nothing -> notHandled "Exotic literal" (ppr lit)
  where
    mb_lit_name = case lit of
                 HsInteger _ _ _  -> Just integerLName
                 HsInt _          -> Just integerLName
                 HsIntPrim _ _    -> Just intPrimLName
                 HsWordPrim _ _   -> Just wordPrimLName
                 HsFloatPrim _    -> Just floatPrimLName
                 HsDoublePrim _   -> Just doublePrimLName
                 HsChar _ _       -> Just charLName
                 HsCharPrim _ _   -> Just charPrimLName
                 HsString _ _     -> Just stringLName
                 HsRat _ _        -> Just rationalLName
                 _                -> Nothing

mk_integer :: Integer -> DsM (HsLit)
mk_integer  i = do integer_ty <- lookupType integerTyConName
                   return $ HsInteger (show i) i integer_ty

mk_rational :: FractionalLit -> DsM (HsLit)
mk_rational r = do rat_ty <- lookupType rationalTyConName
                   return $ HsRat r rat_ty
mk_string :: FastString -> DsM (HsLit)
mk_string s = return $ HsString (show s) s

mk_char :: Char -> DsM (HsLit)
mk_char c = return $ HsChar (show c) c

repOverloadedLiteral :: HsOverLit Name -> DsM (Core TH.Lit)
repOverloadedLiteral (OverLit { ol_val = val})
  = do { lit <- mk_lit val; repLiteral lit }
        -- The type Rational will be in the environment, because
        -- the smart constructor 'TH.Syntax.rationalL' uses it in its type,
        -- and rationalL is sucked in when any TH stuff is used

mk_lit :: OverLitVal -> DsM (HsLit)
mk_lit (HsIntegral i)     = mk_integer  (il_value i)
mk_lit (HsFractional f)   = mk_rational f
mk_lit (HsIsString _ s)   = mk_string   s

-- repNameS :: Core String -> DsM (Core TH.Name)
-- repNameS (MkC name) = rep2 mkNameSName [name]

--------------- Miscellaneous -------------------

repGensym :: Core String -> DsM (Core (TH.Q TH.Name))
repGensym (MkC lit_str) = rep2 newNameName [lit_str]

repBindQ :: Type -> Type        -- a and b
         -> Core (TH.Q a) -> Core (a -> TH.Q b) -> DsM (Core (TH.Q b))
repBindQ ty_a ty_b (MkC x) (MkC y)
  = rep2 bindQName [Type ty_a, Type ty_b, x, y]

repSequenceQ :: Type -> Core [TH.Q a] -> DsM (Core (TH.Q [a]))
repSequenceQ ty_a (MkC list)
  = rep2 sequenceQName [Type ty_a, list]

-- repUnboundVar :: Core TH.Name -> DsM (Core TH.ExpQ)
-- repUnboundVar (MkC name) = rep2 unboundVarEName [name]

-- repOverLabel :: FastString -> DsM (Core TH.ExpQ)
-- repOverLabel fs = do
--                     (MkC s) <- coreStringLit $ unpackFS fs
--                     rep2 labelEName [s]


------------ Lists -------------------
-- turn a list of patterns into a single pattern matching a list

repList :: Name -> (a  -> DsM (Core b))
                    -> [a] -> DsM (Core [b])
repList tc_name f args
  = do { args1 <- mapM f args
       ; coreList tc_name args1 }

coreList :: Name    -- Of the TyCon of the element type
         -> [Core a] -> DsM (Core [a])
coreList tc_name es
  = do { elt_ty <- lookupType tc_name; return (coreList' elt_ty es) }

coreList' :: Type       -- The element type
          -> [Core a] -> Core [a]
coreList' elt_ty es = MkC (mkListExpr elt_ty (map unC es ))

nonEmptyCoreList :: [Core a] -> Core [a]
  -- The list must be non-empty so we can get the element type
  -- Otherwise use coreList
nonEmptyCoreList []           = panic "coreList: empty argument"
nonEmptyCoreList xs@(MkC x:_) = MkC (mkListExpr (exprType x) (map unC xs))

coreStringLit :: String -> DsM (Core String)
coreStringLit s = do { z <- mkStringExpr s; return(MkC z) }

------------------- Maybe ------------------

-- | Construct Core expression for Nothing of a given type name
coreNothing :: Name        -- ^ Name of the TyCon of the element type
            -> DsM (Core (Maybe a))
coreNothing tc_name =
    do { elt_ty <- lookupType tc_name; return (coreNothing' elt_ty) }

-- | Construct Core expression for Nothing of a given type
coreNothing' :: Type       -- ^ The element type
             -> Core (Maybe a)
coreNothing' elt_ty = MkC (mkNothingExpr elt_ty)

-- | Store given Core expression in a Just of a given type name
coreJust :: Name        -- ^ Name of the TyCon of the element type
         -> Core a -> DsM (Core (Maybe a))
coreJust tc_name es
  = do { elt_ty <- lookupType tc_name; return (coreJust' elt_ty es) }

-- | Store given Core expression in a Just of a given type
coreJust' :: Type       -- ^ The element type
          -> Core a -> Core (Maybe a)
coreJust' elt_ty es = MkC (mkJustExpr elt_ty (unC es))

------------ Literals & Variables -------------------

coreIntLit :: Int -> DsM (Core Int)
coreIntLit i = do dflags <- getDynFlags
                  return (MkC (mkIntExprInt dflags i))

coreVar :: Id -> Core TH.Name   -- The Id has type Name
coreVar id = MkC (Var id)

----------------- Failure -----------------------
notHandledL :: SrcSpan -> String -> SDoc -> DsM a
notHandledL loc what doc
  | isGoodSrcSpan loc
  = putSrcSpanDs loc $ notHandled what doc
  | otherwise
  = notHandled what doc

notHandled :: String -> SDoc -> DsM a
notHandled what doc = failWithDs msg
  where
    msg = hang (text what <+> text "not (yet) handled by Template Haskell")
             2 doc
