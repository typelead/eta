{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Desugaring expressions.
-}

{-# LANGUAGE CPP #-}

module Eta.DeSugar.DsExpr ( dsExpr, dsLExpr, dsLocalBinds, dsValBinds, dsLit ) where

import Eta.DeSugar.Match
import Eta.DeSugar.MatchLit
import Eta.DeSugar.DsBinds
import Eta.DeSugar.DsGRHSs
import Eta.DeSugar.DsListComp
import Eta.DeSugar.DsUtils
import Eta.DeSugar.DsArrows
import Eta.DeSugar.DsMonad
import Eta.BasicTypes.Name
import Eta.BasicTypes.NameEnv
import Eta.Types.FamInstEnv( topNormaliseType )

#ifdef ETA_REPL
-- Template Haskell stuff iff bootstrapped
import Eta.DeSugar.DsMeta
#endif

import Eta.HsSyn.HsSyn

import Eta.Utils.Platform
-- NB: The desugarer, which straddles the source and Core worlds, sometimes
--     needs to see source types
import Eta.TypeCheck.TcType
import Eta.Types.Coercion ( Role(..) )
import Eta.TypeCheck.TcEvidence
import Eta.TypeCheck.TcHsSyn
import Eta.TypeCheck.TcRnMonad
import Eta.Types.Type
import Eta.Core.CoreSyn
import Eta.Core.CoreUtils
import Eta.Core.CoreFVs
import Eta.Core.MkCore

import Eta.Main.DynFlags
import Eta.Profiling.CostCentre
import Eta.BasicTypes.Id
import Eta.BasicTypes.Module
import Eta.BasicTypes.VarSet
import Eta.BasicTypes.VarEnv
import Eta.BasicTypes.ConLike
import Eta.BasicTypes.DataCon
import Eta.Prelude.TysWiredIn
import Eta.Prelude.PrelNames
import Eta.BasicTypes.BasicTypes
import Eta.Utils.Maybes
import Eta.BasicTypes.SrcLoc
import Eta.Utils.Util
import Eta.Utils.Bag
import Eta.Utils.Outputable
import qualified Eta.Utils.Outputable as Outputable
import Eta.Utils.FastString

import Eta.BasicTypes.IdInfo
import Data.IORef       ( atomicModifyIORef, modifyIORef )

import Control.Monad
import GHC.Fingerprint

#include "HsVersions.h"

{-
************************************************************************
*                                                                      *
                dsLocalBinds, dsValBinds
*                                                                      *
************************************************************************
-}

dsLocalBinds :: HsLocalBinds Id -> CoreExpr -> DsM CoreExpr
dsLocalBinds EmptyLocalBinds    body = return body
dsLocalBinds (HsValBinds binds) body = dsValBinds binds body
dsLocalBinds (HsIPBinds binds)  body = dsIPBinds  binds body

-------------------------
dsValBinds :: HsValBinds Id -> CoreExpr -> DsM CoreExpr
dsValBinds (ValBindsOut binds _) body = foldrM ds_val_bind body binds
dsValBinds (ValBindsIn  _     _) _    = panic "dsValBinds ValBindsIn"

-------------------------
dsIPBinds :: HsIPBinds Id -> CoreExpr -> DsM CoreExpr
dsIPBinds (IPBinds ip_binds ev_binds) body
  = do  { ds_binds <- dsTcEvBinds ev_binds
        ; let inner = mkCoreLets ds_binds body
                -- The dict bindings may not be in
                -- dependency order; hence Rec
        ; foldrM ds_ip_bind inner ip_binds }
  where
    ds_ip_bind (L _ (IPBind ~(Right n) e)) body
      = do e' <- dsLExpr e
           return (Let (NonRec n e') body)

-------------------------
ds_val_bind :: (RecFlag, LHsBinds Id) -> CoreExpr -> DsM CoreExpr
-- Special case for bindings which bind unlifted variables
-- We need to do a case right away, rather than building
-- a tuple and doing selections.
-- Silently ignore INLINE and SPECIALISE pragmas...
ds_val_bind (NonRecursive, hsbinds) body
  | [L loc bind] <- bagToList hsbinds,
        -- Non-recursive, non-overloaded bindings only come in ones
        -- ToDo: in some bizarre case it's conceivable that there
        --       could be dict binds in the 'binds'.  (See the notes
        --       below.  Then pattern-match would fail.  Urk.)
    unliftedMatchOnly bind
  = putSrcSpanDs loc (dsUnliftedBind bind body)

-- Ordinary case for bindings; none should be unlifted
ds_val_bind (_is_rec, binds) body
  = do  { (force_vars,prs) <- dsLHsBinds binds
        ; let body' = foldr seqVar body force_vars
        ; ASSERT2( not (any (isUnLiftedType . idType . fst) prs), ppr _is_rec $$ ppr binds )
          case prs of
            [] -> return body
            _ -> return (Let (Rec prs) body') }
        -- Use a Rec regardless of is_rec.
        -- Why? Because it allows the binds to be all
        -- mixed up, which is what happens in one rare case
        -- Namely, for an AbsBind with no tyvars and no dicts,
        --         but which does have dictionary bindings.
        -- See notes with TcSimplify.inferLoop [NO TYVARS]
        -- It turned out that wrapping a Rec here was the easiest solution
        --
        -- NB The previous case dealt with unlifted bindings, so we
        --    only have to deal with lifted ones now; so Rec is ok

------------------
dsUnliftedBind :: HsBind Id -> CoreExpr -> DsM CoreExpr
dsUnliftedBind (AbsBinds { abs_tvs = [], abs_ev_vars = []
               , abs_exports = exports
               , abs_ev_binds = ev_binds
               , abs_binds = lbinds }) body
  = do { let body1 = foldr bind_export body exports
             bind_export export b = bindNonRec (abe_poly export) (Var (abe_mono export)) b
       ; body2 <- foldlBagM (\body lbind -> dsUnliftedBind (unLoc lbind) body)
                            body1 lbinds
       ; ds_binds <- dsTcEvBinds ev_binds
       ; return (mkCoreLets ds_binds body2) }

dsUnliftedBind (FunBind { fun_id = L _ fun
                     , fun_matches = matches
                     , fun_co_fn = co_fn
                     , fun_tick = tick
                     , fun_infix = inf }) body

            -- Can't be a bang pattern (that looks like a PatBind)
            -- so must be simply unboxed
   = do { (args, rhs) <- matchWrapper (FunRhs (idName fun) inf) Nothing matches
       ; MASSERT( null args ) -- Functions aren't lifted
       ; MASSERT( isIdHsWrapper co_fn )
       ; let rhs' = mkOptTickBox tick rhs
       ; return (bindNonRec fun rhs' body) }

dsUnliftedBind (PatBind {pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty }) body
  =     -- let C x# y# = rhs in body
        -- ==> case rhs of C x# y# -> body
    do { rhs <- dsGuarded grhss ty
       ; let upat = unLoc pat
             eqn = EqnInfo { eqn_pats = [upat],
                             eqn_rhs = cantFailMatchResult body }
       ; var    <- selectMatchVar upat
       ; result <- matchEquations PatBindRhs [var] [eqn] (exprType body)
       ; return (bindNonRec var rhs result) }

dsUnliftedBind bind body = pprPanic "dsLet: unlifted" (ppr bind $$ ppr body)

----------------------
unliftedMatchOnly :: HsBind Id -> Bool
unliftedMatchOnly (AbsBinds { abs_binds = lbinds })
  = anyBag (unliftedMatchOnly . unLoc) lbinds
unliftedMatchOnly (PatBind { pat_lhs = lpat, pat_rhs_ty = rhs_ty })
  =  isUnLiftedType rhs_ty
  || isUnliftedLPat lpat
  || any (isUnLiftedType . idType) (collectPatBinders lpat)
unliftedMatchOnly (FunBind { fun_id = L _ id })
  = isUnLiftedType (idType id)
unliftedMatchOnly _ = False -- I hope!  Checked immediately by caller in fact

{-
************************************************************************
*                                                                      *
\subsection[DsExpr-vars-and-cons]{Variables, constructors, literals}
*                                                                      *
************************************************************************
-}

dsLExpr :: LHsExpr Id -> DsM CoreExpr

dsLExpr (L loc e) = putSrcSpanDs loc $ dsExpr e

dsExpr :: HsExpr Id -> DsM CoreExpr
dsExpr (HsPar e)              = dsLExpr e
dsExpr (ExprWithTySigOut e _) = dsLExpr e
dsExpr (HsVar var)            = return (varToCoreExpr var)   -- See Note [Desugaring vars]
dsExpr (HsIPVar _)            = panic "dsExpr: HsIPVar"
dsExpr (HsOverLabel _)        = panic "dsExpr: HsOverLabel"
dsExpr (HsLit lit)            = dsLit lit
dsExpr (HsOverLit lit)        = dsOverLit lit

dsExpr (HsWrap co_fn e)
  = do { e' <- dsExpr e
       ; wrapped_e <- dsHsWrapper co_fn e'
       ; dflags <- getDynFlags
       ; warnAboutIdentities dflags e' (exprType wrapped_e)
       ; return wrapped_e }

dsExpr (NegApp expr neg_expr)
  = App <$> dsExpr neg_expr <*> dsLExpr expr

dsExpr (HsLam a_Match)
  = uncurry mkLams <$> matchWrapper LambdaExpr Nothing a_Match

dsExpr (HsLamCase arg matches)
  = do { arg_var <- newSysLocalDs arg
       ; ([discrim_var], matching_code) <- matchWrapper CaseAlt Nothing matches
       ; return $ Lam arg_var $ bindNonRec discrim_var (Var arg_var) matching_code }

dsExpr (HsApp fun arg)
  = mkCoreAppDs <$> dsLExpr fun <*>  dsLExpr arg

dsExpr (HsUnboundVar _) = panic "dsExpr: HsUnboundVar"

{-
Note [Desugaring vars]
~~~~~~~~~~~~~~~~~~~~~~
In one situation we can get a *coercion* variable in a HsVar, namely
the support method for an equality superclass:
   class (a~b) => C a b where ...
   instance (blah) => C (T a) (T b) where ..
Then we get
   $dfCT :: forall ab. blah => C (T a) (T b)
   $dfCT ab blah = MkC ($c$p1C a blah) ($cop a blah)

   $c$p1C :: forall ab. blah => (T a ~ T b)
   $c$p1C ab blah = let ...; g :: T a ~ T b = ... } in g

That 'g' in the 'in' part is an evidence variable, and when
converting to core it must become a CO.

Operator sections.  At first it looks as if we can convert
\begin{verbatim}
        (expr op)
\end{verbatim}
to
\begin{verbatim}
        \x -> op expr x
\end{verbatim}

But no!  expr might be a redex, and we can lose laziness badly this
way.  Consider
\begin{verbatim}
        map (expr op) xs
\end{verbatim}
for example.  So we convert instead to
\begin{verbatim}
        let y = expr in \x -> op y x
\end{verbatim}
If \tr{expr} is actually just a variable, say, then the simplifier
will sort it out.
-}

dsExpr (OpApp e1 op _ e2)
  = -- for the type of y, we need the type of op's 2nd argument
    mkCoreAppsDs <$> dsLExpr op <*> mapM dsLExpr [e1, e2]

dsExpr (SectionL expr op)       -- Desugar (e !) to ((!) e)
  = mkCoreAppDs <$> dsLExpr op <*> dsLExpr expr

-- dsLExpr (SectionR op expr)   -- \ x -> op x expr
dsExpr (SectionR op expr) = do
    core_op <- dsLExpr op
    -- for the type of x, we need the type of op's 2nd argument
    let (x_ty:y_ty:_, _) = splitFunTys (exprType core_op)
        -- See comment with SectionL
    y_core <- dsLExpr expr
    x_id <- newSysLocalDs x_ty
    y_id <- newSysLocalDs y_ty
    return (bindNonRec y_id y_core $
            Lam x_id (mkCoreAppsDs core_op [Var x_id, Var y_id]))

dsExpr (ExplicitTuple tup_args boxity)
  = do { let go (lam_vars, args) (L _ (Missing ty))
                    -- For every missing expression, we need
                    -- another lambda in the desugaring.
               = do { lam_var <- newSysLocalDs ty
                    ; return (lam_var : lam_vars, Var lam_var : args) }
             go (lam_vars, args) (L _ (Present expr))
                    -- Expressions that are present don't generate
                    -- lambdas, just arguments.
               = do { core_expr <- dsLExpr expr
                    ; return (lam_vars, core_expr : args) }

       ; (lam_vars, args) <- foldM go ([], []) (reverse tup_args)
                -- The reverse is because foldM goes left-to-right

       ; return $ mkCoreLams lam_vars $
                  mkCoreConApps (tupleCon (boxityNormalTupleSort boxity) (length tup_args))
                                (map (Type . exprType) args ++ args) }

dsExpr (HsSCC _ cc expr@(L loc _)) = do
    dflags <- getDynFlags
    if gopt Opt_SccProfilingOn dflags
      then do
        mod_name <- getModule
        count <- goptM Opt_ProfCountEntries
        uniq <- newUnique
        Tick (ProfNote (mkUserCC cc mod_name loc uniq) count True)
               <$> dsLExpr expr
      else dsLExpr expr

dsExpr (HsCoreAnn _ _ expr)
  = dsLExpr expr

dsExpr (HsCase discrim matches)
  = do { core_discrim <- dsLExpr discrim
       ; ([discrim_var], matching_code) <- matchWrapper CaseAlt (Just discrim) matches
       ; return (bindNonRec discrim_var core_discrim matching_code) }

-- Pepe: The binds are in scope in the body but NOT in the binding group
--       This is to avoid silliness in breakpoints
dsExpr (HsLet binds body) = do
    body' <- dsLExpr body
    dsLocalBinds binds body'

-- We need the `ListComp' form to use `deListComp' (rather than the "do" form)
-- because the interpretation of `stmts' depends on what sort of thing it is.
--
dsExpr (HsDo ListComp     stmts res_ty) = dsListComp stmts res_ty
dsExpr (HsDo PArrComp     stmts _)      = dsPArrComp (map unLoc stmts)
dsExpr (HsDo DoExpr       stmts _)      = dsDo stmts
dsExpr (HsDo GhciStmtCtxt stmts _)      = dsDo stmts
dsExpr (HsDo MDoExpr      stmts _)      = dsDo stmts
dsExpr (HsDo MonadComp    stmts _)      = dsMonadComp stmts

dsExpr (HsIf mb_fun guard_expr then_expr else_expr)
  = do { pred <- dsLExpr guard_expr
       ; b1 <- dsLExpr then_expr
       ; b2 <- dsLExpr else_expr
       ; case mb_fun of
           Just fun -> do { core_fun <- dsExpr fun
                          ; return (mkCoreApps core_fun [pred,b1,b2]) }
           Nothing  -> return $ mkIfThenElse pred b1 b2 }

dsExpr (HsMultiIf res_ty alts)
  | null alts
  = mkErrorExpr

  | otherwise
  = do { match_result <- liftM (foldr1 combineMatchResults)
                               (mapM (dsGRHS IfAlt res_ty) alts)
       ; error_expr   <- mkErrorExpr
       ; extractMatchResult match_result error_expr }
  where
    mkErrorExpr = mkErrorAppDs nON_EXHAUSTIVE_GUARDS_ERROR_ID res_ty
                               (ptext (sLit "multi-way if"))

{-
\noindent
\underline{\bf Various data construction things}
             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}

dsExpr (ExplicitList elt_ty wit xs)
  = dsExplicitList elt_ty wit xs

-- We desugar [:x1, ..., xn:] as
--   singletonP x1 +:+ ... +:+ singletonP xn
--
dsExpr (ExplicitPArr ty []) = do
    emptyP <- dsDPHBuiltin emptyPVar
    return (Var emptyP `App` Type ty)
dsExpr (ExplicitPArr ty xs) = do
    singletonP <- dsDPHBuiltin singletonPVar
    appP       <- dsDPHBuiltin appPVar
    xs'        <- mapM dsLExpr xs
    return . foldr1 (binary appP) $ map (unary singletonP) xs'
  where
    unary  fn x   = mkApps (Var fn) [Type ty, x]
    binary fn x y = mkApps (Var fn) [Type ty, x, y]

dsExpr (ArithSeq expr witness seq)
  = case witness of
     Nothing -> dsArithSeq expr seq
     Just fl -> do {
       ; fl' <- dsExpr fl
       ; newArithSeq <- dsArithSeq expr seq
       ; return (App fl' newArithSeq)}

dsExpr (PArrSeq expr (FromTo from to))
  = mkApps <$> dsExpr expr <*> mapM dsLExpr [from, to]

dsExpr (PArrSeq expr (FromThenTo from thn to))
  = mkApps <$> dsExpr expr <*> mapM dsLExpr [from, thn, to]

dsExpr (PArrSeq _ _)
  = panic "DsExpr.dsExpr: Infinite parallel array!"
    -- the parser shouldn't have generated it and the renamer and typechecker
    -- shouldn't have let it through

{-
\noindent
\underline{\bf Static Pointers}
               ~~~~~~~~~~~~~~~
\begin{verbatim}
    g = ... static f ...
==>
    sptEntry:N = StaticPtr
        (fingerprintString "pkgKey:module.sptEntry:N")
        (StaticPtrInfo "current pkg key" "current module" "sptEntry:0")
        f
    g = ... sptEntry:N
\end{verbatim}
-}

dsExpr (HsStatic expr@(L loc _)) = do
    expr_ds <- dsLExpr expr
    let ty = exprType expr_ds
    n' <- mkSptEntryName loc
    static_binds_var <- dsGetStaticBindsVar

    staticPtrTyCon       <- dsLookupTyCon   staticPtrTyConName
    staticPtrInfoDataCon <- dsLookupDataCon staticPtrInfoDataConName
    staticPtrDataCon     <- dsLookupDataCon staticPtrDataConName
    fingerprintDataCon   <- dsLookupDataCon fingerprintDataConName

    dflags <- getDynFlags
    let (line, col) = case loc of
           RealSrcSpan r -> ( srcLocLine $ realSrcSpanStart r
                            , srcLocCol  $ realSrcSpanStart r
                            )
           _             -> (0, 0)
        srcLoc = mkCoreConApps (tupleCon BoxedTuple 2)
                     [ Type intTy              , Type intTy
                     , mkIntExprInt dflags line, mkIntExprInt dflags col
                     ]
    info <- mkConApp staticPtrInfoDataCon <$>
            (++[srcLoc]) <$>
            mapM mkStringExprFS
                 [ unitIdFS $ moduleUnitId $ nameModule n'
                 , moduleNameFS $ moduleName $ nameModule n'
                 , occNameFS    $ nameOccName n'
                 ]
    let tvars = tyVarsOfTypeList ty
        speTy = mkForAllTys tvars $ mkTyConApp staticPtrTyCon [ty]
        speId = mkExportedLocalId VanillaId n' speTy
        fp@(Fingerprint w0 w1) = fingerprintName $ idName speId
        fp_core = mkConApp fingerprintDataCon
                    [ mkWord64LitWordRep dflags w0
                    , mkWord64LitWordRep dflags w1
                    ]
        sp    = mkConApp staticPtrDataCon [Type ty, fp_core, info, expr_ds]
    liftIO $ modifyIORef static_binds_var ((fp, (speId, mkLams tvars sp)) :)
    putSrcSpanDs loc $ return $ mkTyApps (Var speId) (map mkTyVarTy tvars)

  where

    -- | Choose either 'Word64#' or 'Word#' to represent the arguments of the
    -- 'Fingerprint' data constructor.
    mkWord64LitWordRep dflags
      | platformWordSize (targetPlatform dflags) < 8 = mkWord64LitWord64
      | otherwise = mkWordLit dflags . toInteger

    fingerprintName :: Name -> Fingerprint
    fingerprintName n = fingerprintString $ unpackFS $ concatFS
        [ unitIdFS $ moduleUnitId $ nameModule n
        , fsLit ":"
        , moduleNameFS (moduleName $ nameModule n)
        , fsLit "."
        , occNameFS $ occName n
        ]

{-
\noindent
\underline{\bf Record construction and update}
             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For record construction we do this (assuming T has three arguments)
\begin{verbatim}
        T { op2 = e }
==>
        let err = /\a -> recConErr a
        T (recConErr t1 "M.lhs/230/op1")
          e
          (recConErr t1 "M.lhs/230/op3")
\end{verbatim}
@recConErr@ then converts its argument string into a proper message
before printing it as
\begin{verbatim}
        M.lhs, line 230: missing field op1 was evaluated
\end{verbatim}

We also handle @C{}@ as valid construction syntax for an unlabelled
constructor @C@, setting all of @C@'s fields to bottom.
-}

dsExpr (RecordCon (L _ data_con_id) con_expr rbinds) = do
    con_expr' <- dsExpr con_expr
    let
        (arg_tys, _) = tcSplitFunTys (exprType con_expr')
        -- A newtype in the corner should be opaque;
        -- hence TcType.tcSplitFunTys

        mk_arg (arg_ty, lbl)    -- Selector id has the field label as its name
          = case findField (rec_flds rbinds) lbl of
              (rhs:rhss) -> ASSERT( null rhss )
                            dsLExpr rhs
              []         -> mkErrorAppDs rEC_CON_ERROR_ID arg_ty (ppr lbl)
        unlabelled_bottom arg_ty = mkErrorAppDs rEC_CON_ERROR_ID arg_ty Outputable.empty

        labels = dataConFieldLabels (idDataCon data_con_id)
        -- The data_con_id is guaranteed to be the wrapper id of the constructor

    con_args <- if null labels
                then mapM unlabelled_bottom arg_tys
                else mapM mk_arg (zipEqual "dsExpr:RecordCon" arg_tys labels)

    return (mkCoreApps con_expr' con_args)

{-
Record update is a little harder. Suppose we have the decl:
\begin{verbatim}
        data T = T1 {op1, op2, op3 :: Int}
               | T2 {op4, op2 :: Int}
               | T3
\end{verbatim}
Then we translate as follows:
\begin{verbatim}
        r { op2 = e }
===>
        let op2 = e in
        case r of
          T1 op1 _ op3 -> T1 op1 op2 op3
          T2 op4 _     -> T2 op4 op2
          other        -> recUpdError "M.lhs/230"
\end{verbatim}
It's important that we use the constructor Ids for @T1@, @T2@ etc on the
RHSs, and do not generate a Core constructor application directly, because the constructor
might do some argument-evaluation first; and may have to throw away some
dictionaries.

Note [Update for GADTs]
~~~~~~~~~~~~~~~~~~~~~~~
Consider
   data T a b where
     T1 { f1 :: a } :: T a Int

Then the wrapper function for T1 has type
   $WT1 :: a -> T a Int
But if x::T a b, then
   x { f1 = v } :: T a b   (not T a Int!)
So we need to cast (T a Int) to (T a b).  Sigh.
-}

dsExpr expr@(RecordUpd record_expr (HsRecFields { rec_flds = fields })
                       cons_to_upd in_inst_tys out_inst_tys)
  | null fields
  = dsLExpr record_expr
  | otherwise
  = ASSERT2( notNull cons_to_upd, ppr expr )

    do  { record_expr' <- dsLExpr record_expr
        ; field_binds' <- mapM ds_field fields
        ; let upd_fld_env :: NameEnv Id -- Maps field name to the LocalId of the field binding
              upd_fld_env = mkNameEnv [(f,l) | (f,l,_) <- field_binds']

        -- It's important to generate the match with matchWrapper,
        -- and the right hand sides with applications of the wrapper Id
        -- so that everything works when we are doing fancy unboxing on the
        -- constructor arguments.
        ; alts <- mapM (mk_alt upd_fld_env) cons_to_upd
        ; ([discrim_var], matching_code)
                <- matchWrapper RecUpd Nothing
                                       (MG { mg_alts = alts
                                           , mg_arg_tys = [in_ty]
                                           , mg_res_ty = out_ty
                                           , mg_origin = FromSource })
                                           -- FromSource is not strictly right, but we
                                           -- want incomplete pattern-match warnings

        ; return (add_field_binds field_binds' $
                  bindNonRec discrim_var record_expr' matching_code) }
  where
    ds_field :: LHsRecField Id (LHsExpr Id) -> DsM (Name, Id, CoreExpr)
      -- Clone the Id in the HsRecField, because its Name is that
      -- of the record selector, and we must not make that a local binder
      -- else we shadow other uses of the record selector
      -- Hence 'lcl_id'.  Cf Trac #2735
    ds_field (L _ rec_field) = do { rhs <- dsLExpr (hsRecFieldArg rec_field)
                                  ; let fld_id = unLoc (hsRecFieldId rec_field)
                                  ; lcl_id <- newSysLocalDs (idType fld_id)
                                  ; return (idName fld_id, lcl_id, rhs) }

    add_field_binds [] expr = expr
    add_field_binds ((_,b,r):bs) expr = bindNonRec b r (add_field_binds bs expr)

        -- Awkwardly, for families, the match goes
        -- from instance type to family type
    tycon     = dataConTyCon (head cons_to_upd)
    in_ty     = mkTyConApp tycon in_inst_tys
    out_ty    = mkFamilyTyConApp tycon out_inst_tys

    mk_alt upd_fld_env con
      = do { let (univ_tvs, ex_tvs, eq_spec,
                  theta, arg_tys, _) = dataConFullSig con
                 subst = mkTopTvSubst (univ_tvs `zip` in_inst_tys)

                -- I'm not bothering to clone the ex_tvs
           ; eqs_vars   <- mapM newPredVarDs (substTheta subst (eqSpecPreds eq_spec))
           ; theta_vars <- mapM newPredVarDs (substTheta subst theta)
           ; arg_ids    <- newSysLocalsDs (substTys subst arg_tys)
           ; let val_args = zipWithEqual "dsExpr:RecordUpd" mk_val_arg
                                         (dataConFieldLabels con) arg_ids
                 mk_val_arg field_name pat_arg_id
                     = nlHsVar (lookupNameEnv upd_fld_env field_name `orElse` pat_arg_id)
                 inst_con = noLoc $ HsWrap wrap (HsVar (dataConWrapId con))
                        -- Reconstruct with the WrapId so that unpacking happens
                 wrap = mkWpEvVarApps theta_vars          <.>
                        mkWpTyApps    (mkTyVarTys ex_tvs) <.>
                        mkWpTyApps [ty | (tv, ty) <- univ_tvs `zip` out_inst_tys
                                       , not (tv `elemVarEnv` wrap_subst) ]
                 rhs = foldl (\a b -> nlHsApp a b) inst_con val_args

                        -- Tediously wrap the application in a cast
                        -- Note [Update for GADTs]
                 wrap_co = mkTcTyConAppCo Nominal tycon
                                [ lookup tv ty | (tv,ty) <- univ_tvs `zip` out_inst_tys ]
                 lookup univ_tv ty = case lookupVarEnv wrap_subst univ_tv of
                                        Just co' -> co'
                                        Nothing  -> mkTcReflCo Nominal ty
                 wrap_subst = mkVarEnv [ (tv, mkTcSymCo (mkTcCoVarCo eq_var))
                                       | ((tv,_),eq_var) <- eq_spec `zip` eqs_vars ]

                 pat = noLoc $ ConPatOut { pat_con = noLoc (RealDataCon con)
                                         , pat_tvs = ex_tvs
                                         , pat_dicts = eqs_vars ++ theta_vars
                                         , pat_binds = emptyTcEvBinds
                                         , pat_args = PrefixCon $ map nlVarPat arg_ids
                                         , pat_arg_tys = in_inst_tys
                                         , pat_wrap = idHsWrapper }
           ; let wrapped_rhs | null eq_spec = rhs
                             | otherwise    = mkLHsWrap (mkWpCast (mkTcSubCo wrap_co)) rhs
           ; return (mkSimpleMatch [pat] wrapped_rhs) }

-- Here is where we desugar the Template Haskell brackets and escapes

-- Template Haskell stuff

dsExpr (HsRnBracketOut _ _) = panic "dsExpr HsRnBracketOut"
#ifdef ETA_REPL
dsExpr (HsTcBracketOut x ps) = dsBracket x ps
#else
dsExpr (HsTcBracketOut _ _) = panic "dsExpr HsBracketOut"
#endif
dsExpr (HsSpliceE s)      = pprPanic "dsExpr:splice" (ppr s)

-- Arrow notation extension
dsExpr (HsProc pat cmd) = dsProcExpr pat cmd

-- Hpc Support

dsExpr (HsTick tickish e) = do
  e' <- dsLExpr e
  return (Tick tickish e')

-- There is a problem here. The then and else branches
-- have no free variables, so they are open to lifting.
-- We need someway of stopping this.
-- This will make no difference to binary coverage
-- (did you go here: YES or NO), but will effect accurate
-- tick counting.

dsExpr (HsBinTick ixT ixF e) = do
  e2 <- dsLExpr e
  do { ASSERT(exprType e2 `eqType` boolTy)
       mkBinaryTickBox ixT ixF e2
     }

dsExpr (HsTickPragma _ _ expr) = do
  dflags <- getDynFlags
  if gopt Opt_Hpc dflags
    then panic "dsExpr:HsTickPragma"
    else dsLExpr expr

-- HsSyn constructs that just shouldn't be here:
dsExpr (ExprWithTySig {})  = panic "dsExpr:ExprWithTySig"
dsExpr (HsBracket     {})  = panic "dsExpr:HsBracket"
dsExpr (HsArrApp      {})  = panic "dsExpr:HsArrApp"
dsExpr (HsArrForm     {})  = panic "dsExpr:HsArrForm"
dsExpr (EWildPat      {})  = panic "dsExpr:EWildPat"
dsExpr (EAsPat        {})  = panic "dsExpr:EAsPat"
dsExpr (EViewPat      {})  = panic "dsExpr:EViewPat"
dsExpr (ELazyPat      {})  = panic "dsExpr:ELazyPat"
dsExpr (HsType        {})  = panic "dsExpr:HsType"
dsExpr (HsDo          {})  = panic "dsExpr:HsDo"



findField :: [LHsRecField Id arg] -> Name -> [arg]
findField rbinds lbl
  = [rhs | L _ (HsRecField { hsRecFieldId = id, hsRecFieldArg = rhs }) <- rbinds
         , lbl == idName (unLoc id) ]

{-
%--------------------------------------------------------------------

Note [Desugaring explicit lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Explicit lists are desugared in a cleverer way to prevent some
fruitless allocations.  Essentially, whenever we see a list literal
[x_1, ..., x_n] we:

1. Find the tail of the list that can be allocated statically (say
   [x_k, ..., x_n]) by later stages and ensure we desugar that
   normally: this makes sure that we don't cause a code size increase
   by having the cons in that expression fused (see later) and hence
   being unable to statically allocate any more

2. For the prefix of the list which cannot be allocated statically,
   say [x_1, ..., x_(k-1)], we turn it into an expression involving
   build so that if we find any foldrs over it it will fuse away
   entirely!

   So in this example we will desugar to:
   build (\c n -> x_1 `c` x_2 `c` .... `c` foldr c n [x_k, ..., x_n]

   If fusion fails to occur then build will get inlined and (since we
   defined a RULE for foldr (:) []) we will get back exactly the
   normal desugaring for an explicit list.

This optimisation can be worth a lot: up to 25% of the total
allocation in some nofib programs. Specifically

        Program           Size    Allocs   Runtime  CompTime
        rewrite          +0.0%    -26.3%      0.02     -1.8%
           ansi          -0.3%    -13.8%      0.00     +0.0%
           lift          +0.0%     -8.7%      0.00     -2.3%

Of course, if rules aren't turned on then there is pretty much no
point doing this fancy stuff, and it may even be harmful.

=======>  Note by SLPJ Dec 08.

I'm unconvinced that we should *ever* generate a build for an explicit
list.  See the comments in GHC.Base about the foldr/cons rule, which
points out that (foldr k z [a,b,c]) may generate *much* less code than
(a `k` b `k` c `k` z).

Furthermore generating builds messes up the LHS of RULES.
Example: the foldr/single rule in GHC.Base
   foldr k z [x] = ...
We do not want to generate a build invocation on the LHS of this RULE!

We fix this by disabling rules in rule LHSs, and testing that
flag here; see Note [Desugaring RULE left hand sides] in Desugar

To test this I've added a (static) flag -fsimple-list-literals, which
makes all list literals be generated via the simple route.
-}

dsExplicitList :: PostTc Id Type -> Maybe (SyntaxExpr Id) -> [LHsExpr Id]
               -> DsM CoreExpr
-- See Note [Desugaring explicit lists]
dsExplicitList elt_ty Nothing xs
  = do { dflags <- getDynFlags
       ; xs' <- mapM dsLExpr xs
       ; let (dynamic_prefix, static_suffix) = spanTail is_static xs'
       ; if gopt Opt_SimpleListLiterals dflags        -- -fsimple-list-literals
         || not (gopt Opt_EnableRewriteRules dflags)  -- Rewrite rules off
                -- Don't generate a build if there are no rules to eliminate it!
                -- See Note [Desugaring RULE left hand sides] in Desugar
         || null dynamic_prefix   -- Avoid build (\c n. foldr c n xs)!
         then return $ mkListExpr elt_ty xs'
         else mkBuildExpr elt_ty (mkSplitExplicitList dynamic_prefix static_suffix) }
  where
    is_static :: CoreExpr -> Bool
    is_static e = all is_static_var (varSetElems (exprFreeVars e))

    is_static_var :: Var -> Bool
    is_static_var v
      | isId v = isExternalName (idName v)  -- Top-level things are given external names
      | otherwise = False                   -- Type variables

    mkSplitExplicitList prefix suffix (c, _) (n, n_ty)
      = do { let suffix' = mkListExpr elt_ty suffix
           ; folded_suffix <- mkFoldrExpr elt_ty n_ty (Var c) (Var n) suffix'
           ; return (foldr (App . App (Var c)) folded_suffix prefix) }

dsExplicitList elt_ty (Just fln) xs
  = do { fln' <- dsExpr fln
       ; list <- dsExplicitList elt_ty Nothing xs
       ; dflags <- getDynFlags
       ; return (App (App fln' (mkIntExprInt dflags (length xs))) list) }

spanTail :: (a -> Bool) -> [a] -> ([a], [a])
spanTail f xs = (reverse rejected, reverse satisfying)
    where (satisfying, rejected) = span f $ reverse xs

dsArithSeq :: PostTcExpr -> (ArithSeqInfo Id) -> DsM CoreExpr
dsArithSeq expr (From from)
  = App <$> dsExpr expr <*> dsLExpr from
dsArithSeq expr (FromTo from to)
  = do dflags <- getDynFlags
       warnAboutEmptyEnumerations dflags from Nothing to
       expr' <- dsExpr expr
       from' <- dsLExpr from
       to'   <- dsLExpr to
       return $ mkApps expr' [from', to']
dsArithSeq expr (FromThen from thn)
  = mkApps <$> dsExpr expr <*> mapM dsLExpr [from, thn]
dsArithSeq expr (FromThenTo from thn to)
  = do dflags <- getDynFlags
       warnAboutEmptyEnumerations dflags from (Just thn) to
       expr' <- dsExpr expr
       from' <- dsLExpr from
       thn'  <- dsLExpr thn
       to'   <- dsLExpr to
       return $ mkApps expr' [from', thn', to']

{-
Desugar 'do' and 'mdo' expressions (NOT list comprehensions, they're
handled in DsListComp).  Basically does the translation given in the
Haskell 98 report:
-}

dsDo :: [ExprLStmt Id] -> DsM CoreExpr
dsDo stmts
  = goL stmts
  where
    goL [] = panic "dsDo"
    goL (L loc stmt:lstmts) = putSrcSpanDs loc (go loc stmt lstmts)

    go _ (LastStmt body _ _) stmts
      = ASSERT( null stmts ) dsLExpr body
        -- The 'return' op isn't used for 'do' expressions

    go _ (BodyStmt rhs then_expr _ _) stmts
      = do { rhs2 <- dsLExpr rhs
           ; warnDiscardedDoBindings rhs (exprType rhs2)
           ; then_expr2 <- dsExpr then_expr
           ; rest <- goL stmts
           ; return (mkApps then_expr2 [rhs2, rest]) }

    go _ (LetStmt binds) stmts
      = do { rest <- goL stmts
           ; dsLocalBinds binds rest }

    go _ (BindStmt pat rhs bind_op fail_op) stmts
      = do  { body     <- goL stmts
            ; rhs'     <- dsLExpr rhs
            ; bind_op' <- dsExpr bind_op
            ; var   <- selectSimpleMatchVarL pat
            ; let bind_ty = exprType bind_op'   -- rhs -> (pat -> res1) -> res2
                  res1_ty = funResultTy (funArgTy (funResultTy bind_ty))
            ; match <- matchSinglePat (Var var) (StmtCtxt DoExpr) pat
                                      res1_ty (cantFailMatchResult body)
            ; match_code <- handle_failure pat match fail_op
            ; return (mkApps bind_op' [rhs', Lam var match_code]) }

    go _ (ApplicativeStmt args mb_join body_ty) stmts
      = do {
             let
               (pats, rhss) = unzip (map (do_arg . snd) args)

               do_arg (ApplicativeArgOne pat expr _) =
                 (pat, dsLExpr expr)
               do_arg (ApplicativeArgMany stmts ret pat) =
                 (pat, dsDo (stmts ++ [noLoc $ mkLastStmt (noLoc ret)]))

               arg_tys = map hsLPatType pats

           ; rhss' <- sequence rhss
           ; ops' <- mapM dsExpr (map fst args)

           ; let body' = noLoc $ HsDo DoExpr stmts body_ty

           ; let fun = L noSrcSpan $ HsLam $
                   MG { mg_alts = [mkSimpleMatch pats body']
                      , mg_arg_tys = arg_tys
                      , mg_res_ty = body_ty
                      , mg_origin = Generated }

           ; fun' <- dsLExpr fun
           ; let mk_ap_call l (op,r) = mkApps op [l,r]
                 expr = foldl mk_ap_call fun' (zip ops' rhss')
           ; case mb_join of
               Nothing -> return expr
               Just join_op ->
                 do { join_op' <- dsExpr join_op
                    ; return (App join_op' expr) } }

    go loc (RecStmt { recS_stmts = rec_stmts, recS_later_ids = later_ids
                    , recS_rec_ids = rec_ids, recS_ret_fn = return_op
                    , recS_mfix_fn = mfix_op, recS_bind_fn = bind_op
                    , recS_rec_rets = rec_rets, recS_ret_ty = body_ty }) stmts
      = goL (new_bind_stmt : stmts)  -- rec_ids can be empty; eg  rec { print 'x' }
      where
        new_bind_stmt = L loc $ BindStmt (mkBigLHsPatTupId later_pats)
                                         mfix_app bind_op
                                         noSyntaxExpr  -- Tuple cannot fail

        tup_ids      = rec_ids ++ filterOut (`elem` rec_ids) later_ids
        tup_ty       = mkBigCoreTupTy (map idType tup_ids) -- Deals with singleton case
        rec_tup_pats = map nlVarPat tup_ids
        later_pats   = rec_tup_pats
        rets         = map noLoc rec_rets
        mfix_app     = nlHsApp (noLoc mfix_op) mfix_arg
        mfix_arg     = noLoc $ HsLam (MG { mg_alts = [mkSimpleMatch [mfix_pat] body]
                                         , mg_arg_tys = [tup_ty], mg_res_ty = body_ty
                                         , mg_origin = Generated })
        mfix_pat     = noLoc $ LazyPat $ mkBigLHsPatTupId rec_tup_pats
        body         = noLoc $ HsDo DoExpr (rec_stmts ++ [ret_stmt]) body_ty
        ret_app      = nlHsApp (noLoc return_op) (mkBigLHsTupId rets)
        ret_stmt     = noLoc $ mkLastStmt ret_app
                     -- This LastStmt will be desugared with dsDo,
                     -- which ignores the return_op in the LastStmt,
                     -- so we must apply the return_op explicitly

    go _ (ParStmt   {}) _ = panic "dsDo ParStmt"
    go _ (TransStmt {}) _ = panic "dsDo TransStmt"

handle_failure :: LPat Id -> MatchResult -> SyntaxExpr Id -> DsM CoreExpr
    -- In a do expression, pattern-match failure just calls
    -- the monadic 'fail' rather than throwing an exception
handle_failure pat match fail_op
  | matchCanFail match
  = do { fail_op' <- dsExpr fail_op
       ; dflags <- getDynFlags
       ; fail_msg <- mkStringExpr (mk_fail_msg dflags pat)
       ; extractMatchResult match (App fail_op' fail_msg) }
  | otherwise
  = extractMatchResult match (error "It can't fail")

mk_fail_msg :: DynFlags -> Located e -> String
mk_fail_msg dflags pat = "Pattern match failure in do expression at " ++
                         showPpr dflags (getLoc pat)

{-
************************************************************************
*                                                                      *
\subsection{Errors and contexts}
*                                                                      *
************************************************************************
-}

-- Warn about certain types of values discarded in monadic bindings (#3263)
warnDiscardedDoBindings :: LHsExpr Id -> Type -> DsM ()
warnDiscardedDoBindings rhs rhs_ty
  | Just (m_ty, elt_ty) <- tcSplitAppTy_maybe rhs_ty
  = do { warn_unused <- woptM Opt_WarnUnusedDoBind
       ; warn_wrong <- woptM Opt_WarnWrongDoBind
       ; when (warn_unused || warn_wrong) $
    do { fam_inst_envs <- dsGetFamInstEnvs
       ; let norm_elt_ty = topNormaliseType fam_inst_envs elt_ty

           -- Warn about discarding non-() things in 'monadic' binding
       ; if warn_unused && not (isUnitTy norm_elt_ty)
         then warnDs (Reason Opt_WarnUnusedDoBind)
                     (badMonadBind rhs elt_ty)
         else

           -- Warn about discarding m a things in 'monadic' binding of the same type,
           -- but only if we didn't already warn due to Opt_WarnUnusedDoBind
           when warn_wrong $
                do { case tcSplitAppTy_maybe norm_elt_ty of
                         Just (elt_m_ty, _)
                            | m_ty `eqType` topNormaliseType fam_inst_envs elt_m_ty
                            -> warnDs (Reason Opt_WarnWrongDoBind)
                                      (badMonadBind rhs elt_ty)
                         _ -> return () } } }

  | otherwise   -- RHS does have type of form (m ty), which is weird
  = return ()   -- but at lesat this warning is irrelevant

badMonadBind :: LHsExpr Id -> Type -> SDoc
badMonadBind rhs elt_ty
  = vcat [ hang (text "A do-notation statement discarded a result of type")
              2 (quotes (ppr elt_ty))
         , hang (text "Suppress this warning by saying")
              2 (quotes $ ptext (sLit "_ <-") <+> ppr rhs)
         ]

{-
************************************************************************
*                                                                      *
\subsection{Static pointers}
*                                                                      *
************************************************************************
-}

-- | Creates an name for an entry in the Static Pointer Table.
--
-- The name has the form @sptEntry:<N>@ where @<N>@ is generated from a
-- per-module counter.
--
mkSptEntryName :: SrcSpan -> DsM Name
mkSptEntryName loc = do
    uniq <- newUnique
    mod  <- getModule
    occ  <- mkWrapperName "sptEntry"
    return $ mkExternalName uniq mod occ loc
  where
    mkWrapperName what
      = do dflags <- getDynFlags
           thisMod <- getModule
           let -- Note [Generating fresh names for ccall wrapper]
               -- in compiler/typecheck/TcEnv.hs
               wrapperRef = nextWrapperNum dflags
           wrapperNum <- liftIO $ atomicModifyIORef wrapperRef $ \mod_env ->
               let num = lookupWithDefaultModuleEnv mod_env 0 thisMod
                in (extendModuleEnv mod_env thisMod (num+1), num)
           return $ mkVarOcc $ what ++ ":" ++ show wrapperNum
