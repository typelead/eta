{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[WorkWrap]{Worker/wrapper-generating back-end of strictness analyser}
-}

{-# LANGUAGE CPP #-}
module Eta.StrAnal.WorkWrap ( wwTopBinds ) where

import Eta.Core.CoreSyn
import Eta.Core.CoreUnfold       ( certainlyWillInline, mkWwInlineRule, mkWorkerUnfolding )
import Eta.Core.CoreUtils        ( exprType, exprIsHNF )
import Eta.Core.CoreArity        ( exprArity )
import Eta.BasicTypes.Var
import Eta.BasicTypes.Id
import Eta.BasicTypes.IdInfo
import Eta.BasicTypes.UniqSupply
import Eta.BasicTypes.BasicTypes
import Eta.Main.DynFlags
import Eta.BasicTypes.VarEnv           ( isEmptyVarEnv )
import Eta.BasicTypes.Demand
import Eta.StrAnal.WwLib
import Eta.Utils.Util
import Eta.Utils.Outputable
import Eta.Types.FamInstEnv
import Eta.Utils.MonadUtils

#include "HsVersions.h"

{-
We take Core bindings whose binders have:

\begin{enumerate}

\item Strictness attached (by the front-end of the strictness
analyser), and / or

\item Constructed Product Result information attached by the CPR
analysis pass.

\end{enumerate}

and we return some ``plain'' bindings which have been
worker/wrapper-ified, meaning:

\begin{enumerate}

\item Functions have been split into workers and wrappers where
appropriate.  If a function has both strictness and CPR properties
then only one worker/wrapper doing both transformations is produced;

\item Binders' @IdInfos@ have been updated to reflect the existence of
these workers/wrappers (this is where we get STRICTNESS and CPR pragma
info for exported values).
\end{enumerate}
-}

wwTopBinds :: DynFlags -> FamInstEnvs -> UniqSupply -> CoreProgram -> CoreProgram

wwTopBinds dflags fam_envs us top_binds
  = initUs_ us $ do
    top_binds' <- mapM (wwBind dflags fam_envs) top_binds
    return (concat top_binds')

{-
************************************************************************
*                                                                      *
\subsection[wwBind-wwExpr]{@wwBind@ and @wwExpr@}
*                                                                      *
************************************************************************

@wwBind@ works on a binding, trying each \tr{(binder, expr)} pair in
turn.  Non-recursive case first, then recursive...
-}

wwBind  :: DynFlags
        -> FamInstEnvs
        -> CoreBind
        -> UniqSM [CoreBind]    -- returns a WwBinding intermediate form;
                                -- the caller will convert to Expr/Binding,
                                -- as appropriate.

wwBind dflags fam_envs (NonRec binder rhs) = do
    new_rhs <- wwExpr dflags fam_envs rhs
    new_pairs <- tryWW dflags fam_envs NonRecursive binder new_rhs
    return [NonRec b e | (b,e) <- new_pairs]
      -- Generated bindings must be non-recursive
      -- because the original binding was.

wwBind dflags fam_envs (Rec pairs)
  = return . Rec <$> concatMapM do_one pairs
  where
    do_one (binder, rhs) = do new_rhs <- wwExpr dflags fam_envs rhs
                              tryWW dflags fam_envs Recursive binder new_rhs

{-
@wwExpr@ basically just walks the tree, looking for appropriate
annotations that can be used. Remember it is @wwBind@ that does the
matching by looking for strict arguments of the correct type.
@wwExpr@ is a version that just returns the ``Plain'' Tree.
-}

wwExpr :: DynFlags -> FamInstEnvs -> CoreExpr -> UniqSM CoreExpr

wwExpr _      _ e@(Type {}) = return e
wwExpr _      _ e@(Coercion {}) = return e
wwExpr _      _ e@(Lit  {}) = return e
wwExpr _      _ e@(Var  {}) = return e

wwExpr dflags fam_envs (Lam binder expr)
  = Lam binder <$> wwExpr dflags fam_envs expr

wwExpr dflags fam_envs (App f a)
  = App <$> wwExpr dflags fam_envs f <*> wwExpr dflags fam_envs a

wwExpr dflags fam_envs (Tick note expr)
  = Tick note <$> wwExpr dflags fam_envs expr

wwExpr dflags fam_envs (Cast expr co) = do
    new_expr <- wwExpr dflags fam_envs expr
    return (Cast new_expr co)

wwExpr dflags fam_envs (Let bind expr)
  = mkLets <$> wwBind dflags fam_envs bind <*> wwExpr dflags fam_envs expr

wwExpr dflags fam_envs (Case expr binder ty alts) = do
    new_expr <- wwExpr dflags fam_envs expr
    new_alts <- mapM ww_alt alts
    return (Case new_expr binder ty new_alts)
  where
    ww_alt (con, binders, rhs) = do
        new_rhs <- wwExpr dflags fam_envs rhs
        return (con, binders, new_rhs)

{-
************************************************************************
*                                                                      *
\subsection[tryWW]{@tryWW@: attempt a worker/wrapper pair}
*                                                                      *
************************************************************************

@tryWW@ just accumulates arguments, converts strictness info from the
front-end into the proper form, then calls @mkWwBodies@ to do
the business.

The only reason this is monadised is for the unique supply.

Note [Don't w/w INLINE things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very important to refrain from w/w-ing an INLINE function (ie one
with a stable unfolding) because the wrapper will then overwrite the
old stable unfolding with the wrapper code.

Furthermore, if the programmer has marked something as INLINE,
we may lose by w/w'ing it.

If the strictness analyser is run twice, this test also prevents
wrappers (which are INLINEd) from being re-done.  (You can end up with
several liked-named Ids bouncing around at the same time---absolute
mischief.)

Notice that we refrain from w/w'ing an INLINE function even if it is
in a recursive group.  It might not be the loop breaker.  (We could
test for loop-breaker-hood, but I'm not sure that ever matters.)

Note [Worker-wrapper for INLINABLE functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have
  {-# INLINABLE f #-}
  f :: Ord a => [a] -> Int -> a
  f x y = ....f....

where f is strict in y, we might get a more efficient loop by w/w'ing
f.  But that would make a new unfolding which would overwrite the old
one! So the function would no longer be ININABLE, and in particular
will not be specialised at call sites in other modules.

This comes in practice (Trac #6056).

Solution: do the w/w for strictness analysis, but transfer the Stable
unfolding to the *worker*.  So we will get something like this:

  {-# INLINE[0] f #-}
  f :: Ord a => [a] -> Int -> a
  f d x y = case y of I# y' -> fw d x y'

  {-# INLINABLE[0] fw #-}
  fw :: Ord a => [a] -> Int# -> a
  fw d x y' = let y = I# y' in ...f...

How do we "transfer the unfolding"? Easy: by using the old one, wrapped
in work_fn! See CoreUnfold.mkWorkerUnfolding.

Note [Activation for INLINABLE worker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Follows on from Note [Worker-wrapper for INLINABLE functions]
It is *vital* that if the worker gets an INLINABLE pragma (from the
original function), then the worker has the same phase activation as
the wrapper (or later).  That is necessary to allow the wrapper to
inline into the worker's unfolding: see SimplUtils
Note [Simplifying inside stable unfoldings].

Nothing is lost by giving the worker the same activation as the
worker, because the worker won't have any chance of inlining until the
wrapper does; there's no point in giving it an earlier activation.

Note [Don't w/w inline small non-loop-breaker things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, we refrain from w/w-ing *small* functions, which are not
loop breakers, because they'll inline anyway.  But we must take care:
it may look small now, but get to be big later after other inlining
has happened.  So we take the precaution of adding an INLINE pragma to
any such functions.

I made this change when I observed a big function at the end of
compilation with a useful strictness signature but no w-w.  (It was
small during demand analysis, we refrained from w/w, and then got big
when something was inlined in its rhs.) When I measured it on nofib,
it didn't make much difference; just a few percent improved allocation
on one benchmark (bspt/Euclid.space).  But nothing got worse.

There is an infelicity though.  We may get something like
      f = g val
==>
      g x = case gw x of r -> I# r

      f {- InlineStable, Template = g val -}
      f = case gw x of r -> I# r

The code for f duplicates that for g, without any real benefit. It
won't really be executed, because calls to f will go via the inlining.

Note [Wrapper activation]
~~~~~~~~~~~~~~~~~~~~~~~~~
When should the wrapper inlining be active?  It must not be active
earlier than the current Activation of the Id (eg it might have a
NOINLINE pragma).  But in fact strictness analysis happens fairly
late in the pipeline, and we want to prioritise specialisations over
strictness.  Eg if we have
  module Foo where
    f :: Num a => a -> Int -> a
    f n 0 = n              -- Strict in the Int, hence wrapper
    f n x = f (n+n) (x-1)

    g :: Int -> Int
    g x = f x x            -- Provokes a specialisation for f

  module Bar where
    import Foo

    h :: Int -> Int
    h x = f 3 x

Then we want the specialisation for 'f' to kick in before the wrapper does.

Now in fact the 'gentle' simplification pass encourages this, by
having rules on, but inlinings off.  But that's kind of lucky. It seems
more robust to give the wrapper an Activation of (ActiveAfter 0),
so that it becomes active in an importing module at the same time that
it appears in the first place in the defining module.

At one stage I tried making the wrapper inlining always-active, and
that had a very bad effect on nofib/imaginary/x2n1; a wrapper was
inlined before the specialisation fired.
-}

tryWW   :: DynFlags
        -> FamInstEnvs
        -> RecFlag
        -> Id                           -- The fn binder
        -> CoreExpr                     -- The bound rhs; its innards
                                        --   are already ww'd
        -> UniqSM [(Id, CoreExpr)]      -- either *one* or *two* pairs;
                                        -- if one, then no worker (only
                                        -- the orig "wrapper" lives on);
                                        -- if two, then a worker and a
                                        -- wrapper.
tryWW dflags fam_envs is_rec fn_id rhs
  | isNeverActive inline_act
        -- No point in worker/wrappering if the thing is never inlined!
        -- Because the no-inline prag will prevent the wrapper ever
        -- being inlined at a call site.
        --
        -- Furthermore, don't even expose strictness info
  = return [ (fn_id, rhs) ]

  | not loop_breaker
  , Just stable_unf <- certainlyWillInline dflags fn_unf
  = return [ (fn_id `setIdUnfolding` stable_unf, rhs) ]
        -- Note [Don't w/w inline small non-loop-breaker, or INLINE, things]
        -- NB: use idUnfolding because we don't want to apply
        --     this criterion to a loop breaker!

  | is_fun
  = splitFun dflags fam_envs new_fn_id fn_info wrap_dmds res_info rhs

  | is_thunk                                   -- See Note [Thunk splitting]
  = splitThunk dflags fam_envs is_rec new_fn_id rhs

  | otherwise
  = return [ (new_fn_id, rhs) ]

  where
    loop_breaker = isStrongLoopBreaker (occInfo fn_info)
    fn_info      = idInfo fn_id
    inline_act   = inlinePragmaActivation (inlinePragInfo fn_info)
    fn_unf       = unfoldingInfo fn_info

        -- In practice it always will have a strictness
        -- signature, even if it's a uninformative one
    strict_sig  = strictnessInfo fn_info
    StrictSig (DmdType env wrap_dmds res_info) = strict_sig

        -- new_fn_id has the DmdEnv zapped.
        --      (a) it is never used again
        --      (b) it wastes space
        --      (c) it becomes incorrect as things are cloned, because
        --          we don't push the substitution into it
    new_fn_id | isEmptyVarEnv env = fn_id
              | otherwise         = fn_id `setIdStrictness`
                                     mkClosedStrictSig wrap_dmds res_info

    is_fun    = notNull wrap_dmds
    is_thunk  = not is_fun && not (exprIsHNF rhs)


---------------------
splitFun :: DynFlags -> FamInstEnvs -> Id -> IdInfo -> [Demand] -> DmdResult -> CoreExpr
         -> UniqSM [(Id, CoreExpr)]
splitFun dflags fam_envs fn_id fn_info wrap_dmds res_info rhs
  = WARN( not (wrap_dmds `lengthIs` arity), ppr fn_id <+> (ppr arity $$ ppr wrap_dmds $$ ppr res_info) ) do
    -- The arity should match the signature
    stuff <- mkWwBodies dflags fam_envs fun_ty wrap_dmds res_info one_shots
    case stuff of
      Just (work_demands, wrap_fn, work_fn) -> do
        work_uniq <- getUniqueM
        let work_rhs = work_fn rhs
            work_prag = InlinePragma { inl_src = "{-# INLINE"
                                     , inl_inline = inl_inline inl_prag
                                     , inl_sat    = Nothing
                                     , inl_act    = wrap_act
                                     , inl_rule   = FunLike }
              -- idl_inline: copy from fn_id; see Note [Worker-wrapper for INLINABLE functions]
              -- idl_act: see Note [Activation for INLINABLE workers]
              -- inl_rule: it does not make sense for workers to be constructorlike.

            work_id  = mkWorkerId work_uniq fn_id (exprType work_rhs)
                        `setIdOccInfo` occInfo fn_info
                                -- Copy over occurrence info from parent
                                -- Notably whether it's a loop breaker
                                -- Doesn't matter much, since we will simplify next, but
                                -- seems right-er to do so

                        `setInlinePragma` work_prag

                        `setIdUnfolding` mkWorkerUnfolding dflags work_fn (unfoldingInfo fn_info)
                                -- See Note [Worker-wrapper for INLINABLE functions]

                        `setIdStrictness` mkClosedStrictSig work_demands work_res_info
                                -- Even though we may not be at top level,
                                -- it's ok to give it an empty DmdEnv

                        `setIdArity` exprArity work_rhs
                                -- Set the arity so that the Core Lint check that the
                                -- arity is consistent with the demand type goes through

            wrap_act  = ActiveAfter 0
            wrap_rhs  = wrap_fn work_id
            wrap_prag = InlinePragma { inl_src = "{-# INLINE"
                                     , inl_inline = Inline
                                     , inl_sat    = Nothing
                                     , inl_act    = wrap_act
                                     , inl_rule   = rule_match_info }
                -- See Note [Wrapper activation]
                -- The RuleMatchInfo is (and must be) unaffected

            wrap_id   = fn_id `setIdUnfolding`  mkWwInlineRule wrap_rhs arity
                              `setInlinePragma` wrap_prag
                              `setIdOccInfo`    NoOccInfo
                                -- Zap any loop-breaker-ness, to avoid bleating from Lint
                                -- about a loop breaker with an INLINE rule

        return $ [(work_id, work_rhs), (wrap_id, wrap_rhs)]
            -- Worker first, because wrapper mentions it

      Nothing -> return [(fn_id, rhs)]
  where
    fun_ty          = idType fn_id
    inl_prag        = inlinePragInfo fn_info
    rule_match_info = inlinePragmaRuleMatchInfo inl_prag
    arity           = arityInfo fn_info
                    -- The arity is set by the simplifier using exprEtaExpandArity
                    -- So it may be more than the number of top-level-visible lambdas

    work_res_info | isBotRes res_info = botRes  -- Cpr stuff done by wrapper
                  | otherwise         = topRes

    one_shots = get_one_shots rhs

-- If the original function has one-shot arguments, it is important to
-- make the wrapper and worker have corresponding one-shot arguments too.
-- Otherwise we spuriously float stuff out of case-expression join points,
-- which is very annoying.
get_one_shots :: Expr Var -> [OneShotInfo]
get_one_shots (Lam b e)
  | isId b    = idOneShotInfo b : get_one_shots e
  | otherwise = get_one_shots e
get_one_shots (Tick _ e) = get_one_shots e
get_one_shots _          = []

{-
Note [Do not split void functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this rather common form of binding:
        $j = \x:Void# -> ...no use of x...

Since x is not used it'll be marked as absent.  But there is no point
in w/w-ing because we'll simply add (\y:Void#), see WwLib.mkWorerArgs.

If x has a more interesting type (eg Int, or Int#), there *is* a point
in w/w so that we don't pass the argument at all.

Note [Thunk splitting]
~~~~~~~~~~~~~~~~~~~~~~
Suppose x is used strictly (never mind whether it has the CPR
property).

      let
        x* = x-rhs
      in body

splitThunk transforms like this:

      let
        x* = case x-rhs of { I# a -> I# a }
      in body

Now simplifier will transform to

      case x-rhs of
        I# a -> let x* = I# a
                in body

which is what we want. Now suppose x-rhs is itself a case:

        x-rhs = case e of { T -> I# a; F -> I# b }

The join point will abstract over a, rather than over (which is
what would have happened before) which is fine.

Notice that x certainly has the CPR property now!

In fact, splitThunk uses the function argument w/w splitting
function, so that if x's demand is deeper (say U(U(L,L),L))
then the splitting will go deeper too.
-}

-- See Note [Thunk splitting]
-- splitThunk converts the *non-recursive* binding
--      x = e
-- into
--      x = let x = e
--          in case x of
--               I# y -> let x = I# y in x }
-- See comments above. Is it not beautifully short?
-- Moreover, it works just as well when there are
-- several binders, and if the binders are lifted
-- E.g.     x = e
--     -->  x = let x = e in
--              case x of (a,b) -> let x = (a,b)  in x

splitThunk :: DynFlags -> FamInstEnvs -> RecFlag -> Var -> Expr Var -> UniqSM [(Var, Expr Var)]
splitThunk dflags fam_envs is_rec fn_id rhs
  = do { (useful,_, wrap_fn, work_fn) <- mkWWstr dflags fam_envs [fn_id]
       ; let res = [ (fn_id, Let (NonRec fn_id rhs) (wrap_fn (work_fn (Var fn_id)))) ]
       ; if useful then ASSERT2( isNonRec is_rec, ppr fn_id ) -- The thunk must be non-recursive
                   return res
                   else return [(fn_id, rhs)] }
