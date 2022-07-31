{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section{SetLevels}

                ***************************
                        Overview
                ***************************

1. We attach binding levels to Core bindings, in preparation for floating
   outwards (@FloatOut@).

2. We also let-ify many expressions (notably case scrutinees), so they
   will have a fighting chance of being floated sensible.

3. We clone the binders of any floatable let-binding, so that when it is
   floated out it will be unique.  (This used to be done by the simplifier
   but the latter now only ensures that there's no shadowing; indeed, even
   that may not be true.)

   NOTE: this can't be done using the uniqAway idea, because the variable
         must be unique in the whole program, not just its current scope,
         because two variables in different scopes may float out to the
         same top level place

   NOTE: Very tiresomely, we must apply this substitution to
         the rules stored inside a variable too.

   We do *not* clone top-level bindings, because some of them must not change,
   but we *do* clone bindings that are heading for the top level

4. In the expression
        case x of wild { p -> ...wild... }
   we substitute x for wild in the RHS of the case alternatives:
        case x of wild { p -> ...x... }
   This means that a sub-expression involving x is not "trapped" inside the RHS.
   And it's not inconvenient because we already have a substitution.

  Note that this is EXACTLY BACKWARDS from the what the simplifier does.
  The simplifier tries to get rid of occurrences of x, in favour of wild,
  in the hope that there will only be one remaining occurrence of x, namely
  the scrutinee of the case, and we can inline it.
-}

{-# LANGUAGE CPP #-}
module Eta.SimplCore.SetLevels (
        setLevels,

        Level(..), tOP_LEVEL,
        LevelledBind, LevelledExpr, LevelledBndr,
        FloatSpec(..), floatSpecLevel,

        incMinorLvl, ltMajLvl, ltLvl, isTopLvl
    ) where

#include "HsVersions.h"

import Eta.Core.CoreSyn
import Eta.SimplCore.CoreMonad        ( FloatOutSwitches(..) )
import Eta.Core.CoreUtils        ( exprType, exprOkForSpeculation, exprIsBottom )
import Eta.Core.CoreArity        ( exprBotStrictness_maybe )
import Eta.Core.CoreFVs          -- all of it
import Eta.Types.Coercion         ( isCoVar )
import Eta.Core.CoreSubst        ( Subst, emptySubst, substBndrs, substRecBndrs,
                          extendIdSubst, extendSubstWithVar, cloneBndrs,
                          cloneRecIdBndrs, substTy, substCo, substDVarSet )
import Eta.Core.MkCore           ( sortQuantVars )
import Eta.BasicTypes.Id
import Eta.BasicTypes.IdInfo
import Eta.BasicTypes.Var
import Eta.BasicTypes.VarSet
import Eta.BasicTypes.VarEnv
import Eta.BasicTypes.Literal          ( litIsTrivial )
import Eta.BasicTypes.Demand           ( StrictSig )
import Eta.BasicTypes.Name             ( getOccName, mkSystemVarName )
import Eta.BasicTypes.OccName          ( occNameString )
import Eta.Types.Type             ( isUnLiftedType, Type, mkPiTypes )
import Eta.BasicTypes.BasicTypes       ( Arity, RecFlag(..) )
import Eta.BasicTypes.UniqSupply
import Eta.Utils.Util
import Eta.Utils.Outputable
import Eta.Utils.FastString
import Eta.Utils.UniqDFM ( nonDetFoldUDFM )
import Eta.Utils.FV
{-
************************************************************************
*                                                                      *
\subsection{Level numbers}
*                                                                      *
************************************************************************
-}

type LevelledExpr = TaggedExpr FloatSpec
type LevelledBind = TaggedBind FloatSpec
type LevelledBndr = TaggedBndr FloatSpec

data Level = Level Int  -- Major level: number of enclosing value lambdas
                   Int  -- Minor level: number of big-lambda and/or case
                        -- expressions between here and the nearest
                        -- enclosing value lambda

data FloatSpec
  = FloatMe Level       -- Float to just inside the binding
                        --    tagged with this level
  | StayPut Level       -- Stay where it is; binding is
                        --     tagged with tihs level

floatSpecLevel :: FloatSpec -> Level
floatSpecLevel (FloatMe l) = l
floatSpecLevel (StayPut l) = l

{-
The {\em level number} on a (type-)lambda-bound variable is the
nesting depth of the (type-)lambda which binds it.  The outermost lambda
has level 1, so (Level 0 0) means that the variable is bound outside any lambda.

On an expression, it's the maximum level number of its free
(type-)variables.  On a let(rec)-bound variable, it's the level of its
RHS.  On a case-bound variable, it's the number of enclosing lambdas.

Top-level variables: level~0.  Those bound on the RHS of a top-level
definition but ``before'' a lambda; e.g., the \tr{x} in (levels shown
as ``subscripts'')...
\begin{verbatim}
a_0 = let  b_? = ...  in
           x_1 = ... b ... in ...
\end{verbatim}

The main function @lvlExpr@ carries a ``context level'' (@ctxt_lvl@).
That's meant to be the level number of the enclosing binder in the
final (floated) program.  If the level number of a sub-expression is
less than that of the context, then it might be worth let-binding the
sub-expression so that it will indeed float.

If you can float to level @Level 0 0@ worth doing so because then your
allocation becomes static instead of dynamic.  We always start with
context @Level 0 0@.


Note [FloatOut inside INLINE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@InlineCtxt@ very similar to @Level 0 0@, but is used for one purpose:
to say "don't float anything out of here".  That's exactly what we
want for the body of an INLINE, where we don't want to float anything
out at all.  See notes with lvlMFE below.

But, check this out:

-- At one time I tried the effect of not float anything out of an InlineMe,
-- but it sometimes works badly.  For example, consider PrelArr.done.  It
-- has the form         __inline (\d. e)
-- where e doesn't mention d.  If we float this to
--      __inline (let x = e in \d. x)
-- things are bad.  The inliner doesn't even inline it because it doesn't look
-- like a head-normal form.  So it seems a lesser evil to let things float.
-- In SetLevels we do set the context to (Level 0 0) when we get to an InlineMe
-- which discourages floating out.

So the conclusion is: don't do any floating at all inside an InlineMe.
(In the above example, don't float the {x=e} out of the \d.)

One particular case is that of workers: we don't want to float the
call to the worker outside the wrapper, otherwise the worker might get
inlined into the floated expression, and an importing module won't see
the worker at all.
-}

instance Outputable FloatSpec where
  ppr (FloatMe l) = char 'F' <> ppr l
  ppr (StayPut l) = ppr l

tOP_LEVEL :: Level
tOP_LEVEL   = Level 0 0

incMajorLvl :: Level -> Level
incMajorLvl (Level major _) = Level (major + 1) 0

incMinorLvl :: Level -> Level
incMinorLvl (Level major minor) = Level major (minor+1)

maxLvl :: Level -> Level -> Level
maxLvl l1@(Level maj1 min1) l2@(Level maj2 min2)
  | (maj1 > maj2) || (maj1 == maj2 && min1 > min2) = l1
  | otherwise                                      = l2

ltLvl :: Level -> Level -> Bool
ltLvl (Level maj1 min1) (Level maj2 min2)
  = (maj1 < maj2) || (maj1 == maj2 && min1 < min2)

ltMajLvl :: Level -> Level -> Bool
    -- Tells if one level belongs to a difft *lambda* level to another
ltMajLvl (Level maj1 _) (Level maj2 _) = maj1 < maj2

isTopLvl :: Level -> Bool
isTopLvl (Level 0 0) = True
isTopLvl _           = False

instance Outputable Level where
  ppr (Level maj min) = hcat [ char '<', int maj, char ',', int min, char '>' ]

instance Eq Level where
  (Level maj1 min1) == (Level maj2 min2) = maj1 == maj2 && min1 == min2

{-
************************************************************************
*                                                                      *
\subsection{Main level-setting code}
*                                                                      *
************************************************************************
-}

setLevels :: FloatOutSwitches
          -> CoreProgram
          -> UniqSupply
          -> [LevelledBind]

setLevels float_lams binds us
  = initLvl us (do_them init_env binds)
  where
    init_env = initialEnv float_lams

    do_them :: LevelEnv -> [CoreBind] -> LvlM [LevelledBind]
    do_them _ [] = return []
    do_them env (b:bs)
      = do { (lvld_bind, env') <- lvlTopBind env b
           ; lvld_binds <- do_them env' bs
           ; return (lvld_bind : lvld_binds) }

lvlTopBind :: LevelEnv -> Bind Id -> LvlM (LevelledBind, LevelEnv)
lvlTopBind env (NonRec bndr rhs)
  = do { rhs' <- lvlExpr env (freeVars rhs)
       ; let (env', [bndr']) = substAndLvlBndrs NonRecursive env tOP_LEVEL [bndr]
       ; return (NonRec bndr' rhs', env') }

lvlTopBind env (Rec pairs)
  = do let (bndrs,rhss) = unzip pairs
           (env', bndrs') = substAndLvlBndrs Recursive env tOP_LEVEL bndrs
       rhss' <- mapM (lvlExpr env' . freeVars) rhss
       return (Rec (bndrs' `zip` rhss'), env')

{-
************************************************************************
*                                                                      *
\subsection{Setting expression levels}
*                                                                      *
************************************************************************

Note [Floating over-saturated applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we see (f x y), and (f x) is a redex (ie f's arity is 1),
we call (f x) an "over-saturated application"

Should we float out an over-sat app, if can escape a value lambda?
It is sometimes very beneficial (-7% runtime -4% alloc over nofib -O2).
But we don't want to do it for class selectors, because the work saved
is minimal, and the extra local thunks allocated cost money.

Arguably we could float even class-op applications if they were going to
top level -- but then they must be applied to a constant dictionary and
will almost certainly be optimised away anyway.
-}

lvlExpr :: LevelEnv             -- Context
        -> CoreExprWithFVs      -- Input expression
        -> LvlM LevelledExpr    -- Result expression

{-
The @ctxt_lvl@ is, roughly, the level of the innermost enclosing
binder.  Here's an example

        v = \x -> ...\y -> let r = case (..x..) of
                                        ..x..
                           in ..

When looking at the rhs of @r@, @ctxt_lvl@ will be 1 because that's
the level of @r@, even though it's inside a level-2 @\y@.  It's
important that @ctxt_lvl@ is 1 and not 2 in @r@'s rhs, because we
don't want @lvlExpr@ to turn the scrutinee of the @case@ into an MFE
--- because it isn't a *maximal* free expression.

If there were another lambda in @r@'s rhs, it would get level-2 as well.
-}

lvlExpr env (_, AnnType ty)     = return (Type (substTy (le_subst env) ty))
lvlExpr env (_, AnnCoercion co) = return (Coercion (substCo (le_subst env) co))
lvlExpr env (_, AnnVar v)       = return (lookupVar env v)
lvlExpr _   (_, AnnLit lit)     = return (Lit lit)

lvlExpr env (_, AnnCast expr (_, co)) = do
    expr' <- lvlExpr env expr
    return (Cast expr' (substCo (le_subst env) co))

lvlExpr env (_, AnnTick tickish expr) = do
    expr' <- lvlExpr env expr
    return (Tick tickish expr')

lvlExpr env expr@(_, AnnApp _ _) = do
    let
      (fun, args) = collectAnnArgs expr
    --
    case fun of
      (_, AnnVar f) | floatOverSat env   -- See Note [Floating over-saturated applications]
                    , arity > 0
                    , arity < n_val_args
                    , Nothing <- isClassOpId_maybe f ->
        do
         let (lapp, rargs) = left (n_val_args - arity) expr []
         rargs' <- mapM (lvlMFE False env) rargs
         lapp' <- lvlMFE False env lapp
         return (foldl App lapp' rargs')
        where
         n_val_args = count (isValArg . deAnnotate) args
         arity = idArity f

         -- separate out the PAP that we are floating from the extra
         -- arguments, by traversing the spine until we have collected
         -- (n_val_args - arity) value arguments.
         left 0 e               rargs = (e, rargs)
         left n (_, AnnApp f a) rargs
            | isValArg (deAnnotate a) = left (n-1) f (a:rargs)
            | otherwise               = left n     f (a:rargs)
         left _ _ _                   = panic "SetLevels.lvlExpr.left"

         -- No PAPs that we can float: just carry on with the
         -- arguments and the function.
      _otherwise -> do
         args' <- mapM (lvlMFE False env) args
         fun'  <- lvlExpr env fun
         return (foldl App fun' args')

-- We don't split adjacent lambdas.  That is, given
--      \x y -> (x+1,y)
-- we don't float to give
--      \x -> let v = x+1 in \y -> (v,y)
-- Why not?  Because partial applications are fairly rare, and splitting
-- lambdas makes them more expensive.

lvlExpr env expr@(_, AnnLam {})
  = do { new_body <- lvlMFE True new_env body
       ; return (mkLams new_bndrs new_body) }
  where
    (bndrs, body)        = collectAnnBndrs expr
    (env1, bndrs1)       = substBndrsSL NonRecursive env bndrs
    (new_env, new_bndrs) = lvlLamBndrs env1 (le_ctxt_lvl env) bndrs1
        -- At one time we called a special version of collectBinders,
        -- which ignored coercions, because we don't want to split
        -- a lambda like this (\x -> coerce t (\s -> ...))
        -- This used to happen quite a bit in state-transformer programs,
        -- but not nearly so much now non-recursive newtypes are transparent.
        -- [See SetLevels rev 1.50 for a version with this approach.]

lvlExpr env (_, AnnLet bind body)
  = do { (bind', new_env) <- lvlBind env bind
       ; body' <- lvlExpr new_env body
           -- No point in going via lvlMFE here.  If the binding is alive
           -- (mentioned in body), and the whole let-expression doesn't
           -- float, then neither will the body
       ; return (Let bind' body') }

lvlExpr env (_, AnnCase scrut@(scrut_fvs,_) case_bndr ty alts)
  = do { scrut' <- lvlMFE True env scrut
       ; lvlCase env scrut_fvs scrut' case_bndr ty alts }

-------------------------------------------
lvlCase :: LevelEnv             -- Level of in-scope names/tyvars
        -> DVarSet               -- Free vars of input scrutinee
        -> LevelledExpr         -- Processed scrutinee
        -> Id -> Type           -- Case binder and result type
        -> [AnnAlt Id DVarSet]   -- Input alternatives
        -> LvlM LevelledExpr    -- Result expression
lvlCase env scrut_fvs scrut' case_bndr ty alts
  | [(con@(DataAlt {}), bs, body)] <- alts
  , exprOkForSpeculation scrut'   -- See Note [Check the output scrutinee for okForSpec]
  , not (isTopLvl dest_lvl)       -- Can't have top-level cases
  =     -- See Note [Floating cases]
        -- Always float the case if possible
        -- Unlike lets we don't insist that it escapes a value lambda
    do { (rhs_env, (case_bndr':bs')) <- cloneVars NonRecursive env dest_lvl (case_bndr:bs)
                   -- We don't need to use extendCaseBndrLvlEnv here
                   -- because we are floating the case outwards so
                   -- no need to do the binder-swap thing
       ; body' <- lvlMFE True rhs_env body
       ; let alt' = (con, [TB b (StayPut dest_lvl) | b <- bs'], body')
       ; return (Case scrut' (TB case_bndr' (FloatMe dest_lvl)) ty [alt']) }

  | otherwise     -- Stays put
  = do { let (alts_env1, [case_bndr']) = substAndLvlBndrs NonRecursive env incd_lvl [case_bndr]
             alts_env = extendCaseBndrEnv alts_env1 case_bndr scrut'
       ; alts' <- mapM (lvl_alt alts_env) alts
       ; return (Case scrut' case_bndr' ty alts') }
  where
      incd_lvl = incMinorLvl (le_ctxt_lvl env)
      dest_lvl = maxFvLevel (const True) env scrut_fvs
              -- Don't abstract over type variables, hence const True

      lvl_alt alts_env (con, bs, rhs)
        = do { rhs' <- lvlMFE True new_env rhs
             ; return (con, bs', rhs') }
        where
          (new_env, bs') = substAndLvlBndrs NonRecursive alts_env incd_lvl bs

{-
Note [Floating cases]
~~~~~~~~~~~~~~~~~~~~~
Consider this:
  data T a = MkT !a
  f :: T Int -> blah
  f x vs = case x of { MkT y ->
             let f vs = ...(case y of I# w -> e)...f..
             in f vs
Here we can float the (case y ...) out , because y is sure
to be evaluated, to give
  f x vs = case x of { MkT y ->
           caes y of I# w ->
             let f vs = ...(e)...f..
             in f vs

That saves unboxing it every time round the loop.  It's important in
some DPH stuff where we really want to avoid that repeated unboxing in
the inner loop.

Things to note
 * We can't float a case to top level
 * It's worth doing this float even if we don't float
   the case outside a value lambda.  Example
     case x of {
       MkT y -> (case y of I# w2 -> ..., case y of I# w2 -> ...)
   If we floated the cases out we could eliminate one of them.
 * We only do this with a single-alternative case

Note [Check the output scrutinee for okForSpec]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
  case x of y {
    A -> ....(case y of alts)....
  }
Because of the binder-swap, the inner case will get substituted to
(case x of ..).  So when testing whether the scrutinee is
okForSpecuation we must be careful to test the *result* scrutinee ('x'
in this case), not the *input* one 'y'.  The latter *is* ok for
speculation here, but the former is not -- and indeed we can't float
the inner case out, at least not unless x is also evaluated at its
binding site.

That's why we apply exprOkForSpeculation to scrut' and not to scrut.
-}

lvlMFE ::  Bool                 -- True <=> strict context [body of case or let]
        -> LevelEnv             -- Level of in-scope names/tyvars
        -> CoreExprWithFVs      -- input expression
        -> LvlM LevelledExpr    -- Result expression
-- lvlMFE is just like lvlExpr, except that it might let-bind
-- the expression, so that it can itself be floated.

lvlMFE _ env (_, AnnType ty)
  = return (Type (substTy (le_subst env) ty))

-- No point in floating out an expression wrapped in a coercion or note
-- If we do we'll transform  lvl = e |> co
--                       to  lvl' = e; lvl = lvl' |> co
-- and then inline lvl.  Better just to float out the payload.
lvlMFE strict_ctxt env (_, AnnTick t e)
  = do { e' <- lvlMFE strict_ctxt env e
       ; return (Tick t e') }

lvlMFE strict_ctxt env (_, AnnCast e (_, co))
  = do  { e' <- lvlMFE strict_ctxt env e
        ; return (Cast e' (substCo (le_subst env) co)) }

-- Note [Case MFEs]
lvlMFE True env e@(_, AnnCase {})
  = lvlExpr env e     -- Don't share cases

lvlMFE strict_ctxt env ann_expr@(fvs, _)
  |  isUnLiftedType (exprType expr)
         -- Can't let-bind it; see Note [Unlifted MFEs]
         -- This includes coercions, which we don't want to float anyway
         -- NB: no need to substitute cos isUnLiftedType doesn't change
  || notWorthFloating ann_expr abs_vars
  || not float_me
  =     -- Don't float it out
    lvlExpr env ann_expr

  | otherwise   -- Float it out!
  = do { expr' <- lvlFloatRhs abs_vars dest_lvl env ann_expr
       ; var   <- newLvlVar expr' is_bot
       ; return (Let (NonRec (TB var (FloatMe dest_lvl)) expr')
                     (mkVarApps (Var var) abs_vars)) }
  where
    expr     = deAnnotate ann_expr
    is_bot   = exprIsBottom expr      -- Note [Bottoming floats]
    dest_lvl = destLevel env fvs (isFunction ann_expr) is_bot
    abs_vars = abstractVars dest_lvl env fvs

        -- A decision to float entails let-binding this thing, and we only do
        -- that if we'll escape a value lambda, or will go to the top level.
    float_me = dest_lvl `ltMajLvl` (le_ctxt_lvl env)    -- Escapes a value lambda
                -- OLD CODE: not (exprIsCheap expr) || isTopLvl dest_lvl
                --           see Note [Escaping a value lambda]

            || (isTopLvl dest_lvl       -- Only float if we are going to the top level
                && floatConsts env      --   and the floatConsts flag is on
                && not strict_ctxt)     -- Don't float from a strict context
          -- We are keen to float something to the top level, even if it does not
          -- escape a lambda, because then it needs no allocation.  But it's controlled
          -- by a flag, because doing this too early loses opportunities for RULES
          -- which (needless to say) are important in some nofib programs
          -- (gcd is an example).
          --
          -- Beware:
          --    concat = /\ a -> foldr ..a.. (++) []
          -- was getting turned into
          --    lvl    = /\ a -> foldr ..a.. (++) []
          --    concat = /\ a -> lvl a
          -- which is pretty stupid.  Hence the strict_ctxt test
          --
          -- Also a strict contxt includes unboxed values, and they
          -- can't be bound at top level

{-
Note [Unlifted MFEs]
~~~~~~~~~~~~~~~~~~~~
We don't float unlifted MFEs, which potentially loses big opportunities.
For example:
        \x -> f (h y)
where h :: Int -> Int# is expensive. We'd like to float the (h y) outside
the \x, but we don't because it's unboxed.  Possible solution: box it.

Note [Bottoming floats]
~~~~~~~~~~~~~~~~~~~~~~~
If we see
        f = \x. g (error "urk")
we'd like to float the call to error, to get
        lvl = error "urk"
        f = \x. g lvl
Furthermore, we want to float a bottoming expression even if it has free
variables:
        f = \x. g (let v = h x in error ("urk" ++ v))
Then we'd like to abstract over 'x' can float the whole arg of g:
        lvl = \x. let v = h x in error ("urk" ++ v)
        f = \x. g (lvl x)
See Maessen's paper 1999 "Bottom extraction: factoring error handling out
of functional programs" (unpublished I think).

When we do this, we set the strictness and arity of the new bottoming
Id, *immediately*, for two reasons:

  * To prevent the abstracted thing being immediately inlined back in again
    via preInlineUnconditionally.  The latter has a test for bottoming Ids
    to stop inlining them, so we'd better make sure it *is* a bottoming Id!

  * So that it's properly exposed as such in the interface file, even if
    this is all happening after strictness analysis.

Note [Bottoming floats: eta expansion] c.f Note [Bottoming floats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tiresomely, though, the simplifier has an invariant that the manifest
arity of the RHS should be the same as the arity; but we can't call
etaExpand during SetLevels because it works over a decorated form of
CoreExpr.  So we do the eta expansion later, in FloatOut.

Note [Case MFEs]
~~~~~~~~~~~~~~~~
We don't float a case expression as an MFE from a strict context.  Why not?
Because in doing so we share a tiny bit of computation (the switch) but
in exchange we build a thunk, which is bad.  This case reduces allocation
by 7% in spectral/puzzle (a rather strange benchmark) and 1.2% in real/fem.
Doesn't change any other allocation at all.
-}

annotateBotStr :: Id -> Maybe (Arity, StrictSig) -> Id
-- See Note [Bottoming floats] for why we want to add
-- bottoming information right now
annotateBotStr id Nothing            = id
annotateBotStr id (Just (arity, sig)) = id `setIdArity` arity
                                           `setIdStrictness` sig

notWorthFloating :: CoreExprWithFVs -> [Var] -> Bool
-- Returns True if the expression would be replaced by
-- something bigger than it is now.  For example:
--   abs_vars = tvars only:  return True if e is trivial,
--                           but False for anything bigger
--   abs_vars = [x] (an Id): return True for trivial, or an application (f x)
--                           but False for (f x x)
--
-- One big goal is that floating should be idempotent.  Eg if
-- we replace e with (lvl79 x y) and then run FloatOut again, don't want
-- to replace (lvl79 x y) with (lvl83 x y)!

notWorthFloating e abs_vars
  = go e (count isId abs_vars)
  where
    go (_, AnnVar {}) n    = n >= 0
    go (_, AnnLit lit) n   = ASSERT( n==0 )
                             litIsTrivial lit   -- Note [Floating literals]
    go (_, AnnTick t e) n  = not (tickishIsCode t) && go e n
    go (_, AnnCast e _)  n = go e n
    go (_, AnnApp e arg) n
       | (_, AnnType {}) <- arg = go e n
       | (_, AnnCoercion {}) <- arg = go e n
       | n==0                   = False
       | is_triv arg            = go e (n-1)
       | otherwise              = False
    go _ _                      = False

    is_triv (_, AnnLit {})                = True        -- Treat all literals as trivial
    is_triv (_, AnnVar {})                = True        -- (ie not worth floating)
    is_triv (_, AnnCast e _)              = is_triv e
    is_triv (_, AnnApp e (_, AnnType {})) = is_triv e
    is_triv (_, AnnApp e (_, AnnCoercion {})) = is_triv e
    is_triv (_, AnnTick t e)              = not (tickishIsCode t) && is_triv e
    is_triv _                             = False

{-
Note [Floating literals]
~~~~~~~~~~~~~~~~~~~~~~~~
It's important to float Integer literals, so that they get shared,
rather than being allocated every time round the loop.
Hence the litIsTrivial.

We'd *like* to share MachStr literal strings too, mainly so we could
CSE them, but alas can't do so directly because they are unlifted.


Note [Escaping a value lambda]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to float even cheap expressions out of value lambdas,
because that saves allocation.  Consider
        f = \x.  .. (\y.e) ...
Then we'd like to avoid allocating the (\y.e) every time we call f,
(assuming e does not mention x).

An example where this really makes a difference is simplrun009.

Another reason it's good is because it makes SpecContr fire on functions.
Consider
        f = \x. ....(f (\y.e))....
After floating we get
        lvl = \y.e
        f = \x. ....(f lvl)...
and that is much easier for SpecConstr to generate a robust specialisation for.

The OLD CODE (given where this Note is referred to) prevents floating
of the example above, so I just don't understand the old code.  I
don't understand the old comment either (which appears below).  I
measured the effect on nofib of changing OLD CODE to 'True', and got
zeros everywhere, but a 4% win for 'puzzle'.  Very small 0.5% loss for
'cse'; turns out to be because our arity analysis isn't good enough
yet (mentioned in Simon-nofib-notes).

OLD comment was:
         Even if it escapes a value lambda, we only
         float if it's not cheap (unless it'll get all the
         way to the top).  I've seen cases where we
         float dozens of tiny free expressions, which cost
         more to allocate than to evaluate.
         NB: exprIsCheap is also true of bottom expressions, which
             is good; we don't want to share them

        It's only Really Bad to float a cheap expression out of a
        strict context, because that builds a thunk that otherwise
        would never be built.  So another alternative would be to
        add
                || (strict_ctxt && not (exprIsBottom expr))
        to the condition above. We should really try this out.


************************************************************************
*                                                                      *
\subsection{Bindings}
*                                                                      *
************************************************************************

The binding stuff works for top level too.
-}

lvlBind :: LevelEnv
        -> CoreBindWithFVs
        -> LvlM (LevelledBind, LevelEnv)

lvlBind env (AnnNonRec bndr rhs@(rhs_fvs,_))
  | isTyVar bndr    -- Don't do anything for TyVar binders
                    --   (simplifier gets rid of them pronto)
  || isCoVar bndr   -- Difficult to fix up CoVar occurrences (see extendPolyLvlEnv)
                    -- so we will ignore this case for now
  || not (profitableFloat env dest_lvl)
  || (isTopLvl dest_lvl && isUnLiftedType (idType bndr))
          -- We can't float an unlifted binding to top level, so we don't
          -- float it at all.  It's a bit brutal, but unlifted bindings
          -- aren't expensive either
  = -- No float
    do { rhs' <- lvlExpr env rhs
       ; let  bind_lvl        = incMinorLvl (le_ctxt_lvl env)
              (env', [bndr']) = substAndLvlBndrs NonRecursive env bind_lvl [bndr]
       ; return (NonRec bndr' rhs', env') }

  -- Otherwise we are going to float
  | null abs_vars
  = do {  -- No type abstraction; clone existing binder
         rhs' <- lvlExpr (setCtxtLvl env dest_lvl) rhs
       ; (env', [bndr']) <- cloneVars NonRecursive env dest_lvl [bndr]
       ; return (NonRec (TB bndr' (FloatMe dest_lvl)) rhs', env') }

  | otherwise
  = do {  -- Yes, type abstraction; create a new binder, extend substitution, etc
         rhs' <- lvlFloatRhs abs_vars dest_lvl env rhs
       ; (env', [bndr']) <- newPolyBndrs dest_lvl env abs_vars [bndr]
       ; return (NonRec (TB bndr' (FloatMe dest_lvl)) rhs', env') }

  where
    bind_fvs   = rhs_fvs `unionDVarSet` fvDVarSet (idFVs bndr)
    abs_vars   = abstractVars dest_lvl env bind_fvs
    dest_lvl   = destLevel env bind_fvs (isFunction rhs) is_bot
    is_bot     = exprIsBottom (deAnnotate rhs)

lvlBind env (AnnRec pairs)
  | not (profitableFloat env dest_lvl)
  = do { let bind_lvl = incMinorLvl (le_ctxt_lvl env)
             (env', bndrs') = substAndLvlBndrs Recursive env bind_lvl bndrs
       ; rhss' <- mapM (lvlExpr env') rhss
       ; return (Rec (bndrs' `zip` rhss'), env') }

  | null abs_vars
  = do { (new_env, new_bndrs) <- cloneVars Recursive env dest_lvl bndrs
       ; new_rhss <- mapM (lvlExpr (setCtxtLvl new_env dest_lvl)) rhss
       ; return ( Rec ([TB b (FloatMe dest_lvl) | b <- new_bndrs] `zip` new_rhss)
                , new_env) }

-- ToDo: when enabling the floatLambda stuff,
--       I think we want to stop doing this
  | [(bndr,rhs)] <- pairs
  , count isId abs_vars > 1
  = do  -- Special case for self recursion where there are
        -- several variables carried around: build a local loop:
        --      poly_f = \abs_vars. \lam_vars . letrec f = \lam_vars. rhs in f lam_vars
        -- This just makes the closures a bit smaller.  If we don't do
        -- this, allocation rises significantly on some programs
        --
        -- We could elaborate it for the case where there are several
        -- mutually functions, but it's quite a bit more complicated
        --
        -- This all seems a bit ad hoc -- sigh
    let (rhs_env, abs_vars_w_lvls) = lvlLamBndrs env dest_lvl abs_vars
        rhs_lvl = le_ctxt_lvl rhs_env

    (rhs_env', [new_bndr]) <- cloneVars Recursive rhs_env rhs_lvl [bndr]
    let
        (lam_bndrs, rhs_body)   = collectAnnBndrs rhs
        (body_env1, lam_bndrs1) = substBndrsSL NonRecursive rhs_env' lam_bndrs
        (body_env2, lam_bndrs2) = lvlLamBndrs body_env1 rhs_lvl lam_bndrs1
    new_rhs_body <- lvlExpr body_env2 rhs_body
    (poly_env, [poly_bndr]) <- newPolyBndrs dest_lvl env abs_vars [bndr]
    return (Rec [(TB poly_bndr (FloatMe dest_lvl)
                 , mkLams abs_vars_w_lvls $
                   mkLams lam_bndrs2 $
                   Let (Rec [( TB new_bndr (StayPut rhs_lvl)
                             , mkLams lam_bndrs2 new_rhs_body)])
                       (mkVarApps (Var new_bndr) lam_bndrs1))]
           , poly_env)

  | otherwise  -- Non-null abs_vars
  = do { (new_env, new_bndrs) <- newPolyBndrs dest_lvl env abs_vars bndrs
       ; new_rhss <- mapM (lvlFloatRhs abs_vars dest_lvl new_env) rhss
       ; return ( Rec ([TB b (FloatMe dest_lvl) | b <- new_bndrs] `zip` new_rhss)
                , new_env) }

  where
    (bndrs,rhss) = unzip pairs

        -- Finding the free vars of the binding group is annoying
    bind_fvs = ((unionDVarSets [ freeVarsOf rhs | (_, rhs) <- pairs])
                `unionDVarSet`
                (fvDVarSet $ unionsFV [ idFVs bndr
                                      | (bndr, (_,_)) <- pairs]))
               `delDVarSetList`
                bndrs

    dest_lvl = destLevel env bind_fvs (all isFunction rhss) False
    abs_vars = abstractVars dest_lvl env bind_fvs

profitableFloat :: LevelEnv -> Level -> Bool
profitableFloat env dest_lvl
  =  (dest_lvl `ltMajLvl` le_ctxt_lvl env)  -- Escapes a value lambda
  || isTopLvl dest_lvl                      -- Going all the way to top level

----------------------------------------------------
-- Three help functions for the type-abstraction case

lvlFloatRhs :: [OutVar] -> Level -> LevelEnv -> CoreExprWithFVs
            -> UniqSM (Expr LevelledBndr)
lvlFloatRhs abs_vars dest_lvl env rhs
  = do { rhs' <- lvlExpr rhs_env rhs
       ; return (mkLams abs_vars_w_lvls rhs') }
  where
    (rhs_env, abs_vars_w_lvls) = lvlLamBndrs env dest_lvl abs_vars

{-
************************************************************************
*                                                                      *
\subsection{Deciding floatability}
*                                                                      *
************************************************************************
-}

substAndLvlBndrs :: RecFlag -> LevelEnv -> Level -> [InVar] -> (LevelEnv, [LevelledBndr])
substAndLvlBndrs is_rec env lvl bndrs
  = lvlBndrs subst_env lvl subst_bndrs
  where
    (subst_env, subst_bndrs) = substBndrsSL is_rec env bndrs

substBndrsSL :: RecFlag -> LevelEnv -> [InVar] -> (LevelEnv, [OutVar])
-- So named only to avoid the name clash with CoreSubst.substBndrs
substBndrsSL is_rec env@(LE { le_subst = subst, le_env = id_env }) bndrs
  = ( env { le_subst    = subst'
          , le_env      = foldl add_id  id_env (bndrs `zip` bndrs') }
    , bndrs')
  where
    (subst', bndrs') = case is_rec of
                         NonRecursive -> substBndrs    subst bndrs
                         Recursive    -> substRecBndrs subst bndrs

lvlLamBndrs :: LevelEnv -> Level -> [OutVar] -> (LevelEnv, [LevelledBndr])
-- Compute the levels for the binders of a lambda group
lvlLamBndrs env lvl bndrs
  = lvlBndrs env new_lvl bndrs
  where
    new_lvl | any is_major bndrs = incMajorLvl lvl
            | otherwise          = incMinorLvl lvl

    is_major bndr = isId bndr && not (isProbablyOneShotLambda bndr)
       -- The "probably" part says "don't float things out of a
       -- probable one-shot lambda"
       -- See Note [Computing one-shot info] in Demand.lhs


lvlBndrs :: LevelEnv -> Level -> [CoreBndr] -> (LevelEnv, [LevelledBndr])
-- The binders returned are exactly the same as the ones passed,
-- apart from applying the substitution, but they are now paired
-- with a (StayPut level)
--
-- The returned envt has ctxt_lvl updated to the new_lvl
--
-- All the new binders get the same level, because
-- any floating binding is either going to float past
-- all or none.  We never separate binders.
lvlBndrs env@(LE { le_lvl_env = lvl_env }) new_lvl bndrs
  = ( env { le_ctxt_lvl = new_lvl
          , le_lvl_env  = foldl add_lvl lvl_env bndrs }
    , lvld_bndrs)
  where
    lvld_bndrs    = [TB bndr (StayPut new_lvl) | bndr <- bndrs]
    add_lvl env v = extendVarEnv env v new_lvl

  -- Destination level is the max Id level of the expression
  -- (We'll abstract the type variables, if any.)
destLevel :: LevelEnv -> DVarSet
          -> Bool   -- True <=> is function
          -> Bool   -- True <=> is bottom
          -> Level
destLevel env fvs is_function is_bot
  | is_bot = tOP_LEVEL  -- Send bottoming bindings to the top
                        -- regardless; see Note [Bottoming floats]
  | Just n_args <- floatLams env
  , n_args > 0  -- n=0 case handled uniformly by the 'otherwise' case
  , is_function
  , countFreeIds fvs <= n_args
  = tOP_LEVEL   -- Send functions to top level; see
                -- the comments with isFunction

  | otherwise = maxFvLevel isId env fvs  -- Max over Ids only; the tyvars
                                         -- will be abstracted

isFunction :: CoreExprWithFVs -> Bool
-- The idea here is that we want to float *functions* to
-- the top level.  This saves no work, but
--      (a) it can make the host function body a lot smaller,
--              and hence inlinable.
--      (b) it can also save allocation when the function is recursive:
--          h = \x -> letrec f = \y -> ...f...y...x...
--                    in f x
--     becomes
--          f = \x y -> ...(f x)...y...x...
--          h = \x -> f x x
--     No allocation for f now.
-- We may only want to do this if there are sufficiently few free
-- variables.  We certainly only want to do it for values, and not for
-- constructors.  So the simple thing is just to look for lambdas
isFunction (_, AnnLam b e) | isId b    = True
                           | otherwise = isFunction e
-- isFunction (_, AnnTick _ e)          = isFunction e  -- dubious
isFunction _                           = False

countFreeIds :: DVarSet -> Int
countFreeIds = nonDetFoldUDFM add 0
 -- It's OK to use nonDetFoldUDFM here because we're just counting things.
  where
    add :: Var -> Int -> Int
    add v n | isId v    = n+1
            | otherwise = n

{-
************************************************************************
*                                                                      *
\subsection{Free-To-Level Monad}
*                                                                      *
************************************************************************
-}

type InVar  = Var   -- Pre  cloning
type InId   = Id    -- Pre  cloning
type OutVar = Var   -- Post cloning
type OutId  = Id    -- Post cloning

data LevelEnv
  = LE { le_switches :: FloatOutSwitches
       , le_ctxt_lvl :: Level           -- The current level
       , le_lvl_env  :: VarEnv Level    -- Domain is *post-cloned* TyVars and Ids
       , le_subst    :: Subst           -- Domain is pre-cloned TyVars and Ids
                                        -- The Id -> CoreExpr in the Subst is ignored
                                        -- (since we want to substitute a LevelledExpr for
                                        -- an Id via le_env) but we do use the Co/TyVar substs
       , le_env      :: IdEnv ([OutVar], LevelledExpr)  -- Domain is pre-cloned Ids
    }
        -- We clone let- and case-bound variables so that they are still
        -- distinct when floated out; hence the le_subst/le_env.
        -- (see point 3 of the module overview comment).
        -- We also use these envs when making a variable polymorphic
        -- because we want to float it out past a big lambda.
        --
        -- The le_subst and le_env always implement the same mapping, but the
        -- le_subst maps to CoreExpr and the le_env to LevelledExpr
        -- Since the range is always a variable or type application,
        -- there is never any difference between the two, but sadly
        -- the types differ.  The le_subst is used when substituting in
        -- a variable's IdInfo; the le_env when we find a Var.
        --
        -- In addition the le_env records a list of tyvars free in the
        -- type application, just so we don't have to call freeVars on
        -- the type application repeatedly.
        --
        -- The domain of the both envs is *pre-cloned* Ids, though
        --
        -- The domain of the le_lvl_env is the *post-cloned* Ids

initialEnv :: FloatOutSwitches -> LevelEnv
initialEnv float_lams
  = LE { le_switches = float_lams
       , le_ctxt_lvl = tOP_LEVEL
       , le_lvl_env = emptyVarEnv
       , le_subst = emptySubst
       , le_env = emptyVarEnv }

floatLams :: LevelEnv -> Maybe Int
floatLams le = floatOutLambdas (le_switches le)

floatConsts :: LevelEnv -> Bool
floatConsts le = floatOutConstants (le_switches le)

floatOverSat :: LevelEnv -> Bool
floatOverSat le = floatOutOverSatApps (le_switches le)

setCtxtLvl :: LevelEnv -> Level -> LevelEnv
setCtxtLvl env lvl = env { le_ctxt_lvl = lvl }

-- extendCaseBndrLvlEnv adds the mapping case-bndr->scrut-var if it can
-- (see point 4 of the module overview comment)
extendCaseBndrEnv :: LevelEnv
                  -> Id                 -- Pre-cloned case binder
                  -> Expr LevelledBndr  -- Post-cloned scrutinee
                  -> LevelEnv
extendCaseBndrEnv le@(LE { le_subst = subst, le_env = id_env })
                  case_bndr (Var scrut_var)
  = le { le_subst   = extendSubstWithVar subst case_bndr scrut_var
       , le_env     = add_id id_env (case_bndr, scrut_var) }
extendCaseBndrEnv env _ _ = env

maxFvLevel :: (Var -> Bool) -> LevelEnv -> DVarSet -> Level
maxFvLevel max_me (LE { le_lvl_env = lvl_env, le_env = id_env }) var_set
  = foldDVarSet max_in tOP_LEVEL var_set
  where
    max_in in_var lvl
       = foldr max_out lvl (case lookupVarEnv id_env in_var of
                                Just (abs_vars, _) -> abs_vars
                                Nothing            -> [in_var])

    max_out out_var lvl
        | max_me out_var = case lookupVarEnv lvl_env out_var of
                                Just lvl' -> maxLvl lvl' lvl
                                Nothing   -> lvl
        | otherwise = lvl       -- Ignore some vars depending on max_me

lookupVar :: LevelEnv -> Id -> LevelledExpr
lookupVar le v = case lookupVarEnv (le_env le) v of
                    Just (_, expr) -> expr
                    _              -> Var v

abstractVars :: Level -> LevelEnv -> DVarSet -> [OutVar]
        -- Find the variables in fvs, free vars of the target expression,
        -- whose level is greater than the destination level
        -- These are the ones we are going to abstract out
        --
        -- Note that to get reproducible builds, the variables need to be
        -- abstracted in deterministic order, not dependent on the values of
        -- Uniques. This is achieved by using DVarSets, deterministic free
        -- variable computation and deterministic sort.
        -- See Note [Unique Determinism] in Unique for explanation of why
        -- Uniques are not deterministic.
abstractVars dest_lvl (LE { le_subst = subst, le_lvl_env = lvl_env }) in_fvs
  = map zap $ sortQuantVars $ uniq
    [out_var | out_fv  <- dVarSetElems (substDVarSet subst in_fvs)
             , out_var <- dVarSetElems (close out_fv)
             , abstract_me out_var ]
        -- NB: it's important to call abstract_me only on the OutIds the
        -- come from substDVarSet (not on fv, which is an InId)
  where
    uniq :: [Var] -> [Var]
    -- Remove duplicates, preserving order
    uniq = dVarSetElems . mkDVarSet

    abstract_me v = case lookupVarEnv lvl_env v of
                        Just lvl -> dest_lvl `ltLvl` lvl
                        Nothing  -> False

        -- We are going to lambda-abstract, so nuke any IdInfo,
        -- and add the tyvars of the Id (if necessary)
    zap v | isId v = WARN( isStableUnfolding (idUnfolding v) ||
                           not (isEmptyRuleInfo (idSpecialisation v)),
                           text "absVarsOf: discarding info on" <+> ppr v )
                     setIdInfo v vanillaIdInfo
          | otherwise = v

    close :: Var -> DVarSet  -- Close over variables free in the type
                            -- Result includes the input variable itself
    close v = foldDVarSet (unionDVarSet . close)
                         (unitDVarSet v)
                         (fvDVarSet $ varTypeTyFVs v)

type LvlM result = UniqSM result

initLvl :: UniqSupply -> UniqSM a -> a
initLvl = initUs_

newPolyBndrs :: Level -> LevelEnv -> [OutVar] -> [InId] -> UniqSM (LevelEnv, [OutId])
-- The envt is extended to bind the new bndrs to dest_lvl, but
-- the ctxt_lvl is unaffected
newPolyBndrs dest_lvl
             env@(LE { le_lvl_env = lvl_env, le_subst = subst, le_env = id_env })
             abs_vars bndrs
 = ASSERT( all (not . isCoVar) bndrs )   -- What would we add to the CoSubst in this case. No easy answer.
   do { uniqs <- getUniquesM
      ; let new_bndrs = zipWith mk_poly_bndr bndrs uniqs
            bndr_prs  = bndrs `zip` new_bndrs
            env' = env { le_lvl_env = foldl add_lvl   lvl_env new_bndrs
                       , le_subst   = foldl add_subst subst   bndr_prs
                       , le_env     = foldl add_id    id_env  bndr_prs }
      ; return (env', new_bndrs) }
  where
    add_lvl   env v' = extendVarEnv env v' dest_lvl
    add_subst env (v, v') = extendIdSubst env v (mkVarApps (Var v') abs_vars)
    add_id    env (v, v') = extendVarEnv env v ((v':abs_vars), mkVarApps (Var v') abs_vars)

    mk_poly_bndr bndr uniq = transferPolyIdInfo bndr abs_vars $         -- Note [transferPolyIdInfo] in Id.lhs
                             mkSysLocal (mkFastString str) uniq poly_ty
                           where
                             str     = "poly_" ++ occNameString (getOccName bndr)
                             poly_ty = mkPiTypes abs_vars (substTy subst (idType bndr))

newLvlVar :: LevelledExpr        -- The RHS of the new binding
          -> Bool                -- Whether it is bottom
          -> LvlM Id
newLvlVar lvld_rhs is_bot
  = do { uniq <- getUniqueM
       ; return (add_bot_info (mkLocalId (mk_name uniq) rhs_ty)) }
  where
    add_bot_info var  -- We could call annotateBotStr always, but the is_bot
                      -- flag just tells us when we don't need to do so
       | is_bot    = annotateBotStr var (exprBotStrictness_maybe de_tagged_rhs)
       | otherwise = var
    de_tagged_rhs = deTagExpr lvld_rhs
    rhs_ty = exprType de_tagged_rhs
    mk_name uniq = mkSystemVarName uniq (mkFastString "lvl")

cloneVars :: RecFlag -> LevelEnv -> Level -> [Var] -> LvlM (LevelEnv, [Var])
-- Works for Ids, TyVars and CoVars
-- The dest_lvl is attributed to the binders in the new env,
-- but cloneVars doesn't affect the ctxt_lvl of the incoming env
cloneVars is_rec
          env@(LE { le_subst = subst, le_lvl_env = lvl_env, le_env = id_env })
          dest_lvl vs
  = do { us <- getUniqueSupplyM
       ; let (subst', vs1) = case is_rec of
                               NonRecursive -> cloneBndrs      subst us vs
                               Recursive    -> cloneRecIdBndrs subst us vs
             vs2  = map zap_demand_info vs1  -- See Note [Zapping the demand info]
             prs  = vs `zip` vs2
             env' = env { le_lvl_env = foldl add_lvl lvl_env vs2
                        , le_subst   = subst'
                        , le_env     = foldl add_id id_env prs }

       ; return (env', vs2) }
  where
     add_lvl env v_cloned = extendVarEnv env v_cloned dest_lvl

add_id :: IdEnv ([Var], LevelledExpr) -> (Var, Var) -> IdEnv ([Var], LevelledExpr)
add_id id_env (v, v1)
  | isTyVar v = delVarEnv    id_env v
  | otherwise = extendVarEnv id_env v ([v1], ASSERT(not (isCoVar v1)) Var v1)

zap_demand_info :: Var -> Var
zap_demand_info v
  | isId v    = zapIdDemandInfo v
  | otherwise = v

{-
Note [Zapping the demand info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
VERY IMPORTANT: we must zap the demand info if the thing is going to
float out, because it may be less demanded than at its original
binding site.  Eg
   f :: Int -> Int
   f x = let v = 3*4 in v+x
Here v is strict; but if we float v to top level, it isn't any more.
-}
