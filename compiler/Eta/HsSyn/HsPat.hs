{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PatSyntax]{Abstract Haskell syntax---patterns}
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}

module Eta.HsSyn.HsPat (
        Pat(..), InPat, OutPat, LPat,

        HsConDetails(..),
        HsConPatDetails, hsConPatArgs,
        HsRecFields(..), HsRecField(..), LHsRecField, hsRecFields,  hsRecFieldsArgs,

        mkPrefixConPat, mkCharLitPat, mkNilPat,

        isUnliftedHsBind, looksLazyPatBind,
        isUnliftedLPat, isBangedLPat, isBangedPatBind,
        hsPatNeedsParens, collectEvVarsPats,
        isCompoundPat, parenthesizeCompoundPat,
        isIrrefutableHsPat,

        pprParendLPat, pprConArgs
    ) where

import {-# SOURCE #-} Eta.HsSyn.HsExpr (SyntaxExpr, LHsExpr, HsSplice, pprLExpr, pprSplice)

-- friends:
import Eta.HsSyn.HsBinds
import Eta.HsSyn.HsLit
import Eta.HsSyn.PlaceHolder ( PostTc,DataId )
import Eta.HsSyn.HsTypes
import Eta.TypeCheck.TcEvidence
import Eta.BasicTypes.BasicTypes
import Eta.BasicTypes.RdrName
import Eta.Utils.Bag
-- others:
import Eta.Core.PprCore          ( {- instance OutputableBndr TyVar -} )
import Eta.Prelude.TysWiredIn
import Eta.BasicTypes.Var
import Eta.BasicTypes.ConLike
import Eta.BasicTypes.DataCon
import Eta.Types.TyCon
import Eta.Utils.Outputable
import Eta.Types.Type
import Eta.BasicTypes.SrcLoc
import Eta.Utils.FastString
-- libraries:
import Data.Data hiding (TyCon,Fixity)
import Data.Maybe

type InPat id  = LPat id        -- No 'Out' constructors
type OutPat id = LPat id        -- No 'In' constructors

type LPat id = Located (Pat id)

-- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnBang'

-- For details on above see note [Api annotations] in ApiAnnotation
data Pat id
  =     ------------ Simple patterns ---------------
    WildPat     (PostTc id Type)        -- Wild card
        -- The sole reason for a type on a WildPat is to
        -- support hsPatType :: Pat Id -> Type

  | VarPat      id                      -- Variable
  | LazyPat     (LPat id)               -- Lazy pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnTilde'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | AsPat       (Located id) (LPat id)  -- As pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnAt'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | ParPat      (LPat id)               -- Parenthesised pattern
                                        -- See Note [Parens in HsSyn] in HsExpr
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
    --                                    'ApiAnnotation.AnnClose' @')'@

    -- For details on above see note [Api annotations] in ApiAnnotation
  | BangPat     (LPat id)               -- Bang pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnBang'

    -- For details on above see note [Api annotations] in ApiAnnotation

        ------------ Lists, tuples, arrays ---------------
  | ListPat     [LPat id]                            -- Syntactic list
                (PostTc id Type)                     -- The type of the elements
                (Maybe (PostTc id Type, SyntaxExpr id)) -- For rebindable syntax
                   -- For OverloadedLists a Just (ty,fn) gives
                   -- overall type of the pattern, and the toList
                   -- function to convert the scrutinee to a list value
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
    --                                    'ApiAnnotation.AnnClose' @']'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | TuplePat    [LPat id]        -- Tuple sub-patterns
                Boxity           -- UnitPat is TuplePat []
                [PostTc id Type] -- [] before typechecker, filled in afterwards
                                 -- with the types of the tuple components
        -- You might think that the PostTc id Type was redundant, because we can
        -- get the pattern type by getting the types of the sub-patterns.
        -- But it's essential
        --      data T a where
        --        T1 :: Int -> T Int
        --      f :: (T a, a) -> Int
        --      f (T1 x, z) = z
        -- When desugaring, we must generate
        --      f = /\a. \v::a.  case v of (t::T a, w::a) ->
        --                       case t of (T1 (x::Int)) ->
        -- Note the (w::a), NOT (w::Int), because we have not yet
        -- refined 'a' to Int.  So we must know that the second component
        -- of the tuple is of type 'a' not Int.  See selectMatchVar
        -- (June 14: I'm not sure this comment is right; the sub-patterns
        --           will be wrapped in CoPats, no?)
    -- ^ - 'ApiAnnotation.AnnKeywordId' :
    --            'ApiAnnotation.AnnOpen' @'('@ or @'(#'@,
    --            'ApiAnnotation.AnnClose' @')'@ or  @'#)'@

    -- For details on above see note [Api annotations] in ApiAnnotation
  | PArrPat     [LPat id]               -- Syntactic parallel array
                (PostTc id Type)        -- The type of the elements
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'[:'@,
    --                                    'ApiAnnotation.AnnClose' @':]'@

    -- For details on above see note [Api annotations] in ApiAnnotation
        ------------ Constructor patterns ---------------
  | ConPatIn    (Located id)
                (HsConPatDetails id)

  | ConPatOut {
        pat_con     :: Located ConLike,
        pat_arg_tys :: [Type],          -- The universal arg types, 1-1 with the universal
                                        -- tyvars of the constructor/pattern synonym
                                        --   Use (conLikeResTy pat_con pat_arg_tys) to get
                                        --   the type of the pattern

        pat_tvs   :: [TyVar],           -- Existentially bound type variables (tyvars only)
        pat_dicts :: [EvVar],           -- Ditto *coercion variables* and *dictionaries*
                                        -- One reason for putting coercion variable here, I think,
                                        --      is to ensure their kinds are zonked
        pat_binds :: TcEvBinds,         -- Bindings involving those dictionaries
        pat_args  :: HsConPatDetails id,
        pat_wrap  :: HsWrapper          -- Extra wrapper to pass to the matcher
    }

        ------------ View patterns ---------------
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ViewPat       (LHsExpr id)
                  (LPat id)
                  (PostTc id Type)  -- The overall type of the pattern
                                    -- (= the argument type of the view function)
                                    -- for hsPatType.

        ------------ Pattern splices ---------------
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'$('@
  --        'ApiAnnotation.AnnClose' @')'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | SplicePat       (HsSplice id)

        ------------ Literal and n+k patterns ---------------
  | LitPat          HsLit               -- Used for *non-overloaded* literal patterns:
                                        -- Int#, Char#, Int, Char, String, etc.

  | NPat                -- Used for all overloaded literals,
                        -- including overloaded strings with -XOverloadedStrings
                    (Located (HsOverLit id))    -- ALWAYS positive
                    (Maybe (SyntaxExpr id))     -- Just (Name of 'negate') for negative
                                                -- patterns, Nothing otherwise
                    (SyntaxExpr id)             -- Equality checker, of type t->t->Bool

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnVal' @'+'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | NPlusKPat       (Located id)        -- n+k pattern
                    (Located (HsOverLit id)) -- It'll always be an HsIntegral
                    (SyntaxExpr id)     -- (>=) function, of type t->t->Bool
                    (SyntaxExpr id)     -- Name of '-' (see RnEnv.lookupSyntaxName)

        ------------ Pattern type signatures ---------------
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | SigPatIn        (LPat id)                  -- Pattern with a type signature
                    (HsWithBndrs id (LHsType id)) -- Signature can bind both
                                                  -- kind and type vars

  | SigPatOut       (LPat id)           -- Pattern with a type signature
                    Type

        ------------ Pattern coercions (translation only) ---------------
  | CoPat       HsWrapper               -- If co :: t1 ~ t2, p :: t2,
                                        -- then (CoPat co p) :: t1
                (Pat id)                -- Why not LPat?  Ans: existing locn will do
                Type                    -- Type of whole pattern, t1
        -- During desugaring a (CoPat co pat) turns into a cast with 'co' on
        -- the scrutinee, followed by a match on 'pat'
  deriving (Typeable)
deriving instance (DataId id) => Data (Pat id)

-- HsConDetails is use for patterns/expressions *and* for data type declarations

data HsConDetails arg rec
  = PrefixCon [arg]             -- C p1 p2 p3
  | RecCon    rec               -- C { x = p1, y = p2 }
  | InfixCon  arg arg           -- p1 `C` p2
  deriving (Data, Typeable)

type HsConPatDetails id = HsConDetails (LPat id) (HsRecFields id (LPat id))

hsConPatArgs :: HsConPatDetails id -> [LPat id]
hsConPatArgs (PrefixCon ps)   = ps
hsConPatArgs (RecCon fs)      = map (hsRecFieldArg . unLoc) (rec_flds fs)
hsConPatArgs (InfixCon p1 p2) = [p1,p2]

{-
However HsRecFields is used only for patterns and expressions
(not data type declarations)
-}

data HsRecFields id arg         -- A bunch of record fields
                                --      { x = 3, y = True }
        -- Used for both expressions and patterns
  = HsRecFields { rec_flds   :: [LHsRecField id arg],
                  rec_dotdot :: Maybe Int }  -- Note [DotDot fields]
  deriving (Data, Typeable)

-- Note [DotDot fields]
-- ~~~~~~~~~~~~~~~~~~~~
-- The rec_dotdot field means this:
--   Nothing => the normal case
--   Just n  => the group uses ".." notation,
--
-- In the latter case:
--
--   *before* renamer: rec_flds are exactly the n user-written fields
--
--   *after* renamer:  rec_flds includes *all* fields, with
--                     the first 'n' being the user-written ones
--                     and the remainder being 'filled in' implicitly

type LHsRecField id arg = Located (HsRecField id arg)
-- |  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnEqual',

-- For details on above see note [Api annotations] in ApiAnnotation
data HsRecField id arg = HsRecField {
        hsRecFieldId  :: Located id,
        hsRecFieldArg :: arg,           -- Filled in by renamer
        hsRecPun      :: Bool           -- Note [Punning]
  } deriving (Data, Typeable)

-- Note [Punning]
-- ~~~~~~~~~~~~~~
-- If you write T { x, y = v+1 }, the HsRecFields will be
--      HsRecField x x True ...
--      HsRecField y (v+1) False ...
-- That is, for "punned" field x is expanded (in the renamer)
-- to x=x; but with a punning flag so we can detect it later
-- (e.g. when pretty printing)
--
-- If the original field was qualified, we un-qualify it, thus
--    T { A.x } means T { A.x = x }

hsRecFields :: HsRecFields id arg -> [id]
hsRecFields rbinds = map (unLoc . hsRecFieldId . unLoc) (rec_flds rbinds)

-- Probably won't typecheck at once, things have changed :/
hsRecFieldsArgs :: HsRecFields id arg -> [arg]
hsRecFieldsArgs rbinds = map (hsRecFieldArg . unLoc) (rec_flds rbinds)

{-
************************************************************************
*                                                                      *
*              Printing patterns
*                                                                      *
************************************************************************
-}

instance (OutputableBndr name) => Outputable (Pat name) where
    ppr = pprPat

pprPatBndr :: OutputableBndr name => name -> SDoc
pprPatBndr var                  -- Print with type info if -dppr-debug is on
  = getPprStyle $ \ sty ->
    if debugStyle sty then
        parens (pprBndr LambdaBind var)         -- Could pass the site to pprPat
                                                -- but is it worth it?
    else
        pprPrefixOcc var

pprParendLPat :: (OutputableBndr name) => LPat name -> SDoc
pprParendLPat (L _ p) = pprParendPat p

pprParendPat :: (OutputableBndr name) => Pat name -> SDoc
pprParendPat p | hsPatNeedsParens p = parens (pprPat p)
               | otherwise          = pprPat p

pprPat :: (OutputableBndr name) => Pat name -> SDoc
pprPat (VarPat var)       = pprPatBndr var
pprPat (WildPat _)        = char '_'
pprPat (LazyPat pat)      = char '~' <> pprParendLPat pat
pprPat (BangPat pat)      = char '!' <> pprParendLPat pat
pprPat (AsPat name pat)   = hcat [pprPrefixOcc (unLoc name), char '@', pprParendLPat pat]
pprPat (ViewPat expr pat _) = hcat [pprLExpr expr, text " -> ", ppr pat]
pprPat (ParPat pat)         = parens (ppr pat)
pprPat (ListPat pats _ _)     = brackets (interpp'SP pats)
pprPat (PArrPat pats _)     = paBrackets (interpp'SP pats)
pprPat (TuplePat pats bx _) = tupleParens (boxityNormalTupleSort bx) (interpp'SP pats)

pprPat (ConPatIn con details) = pprUserCon (unLoc con) details
pprPat (ConPatOut { pat_con = con, pat_tvs = tvs, pat_dicts = dicts,
                    pat_binds = binds, pat_args = details })
  = getPprStyle $ \ sty ->      -- Tiresome; in TcBinds.tcRhs we print out a
    if debugStyle sty then      -- typechecked Pat in an error message,
                                -- and we want to make sure it prints nicely
        ppr con
          <> braces (sep [ hsep (map pprPatBndr (tvs ++ dicts))
                         , ppr binds])
          <+> pprConArgs details
    else pprUserCon (unLoc con) details

pprPat (LitPat s)           = ppr s
pprPat (NPat l Nothing  _)  = ppr l
pprPat (NPat l (Just _) _)  = char '-' <> ppr l
pprPat (NPlusKPat n k _ _)  = hcat [ppr n, char '+', ppr k]
pprPat (SplicePat splice)   = pprSplice splice
pprPat (CoPat co pat _)     = pprHsWrapper (ppr pat) co
pprPat (SigPatIn pat ty)    = ppr pat <+> dcolon <+> ppr ty
pprPat (SigPatOut pat ty)   = ppr pat <+> dcolon <+> ppr ty

pprUserCon :: (OutputableBndr con, OutputableBndr id) => con -> HsConPatDetails id -> SDoc
pprUserCon c (InfixCon p1 p2) = ppr p1 <+> pprInfixOcc c <+> ppr p2
pprUserCon c details          = pprPrefixOcc c <+> pprConArgs details

pprConArgs ::  OutputableBndr id => HsConPatDetails id -> SDoc
pprConArgs (PrefixCon pats) = sep (map pprParendLPat pats)
pprConArgs (InfixCon p1 p2) = sep [pprParendLPat p1, pprParendLPat p2]
pprConArgs (RecCon rpats)   = ppr rpats

instance (OutputableBndr id, Outputable arg)
      => Outputable (HsRecFields id arg) where
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Nothing })
        = braces (fsep (punctuate comma (map ppr flds)))
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Just n })
        = braces (fsep (punctuate comma (map ppr (take n flds) ++ [dotdot])))
        where
          dotdot = ptext (sLit "..") <+> ifPprDebug (ppr (drop n flds))

instance (OutputableBndr id, Outputable arg)
      => Outputable (HsRecField id arg) where
  ppr (HsRecField { hsRecFieldId = f, hsRecFieldArg = arg,
                    hsRecPun = pun })
    = ppr f <+> (ppUnless pun $ equals <+> ppr arg)

{-
************************************************************************
*                                                                      *
*              Building patterns
*                                                                      *
************************************************************************
-}

mkPrefixConPat :: DataCon -> [OutPat id] -> [Type] -> OutPat id
-- Make a vanilla Prefix constructor pattern
mkPrefixConPat dc pats tys
  = noLoc $ ConPatOut { pat_con = noLoc (RealDataCon dc), pat_tvs = [], pat_dicts = [],
                        pat_binds = emptyTcEvBinds, pat_args = PrefixCon pats,
                        pat_arg_tys = tys, pat_wrap = idHsWrapper }

mkNilPat :: Type -> OutPat id
mkNilPat ty = mkPrefixConPat nilDataCon [] [ty]

mkCharLitPat :: String -> Char -> OutPat id
mkCharLitPat src c = mkPrefixConPat charDataCon
                                    [noLoc $ LitPat (HsCharPrim src c)] []

{-
************************************************************************
*                                                                      *
* Predicates for checking things about pattern-lists in EquationInfo   *
*                                                                      *
************************************************************************

\subsection[Pat-list-predicates]{Look for interesting things in patterns}

Unlike in the Wadler chapter, where patterns are either ``variables''
or ``constructors,'' here we distinguish between:
\begin{description}
\item[unfailable:]
Patterns that cannot fail to match: variables, wildcards, and lazy
patterns.

These are the irrefutable patterns; the two other categories
are refutable patterns.

\item[constructor:]
A non-literal constructor pattern (see next category).

\item[literal patterns:]
At least the numeric ones may be overloaded.
\end{description}

A pattern is in {\em exactly one} of the above three categories; `as'
patterns are treated specially, of course.

The 1.3 report defines what ``irrefutable'' and ``failure-free'' patterns are.
-}

isUnliftedLPat :: LPat id -> Bool
isUnliftedLPat (L _ (ParPat p))             = isUnliftedLPat p
isUnliftedLPat (L _ (TuplePat _ Unboxed _)) = True
isUnliftedLPat _                            = False

isUnliftedHsBind :: HsBind id -> Bool
-- A pattern binding with an outermost bang or unboxed tuple must be matched strictly
-- Defined in this module because HsPat is above HsBinds in the import graph
isUnliftedHsBind (PatBind { pat_lhs = p }) = isUnliftedLPat p
isUnliftedHsBind _                         = False

isBangedPatBind :: HsBind id -> Bool
isBangedPatBind (PatBind {pat_lhs = pat}) = isBangedLPat pat
isBangedPatBind _ = False

isBangedLPat :: LPat id -> Bool
isBangedLPat (L _ (ParPat p))   = isBangedLPat p
isBangedLPat (L _ (BangPat {})) = True
isBangedLPat _                  = False

looksLazyPatBind :: HsBind id -> Bool
-- Returns True of anything *except*
--     a StrictHsBind (as above) or
--     a VarPat
-- In particular, returns True of a pattern binding with a compound pattern, like (I# x)
looksLazyPatBind (PatBind { pat_lhs = p }) = looksLazyLPat p
looksLazyPatBind _                         = False

looksLazyLPat :: LPat id -> Bool
looksLazyLPat (L _ (ParPat p))             = looksLazyLPat p
looksLazyLPat (L _ (AsPat _ p))            = looksLazyLPat p
looksLazyLPat (L _ (BangPat {}))           = False
looksLazyLPat (L _ (TuplePat _ Unboxed _)) = False
looksLazyLPat (L _ (VarPat {}))            = False
looksLazyLPat (L _ (WildPat {}))           = False
looksLazyLPat _                            = True

isIrrefutableHsPat :: OutputableBndr id => LPat id -> Bool
-- (isIrrefutableHsPat p) is true if matching against p cannot fail,
-- in the sense of falling through to the next pattern.
--      (NB: this is not quite the same as the (silly) defn
--      in 3.17.2 of the Haskell 98 report.)
--
-- isIrrefutableHsPat returns False if it's in doubt; specifically
-- on a ConPatIn it doesn't know the size of the constructor family
-- But if it returns True, the pattern is definitely irrefutable
isIrrefutableHsPat pat
  = go pat
  where
    go (L _ pat) = go1 pat

    go1 (WildPat {})        = True
    go1 (VarPat {})         = True
    go1 (LazyPat {})        = True
    go1 (BangPat pat)       = go pat
    go1 (CoPat _ pat _)     = go1 pat
    go1 (ParPat pat)        = go pat
    go1 (AsPat _ pat)       = go pat
    go1 (ViewPat _ pat _)   = go pat
    go1 (SigPatIn pat _)    = go pat
    go1 (SigPatOut pat _)   = go pat
    go1 (TuplePat pats _ _) = all go pats
    go1 (ListPat {}) = False
    go1 (PArrPat {})        = False     -- ?

    go1 (ConPatIn {})       = False     -- Conservative
    go1 (ConPatOut{ pat_con = L _ (RealDataCon con), pat_args = details })
        =  isJust (tyConSingleDataCon_maybe (dataConTyCon con))
           -- NB: tyConSingleDataCon_maybe, *not* isProductTyCon, because
           -- the latter is false of existentials. See Trac #4439
        && all go (hsConPatArgs details)
    go1 (ConPatOut{ pat_con = L _ (PatSynCon _pat) })
        = False -- Conservative

    go1 (LitPat {})    = False
    go1 (NPat {})      = False
    go1 (NPlusKPat {}) = False

    -- Both should be gotten rid of by renamer before
    -- isIrrefutablePat is called
    go1 (SplicePat {})     = urk pat

    urk pat = pprPanic "isIrrefutableHsPat:" (ppr pat)

hsPatNeedsParens :: Pat a -> Bool
hsPatNeedsParens (NPlusKPat {})      = True
hsPatNeedsParens (SplicePat {})      = False
hsPatNeedsParens (ConPatIn _ ds)     = conPatNeedsParens ds
hsPatNeedsParens p@(ConPatOut {})    = conPatNeedsParens (pat_args p)
hsPatNeedsParens (SigPatIn {})       = True
hsPatNeedsParens (SigPatOut {})      = True
hsPatNeedsParens (ViewPat {})        = True
hsPatNeedsParens (CoPat {})          = True
hsPatNeedsParens (WildPat {})        = False
hsPatNeedsParens (VarPat {})         = False
hsPatNeedsParens (LazyPat {})        = False
hsPatNeedsParens (BangPat {})        = False
hsPatNeedsParens (ParPat {})         = False
hsPatNeedsParens (AsPat {})          = False
hsPatNeedsParens (TuplePat {})       = False
hsPatNeedsParens (ListPat {})        = False
hsPatNeedsParens (PArrPat {})        = False
hsPatNeedsParens (LitPat {})         = False
hsPatNeedsParens (NPat {})           = False

conPatNeedsParens :: HsConDetails a b -> Bool
conPatNeedsParens (PrefixCon args) = not (null args)
conPatNeedsParens (InfixCon {})    = True
conPatNeedsParens (RecCon {})      = True

-- | Returns 'True' for compound patterns that need parentheses when used in
-- an argument position.
--
-- Note that this is different from 'hsPatNeedsParens', which only says if
-- a pattern needs to be parenthesized to parse in /any/ position, whereas
-- 'isCompoundPat' says if a pattern needs to be parenthesized in an /argument/
-- position. In other words, @'hsPatNeedsParens' x@ implies
-- @'isCompoundPat' x@, but not necessarily the other way around.
isCompoundPat :: Pat a -> Bool
isCompoundPat (NPlusKPat {})       = True
isCompoundPat (SplicePat {})       = False
isCompoundPat (ConPatIn _ ds)      = isCompoundConPat ds
isCompoundPat p@(ConPatOut {})     = isCompoundConPat (pat_args p)
isCompoundPat (SigPatIn {})        = True
isCompoundPat (SigPatOut {})       = True
isCompoundPat (ViewPat {})         = True
isCompoundPat (CoPat _ p _)        = isCompoundPat p
isCompoundPat (WildPat {})         = False
isCompoundPat (VarPat {})          = False
isCompoundPat (LazyPat {})         = False
isCompoundPat (BangPat {})         = False
isCompoundPat (ParPat {})          = False
isCompoundPat (AsPat {})           = False
isCompoundPat (TuplePat {})        = False
-- isCompoundPat (SumPat {})          = False
isCompoundPat (ListPat {})         = False
isCompoundPat (PArrPat {})         = False
isCompoundPat (LitPat p)           = isCompoundHsLit p
isCompoundPat (NPat (L _ p) _ _)   = isCompoundHsOverLit p
-- isCompoundPat (XPat {})            = False -- Assumption

-- | Returns 'True' for compound constructor patterns that need parentheses
-- when used in an argument position.
--
-- Note that this is different from 'conPatNeedsParens', which only says if
-- a constructor pattern needs to be parenthesized to parse in /any/ position,
-- whereas 'isCompoundConPat' says if a pattern needs to be parenthesized in an
-- /argument/ position. In other words, @'conPatNeedsParens' x@ implies
-- @'isCompoundConPat' x@, but not necessarily the other way around.
isCompoundConPat :: HsConDetails a b -> Bool
isCompoundConPat (PrefixCon args) = not (null args)
isCompoundConPat (InfixCon {})    = True
isCompoundConPat (RecCon {})      = False

-- | @'parenthesizeCompoundPat' p@ checks if @'isCompoundPat' p@ is true, and
-- if so, surrounds @p@ with a 'ParPat'. Otherwise, it simply returns @p@.
parenthesizeCompoundPat :: LPat RdrName -> LPat RdrName
parenthesizeCompoundPat lp@(L loc p)
  | isCompoundPat p = L loc (ParPat lp)
  | otherwise       = lp

{-
% Collect all EvVars from all constructor patterns
-}

-- May need to add more cases
collectEvVarsPats :: [Pat id] -> Bag EvVar
collectEvVarsPats = unionManyBags . map collectEvVarsPat

collectEvVarsLPat :: LPat id -> Bag EvVar
collectEvVarsLPat (L _ pat) = collectEvVarsPat pat

collectEvVarsPat :: Pat id -> Bag EvVar
collectEvVarsPat pat =
  case pat of
    LazyPat  p        -> collectEvVarsLPat p
    AsPat _  p        -> collectEvVarsLPat p
    ParPat   p        -> collectEvVarsLPat p
    BangPat  p        -> collectEvVarsLPat p
    ListPat  ps _ _   -> unionManyBags $ map collectEvVarsLPat ps
    TuplePat ps _ _   -> unionManyBags $ map collectEvVarsLPat ps
    PArrPat  ps _     -> unionManyBags $ map collectEvVarsLPat ps
    ConPatOut {pat_dicts = dicts, pat_args  = args}
                      -> unionBags (listToBag dicts)
                                   $ unionManyBags
                                   $ map collectEvVarsLPat
                                   $ hsConPatArgs args
    SigPatOut p _     -> collectEvVarsLPat p
    CoPat _ p _       -> collectEvVarsPat  p
    ConPatIn _  _     -> panic "foldMapPatBag: ConPatIn"
    SigPatIn _ _      -> panic "foldMapPatBag: SigPatIn"
    _other_pat        -> emptyBag
