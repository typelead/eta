{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}

-- | Abstract Haskell syntax for expressions.
module Eta.HsSyn.HsExpr where

#include "HsVersions.h"

-- friends:
import Eta.HsSyn.HsDecls
import Eta.HsSyn.HsPat
import Eta.HsSyn.HsLit
import Eta.HsSyn.PlaceHolder ( PostTc, PostRn, DataId)
import Eta.HsSyn.HsTypes
import Eta.HsSyn.HsBinds

-- others:
import Eta.TypeCheck.TcEvidence
import Eta.Core.CoreSyn
import Eta.BasicTypes.Var
import Eta.BasicTypes.RdrName
import Eta.BasicTypes.Name
import Eta.BasicTypes.BasicTypes
import Eta.BasicTypes.DataCon
import Eta.BasicTypes.SrcLoc
import Eta.Utils.Util
import Eta.Main.StaticFlags( opt_PprStyle_Debug )
import Eta.Utils.Outputable
import Eta.Utils.FastString
import Eta.Types.Type
import Eta.REPL.RemoteTypes ( ForeignRef )

-- libraries:
import Data.Data hiding (Fixity(..))
import qualified Data.Data as Data (Fixity(..))
import Data.Maybe (isNothing)

{-
************************************************************************
*                                                                      *
\subsection{Expressions proper}
*                                                                      *
************************************************************************
-}

-- * Expressions proper

type LHsExpr id = Located (HsExpr id)
  -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma' when
  --   in a list

  -- For details on above see note [Api annotations] in ApiAnnotation

-------------------------
-- | PostTcExpr is an evidence expression attached to the syntax tree by the
-- type checker (c.f. postTcType).
type PostTcExpr  = HsExpr Id
-- | We use a PostTcTable where there are a bunch of pieces of evidence, more
-- than is convenient to keep individually.
type PostTcTable = [(Name, PostTcExpr)]

noPostTcExpr :: PostTcExpr
noPostTcExpr = HsLit (HsString "" (fsLit "noPostTcExpr"))

noPostTcTable :: PostTcTable
noPostTcTable = []

-------------------------
-- | SyntaxExpr is like 'PostTcExpr', but it's filled in a little earlier,
-- by the renamer.  It's used for rebindable syntax.
--
-- E.g. @(>>=)@ is filled in before the renamer by the appropriate 'Name' for
--      @(>>=)@, and then instantiated by the type checker with its type args
--      etc

type SyntaxExpr id = HsExpr id

noSyntaxExpr :: SyntaxExpr id -- Before renaming, and sometimes after,
                              -- (if the syntax slot makes no sense)
noSyntaxExpr = HsLit (HsString "" (fsLit "noSyntaxExpr"))


type CmdSyntaxTable id = [(Name, SyntaxExpr id)]
-- See Note [CmdSyntaxTable]

{-
Note [CmdSyntaxtable]
~~~~~~~~~~~~~~~~~~~~~
Used only for arrow-syntax stuff (HsCmdTop), the CmdSyntaxTable keeps
track of the methods needed for a Cmd.

* Before the renamer, this list is an empty list

* After the renamer, it takes the form @[(std_name, HsVar actual_name)]@
  For example, for the 'arr' method
   * normal case:            (GHC.Control.Arrow.arr, HsVar GHC.Control.Arrow.arr)
   * with rebindable syntax: (GHC.Control.Arrow.arr, arr_22)
             where @arr_22@ is whatever 'arr' is in scope

* After the type checker, it takes the form [(std_name, <expression>)]
  where <expression> is the evidence for the method.  This evidence is
  instantiated with the class, but is still polymorphic in everything
  else.  For example, in the case of 'arr', the evidence has type
         forall b c. (b->c) -> a b c
  where 'a' is the ambient type of the arrow.  This polymorphism is
  important because the desugarer uses the same evidence at multiple
  different types.

This is Less Cool than what we normally do for rebindable syntax, which is to
make fully-instantiated piece of evidence at every use site.  The Cmd way
is Less Cool because
  * The renamer has to predict which methods are needed.
    See the tedious RnExpr.methodNamesCmd.

  * The desugarer has to know the polymorphic type of the instantiated
    method. This is checked by Inst.tcSyntaxName, but is less flexible
    than the rest of rebindable syntax, where the type is less
    pre-ordained.  (And this flexibility is useful; for example we can
    typecheck do-notation with (>>=) :: m1 a -> (a -> m2 b) -> m2 b.)
-}

-- | A Haskell expression.
data HsExpr id
  = HsVar     id                        -- ^ Variable
  | HsIPVar   HsIPName                  -- ^ Implicit parameter
  | HsOverLit (HsOverLit id)            -- ^ Overloaded literals

  | HsLit     HsLit                     -- ^ Simple (non-overloaded) literals

  | HsLam     (MatchGroup id (LHsExpr id)) -- ^ Lambda abstraction. Currently always a single match
       --
       -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLam',
       --       'ApiAnnotation.AnnRarrow',

       -- For details on above see note [Api annotations] in ApiAnnotation

  | HsLamCase (PostTc id Type) (MatchGroup id (LHsExpr id)) -- ^ Lambda-case
       --
       -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLam',
       --           'ApiAnnotation.AnnCase','ApiAnnotation.AnnOpen',
       --           'ApiAnnotation.AnnClose'

       -- For details on above see note [Api annotations] in ApiAnnotation

  | HsApp     (LHsExpr id) (LHsExpr id) -- ^ Application

  -- | Operator applications:
  -- NB Bracketed ops such as (+) come out as Vars.

  -- NB We need an expr for the operator in an OpApp/Section since
  -- the typechecker may need to apply the operator to a few types.

  | OpApp       (LHsExpr id)    -- left operand
                (LHsExpr id)    -- operator
                (PostRn id Fixity) -- Renamer adds fixity; bottom until then
                (LHsExpr id)    -- right operand

  -- | Negation operator. Contains the negated expression and the name
  -- of 'negate'
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnMinus'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | NegApp      (LHsExpr id)
                (SyntaxExpr id)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
  --             'ApiAnnotation.AnnClose' @')'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsPar       (LHsExpr id)    -- ^ Parenthesised expr; see Note [Parens in HsSyn]

  | SectionL    (LHsExpr id)    -- operand; see Note [Sections in HsSyn]
                (LHsExpr id)    -- operator
  | SectionR    (LHsExpr id)    -- operator; see Note [Sections in HsSyn]
                (LHsExpr id)    -- operand

  -- | Used for explicit tuples and sections thereof
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --         'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExplicitTuple
        [LHsTupArg id]
        Boxity

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnCase',
  --       'ApiAnnotation.AnnOf','ApiAnnotation.AnnOpen' @'{'@,
  --       'ApiAnnotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsCase      (LHsExpr id)
                (MatchGroup id (LHsExpr id))

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnIf',
  --       'ApiAnnotation.AnnSemi',
  --       'ApiAnnotation.AnnThen','ApiAnnotation.AnnSemi',
  --       'ApiAnnotation.AnnElse',

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsIf        (Maybe (SyntaxExpr id)) -- cond function
                                        -- Nothing => use the built-in 'if'
                                        -- See Note [Rebindable if]
                (LHsExpr id)    --  predicate
                (LHsExpr id)    --  then part
                (LHsExpr id)    --  else part

  -- | Multi-way if
  --
  -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnIf'
  --       'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose',

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsMultiIf   (PostTc id Type) [LGRHS id (LHsExpr id)]

  -- | let(rec)
  --
  -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLet',
  --       'ApiAnnotation.AnnOpen' @'{'@,
  --       'ApiAnnotation.AnnClose' @'}'@,'ApiAnnotation.AnnIn'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsLet       (HsLocalBinds id)
                (LHsExpr  id)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDo',
  --             'ApiAnnotation.AnnOpen', 'ApiAnnotation.AnnSemi',
  --             'ApiAnnotation.AnnVbar',
  --             'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsDo        (HsStmtContext Name) -- The parameterisation is unimportant
                                     -- because in this context we never use
                                     -- the PatGuard or ParStmt variant
                [ExprLStmt id]       -- "do":one or more stmts
                (PostTc id Type)     -- Type of the whole expression

  -- | Syntactic list: [a,b,c,...]
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
  --              'ApiAnnotation.AnnClose' @']'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExplicitList
                (PostTc id Type)        -- Gives type of components of list
                (Maybe (SyntaxExpr id)) -- For OverloadedLists, the fromListN witness
                [LHsExpr id]

  -- | Syntactic parallel array: [:e1, ..., en:]
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'[:'@,
  --              'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnComma',
  --              'ApiAnnotation.AnnVbar'
  --              'ApiAnnotation.AnnClose' @':]'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExplicitPArr
                (PostTc id Type)   -- type of elements of the parallel array
                [LHsExpr id]

  -- | Record construction
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{'@,
  --         'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | RecordCon   (Located id)       -- The constructor.  After type checking
                                   -- it's the dataConWrapId of the constructor
                PostTcExpr         -- Data con Id applied to type args
                (HsRecordBinds id)

  -- | Record update
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{'@,
  --         'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | RecordUpd   (LHsExpr id)
                (HsRecordBinds id)
--              (HsMatchGroup Id)  -- Filled in by the type checker to be
--                                 -- a match that does the job
                [DataCon]          -- Filled in by the type checker to the
                                   -- _non-empty_ list of DataCons that have
                                   -- all the upd'd fields
                [PostTc id Type]   -- Argument types of *input* record type
                [PostTc id Type]   --              and  *output* record type
  -- For a type family, the arg types are of the *instance* tycon,
  -- not the family tycon

  -- | Expression with an explicit type signature. @e :: type@
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExprWithTySig
                (LHsExpr id)
                (LHsType id)
                (PostRn id [Name])      -- After renaming, the list of Names
                                        -- contains the named and unnamed
                                        -- wildcards brought in scope by the
                                        -- signature

  | ExprWithTySigOut                    -- TRANSLATION
                (LHsExpr id)
                (LHsType Name)          -- Retain the signature for
                                        -- round-tripping purposes

  -- | Arithmetic sequence
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
  --              'ApiAnnotation.AnnComma','ApiAnnotation.AnnDotdot',
  --              'ApiAnnotation.AnnClose' @']'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ArithSeq
                PostTcExpr
                (Maybe (SyntaxExpr id))   -- For OverloadedLists, the fromList witness
                (ArithSeqInfo id)

  -- | Arithmetic sequence for parallel array
  --
  -- > [:e1..e2:] or [:e1, e2..e3:]
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'[:'@,
  --              'ApiAnnotation.AnnComma','ApiAnnotation.AnnDotdot',
  --              'ApiAnnotation.AnnVbar',
  --              'ApiAnnotation.AnnClose' @':]'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | PArrSeq
                PostTcExpr
                (ArithSeqInfo id)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{-\# SCC'@,
  --             'ApiAnnotation.AnnVal' or 'ApiAnnotation.AnnValStr',
  --              'ApiAnnotation.AnnClose' @'\#-}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsSCC       SourceText            -- Note [Pragma source text] in BasicTypes
                FastString            -- "set cost centre" SCC pragma
                (LHsExpr id)          -- expr whose cost is to be measured

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{-\# CORE'@,
  --             'ApiAnnotation.AnnVal', 'ApiAnnotation.AnnClose' @'\#-}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsCoreAnn   SourceText            -- Note [Pragma source text] in BasicTypes
                FastString            -- hdaume: core annotation
                (LHsExpr id)

  -----------------------------------------------------------
  -- MetaHaskell Extensions

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --         'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose',
  --         'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsBracket    (HsBracket id)

    -- See Note [Pending Splices]
  | HsRnBracketOut
      (HsBracket Name)     -- Output of the renamer is the *original* renamed
                           -- expression, plus
      [PendingRnSplice]    -- _renamed_ splices to be type checked

  | HsTcBracketOut
      (HsBracket Name)     -- Output of the type checker is the *original*
                           -- renamed expression, plus
      [PendingTcSplice]    -- _typechecked_ splices to be
                           -- pasted back in by the desugarer

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --         'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsSpliceE  (HsSplice id)

  -----------------------------------------------------------
  -- Arrow notation extension

  -- | @proc@ notation for Arrows
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnProc',
  --          'ApiAnnotation.AnnRarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsProc      (LPat id)               -- arrow abstraction, proc
                (LHsCmdTop id)          -- body of the abstraction
                                        -- always has an empty stack

  ---------------------------------------
  -- static pointers extension
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnStatic',

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsStatic    (LHsExpr id)

  ---------------------------------------
  -- The following are commands, not expressions proper
  -- They are only used in the parsing stage and are removed
  --    immediately in parser.RdrHsSyn.checkCommand

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.Annlarrowtail',
  --          'ApiAnnotation.Annrarrowtail','ApiAnnotation.AnnLarrowtail',
  --          'ApiAnnotation.AnnRarrowtail'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsArrApp             -- Arrow tail, or arrow application (f -< arg)
        (LHsExpr id)     -- arrow expression, f
        (LHsExpr id)     -- input expression, arg
        (PostTc id Type) -- type of the arrow expressions f,
                         -- of the form a t t', where arg :: t
        HsArrAppType     -- higher-order (-<<) or first-order (-<)
        Bool             -- True => right-to-left (f -< arg)
                         -- False => left-to-right (arg >- f)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'(|'@,
  --         'ApiAnnotation.AnnClose' @'|)'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsArrForm            -- Command formation,  (| e cmd1 .. cmdn |)
        (LHsExpr id)     -- the operator
                         -- after type-checking, a type abstraction to be
                         -- applied to the type of the local environment tuple
        (Maybe Fixity)   -- fixity (filled in by the renamer), for forms that
                         -- were converted from OpApp's by the renamer
        [LHsCmdTop id]   -- argument commands

  ---------------------------------------
  -- Haskell program coverage (Hpc) Support

  | HsTick
     (Tickish id)
     (LHsExpr id)                       -- sub-expression

  | HsBinTick
     Int                                -- module-local tick number for True
     Int                                -- module-local tick number for False
     (LHsExpr id)                       -- sub-expression

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --       'ApiAnnotation.AnnOpen' @'{-\# GENERATED'@,
  --       'ApiAnnotation.AnnVal','ApiAnnotation.AnnVal',
  --       'ApiAnnotation.AnnColon','ApiAnnotation.AnnVal',
  --       'ApiAnnotation.AnnMinus',
  --       'ApiAnnotation.AnnVal','ApiAnnotation.AnnColon',
  --       'ApiAnnotation.AnnVal',
  --       'ApiAnnotation.AnnClose' @'\#-}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsTickPragma                      -- A pragma introduced tick
     SourceText                       -- Note [Pragma source text] in BasicTypes
     (FastString,(Int,Int),(Int,Int)) -- external span for this tick
     (LHsExpr id)

  ---------------------------------------
  -- These constructors only appear temporarily in the parser.
  -- The renamer translates them into the Right Thing.

  | EWildPat                 -- wildcard

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnAt'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | EAsPat      (Located id) -- as pattern
                (LHsExpr id)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | EViewPat    (LHsExpr id) -- view pattern
                (LHsExpr id)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnTilde'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ELazyPat    (LHsExpr id) -- ~ pattern

  | HsType      (LHsType id) -- Explicit type argument; e.g  f {| Int |} x y

  ---------------------------------------
  -- Finally, HsWrap appears only in typechecker output

  |  HsWrap     HsWrapper    -- TRANSLATION
                (HsExpr id)
  |  HsUnboundVar RdrName
  |  HsOverLabel FastString -- ^ Overloaded label (See Note [Overloaded labels]
  deriving (Typeable)
deriving instance (DataId id) => Data (HsExpr id)

-- | HsTupArg is used for tuple sections
--  (,a,) is represented by  ExplicitTuple [Missing ty1, Present a, Missing ty3]
--  Which in turn stands for (\x:ty1 \y:ty2. (x,a,y))
type LHsTupArg id = Located (HsTupArg id)
-- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma'

-- For details on above see note [Api annotations] in ApiAnnotation
data HsTupArg id
  = Present (LHsExpr id)     -- ^ The argument
  | Missing (PostTc id Type) -- ^ The argument is missing, but this is its type
  deriving (Typeable)
deriving instance (DataId id) => Data (HsTupArg id)

tupArgPresent :: LHsTupArg id -> Bool
tupArgPresent (L _ (Present {})) = True
tupArgPresent (L _ (Missing {})) = False

{-
Note [Parens in HsSyn]
~~~~~~~~~~~~~~~~~~~~~~
HsPar (and ParPat in patterns, HsParTy in types) is used as follows

  * Generally HsPar is optional; the pretty printer adds parens where
    necessary.  Eg (HsApp f (HsApp g x)) is fine, and prints 'f (g x)'

  * HsPars are pretty printed as '( .. )' regardless of whether
    or not they are strictly necessary

  * HsPars are respected when rearranging operator fixities.
    So   a * (b + c)  means what it says (where the parens are an HsPar)

Note [Sections in HsSyn]
~~~~~~~~~~~~~~~~~~~~~~~~
Sections should always appear wrapped in an HsPar, thus
         HsPar (SectionR ...)
The parser parses sections in a wider variety of situations
(See Note [Parsing sections]), but the renamer checks for those
parens.  This invariant makes pretty-printing easier; we don't need
a special case for adding the parens round sections.

Note [Rebindable if]
~~~~~~~~~~~~~~~~~~~~
The rebindable syntax for 'if' is a bit special, because when
rebindable syntax is *off* we do not want to treat
   (if c then t else e)
as if it was an application (ifThenElse c t e).  Why not?
Because we allow an 'if' to return *unboxed* results, thus
  if blah then 3# else 4#
whereas that would not be possible using a all to a polymorphic function
(because you can't call a polymorphic function at an unboxed type).

So we use Nothing to mean "use the old built-in typing rule".
-}

instance OutputableBndr id => Outputable (HsExpr id) where
    ppr expr = pprExpr expr

-----------------------
-- pprExpr, pprLExpr, pprBinds call pprDeeper;
-- the underscore versions do not
pprLExpr :: OutputableBndr id => LHsExpr id -> SDoc
pprLExpr (L _ e) = pprExpr e

pprExpr :: OutputableBndr id => HsExpr id -> SDoc
pprExpr e | isAtomicHsExpr e || isQuietHsExpr e =            ppr_expr e
          | otherwise                           = pprDeeper (ppr_expr e)

isQuietHsExpr :: HsExpr id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsExpr (HsPar _) = True
-- applications don't display anything themselves
isQuietHsExpr (HsApp _ _) = True
isQuietHsExpr (OpApp _ _ _ _) = True
isQuietHsExpr _ = False

pprBinds :: (OutputableBndr idL, OutputableBndr idR)
         => HsLocalBindsLR idL idR -> SDoc
pprBinds b = pprDeeper (ppr b)

-----------------------
ppr_lexpr :: OutputableBndr id => LHsExpr id -> SDoc
ppr_lexpr e = ppr_expr (unLoc e)

ppr_expr :: forall id. OutputableBndr id => HsExpr id -> SDoc
ppr_expr (HsVar v)       = pprPrefixOcc v
ppr_expr (HsIPVar v)     = ppr v
ppr_expr (HsOverLabel l) = char '#' <> ppr l
ppr_expr (HsLit lit)     = ppr lit
ppr_expr (HsOverLit lit) = ppr lit
ppr_expr (HsPar e)       = parens (ppr_lexpr e)

ppr_expr (HsCoreAnn _ s e)
  = vcat [ptext (sLit "HsCoreAnn") <+> ftext s, ppr_lexpr e]

ppr_expr (HsApp e1 e2)
  = let (fun, args) = collect_args e1 [e2] in
    hang (ppr_lexpr fun) 2 (sep (map pprParendExpr args))
  where
    collect_args (L _ (HsApp fun arg)) args = collect_args fun (arg:args)
    collect_args fun args = (fun, args)

ppr_expr (OpApp e1 op _ e2)
  = case unLoc op of
      HsVar v -> pp_infixly v
      _       -> pp_prefixly
  where
    pp_e1 = pprDebugParendExpr e1   -- In debug mode, add parens
    pp_e2 = pprDebugParendExpr e2   -- to make precedence clear

    pp_prefixly
      = hang (ppr op) 2 (sep [pp_e1, pp_e2])

    pp_infixly v
      = sep [pp_e1, sep [pprInfixOcc v, nest 2 pp_e2]]

ppr_expr (NegApp e _) = char '-' <+> pprDebugParendExpr e

ppr_expr (SectionL expr op)
  = case unLoc op of
      HsVar v -> pp_infixly v
      _       -> pp_prefixly
  where
    pp_expr = pprDebugParendExpr expr

    pp_prefixly = hang (hsep [text " \\ x_ ->", ppr op])
                       4 (hsep [pp_expr, ptext (sLit "x_ )")])
    pp_infixly v = (sep [pp_expr, pprInfixOcc v])

ppr_expr (SectionR op expr)
  = case unLoc op of
      HsVar v -> pp_infixly v
      _       -> pp_prefixly
  where
    pp_expr = pprDebugParendExpr expr

    pp_prefixly = hang (hsep [text "( \\ x_ ->", ppr op, ptext (sLit "x_")])
                       4 (pp_expr <> rparen)
    pp_infixly v = sep [pprInfixOcc v, pp_expr]

ppr_expr (ExplicitTuple exprs boxity)
  = tupleParens (boxityNormalTupleSort boxity)
                (fcat (ppr_tup_args $ map unLoc exprs))
  where
    ppr_tup_args []               = []
    ppr_tup_args (Present e : es) = (ppr_lexpr e <> punc es) : ppr_tup_args es
    ppr_tup_args (Missing _ : es) = punc es : ppr_tup_args es

    punc (Present {} : _) = comma <> space
    punc (Missing {} : _) = comma
    punc []               = empty

--avoid using PatternSignatures for stage1 code portability
ppr_expr (HsLam matches)
  = pprMatches (LambdaExpr :: HsMatchContext id) matches

ppr_expr (HsLamCase _ matches)
  = sep [ sep [ptext (sLit "\\case {")],
          nest 2 (pprMatches (CaseAlt :: HsMatchContext id) matches <+> char '}') ]

ppr_expr (HsCase expr matches)
  = sep [ sep [ptext (sLit "case"), nest 4 (ppr expr), ptext (sLit "of {")],
          nest 2 (pprMatches (CaseAlt :: HsMatchContext id) matches <+> char '}') ]

ppr_expr (HsIf _ e1 e2 e3)
  = sep [hsep [ptext (sLit "if"), nest 2 (ppr e1), ptext (sLit "then")],
         nest 4 (ppr e2),
         ptext (sLit "else"),
         nest 4 (ppr e3)]

ppr_expr (HsMultiIf _ alts)
  = sep $ ptext (sLit "if") : map ppr_alt alts
  where ppr_alt (L _ (GRHS guards expr)) =
          sep [ char '|' <+> interpp'SP guards
              , ptext (sLit "->") <+> pprDeeper (ppr expr) ]

-- special case: let ... in let ...
ppr_expr (HsLet binds expr@(L _ (HsLet _ _)))
  = sep [hang (ptext (sLit "let")) 2 (hsep [pprBinds binds, ptext (sLit "in")]),
         ppr_lexpr expr]

ppr_expr (HsLet binds expr)
  = sep [hang (ptext (sLit "let")) 2 (pprBinds binds),
         hang (ptext (sLit "in"))  2 (ppr expr)]

ppr_expr (HsDo do_or_list_comp stmts _) = pprDo do_or_list_comp stmts

ppr_expr (ExplicitList _ _ exprs)
  = brackets (pprDeeperList fsep (punctuate comma (map ppr_lexpr exprs)))

ppr_expr (ExplicitPArr _ exprs)
  = paBrackets (pprDeeperList fsep (punctuate comma (map ppr_lexpr exprs)))

ppr_expr (RecordCon con_id _ rbinds)
  = hang (ppr con_id) 2 (ppr rbinds)

ppr_expr (RecordUpd aexp rbinds _ _ _)
  = hang (pprParendExpr aexp) 2 (ppr rbinds)

ppr_expr (ExprWithTySig expr sig _)
  = hang (nest 2 (ppr_lexpr expr) <+> dcolon)
         4 (ppr sig)
ppr_expr (ExprWithTySigOut expr sig)
  = hang (nest 2 (ppr_lexpr expr) <+> dcolon)
         4 (ppr sig)

ppr_expr (ArithSeq _ _ info) = brackets (ppr info)
ppr_expr (PArrSeq  _ info) = paBrackets (ppr info)

ppr_expr EWildPat       = char '_'
ppr_expr (ELazyPat e)   = char '~' <> pprParendExpr e
ppr_expr (EAsPat v e)   = ppr v <> char '@' <> pprParendExpr e
ppr_expr (EViewPat p e) = ppr p <+> ptext (sLit "->") <+> ppr e

ppr_expr (HsSCC _ lbl expr)
  = sep [ ptext (sLit "{-# SCC") <+> doubleQuotes (ftext lbl) <+> ptext (sLit "#-}"),
          pprParendExpr expr ]

ppr_expr (HsWrap co_fn e) = pprHsWrapper (pprExpr e) co_fn
ppr_expr (HsType id)      = ppr id

ppr_expr (HsSpliceE s)         = pprSplice s
ppr_expr (HsBracket b)         = pprHsBracket b
ppr_expr (HsRnBracketOut e []) = ppr e
ppr_expr (HsRnBracketOut e ps) = ppr e $$ ptext (sLit "pending(rn)") <+> ppr ps
ppr_expr (HsTcBracketOut e []) = ppr e
ppr_expr (HsTcBracketOut e ps) = ppr e $$ ptext (sLit "pending(tc)") <+> ppr ps

ppr_expr (HsProc pat (L _ (HsCmdTop cmd _ _ _)))
  = hsep [ptext (sLit "proc"), ppr pat, ptext (sLit "->"), ppr cmd]

ppr_expr (HsStatic e)
  = hsep [ptext (sLit "static"), pprParendExpr e]

ppr_expr (HsTick tickish exp)
  = pprTicks (ppr exp) $
    ppr tickish <+> ppr_lexpr exp
ppr_expr (HsBinTick tickIdTrue tickIdFalse exp)
  = pprTicks (ppr exp) $
    hcat [ptext (sLit "bintick<"),
          ppr tickIdTrue,
          ptext (sLit ","),
          ppr tickIdFalse,
          ptext (sLit ">("),
          ppr exp,ptext (sLit ")")]
ppr_expr (HsTickPragma _ externalSrcLoc exp)
  = pprTicks (ppr exp) $
    hcat [ptext (sLit "tickpragma<"),
          ppr externalSrcLoc,
          ptext (sLit ">("),
          ppr exp,
          ptext (sLit ")")]

ppr_expr (HsArrApp arrow arg _ HsFirstOrderApp True)
  = hsep [ppr_lexpr arrow, larrowt, ppr_lexpr arg]
ppr_expr (HsArrApp arrow arg _ HsFirstOrderApp False)
  = hsep [ppr_lexpr arg, arrowt, ppr_lexpr arrow]
ppr_expr (HsArrApp arrow arg _ HsHigherOrderApp True)
  = hsep [ppr_lexpr arrow, larrowtt, ppr_lexpr arg]
ppr_expr (HsArrApp arrow arg _ HsHigherOrderApp False)
  = hsep [ppr_lexpr arg, arrowtt, ppr_lexpr arrow]

ppr_expr (HsArrForm (L _ (HsVar v)) (Just _) [arg1, arg2])
  = sep [pprCmdArg (unLoc arg1), hsep [pprInfixOcc v, pprCmdArg (unLoc arg2)]]
ppr_expr (HsArrForm op _ args)
  = hang (ptext (sLit "(|") <+> ppr_lexpr op)
         4 (sep (map (pprCmdArg.unLoc) args) <+> ptext (sLit "|)"))
ppr_expr (HsUnboundVar nm)
  = ppr nm

{-
HsSyn records exactly where the user put parens, with HsPar.
So generally speaking we print without adding any parens.
However, some code is internally generated, and in some places
parens are absolutely required; so for these places we use
pprParendExpr (but don't print double parens of course).

For operator applications we don't add parens, because the operator
fixities should do the job, except in debug mode (-dppr-debug) so we
can see the structure of the parse tree.
-}

pprDebugParendExpr :: OutputableBndr id => LHsExpr id -> SDoc
pprDebugParendExpr expr
  = getPprStyle (\sty ->
    if debugStyle sty then pprParendExpr expr
                      else pprLExpr      expr)

pprParendExpr :: OutputableBndr id => LHsExpr id -> SDoc
pprParendExpr expr
  | hsExprNeedsParens (unLoc expr) = parens (pprLExpr expr)
  | otherwise                      = pprLExpr expr
        -- Using pprLExpr makes sure that we go 'deeper'
        -- I think that is usually (always?) right

hsExprNeedsParens :: HsExpr id -> Bool
-- True of expressions for which '(e)' and 'e'
-- mean the same thing
hsExprNeedsParens (ArithSeq {})       = False
hsExprNeedsParens (PArrSeq {})        = False
hsExprNeedsParens (HsLit {})          = False
hsExprNeedsParens (HsOverLit {})      = False
hsExprNeedsParens (HsVar {})          = False
hsExprNeedsParens (HsUnboundVar {})   = False
hsExprNeedsParens (HsIPVar {})        = False
hsExprNeedsParens (HsOverLabel {})    = False
hsExprNeedsParens (ExplicitTuple {})  = False
hsExprNeedsParens (ExplicitList {})   = False
hsExprNeedsParens (ExplicitPArr {})   = False
hsExprNeedsParens (HsPar {})          = False
hsExprNeedsParens (HsBracket {})      = False
hsExprNeedsParens (HsRnBracketOut {}) = False
hsExprNeedsParens (HsTcBracketOut {}) = False
hsExprNeedsParens (HsDo sc _ _)
       | isListCompExpr sc            = False
hsExprNeedsParens _ = True


isAtomicHsExpr :: HsExpr id -> Bool
-- True of a single token
isAtomicHsExpr (HsVar {})        = True
isAtomicHsExpr (HsLit {})        = True
isAtomicHsExpr (HsOverLit {})    = True
isAtomicHsExpr (HsIPVar {})      = True
isAtomicHsExpr (HsOverLabel {})  = True
isAtomicHsExpr (HsUnboundVar {}) = True
isAtomicHsExpr (HsWrap _ e)      = isAtomicHsExpr e
isAtomicHsExpr (HsPar e)         = isAtomicHsExpr (unLoc e)
isAtomicHsExpr _                 = False

{-
************************************************************************
*                                                                      *
\subsection{Commands (in arrow abstractions)}
*                                                                      *
************************************************************************

We re-use HsExpr to represent these.
-}

type LHsCmd id = Located (HsCmd id)

data HsCmd id
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.Annlarrowtail',
  --          'ApiAnnotation.Annrarrowtail','ApiAnnotation.AnnLarrowtail',
  --          'ApiAnnotation.AnnRarrowtail'

  -- For details on above see note [Api annotations] in ApiAnnotation
  = HsCmdArrApp          -- Arrow tail, or arrow application (f -< arg)
        (LHsExpr id)     -- arrow expression, f
        (LHsExpr id)     -- input expression, arg
        (PostTc id Type) -- type of the arrow expressions f,
                         -- of the form a t t', where arg :: t
        HsArrAppType     -- higher-order (-<<) or first-order (-<)
        Bool             -- True => right-to-left (f -< arg)
                         -- False => left-to-right (arg >- f)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'(|'@,
  --         'ApiAnnotation.AnnClose' @'|)'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsCmdArrForm         -- Command formation,  (| e cmd1 .. cmdn |)
        (LHsExpr id)     -- the operator
                         -- after type-checking, a type abstraction to be
                         -- applied to the type of the local environment tuple
        (Maybe Fixity)   -- fixity (filled in by the renamer), for forms that
                         -- were converted from OpApp's by the renamer
        [LHsCmdTop id]   -- argument commands

  | HsCmdApp    (LHsCmd id)
                (LHsExpr id)

  | HsCmdLam    (MatchGroup id (LHsCmd id))     -- kappa
       -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLam',
       --       'ApiAnnotation.AnnRarrow',

       -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdPar    (LHsCmd id)                     -- parenthesised command
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
    --             'ApiAnnotation.AnnClose' @')'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdCase   (LHsExpr id)
                (MatchGroup id (LHsCmd id))     -- bodies are HsCmd's
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnCase',
    --       'ApiAnnotation.AnnOf','ApiAnnotation.AnnOpen' @'{'@,
    --       'ApiAnnotation.AnnClose' @'}'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdIf     (Maybe (SyntaxExpr id))         -- cond function
                (LHsExpr id)                    -- predicate
                (LHsCmd id)                     -- then part
                (LHsCmd id)                     -- else part
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnIf',
    --       'ApiAnnotation.AnnSemi',
    --       'ApiAnnotation.AnnThen','ApiAnnotation.AnnSemi',
    --       'ApiAnnotation.AnnElse',

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdLet    (HsLocalBinds id)               -- let(rec)
                (LHsCmd  id)
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLet',
    --       'ApiAnnotation.AnnOpen' @'{'@,
    --       'ApiAnnotation.AnnClose' @'}'@,'ApiAnnotation.AnnIn'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdDo     [CmdLStmt id]
                (PostTc id Type)                -- Type of the whole expression
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDo',
    --             'ApiAnnotation.AnnOpen', 'ApiAnnotation.AnnSemi',
    --             'ApiAnnotation.AnnVbar',
    --             'ApiAnnotation.AnnClose'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdCast   TcCoercion     -- A simpler version of HsWrap in HsExpr
                (HsCmd id)     -- If   cmd :: arg1 --> res
                               --       co :: arg1 ~ arg2
                               -- Then (HsCmdCast co cmd) :: arg2 --> res
  deriving (Typeable)
deriving instance (DataId id) => Data (HsCmd id)

data HsArrAppType = HsHigherOrderApp | HsFirstOrderApp
  deriving (Data, Typeable)


{- | Top-level command, introducing a new arrow.
This may occur inside a proc (where the stack is empty) or as an
argument of a command-forming operator.
-}

type LHsCmdTop id = Located (HsCmdTop id)

data HsCmdTop id
  = HsCmdTop (LHsCmd id)
             (PostTc id Type)   -- Nested tuple of inputs on the command's stack
             (PostTc id Type)   -- return type of the command
             (CmdSyntaxTable id) -- See Note [CmdSyntaxTable]
  deriving (Typeable)
deriving instance (DataId id) => Data (HsCmdTop id)

instance OutputableBndr id => Outputable (HsCmd id) where
    ppr cmd = pprCmd cmd

-----------------------
-- pprCmd and pprLCmd call pprDeeper;
-- the underscore versions do not
pprLCmd :: OutputableBndr id => LHsCmd id -> SDoc
pprLCmd (L _ c) = pprCmd c

pprCmd :: OutputableBndr id => HsCmd id -> SDoc
pprCmd c | isQuietHsCmd c =            ppr_cmd c
         | otherwise      = pprDeeper (ppr_cmd c)

isQuietHsCmd :: HsCmd id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsCmd (HsCmdPar _) = True
-- applications don't display anything themselves
isQuietHsCmd (HsCmdApp _ _) = True
isQuietHsCmd _ = False

-----------------------
ppr_lcmd :: OutputableBndr id => LHsCmd id -> SDoc
ppr_lcmd c = ppr_cmd (unLoc c)

ppr_cmd :: forall id. OutputableBndr id => HsCmd id -> SDoc
ppr_cmd (HsCmdPar c) = parens (ppr_lcmd c)

ppr_cmd (HsCmdApp c e)
  = let (fun, args) = collect_args c [e] in
    hang (ppr_lcmd fun) 2 (sep (map pprParendExpr args))
  where
    collect_args (L _ (HsCmdApp fun arg)) args = collect_args fun (arg:args)
    collect_args fun args = (fun, args)

--avoid using PatternSignatures for stage1 code portability
ppr_cmd (HsCmdLam matches)
  = pprMatches (LambdaExpr :: HsMatchContext id) matches

ppr_cmd (HsCmdCase expr matches)
  = sep [ sep [ptext (sLit "case"), nest 4 (ppr expr), ptext (sLit "of {")],
          nest 2 (pprMatches (CaseAlt :: HsMatchContext id) matches <+> char '}') ]

ppr_cmd (HsCmdIf _ e ct ce)
  = sep [hsep [ptext (sLit "if"), nest 2 (ppr e), ptext (sLit "then")],
         nest 4 (ppr ct),
         ptext (sLit "else"),
         nest 4 (ppr ce)]

-- special case: let ... in let ...
ppr_cmd (HsCmdLet binds cmd@(L _ (HsCmdLet _ _)))
  = sep [hang (ptext (sLit "let")) 2 (hsep [pprBinds binds, ptext (sLit "in")]),
         ppr_lcmd cmd]

ppr_cmd (HsCmdLet binds cmd)
  = sep [hang (ptext (sLit "let")) 2 (pprBinds binds),
         hang (ptext (sLit "in"))  2 (ppr cmd)]

ppr_cmd (HsCmdDo stmts _)  = pprDo ArrowExpr stmts
ppr_cmd (HsCmdCast co cmd) = sep [ ppr_cmd cmd
                                 , ptext (sLit "|>") <+> ppr co ]

ppr_cmd (HsCmdArrApp arrow arg _ HsFirstOrderApp True)
  = hsep [ppr_lexpr arrow, larrowt, ppr_lexpr arg]
ppr_cmd (HsCmdArrApp arrow arg _ HsFirstOrderApp False)
  = hsep [ppr_lexpr arg, arrowt, ppr_lexpr arrow]
ppr_cmd (HsCmdArrApp arrow arg _ HsHigherOrderApp True)
  = hsep [ppr_lexpr arrow, larrowtt, ppr_lexpr arg]
ppr_cmd (HsCmdArrApp arrow arg _ HsHigherOrderApp False)
  = hsep [ppr_lexpr arg, arrowtt, ppr_lexpr arrow]

ppr_cmd (HsCmdArrForm (L _ (HsVar v)) (Just _) [arg1, arg2])
  = sep [pprCmdArg (unLoc arg1), hsep [pprInfixOcc v, pprCmdArg (unLoc arg2)]]
ppr_cmd (HsCmdArrForm op _ args)
  = hang (ptext (sLit "(|") <> ppr_lexpr op)
         4 (sep (map (pprCmdArg.unLoc) args) <> ptext (sLit "|)"))

pprCmdArg :: OutputableBndr id => HsCmdTop id -> SDoc
pprCmdArg (HsCmdTop cmd@(L _ (HsCmdArrForm _ Nothing [])) _ _ _)
  = ppr_lcmd cmd
pprCmdArg (HsCmdTop cmd _ _ _)
  = parens (ppr_lcmd cmd)

instance OutputableBndr id => Outputable (HsCmdTop id) where
    ppr = pprCmdArg

{-
************************************************************************
*                                                                      *
\subsection{Record binds}
*                                                                      *
************************************************************************
-}

type HsRecordBinds id = HsRecFields id (LHsExpr id)

{-
************************************************************************
*                                                                      *
\subsection{@Match@, @GRHSs@, and @GRHS@ datatypes}
*                                                                      *
************************************************************************

@Match@es are sets of pattern bindings and right hand sides for
functions, patterns or case branches. For example, if a function @g@
is defined as:
\begin{verbatim}
g (x,y) = y
g ((x:ys),y) = y+1,
\end{verbatim}
then \tr{g} has two @Match@es: @(x,y) = y@ and @((x:ys),y) = y+1@.

It is always the case that each element of an @[Match]@ list has the
same number of @pats@s inside it.  This corresponds to saying that
a function defined by pattern matching must have the same number of
patterns in each equation.
-}

data MatchGroup id body
  = MG { mg_alts    :: [LMatch id body]  -- The alternatives
       , mg_arg_tys :: [PostTc id Type]  -- Types of the arguments, t1..tn
       , mg_res_ty  :: PostTc id Type    -- Type of the result, tr
       , mg_origin  :: Origin }
     -- The type is the type of the entire group
     --      t1 -> ... -> tn -> tr
     -- where there are n patterns
  deriving (Typeable)
deriving instance (Data body,DataId id) => Data (MatchGroup id body)

type LMatch id body = Located (Match id body)
-- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi' when in a
--   list

-- For details on above see note [Api annotations] in ApiAnnotation
data Match id body
  = Match {
        m_fun_id_infix :: (Maybe (Located id,Bool)),
          -- fun_id and fun_infix for functions with multiple equations
          -- only present for a RdrName. See note [fun_id in Match]
        m_pats :: [LPat id], -- The patterns
        m_type :: (Maybe (LHsType id)),
                                 -- A type signature for the result of the match
                                 -- Nothing after typechecking
        m_grhss :: (GRHSs id body)
  } deriving (Typeable)
deriving instance (Data body,DataId id) => Data (Match id body)

{-
Note [fun_id in Match]
~~~~~~~~~~~~~~~~~~~~~~

The parser initially creates a FunBind with a single Match in it for
every function definition it sees.

These are then grouped together by getMonoBind into a single FunBind,
where all the Matches are combined.

In the process, all the original FunBind fun_id's bar one are
discarded, including the locations.

This causes a problem for source to source conversions via API
Annotations, so the original fun_ids and infix flags are preserved in
the Match, when it originates from a FunBind.

Example infix function definition requiring individual API Annotations

    (&&&  ) [] [] =  []
    xs    &&&   [] =  xs
    (  &&&  ) [] ys =  ys


-}

isEmptyMatchGroup :: MatchGroup id body -> Bool
isEmptyMatchGroup (MG { mg_alts = ms }) = null ms

matchGroupArity :: MatchGroup id body -> Arity
-- Precondition: MatchGroup is non-empty
-- This is called before type checking, when mg_arg_tys is not set
matchGroupArity (MG { mg_alts = alts })
  | (alt1:_) <- alts = length (hsLMatchPats alt1)
  | otherwise        = panic "matchGroupArity"

hsLMatchPats :: LMatch id body -> [LPat id]
hsLMatchPats (L _ (Match _ pats _ _)) = pats

-- | GRHSs are used both for pattern bindings and for Matches
--
--  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnVbar',
--        'ApiAnnotation.AnnEqual','ApiAnnotation.AnnWhere',
--        'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'
--        'ApiAnnotation.AnnRarrow','ApiAnnotation.AnnSemi'

-- For details on above see note [Api annotations] in ApiAnnotation
data GRHSs id body
  = GRHSs {
      grhssGRHSs :: [LGRHS id body],       -- ^ Guarded RHSs
      grhssLocalBinds :: (HsLocalBinds id) -- ^ The where clause
    } deriving (Typeable)
deriving instance (Data body,DataId id) => Data (GRHSs id body)

type LGRHS id body = Located (GRHS id body)

-- | Guarded Right Hand Side.
data GRHS id body = GRHS [GuardLStmt id] -- Guards
                         body            -- Right hand side
  deriving (Typeable)
deriving instance (Data body,DataId id) => Data (GRHS id body)

-- We know the list must have at least one @Match@ in it.

pprMatches :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
           => HsMatchContext idL -> MatchGroup idR body -> SDoc
pprMatches ctxt (MG { mg_alts = matches })
    = vcat (map (pprMatch ctxt) (map unLoc matches))
      -- Don't print the type; it's only a place-holder before typechecking

-- Exported to HsBinds, which can't see the defn of HsMatchContext
pprFunBind :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
           => idL -> Bool -> MatchGroup idR body -> SDoc
pprFunBind fun inf matches = pprMatches (FunRhs fun inf) matches

-- Exported to HsBinds, which can't see the defn of HsMatchContext
pprPatBind :: forall bndr id body. (OutputableBndr bndr, OutputableBndr id, Outputable body)
           => LPat bndr -> GRHSs id body -> SDoc
pprPatBind pat (grhss)
 = sep [ppr pat, nest 2 (pprGRHSs (PatBindRhs :: HsMatchContext id) grhss)]

pprMatch :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
         => HsMatchContext idL -> Match idR body -> SDoc
pprMatch ctxt (Match _ pats maybe_ty grhss)
  = sep [ sep (herald : map (nest 2 . pprParendLPat) other_pats)
        , nest 2 ppr_maybe_ty
        , nest 2 (pprGRHSs ctxt grhss) ]
  where
    (herald, other_pats)
        = case ctxt of
            FunRhs fun is_infix
                | not is_infix -> (pprPrefixOcc fun, pats)
                        -- f x y z = e
                        -- Not pprBndr; the AbsBinds will
                        -- have printed the signature

                | null pats2 -> (pp_infix, [])
                        -- x &&& y = e

                | otherwise -> (parens pp_infix, pats2)
                        -- (x &&& y) z = e
                where
                  pp_infix = pprParendLPat pat1 <+> pprInfixOcc fun <+> pprParendLPat pat2

            LambdaExpr -> (char '\\', pats)

            _  -> ASSERT( null pats1 )
                  (ppr pat1, [])        -- No parens around the single pat

    (pat1:pats1) = pats
    (pat2:pats2) = pats1
    ppr_maybe_ty = case maybe_ty of
                        Just ty -> dcolon <+> ppr ty
                        Nothing -> empty


pprGRHSs :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
         => HsMatchContext idL -> GRHSs idR body -> SDoc
pprGRHSs ctxt (GRHSs grhss binds)
  = vcat (map (pprGRHS ctxt . unLoc) grhss)
 $$ ppUnless (isEmptyLocalBinds binds)
      (text "where" $$ nest 4 (pprBinds binds))

pprGRHS :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
        => HsMatchContext idL -> GRHS idR body -> SDoc
pprGRHS ctxt (GRHS [] body)
 =  pp_rhs ctxt body

pprGRHS ctxt (GRHS guards body)
 = sep [char '|' <+> interpp'SP guards, pp_rhs ctxt body]

pp_rhs :: Outputable body => HsMatchContext idL -> body -> SDoc
pp_rhs ctxt rhs = matchSeparator ctxt <+> pprDeeper (ppr rhs)

{-
************************************************************************
*                                                                      *
\subsection{Do stmts and list comprehensions}
*                                                                      *
************************************************************************
-}

type LStmt id body = Located (StmtLR id id body)
type LStmtLR idL idR body = Located (StmtLR idL idR body)

type Stmt id body = StmtLR id id body

type CmdLStmt   id = LStmt id (LHsCmd  id)
type CmdStmt    id = Stmt  id (LHsCmd  id)
type ExprLStmt  id = LStmt id (LHsExpr id)
type ExprStmt   id = Stmt  id (LHsExpr id)

type GuardLStmt id = LStmt id (LHsExpr id)
type GuardStmt  id = Stmt  id (LHsExpr id)
type GhciLStmt  id = LStmt id (LHsExpr id)
type GhciStmt   id = Stmt  id (LHsExpr id)

-- The SyntaxExprs in here are used *only* for do-notation and monad
-- comprehensions, which have rebindable syntax. Otherwise they are unused.
-- | API Annotations when in qualifier lists or guards
--  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnVbar',
--         'ApiAnnotation.AnnComma','ApiAnnotation.AnnThen',
--         'ApiAnnotation.AnnBy','ApiAnnotation.AnnBy',
--         'ApiAnnotation.AnnGroup','ApiAnnotation.AnnUsing'

-- For details on above see note [Api annotations] in ApiAnnotation
data StmtLR idL idR body -- body should always be (LHs**** idR)
  = LastStmt  -- Always the last Stmt in ListComp, MonadComp, PArrComp,
              -- and (after the renamer) DoExpr, MDoExpr
              -- Not used for GhciStmtCtxt, PatGuard, which scope over other stuff
                body
                Bool               -- True <=> return was stripped by ApplicativeDo
                (SyntaxExpr idR)   -- The return operator, used only for
                                   -- MonadComp For ListComp, PArrComp, we
                                   -- use the baked-in 'return' For DoExpr,
                                   -- MDoExpr, we don't apply a 'return' at
                                   -- all See Note [Monad Comprehensions] |
                                   -- - 'ApiAnnotation.AnnKeywordId' :
                                   -- 'ApiAnnotation.AnnLarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | BindStmt (LPat idL)
             body
             (SyntaxExpr idR) -- The (>>=) operator; see Note [The type of bind]
             (SyntaxExpr idR) -- The fail operator
             -- The fail operator is noSyntaxExpr
             -- if the pattern match can't fail
  -- | 'ApplicativeStmt' represents an applicative expression built with
  -- <$> and <*>.  It is generated by the renamer, and is desugared into the
  -- appropriate applicative expression by the desugarer, but it is intended
  -- to be invisible in error messages.
  --
  -- For full details, see Note [ApplicativeDo] in RnExpr
  --
  | ApplicativeStmt
             [ ( SyntaxExpr idR
               , ApplicativeArg idL) ]
                      -- [(<$>, e1), (<*>, e2), ..., (<*>, en)]
             (Maybe (SyntaxExpr idR))  -- 'join', if necessary
             (PostTc idR Type)     -- Type of the body


  | BodyStmt body              -- See Note [BodyStmt]
             (SyntaxExpr idR)  -- The (>>) operator
             (SyntaxExpr idR)  -- The `guard` operator; used only in MonadComp
                               -- See notes [Monad Comprehensions]
             (PostTc idR Type) -- Element type of the RHS (used for arrows)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLet'
  --          'ApiAnnotation.AnnOpen' @'{'@,'ApiAnnotation.AnnClose' @'}'@,

  -- For details on above see note [Api annotations] in ApiAnnotation
  | LetStmt  (HsLocalBindsLR idL idR)

  -- ParStmts only occur in a list/monad comprehension
  | ParStmt  [ParStmtBlock idL idR]
             (SyntaxExpr idR)           -- Polymorphic `mzip` for monad comprehensions
             (SyntaxExpr idR)           -- The `>>=` operator
                                        -- See notes [Monad Comprehensions]
            -- After renaming, the ids are the binders
            -- bound by the stmts and used after themp

  | TransStmt {
      trS_form  :: TransForm,
      trS_stmts :: [ExprLStmt idL],   -- Stmts to the *left* of the 'group'
                                      -- which generates the tuples to be grouped

      trS_bndrs :: [(idR, idR)],      -- See Note [TransStmt binder map]

      trS_using :: LHsExpr idR,
      trS_by :: Maybe (LHsExpr idR),  -- "by e" (optional)
        -- Invariant: if trS_form = GroupBy, then grp_by = Just e

      trS_ret :: SyntaxExpr idR,      -- The monomorphic 'return' function for
                                      -- the inner monad comprehensions
      trS_bind :: SyntaxExpr idR,     -- The '(>>=)' operator
      trS_fmap :: SyntaxExpr idR      -- The polymorphic 'fmap' function for desugaring
                                      -- Only for 'group' forms
    }                                 -- See Note [Monad Comprehensions]

  -- Recursive statement (see Note [How RecStmt works] below)
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRec'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | RecStmt
     { recS_stmts :: [LStmtLR idL idR body]

        -- The next two fields are only valid after renaming
     , recS_later_ids :: [idR] -- The ids are a subset of the variables bound by the
                               -- stmts that are used in stmts that follow the RecStmt

     , recS_rec_ids :: [idR]   -- Ditto, but these variables are the "recursive" ones,
                               -- that are used before they are bound in the stmts of
                               -- the RecStmt.
        -- An Id can be in both groups
        -- Both sets of Ids are (now) treated monomorphically
        -- See Note [How RecStmt works] for why they are separate

        -- Rebindable syntax
     , recS_bind_fn :: SyntaxExpr idR -- The bind function
     , recS_ret_fn  :: SyntaxExpr idR -- The return function
     , recS_mfix_fn :: SyntaxExpr idR -- The mfix function

        -- These fields are only valid after typechecking
     , recS_later_rets :: [PostTcExpr] -- (only used in the arrow version)
     , recS_rec_rets :: [PostTcExpr] -- These expressions correspond 1-to-1
                                     -- with recS_later_ids and recS_rec_ids,
                                     -- and are the expressions that should be
                                     -- returned by the recursion.
                                     -- They may not quite be the Ids themselves,
                                     -- because the Id may be *polymorphic*, but
                                     -- the returned thing has to be *monomorphic*,
                                     -- so they may be type applications

      , recS_ret_ty :: PostTc idR Type -- The type of
                                       -- do { stmts; return (a,b,c) }
                                   -- With rebindable syntax the type might not
                                   -- be quite as simple as (m (tya, tyb, tyc)).
      }
  deriving (Typeable)
deriving instance (Data body, DataId idL, DataId idR)
  => Data (StmtLR idL idR body)

data TransForm   -- The 'f' below is the 'using' function, 'e' is the by function
  = ThenForm     -- then f               or    then f by e             (depending on trS_by)
  | GroupForm    -- then group using f   or    then group by e using f (depending on trS_by)
  deriving (Data, Typeable)

data ParStmtBlock idL idR
  = ParStmtBlock
        [ExprLStmt idL]
        [idR]              -- The variables to be returned
        (SyntaxExpr idR)   -- The return operator
  deriving( Typeable )
deriving instance (DataId idL, DataId idR) => Data (ParStmtBlock idL idR)

-- | Applicative Argument
data ApplicativeArg idL
  = ApplicativeArgOne      -- A single statement (BindStmt or BodyStmt)
      (LPat idL)           -- WildPat if it was a BodyStmt (see below)
      (LHsExpr idL)
      Bool                 -- True <=> was a BodyStmt
                           -- False <=> was a BindStmt
                           -- See Note [Applicative BodyStmt]

  | ApplicativeArgMany     -- do { stmts; return vars }
      [ExprLStmt idL]      -- stmts
      (HsExpr idL)         -- return (v1,..,vn), or just (v1,..,vn)
      (LPat idL)           -- (v1,...,vn)
  deriving( Typeable )
deriving instance (DataId idL) => Data (ApplicativeArg idL)

{-
Note [The type of bind in Stmts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some Stmts, notably BindStmt, keep the (>>=) bind operator.
We do NOT assume that it has type
    (>>=) :: m a -> (a -> m b) -> m b
In some cases (see Trac #303, #1537) it might have a more
exotic type, such as
    (>>=) :: m i j a -> (a -> m j k b) -> m i k b
So we must be careful not to make assumptions about the type.
In particular, the monad may not be uniform throughout.

Note [TransStmt binder map]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The [(idR,idR)] in a TransStmt behaves as follows:

  * Before renaming: []

  * After renaming:
          [ (x27,x27), ..., (z35,z35) ]
    These are the variables
       bound by the stmts to the left of the 'group'
       and used either in the 'by' clause,
                or     in the stmts following the 'group'
    Each item is a pair of identical variables.

  * After typechecking:
          [ (x27:Int, x27:[Int]), ..., (z35:Bool, z35:[Bool]) ]
    Each pair has the same unique, but different *types*.

Note [BodyStmt]
~~~~~~~~~~~~~~~
BodyStmts are a bit tricky, because what they mean
depends on the context.  Consider the following contexts:

        A do expression of type (m res_ty)
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        * BodyStmt E any_ty:   do { ....; E; ... }
                E :: m any_ty
          Translation: E >> ...

        A list comprehensions of type [elt_ty]
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        * BodyStmt E Bool:   [ .. | .... E ]
                        [ .. | ..., E, ... ]
                        [ .. | .... | ..., E | ... ]
                E :: Bool
          Translation: if E then fail else ...

        A guard list, guarding a RHS of type rhs_ty
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        * BodyStmt E BooParStmtBlockl:   f x | ..., E, ... = ...rhs...
                E :: Bool
          Translation: if E then fail else ...

        A monad comprehension of type (m res_ty)
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        * BodyStmt E Bool:   [ .. | .... E ]
                E :: Bool
          Translation: guard E >> ...

Array comprehensions are handled like list comprehensions.

Note [How RecStmt works]
~~~~~~~~~~~~~~~~~~~~~~~~
Example:
   HsDo [ BindStmt x ex

        , RecStmt { recS_rec_ids   = [a, c]
                  , recS_stmts     = [ BindStmt b (return (a,c))
                                     , LetStmt a = ...b...
                                     , BindStmt c ec ]
                  , recS_later_ids = [a, b]

        , return (a b) ]

Here, the RecStmt binds a,b,c; but
  - Only a,b are used in the stmts *following* the RecStmt,
  - Only a,c are used in the stmts *inside* the RecStmt
        *before* their bindings

Why do we need *both* rec_ids and later_ids?  For monads they could be
combined into a single set of variables, but not for arrows.  That
follows from the types of the respective feedback operators:

        mfix :: MonadFix m => (a -> m a) -> m a
        loop :: ArrowLoop a => a (b,d) (c,d) -> a b c

* For mfix, the 'a' covers the union of the later_ids and the rec_ids
* For 'loop', 'c' is the later_ids and 'd' is the rec_ids

Note [Typing a RecStmt]
~~~~~~~~~~~~~~~~~~~~~~~
A (RecStmt stmts) types as if you had written

  (v1,..,vn, _, ..., _) <- mfix (\~(_, ..., _, r1, ..., rm) ->
                                 do { stmts
                                    ; return (v1,..vn, r1, ..., rm) })

where v1..vn are the later_ids
      r1..rm are the rec_ids

Note [Monad Comprehensions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Monad comprehensions require separate functions like 'return' and
'>>=' for desugaring. These functions are stored in the statements
used in monad comprehensions. For example, the 'return' of the 'LastStmt'
expression is used to lift the body of the monad comprehension:

  [ body | stmts ]
   =>
  stmts >>= \bndrs -> return body

In transform and grouping statements ('then ..' and 'then group ..') the
'return' function is required for nested monad comprehensions, for example:

  [ body | stmts, then f, rest ]
   =>
  f [ env | stmts ] >>= \bndrs -> [ body | rest ]

BodyStmts require the 'Control.Monad.guard' function for boolean
expressions:

  [ body | exp, stmts ]
   =>
  guard exp >> [ body | stmts ]

Parallel statements require the 'Control.Monad.Zip.mzip' function:

  [ body | stmts1 | stmts2 | .. ]
   =>
  mzip stmts1 (mzip stmts2 (..)) >>= \(bndrs1, (bndrs2, ..)) -> return body

In any other context than 'MonadComp', the fields for most of these
'SyntaxExpr's stay bottom.
-}

instance (OutputableBndr idL, OutputableBndr idR)
    => Outputable (ParStmtBlock idL idR) where
  ppr (ParStmtBlock stmts _ _) = interpp'SP stmts

instance (OutputableBndr idL, OutputableBndr idR, Outputable body)
         => Outputable (StmtLR idL idR body) where
    ppr stmt = pprStmt stmt

pprStmt :: forall idL idR body . (OutputableBndr idL, OutputableBndr idR, Outputable body)
        => (StmtLR idL idR body) -> SDoc
pprStmt (LastStmt expr ret_stripped _)
  = ifPprDebug (text "[last]") <+>
       (if ret_stripped then text "return" else empty) <+>
       ppr expr
pprStmt (BindStmt pat expr _ _)   = hsep [ppr pat, larrow, ppr expr]
pprStmt (LetStmt binds)           = hsep [ptext (sLit "let"), pprBinds binds]
pprStmt (BodyStmt expr _ _ _)     = ppr expr
pprStmt (ParStmt stmtss _ _)      = sep (punctuate (ptext (sLit " | ")) (map ppr stmtss))

pprStmt (TransStmt { trS_stmts = stmts, trS_by = by, trS_using = using, trS_form = form })
  = sep $ punctuate comma (map ppr stmts ++ [pprTransStmt by using form])

pprStmt (RecStmt { recS_stmts = segment, recS_rec_ids = rec_ids
                 , recS_later_ids = later_ids })
  = ptext (sLit "rec") <+>
    vcat [ ppr_do_stmts segment
         , ifPprDebug (vcat [ ptext (sLit "rec_ids=") <> ppr rec_ids
                            , ptext (sLit "later_ids=") <> ppr later_ids])]
pprStmt (ApplicativeStmt args mb_join _)
  = getPprStyle $ \style ->
      if userStyle style
         then pp_for_user
         else pp_debug
  where
  -- make all the Applicative stuff invisible in error messages by
  -- flattening the whole ApplicativeStmt nest back to a sequence
  -- of statements.
   pp_for_user = vcat $ concatMap flattenArg args

   -- ppr directly rather than transforming here, because we need to
   -- inject a "return" which is hard when we're polymorphic in the id
   -- type.
   flattenStmt :: ExprLStmt idL -> [SDoc]
   flattenStmt (L _ (ApplicativeStmt args _ _)) = concatMap flattenArg args
   flattenStmt stmt = [ppr stmt]

   flattenArg :: forall a . (a, ApplicativeArg idL) -> [SDoc]
   flattenArg (_, ApplicativeArgOne pat expr isBody)
     | isBody =  -- See Note [Applicative BodyStmt]
     [ppr (BodyStmt expr noSyntaxExpr noSyntaxExpr (panic "flattenArg: BodyStmt")
             :: ExprStmt idL)]
     | otherwise =
     [ppr (BindStmt pat expr noSyntaxExpr noSyntaxExpr
             :: ExprStmt idL)]
   flattenArg (_, ApplicativeArgMany stmts _ _) =
     concatMap flattenStmt stmts

   pp_debug =
     let
         ap_expr = sep (punctuate (text " |") (map pp_arg args))
     in
       if isNothing mb_join
          then ap_expr
          else text "join" <+> parens ap_expr

   pp_arg :: (a, ApplicativeArg idL) -> SDoc
   pp_arg (_, ApplicativeArgOne pat expr isBody)
     | isBody =  -- See Note [Applicative BodyStmt]
     ppr (BodyStmt expr noSyntaxExpr noSyntaxExpr (panic "pp_arg: pprStmt")
            :: ExprStmt idL)
     | otherwise =
     ppr (BindStmt pat expr noSyntaxExpr noSyntaxExpr
            :: ExprStmt idL)
   pp_arg (_, ApplicativeArgMany stmts return pat) =
     ppr pat <+>
     text "<-" <+>
     ppr (HsDo DoExpr
               (stmts ++
                   [noLoc (LastStmt (noLoc return) False noSyntaxExpr)]) (panic "pprStmt"))

pprTransformStmt :: OutputableBndr id => [id] -> LHsExpr id -> Maybe (LHsExpr id) -> SDoc
pprTransformStmt bndrs using by
  = sep [ ptext (sLit "then") <+> ifPprDebug (braces (ppr bndrs))
        , nest 2 (ppr using)
        , nest 2 (pprBy by)]

pprTransStmt :: Outputable body => Maybe body -> body -> TransForm -> SDoc
pprTransStmt by using ThenForm
  = sep [ ptext (sLit "then"), nest 2 (ppr using), nest 2 (pprBy by)]
pprTransStmt by using GroupForm
  = sep [ ptext (sLit "then group"), nest 2 (pprBy by), nest 2 (ptext (sLit "using") <+> ppr using)]

pprBy :: Outputable body => Maybe body -> SDoc
pprBy Nothing  = empty
pprBy (Just e) = ptext (sLit "by") <+> ppr e

pprDo :: (OutputableBndr id, Outputable body)
      => HsStmtContext any -> [LStmt id body] -> SDoc
pprDo DoExpr        stmts = ptext (sLit "do")  <+> ppr_do_stmts stmts
pprDo GhciStmtCtxt  stmts = ptext (sLit "do")  <+> ppr_do_stmts stmts
pprDo ArrowExpr     stmts = ptext (sLit "do")  <+> ppr_do_stmts stmts
pprDo MDoExpr       stmts = ptext (sLit "mdo") <+> ppr_do_stmts stmts
pprDo ListComp      stmts = brackets    $ pprComp stmts
pprDo PArrComp      stmts = paBrackets  $ pprComp stmts
pprDo MonadComp     stmts = brackets    $ pprComp stmts
pprDo _             _     = panic "pprDo" -- PatGuard, ParStmtCxt

ppr_do_stmts :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
             => [LStmtLR idL idR body] -> SDoc
-- Print a bunch of do stmts, with explicit braces and semicolons,
-- so that we are not vulnerable to layout bugs
ppr_do_stmts stmts
  = lbrace <+> pprDeeperList vcat (punctuate semi (map ppr stmts))
           <+> rbrace

pprComp :: (OutputableBndr id, Outputable body)
        => [LStmt id body] -> SDoc
pprComp quals     -- Prints:  body | qual1, ..., qualn
  | not (null quals)
  , L _ (LastStmt body _ _) <- last quals
  = hang (ppr body <+> char '|') 2 (pprQuals (dropTail 1 quals))
  | otherwise
  = pprPanic "pprComp" (pprQuals quals)

pprQuals :: (OutputableBndr id, Outputable body)
        => [LStmt id body] -> SDoc
-- Show list comprehension qualifiers separated by commas
pprQuals quals = interpp'SP quals

{-
************************************************************************
*                                                                      *
                Template Haskell quotation brackets
*                                                                      *
************************************************************************
-}

data HsSplice id
   = HsTypedSplice       --  $$z  or $$(f 4)
        SpliceDecoration -- Whether $$( ) variant found, for pretty printing
        id               -- A unique name to identify this splice point
        (LHsExpr id)     -- See Note [Pending Splices]

   | HsUntypedSplice     --  $z  or $(f 4)
        SpliceDecoration -- Whether $( ) variant found, for pretty printing
        id               -- A unique name to identify this splice point
        (LHsExpr id)     -- See Note [Pending Splices]

   | HsQuasiQuote        -- See Note [Quasi-quote overview] in TcSplice
        id               -- Splice point
        id               -- Quoter
        SrcSpan          -- The span of the enclosed string
        FastString       -- The enclosed string

   -- AZ:TODO: use XSplice instead of HsSpliced
   | HsSpliced  -- See Note [Delaying modFinalizers in untyped splices] in
                -- RnSplice.
                -- This is the result of splicing a splice. It is produced by
                -- the renamer and consumed by the typechecker. It lives only
                -- between the two.
        ThModFinalizers     -- TH finalizers produced by the splice.
        (HsSplicedThing id) -- The result of splicing

deriving instance (DataId id) => Data (HsSplice id)

-- | A splice can appear with various decorations wrapped around it. This data
-- type captures explicitly how it was originally written, for use in the pretty
-- printer.
data SpliceDecoration
  = HasParens -- ^ $( splice ) or $$( splice )
  | HasDollar -- ^ $splice or $$splice
  | NoParens  -- ^ bare splice
  deriving (Data, Eq, Show)

instance Outputable SpliceDecoration where
  ppr x = text $ show x

isTypedSplice :: HsSplice id -> Bool
isTypedSplice (HsTypedSplice {}) = True
isTypedSplice _                  = False   -- Quasi-quotes are untyped splices

-- | Finalizers produced by a splice with
-- 'Language.Eta.Meta.Syntax.addModFinalizer'
--
-- See Note [Delaying modFinalizers in untyped splices] in RnSplice. For how
-- this is used.
--
-- Think of the type as ForeignRef (TH.Q ()).
newtype ThModFinalizers = ThModFinalizers [ForeignRef ()]

-- A Data instance which ignores the argument of 'ThModFinalizers'.
instance Data ThModFinalizers where
  gunfold _ z _ = z $ ThModFinalizers []
  toConstr  a   = mkConstr (dataTypeOf a) "ThModFinalizers" [] Data.Prefix
  dataTypeOf a  = mkDataType "HsExpr.ThModFinalizers" [toConstr a]

-- | Haskell Spliced Thing
--
-- Values that can result from running a splice.
data HsSplicedThing id
    = HsSplicedExpr (HsExpr id) -- ^ Haskell Spliced Expression
    | HsSplicedTy   (HsType id) -- ^ Haskell Spliced Type
    | HsSplicedPat  (Pat id)    -- ^ Haskell Spliced Pattern

deriving instance (DataId id) => Data (HsSplicedThing id)

-- See Note [Pending Splices]
type SplicePointName = Name

-- | Pending Renamer Splice
data PendingRnSplice
  = PendingRnSplice UntypedSpliceFlavour SplicePointName (LHsExpr Name)

 deriving Data

data UntypedSpliceFlavour
  = UntypedExpSplice
  | UntypedPatSplice
  | UntypedTypeSplice
  | UntypedDeclSplice
  deriving Data

-- | Pending Type-checker Splice
data PendingTcSplice
  -- AZ:TODO: The hard-coded Id feels wrong.
  = PendingTcSplice SplicePointName (LHsExpr Id)
 deriving Data

{-
Note [Pending Splices]
~~~~~~~~~~~~~~~~~~~~~~
When we rename an untyped bracket, we name and lift out all the nested
splices, so that when the typechecker hits the bracket, it can
typecheck those nested splices without having to walk over the untyped
bracket code.  So for example
    [| f $(g x) |]
looks like

    HsBracket (HsApp (HsVar "f") (HsSpliceE _ (g x)))

which the renamer rewrites to

    HsRnBracketOut (HsApp (HsVar f) (HsSpliceE sn (g x)))
                   [PendingRnSplice UntypedExpSplice sn (g x)]

* The 'sn' is the Name of the splice point, the SplicePointName

* The PendingRnExpSplice gives the splice that splice-point name maps to;
  and the typechecker can now conveniently find these sub-expressions

* The other copy of the splice, in the second argument of HsSpliceE
                                in the renamed first arg of HsRnBracketOut
  is used only for pretty printing

There are four varieties of pending splices generated by the renamer,
distinguished by their UntypedSpliceFlavour

 * Pending expression splices (UntypedExpSplice), e.g.,
       [|$(f x) + 2|]

   UntypedExpSplice is also used for
     * quasi-quotes, where the pending expression expands to
          $(quoter "...blah...")
       (see RnSplice.makePending, HsQuasiQuote case)

     * cross-stage lifting, where the pending expression expands to
          $(lift x)
       (see RnSplice.checkCrossStageLifting)

 * Pending pattern splices (UntypedPatSplice), e.g.,
       [| \$(f x) -> x |]

 * Pending type splices (UntypedTypeSplice), e.g.,
       [| f :: $(g x) |]

 * Pending declaration (UntypedDeclSplice), e.g.,
       [| let $(f x) in ... |]

There is a fifth variety of pending splice, which is generated by the type
checker:

  * Pending *typed* expression splices, (PendingTcSplice), e.g.,
        [||1 + $$(f 2)||]

It would be possible to eliminate HsRnBracketOut and use HsBracketOut for the
output of the renamer. However, when pretty printing the output of the renamer,
e.g., in a type error message, we *do not* want to print out the pending
splices. In contrast, when pretty printing the output of the type checker, we
*do* want to print the pending splices. So splitting them up seems to make
sense, although I hate to add another constructor to HsExpr.
-}

instance (OutputableBndr id)
       => Outputable (HsSplicedThing id) where
  ppr (HsSplicedExpr e) = ppr_expr e
  ppr (HsSplicedTy   t) = ppr t
  ppr (HsSplicedPat  p) = ppr p

instance (OutputableBndr id) => Outputable (HsSplice id) where
  ppr s = pprSplice s

pprPendingSplice :: (OutputableBndr id)
                 => SplicePointName -> LHsExpr id -> SDoc
pprPendingSplice n e = angleBrackets (ppr n <> comma <+> ppr e)

pprSpliceDecl ::  (OutputableBndr id) => HsSplice id -> SpliceExplicitFlag -> SDoc
pprSpliceDecl e@HsQuasiQuote{} _ = pprSplice e
pprSpliceDecl e ExplicitSplice   = text "$(" <> ppr_splice_decl e <> text ")"
pprSpliceDecl e ImplicitSplice   = ppr_splice_decl e

ppr_splice_decl :: (OutputableBndr id) => HsSplice id -> SDoc
ppr_splice_decl (HsUntypedSplice _ n e) = ppr_splice empty n e empty
ppr_splice_decl e = pprSplice e

pprSplice :: (OutputableBndr id) => HsSplice id -> SDoc
pprSplice (HsTypedSplice HasParens n e)
  = ppr_splice (text "$$(") n e (text ")")
pprSplice (HsTypedSplice HasDollar n e)
  = ppr_splice (text "$$") n e empty
pprSplice (HsTypedSplice NoParens n e)
  = ppr_splice empty n e empty
pprSplice (HsUntypedSplice HasParens  n e)
  = ppr_splice (text "$(") n e (text ")")
pprSplice (HsUntypedSplice HasDollar n e)
  = ppr_splice (text "$")  n e empty
pprSplice (HsUntypedSplice NoParens n e)
  = ppr_splice empty  n e empty
pprSplice (HsQuasiQuote n q _ s)      = ppr_quasi n q s
pprSplice (HsSpliced _ thing)         = ppr thing

ppr_quasi :: OutputableBndr p => p -> p -> FastString -> SDoc
ppr_quasi n quoter quote = ifPprDebug (brackets (ppr n)) <>
                           char '[' <> ppr quoter <> vbar <>
                           ppr quote <> text "|]"

ppr_splice :: (OutputableBndr id)
           => SDoc -> id -> LHsExpr id -> SDoc -> SDoc
ppr_splice herald n e trail
    = herald <> ifPprDebug (brackets (ppr n)) <> ppr e <> trail

data HsBracket id = ExpBr (LHsExpr id)   -- [|  expr  |]
                  | PatBr (LPat id)      -- [p| pat   |]
                  | DecBrL [LHsDecl id]  -- [d| decls |]; result of parser
                  | DecBrG (HsGroup id)  -- [d| decls |]; result of renamer
                  | TypBr (LHsType id)   -- [t| type  |]
                  | VarBr Bool id        -- True: 'x, False: ''T
                                         -- (The Bool flag is used only in pprHsBracket)
                  | TExpBr (LHsExpr id)  -- [||  expr  ||]
  deriving (Typeable)
deriving instance (DataId id) => Data (HsBracket id)

isTypedBracket :: HsBracket id -> Bool
isTypedBracket (TExpBr {}) = True
isTypedBracket _           = False

instance OutputableBndr id => Outputable (HsBracket id) where
  ppr = pprHsBracket

pprHsBracket :: OutputableBndr id => HsBracket id -> SDoc
pprHsBracket (ExpBr e)   = thBrackets empty (ppr e)
pprHsBracket (PatBr p)   = thBrackets (char 'p') (ppr p)
pprHsBracket (DecBrG gp) = thBrackets (char 'd') (ppr gp)
pprHsBracket (DecBrL ds) = thBrackets (char 'd') (vcat (map ppr ds))
pprHsBracket (TypBr t)   = thBrackets (char 't') (ppr t)
pprHsBracket (VarBr True n)  = char '\''         <> ppr n
pprHsBracket (VarBr False n) = ptext (sLit "''") <> ppr n
pprHsBracket (TExpBr e)  = thTyBrackets (ppr e)

thBrackets :: SDoc -> SDoc -> SDoc
thBrackets pp_kind pp_body = char '[' <> pp_kind <> char '|' <+>
                             pp_body <+> ptext (sLit "|]")

thTyBrackets :: SDoc -> SDoc
thTyBrackets pp_body = ptext (sLit "[||") <+> pp_body <+> ptext (sLit "||]")

instance Outputable PendingRnSplice where
  ppr (PendingRnSplice _ n e) = pprPendingSplice n e

instance Outputable PendingTcSplice where
  ppr (PendingTcSplice n e) = pprPendingSplice n e

{-
************************************************************************
*                                                                      *
\subsection{Enumerations and list comprehensions}
*                                                                      *
************************************************************************
-}

data ArithSeqInfo id
  = From            (LHsExpr id)
  | FromThen        (LHsExpr id)
                    (LHsExpr id)
  | FromTo          (LHsExpr id)
                    (LHsExpr id)
  | FromThenTo      (LHsExpr id)
                    (LHsExpr id)
                    (LHsExpr id)
  deriving (Typeable)
deriving instance (DataId id) => Data (ArithSeqInfo id)

instance OutputableBndr id => Outputable (ArithSeqInfo id) where
    ppr (From e1)             = hcat [ppr e1, pp_dotdot]
    ppr (FromThen e1 e2)      = hcat [ppr e1, comma, space, ppr e2, pp_dotdot]
    ppr (FromTo e1 e3)        = hcat [ppr e1, pp_dotdot, ppr e3]
    ppr (FromThenTo e1 e2 e3)
      = hcat [ppr e1, comma, space, ppr e2, pp_dotdot, ppr e3]

pp_dotdot :: SDoc
pp_dotdot = ptext (sLit " .. ")

{-
************************************************************************
*                                                                      *
\subsection{HsMatchCtxt}
*                                                                      *
************************************************************************
-}

data HsMatchContext id  -- Context of a Match
  = FunRhs id Bool              -- Function binding for f; True <=> written infix
  | LambdaExpr                  -- Patterns of a lambda
  | CaseAlt                     -- Patterns and guards on a case alternative
  | IfAlt                       -- Guards of a multi-way if alternative
  | ProcExpr                    -- Patterns of a proc
  | PatBindRhs                  -- A pattern binding  eg [y] <- e = e

  | RecUpd                      -- Record update [used only in DsExpr to
                                --    tell matchWrapper what sort of
                                --    runtime error message to generate]

  | StmtCtxt (HsStmtContext id) -- Pattern of a do-stmt, list comprehension,
                                -- pattern guard, etc

  | ThPatSplice                 -- A Template Haskell pattern splice
  | ThPatQuote                  -- A Template Haskell pattern quotation [p| (a,b) |]
  | PatSyn                      -- A pattern synonym declaration
  deriving (Data, Typeable)

data HsStmtContext id
  = ListComp
  | MonadComp
  | PArrComp                             -- Parallel array comprehension

  | DoExpr                               -- do { ... }
  | MDoExpr                              -- mdo { ... }  ie recursive do-expression
  | ArrowExpr                            -- do-notation in an arrow-command context

  | GhciStmtCtxt                         -- A command-line Stmt in GHCi pat <- rhs
  | PatGuard (HsMatchContext id)         -- Pattern guard for specified thing
  | ParStmtCtxt (HsStmtContext id)       -- A branch of a parallel stmt
  | TransStmtCtxt (HsStmtContext id)     -- A branch of a transform stmt
  deriving (Data, Typeable)

isListCompExpr :: HsStmtContext id -> Bool
-- Uses syntax [ e | quals ]
isListCompExpr ListComp          = True
isListCompExpr PArrComp          = True
isListCompExpr MonadComp         = True
isListCompExpr (ParStmtCtxt c)   = isListCompExpr c
isListCompExpr (TransStmtCtxt c) = isListCompExpr c
isListCompExpr _                 = False

isMonadCompExpr :: HsStmtContext id -> Bool
isMonadCompExpr MonadComp            = True
isMonadCompExpr (ParStmtCtxt ctxt)   = isMonadCompExpr ctxt
isMonadCompExpr (TransStmtCtxt ctxt) = isMonadCompExpr ctxt
isMonadCompExpr _                    = False

matchSeparator :: HsMatchContext id -> SDoc
matchSeparator (FunRhs {})  = ptext (sLit "=")
matchSeparator CaseAlt      = ptext (sLit "->")
matchSeparator IfAlt        = ptext (sLit "->")
matchSeparator LambdaExpr   = ptext (sLit "->")
matchSeparator ProcExpr     = ptext (sLit "->")
matchSeparator PatBindRhs   = ptext (sLit "=")
matchSeparator (StmtCtxt _) = ptext (sLit "<-")
matchSeparator RecUpd       = panic "unused"
matchSeparator ThPatSplice  = panic "unused"
matchSeparator ThPatQuote   = panic "unused"
matchSeparator PatSyn       = panic "unused"

pprMatchContext :: Outputable id => HsMatchContext id -> SDoc
pprMatchContext ctxt
  | want_an ctxt = ptext (sLit "an") <+> pprMatchContextNoun ctxt
  | otherwise    = ptext (sLit "a")  <+> pprMatchContextNoun ctxt
  where
    want_an (FunRhs {}) = True  -- Use "an" in front
    want_an ProcExpr    = True
    want_an _           = False

pprMatchContextNoun :: Outputable id => HsMatchContext id -> SDoc
pprMatchContextNoun (FunRhs fun _)  = ptext (sLit "equation for")
                                      <+> quotes (ppr fun)
pprMatchContextNoun CaseAlt         = ptext (sLit "case alternative")
pprMatchContextNoun IfAlt           = ptext (sLit "multi-way if alternative")
pprMatchContextNoun RecUpd          = ptext (sLit "record-update construct")
pprMatchContextNoun ThPatSplice     = ptext (sLit "Template Haskell pattern splice")
pprMatchContextNoun ThPatQuote      = ptext (sLit "Template Haskell pattern quotation")
pprMatchContextNoun PatBindRhs      = ptext (sLit "pattern binding")
pprMatchContextNoun LambdaExpr      = ptext (sLit "lambda abstraction")
pprMatchContextNoun ProcExpr        = ptext (sLit "arrow abstraction")
pprMatchContextNoun (StmtCtxt ctxt) = ptext (sLit "pattern binding in")
                                      $$ pprStmtContext ctxt
pprMatchContextNoun PatSyn          = ptext (sLit "pattern synonym declaration")

-----------------
pprAStmtContext, pprStmtContext :: Outputable id => HsStmtContext id -> SDoc
pprAStmtContext ctxt = article <+> pprStmtContext ctxt
  where
    pp_an = ptext (sLit "an")
    pp_a  = ptext (sLit "a")
    article = case ctxt of
                  MDoExpr       -> pp_an
                  PArrComp      -> pp_an
                  GhciStmtCtxt  -> pp_an
                  _             -> pp_a


-----------------
pprStmtContext GhciStmtCtxt    = ptext (sLit "interactive Eta REPL command")
pprStmtContext DoExpr          = ptext (sLit "'do' block")
pprStmtContext MDoExpr         = ptext (sLit "'mdo' block")
pprStmtContext ArrowExpr       = ptext (sLit "'do' block in an arrow command")
pprStmtContext ListComp        = ptext (sLit "list comprehension")
pprStmtContext MonadComp       = ptext (sLit "monad comprehension")
pprStmtContext PArrComp        = ptext (sLit "array comprehension")
pprStmtContext (PatGuard ctxt) = ptext (sLit "pattern guard for") $$ pprMatchContext ctxt

-- Drop the inner contexts when reporting errors, else we get
--     Unexpected transform statement
--     in a transformed branch of
--          transformed branch of
--          transformed branch of monad comprehension
pprStmtContext (ParStmtCtxt c)
 | opt_PprStyle_Debug = sep [ptext (sLit "parallel branch of"), pprAStmtContext c]
 | otherwise          = pprStmtContext c
pprStmtContext (TransStmtCtxt c)
 | opt_PprStyle_Debug = sep [ptext (sLit "transformed branch of"), pprAStmtContext c]
 | otherwise          = pprStmtContext c


-- Used to generate the string for a *runtime* error message
matchContextErrString :: Outputable id => HsMatchContext id -> SDoc
matchContextErrString (FunRhs fun _)             = ptext (sLit "function") <+> ppr fun
matchContextErrString CaseAlt                    = ptext (sLit "case")
matchContextErrString IfAlt                      = ptext (sLit "multi-way if")
matchContextErrString PatBindRhs                 = ptext (sLit "pattern binding")
matchContextErrString RecUpd                     = ptext (sLit "record update")
matchContextErrString LambdaExpr                 = ptext (sLit "lambda")
matchContextErrString ProcExpr                   = ptext (sLit "proc")
matchContextErrString ThPatSplice                = panic "matchContextErrString"  -- Not used at runtime
matchContextErrString ThPatQuote                 = panic "matchContextErrString"  -- Not used at runtime
matchContextErrString PatSyn                     = panic "matchContextErrString"  -- Not used at runtime
matchContextErrString (StmtCtxt (ParStmtCtxt c))   = matchContextErrString (StmtCtxt c)
matchContextErrString (StmtCtxt (TransStmtCtxt c)) = matchContextErrString (StmtCtxt c)
matchContextErrString (StmtCtxt (PatGuard _))      = ptext (sLit "pattern guard")
matchContextErrString (StmtCtxt GhciStmtCtxt)      = ptext (sLit "interactive Eta REPL command")
matchContextErrString (StmtCtxt DoExpr)            = ptext (sLit "'do' block")
matchContextErrString (StmtCtxt ArrowExpr)         = ptext (sLit "'do' block")
matchContextErrString (StmtCtxt MDoExpr)           = ptext (sLit "'mdo' block")
matchContextErrString (StmtCtxt ListComp)          = ptext (sLit "list comprehension")
matchContextErrString (StmtCtxt MonadComp)         = ptext (sLit "monad comprehension")
matchContextErrString (StmtCtxt PArrComp)          = ptext (sLit "array comprehension")

pprMatchInCtxt :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
               => HsMatchContext idL -> Match idR body -> SDoc
pprMatchInCtxt ctxt match  = hang (ptext (sLit "In") <+> pprMatchContext ctxt <> colon)
                             4 (pprMatch ctxt match)

pprStmtInCtxt :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
               => HsStmtContext idL -> StmtLR idL idR body -> SDoc
pprStmtInCtxt ctxt (LastStmt e _ _)
  | isListCompExpr ctxt      -- For [ e | .. ], do not mutter about "stmts"
  = hang (ptext (sLit "In the expression:")) 2 (ppr e)

pprStmtInCtxt ctxt stmt
  = hang (ptext (sLit "In a stmt of") <+> pprAStmtContext ctxt <> colon)
       2 (ppr_stmt stmt)
  where
    -- For Group and Transform Stmts, don't print the nested stmts!
    ppr_stmt (TransStmt { trS_by = by, trS_using = using
                        , trS_form = form }) = pprTransStmt by using form
    ppr_stmt stmt = pprStmt stmt
