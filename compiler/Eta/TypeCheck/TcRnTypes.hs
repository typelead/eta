{-
(c) The University of Glasgow 2006-2012
(c) The GRASP Project, Glasgow University, 1992-2002


Various types used during typechecking, please see TcRnMonad as well for
operations on these types. You probably want to import it, instead of this
module.

All the monads exported here are built on top of the same IOEnv monad. The
monad functions like a Reader monad in the way it passes the environment
around. This is done to allow the environment to be manipulated in a stack
like fashion when entering expressions... ect.

For state that is global and should be returned at the end (e.g not part
of the stack mechanism), you should use an TcRef (= IORef) to store them.
-}

{-# LANGUAGE CPP, ExistentialQuantification #-}

module Eta.TypeCheck.TcRnTypes(
        TcRnIf, TcRn, TcM, RnM, IfM, IfL, IfG, -- The monad is opaque outside this module
        TcRef,

        -- The environment types
        Env(..),
        TcGblEnv(..), TcLclEnv(..),
        IfGblEnv(..), IfLclEnv(..),

        -- Ranamer types
        ErrCtxt, RecFieldEnv(..),
        ImportAvails(..), emptyImportAvails, plusImportAvails,
        WhereFrom(..), mkModDeps, modDepsElts,

        -- Typechecker types
        TcTypeEnv, TcIdBinder(..),
        TcTyThing(..), PromotionErr(..),
        SelfBootInfo(..),
        pprTcTyThingCategory, pprPECategory,

        -- Desugaring types
        DsM, DsLclEnv(..), DsGblEnv(..), PArrBuiltin(..),
        DsMetaEnv, DsMetaVal(..),

        -- Template Haskell
        ThStage(..), SpliceType(..), PendingStuff(..), topStage, topAnnStage, topSpliceStage,
        ThLevel, impLevel, outerLevel, thLevel,

        -- Arrows
        ArrowCtxt(..),

        -- Canonical constraints
        Xi, Ct(..), Cts, emptyCts, andCts, andManyCts, pprCts,
        singleCt, listToCts, ctsElts, consCts, snocCts, extendCtsList,
        isEmptyCts, isCTyEqCan, isCFunEqCan,
        isCDictCan_Maybe, isCFunEqCan_maybe,
        isCIrredEvCan, isCNonCanonical, isWantedCt, isDerivedCt,
        isGivenCt, isHoleCt, isTypedHoleCt, isPartialTypeSigCt,
        isUserTypeErrorCt, getUserTypeErrorMsg,
        ctEvidence, ctLoc, ctPred, ctFlavour, ctEqRel,
        mkNonCanonical, mkNonCanonicalCt,
        ctEvPred, ctEvLoc, ctEvEqRel,
        ctEvTerm, ctEvCoercion, ctEvId, ctEvCheckDepth,

        WantedConstraints(..), insolubleWC, emptyWC, isEmptyWC,
        andWC, unionsWC, addSimples, addImplics, mkSimpleWC, mkImplicWC, addInsols,
        insolublesOnly, dropDerivedWC,

        Implication(..),
        SubGoalCounter(..),
        SubGoalDepth, initialSubGoalDepth, maxSubGoalDepth,
        bumpSubGoalDepth, subGoalCounterValue, subGoalDepthExceeded,
        CtLoc(..), ctLocSpan, ctLocEnv, ctLocOrigin,
        ctLocDepth, bumpCtLocDepth,
        setCtLocOrigin, setCtLocEnv, setCtLocSpan,
        CtOrigin(..), pprCtOrigin,
        pushErrCtxt, pushErrCtxtSameOrigin,

        SkolemInfo(..),

        CtEvidence(..),
        mkGivenLoc,
        isWanted, isGiven, isDerived,
        ctEvRole,

        -- Constraint solver plugins
        TcPlugin(..), TcPluginResult(..), TcPluginSolver,
        TcPluginM, runTcPluginM, unsafeTcPluginTcM,

        CtFlavour(..), ctEvFlavour,

        -- Pretty printing
        pprEvVarTheta,
        pprEvVars, pprEvVarWithType,
        pprArising, pprArisingAt,

        -- Misc other types
        TcId, TcIdSet, HoleSort(..),
        NameShape(..), ContextElement(..), HowMuch(..), TypeError(..),
        HeraldContext(..), pprSigCtxt, pprMatchInCtxt, pprStmtInCtxt, perhapsForallMsg,
        pprUserTypeErrorTy, pprUserTypeErrorTy', Rank(..),
        rankZeroMonoType, tyConArgMonoType, synArgMonoType,
        funArgResRank, forAllAllowed, InstInfo(..), iDFunId, pprInstInfoDetails, InstBindings(..)
  ) where

import Eta.HsSyn.HsSyn
import Eta.Core.CoreSyn
import Eta.Main.HscTypes
import Eta.TypeCheck.TcEvidence
import Eta.Types.Type
import Eta.Types.Class    ( Class, classTyVars )
import Eta.BasicTypes.ConLike  ( ConLike(..) )
import Eta.BasicTypes.DataCon  ( DataCon, dataConUserType, dataConOrigArgTys, dataConTyCon )
import Eta.BasicTypes.PatSyn   ( PatSyn, patSynType )
import Eta.Prelude.TysWiredIn ( coercibleClass )
import Eta.TypeCheck.TcType
import Eta.Main.Annotations
import Eta.Types.InstEnv
import Eta.Types.FamInstEnv
import Eta.Utils.IOEnv
import Eta.Utils.Util
import Eta.BasicTypes.RdrName
import Eta.BasicTypes.Name
import Eta.BasicTypes.NameEnv
import Eta.BasicTypes.NameSet
import Eta.BasicTypes.Avail
import Eta.BasicTypes.Var
import Eta.BasicTypes.VarEnv
import Eta.BasicTypes.Module
import Eta.BasicTypes.SrcLoc
import Eta.BasicTypes.VarSet
import Eta.Main.ErrUtils
import Eta.Main.Error
import Eta.Utils.UniqFM
import Eta.BasicTypes.UniqSupply
import Eta.BasicTypes.BasicTypes
import Eta.BasicTypes.Id
import Eta.Utils.Bag
import Eta.Main.DynFlags
import Eta.Utils.Outputable
import Eta.Utils.ListSetOps
import Eta.Utils.FastString
import Eta.DeSugar.PmExpr
import GHC.Fingerprint
import Eta.Prelude.PrelNames (forall_tv_RDR, dot_tv_RDR, negateName,
                              typeErrorVAppendDataConName,
                              typeErrorTextDataConName,
                              typeErrorShowTypeDataConName,
                              typeErrorAppendDataConName)
import Eta.Types.TyCon
import Eta.Types.Coercion (pprCoAxBranchHdr)
import Eta.Main.Constants ( mAX_TUPLE_SIZE )
import Data.Set (Set)
import qualified Eta.LanguageExtensions as LangExt
import qualified Data.Set as S
import qualified Language.Eta.Meta as TH
import Control.Monad (ap, liftM, msum)
import Data.Maybe
#ifdef ETA_REPL
import Data.Map      ( Map )
import Data.Dynamic  ( Dynamic )
import Data.List     ( sort, intersperse )
import Data.Typeable ( TypeRep )
import Eta.REPL.RemoteTypes
#endif

-- | A 'NameShape' is a substitution on 'Name's that can be used
-- to refine the identities of a hole while we are renaming interfaces
-- (see 'RnModIface').  Specifically, a 'NameShape' for
-- 'ns_module_name' @A@, defines a mapping from @{A.T}@
-- (for some 'OccName' @T@) to some arbitrary other 'Name'.
--
-- The most intruiging thing about a 'NameShape', however, is
-- how it's constructed.  A 'NameShape' is *implied* by the
-- exported 'AvailInfo's of the implementor of an interface:
-- if an implementor of signature @<H>@ exports @M.T@, you implicitly
-- define a substitution from @{H.T}@ to @M.T@.  So a 'NameShape'
-- is computed from the list of 'AvailInfo's that are exported
-- by the implementation of a module, or successively merged
-- together by the export lists of signatures which are joining
-- together.
--
-- It's not the most obvious way to go about doing this, but it
-- does seem to work!
--
-- NB: Can't boot this and put it in NameShape because then we
-- start pulling in too many DynFlags things.
data NameShape = NameShape {
        ns_mod_name :: ModuleName,
        ns_exports :: [AvailInfo],
        ns_map :: OccEnv Name
    }

{-
************************************************************************
*                                                                      *
               Standard monad definition for TcRn
    All the combinators for the monad can be found in TcRnMonad
*                                                                      *
************************************************************************

The monad itself has to be defined here, because it is mentioned by ErrCtxt
-}

type TcRnIf a b = IOEnv (Env a b)
type TcRn       = TcRnIf TcGblEnv TcLclEnv    -- Type inference
type IfM lcl    = TcRnIf IfGblEnv lcl         -- Iface stuff
type IfG        = IfM ()                      --    Top level
type IfL        = IfM IfLclEnv                --    Nested
type DsM        = TcRnIf DsGblEnv DsLclEnv    -- Desugaring

-- TcRn is the type-checking and renaming monad: the main monad that
-- most type-checking takes place in.  The global environment is
-- 'TcGblEnv', which tracks all of the top-level type-checking
-- information we've accumulated while checking a module, while the
-- local environment is 'TcLclEnv', which tracks local information as
-- we move inside expressions.

-- | Historical "renaming monad" (now it's just 'TcRn').
type RnM  = TcRn

-- | Historical "type-checking monad" (now it's just 'TcRn').
type TcM  = TcRn

-- We 'stack' these envs through the Reader like monad infastructure
-- as we move into an expression (although the change is focused in
-- the lcl type).
data Env gbl lcl
  = Env {
        env_top  :: !HscEnv,  -- Top-level stuff that never changes
                             -- Includes all info about imported things
                             -- BangPattern is to fix leak, see #15111

        env_us   :: {-# UNPACK #-} !(IORef UniqSupply),
                             -- Unique supply for local varibles

        env_gbl  :: gbl,     -- Info about things defined at the top level
                             -- of the module being compiled

        env_lcl  :: lcl      -- Nested stuff; changes as we go into
    }

instance ContainsDynFlags (Env gbl lcl) where
    extractDynFlags env = hsc_dflags (env_top env)
    replaceDynFlags env dflags
        = env {env_top = replaceDynFlags (env_top env) dflags}

instance ContainsModule gbl => ContainsModule (Env gbl lcl) where
    extractModule env = extractModule (env_gbl env)


{-
************************************************************************
*                                                                      *
                The interface environments
              Used when dealing with IfaceDecls
*                                                                      *
************************************************************************
-}

data IfGblEnv
  = IfGblEnv {
        -- The type environment for the module being compiled,
        -- in case the interface refers back to it via a reference that
        -- was originally a hi-boot file.
        -- We need the module name so we can test when it's appropriate
        -- to look in this env.
        -- See Note [Tying the knot] in TcIface
        if_rec_types :: Maybe (Module, IfG TypeEnv)
                -- Allows a read effect, so it can be in a mutable
                -- variable; c.f. handling the external package type env
                -- Nothing => interactive stuff, no loops possible
    }

data IfLclEnv
  = IfLclEnv {
        -- The module for the current IfaceDecl
        -- So if we see   f = \x -> x
        -- it means M.f = \x -> x, where M is the if_mod
        -- NB: This is a semantic module, see
        -- Note [Identity versus semantic module]
        if_mod :: Module,

        -- The field is used only for error reporting
        -- if (say) there's a Lint error in it
        if_boot :: Bool,

        if_loc :: SDoc,
                -- Where the interface came from:
                --      .hi file, or GHCi state, or ext core
                -- plus which bit is currently being examined
        if_nsubst :: Maybe NameShape,
        if_tv_env  :: UniqFM TyVar,     -- Nested tyvar bindings
                                        -- (and coercions)
        if_id_env  :: UniqFM Id         -- Nested id binding
    }

{-
************************************************************************
*                                                                      *
                Desugarer monad
*                                                                      *
************************************************************************

Now the mondo monad magic (yes, @DsM@ is a silly name)---carry around
a @UniqueSupply@ and some annotations, which
presumably include source-file location information:
-}

-- If '-XParallelArrays' is given, the desugarer populates this table with the corresponding
-- variables found in 'Data.Array.Parallel'.
--
data PArrBuiltin
        = PArrBuiltin
        { lengthPVar         :: Var     -- ^ lengthP
        , replicatePVar      :: Var     -- ^ replicateP
        , singletonPVar      :: Var     -- ^ singletonP
        , mapPVar            :: Var     -- ^ mapP
        , filterPVar         :: Var     -- ^ filterP
        , zipPVar            :: Var     -- ^ zipP
        , crossMapPVar       :: Var     -- ^ crossMapP
        , indexPVar          :: Var     -- ^ (!:)
        , emptyPVar          :: Var     -- ^ emptyP
        , appPVar            :: Var     -- ^ (+:+)
        , enumFromToPVar     :: Var     -- ^ enumFromToP
        , enumFromThenToPVar :: Var     -- ^ enumFromThenToP
        }

data DsGblEnv
        = DsGblEnv
        { ds_mod          :: Module             -- For SCC profiling
        , ds_fam_inst_env :: FamInstEnv         -- Like tcg_fam_inst_env
        , ds_unqual  :: PrintUnqualified
        , ds_msgs    :: IORef Messages          -- Warning messages
        , ds_if_env  :: (IfGblEnv, IfLclEnv)    -- Used for looking up global,
                                                -- possibly-imported things
        , ds_dph_env :: GlobalRdrEnv            -- exported entities of 'Data.Array.Parallel.Prim'
                                                -- iff '-fvectorise' flag was given as well as
                                                -- exported entities of 'Data.Array.Parallel' iff
                                                -- '-XParallelArrays' was given; otherwise, empty
        , ds_parr_bi :: PArrBuiltin             -- desugarar names for '-XParallelArrays'
        , ds_static_binds :: IORef [(Fingerprint, (Id,CoreExpr))]
          -- ^ Bindings resulted from floating static forms
        }

instance ContainsModule DsGblEnv where
    extractModule = ds_mod

data DsLclEnv = DsLclEnv {
        dsl_meta    :: DsMetaEnv,        -- Template Haskell bindings
        dsl_loc     :: RealSrcSpan,      -- To put in pattern-matching error msgs
        dsl_dicts   :: Bag EvVar,        -- Constraints from GADT pattern-matching
        dsl_tm_cs   :: Bag SimpleEq,
        dsl_pm_iter :: IORef Int         -- no iterations for pmcheck
     }

-- Inside [| |] brackets, the desugarer looks
-- up variables in the DsMetaEnv
type DsMetaEnv = NameEnv DsMetaVal

data DsMetaVal
   = DsBound Id         -- Bound by a pattern inside the [| |].
                        -- Will be dynamically alpha renamed.
                        -- The Id has type THSyntax.Var

   | DsSplice (HsExpr Id) -- These bindings are introduced by
                          -- the PendingSplices on a HsBracketOut


{-
************************************************************************
*                                                                      *
                Global typechecker environment
*                                                                      *
************************************************************************
-}

-- | 'TcGblEnv' describes the top-level of the module at the
-- point at which the typechecker is finished work.
-- It is this structure that is handed on to the desugarer
-- For state that needs to be updated during the typechecking
-- phase and returned at end, use a 'TcRef' (= 'IORef').
data TcGblEnv
  = TcGblEnv {
        tcg_mod          :: Module,         -- ^ Module being compiled
        tcg_semantic_mod :: Module,    -- ^ If a signature, the backing module
            -- See also Note [Identity versus semantic module]
        tcg_src          :: HscSource,
          -- ^ What kind of module (regular Haskell, hs-boot, ext-core)
        -- tcg_sig_of       :: Maybe Module,
        --   -- ^ Are we being compiled as a signature of an implementation?
        -- tcg_impl_rdr_env :: Maybe GlobalRdrEnv,
          -- ^ Environment used only during -sig-of for resolving top level
          -- bindings.  See Note [Signature parameters in TcGblEnv and DynFlags]

        tcg_rdr_env      :: GlobalRdrEnv,   -- ^ Top level envt; used during renaming
        tcg_default      :: Maybe [Type],
          -- ^ Types used for defaulting. @Nothing@ => no @default@ decl

        tcg_fix_env        :: FixityEnv,     -- ^ Just for things in this module
        tcg_field_env      :: RecFieldEnv,   -- ^ Just for things in this module
                                        -- See Note [The interactive package] in HscTypes

        tcg_type_env       :: TypeEnv,
          -- ^ Global type env for the module we are compiling now.  All
          -- TyCons and Classes (for this module) end up in here right away,
          -- along with their derived constructors, selectors.
          --
          -- (Ids defined in this module start in the local envt, though they
          --  move to the global envt during zonking)
          --
          -- NB: for what "things in this module" means, see
          -- Note [The interactive package] in HscTypes

        tcg_type_env_var   :: TcRef TypeEnv,
                -- Used only to initialise the interface-file
                -- typechecker in initIfaceTcRn, so that it can see stuff
                -- bound in this module when dealing with hi-boot recursions
                -- Updated at intervals (e.g. after dealing with types and classes)

        tcg_inst_env       :: !InstEnv,
          -- ^ Instance envt for all /home-package/ modules;
          -- Includes the dfuns in tcg_insts
        -- NB. BangPattern is to fix a leak, see #15111
        tcg_fam_inst_env   :: !FamInstEnv, -- ^ Ditto for family instances
        -- NB. BangPattern is to fix a leak, see #15111
        tcg_ann_env        :: AnnEnv,     -- ^ And for annotations

        tcg_visible_orphan_mods :: ModuleSet,
          -- ^ The set of orphan modules which transitively reachable from
          -- direct imports.  We use this to figure out if an orphan instance
          -- in the global InstEnv should be considered visible.
          -- See Note [Instance lookup and orphan instances] in InstEnv

                -- Now a bunch of things about this module that are simply
                -- accumulated, but never consulted until the end.
                -- Nevertheless, it's convenient to accumulate them along
                -- with the rest of the info from this module.
        tcg_exports :: [AvailInfo],     -- ^ What is exported
        tcg_imports :: ImportAvails,
          -- ^ Information about what was imported from where, including
          -- things bound in this module. Also store Safe Haskell info
          -- here about transative trusted packaage requirements.

        tcg_dus :: DefUses,   -- ^ What is defined in this module and what is used.
        tcg_used_rdrnames :: TcRef (Set RdrName),
          -- See Note [Tracking unused binding and imports]

        tcg_keep :: TcRef NameSet,
          -- ^ Locally-defined top-level names to keep alive.
          --
          -- "Keep alive" means give them an Exported flag, so that the
          -- simplifier does not discard them as dead code, and so that they
          -- are exposed in the interface file (but not to export to the
          -- user).
          --
          -- Some things, like dict-fun Ids and default-method Ids are "born"
          -- with the Exported flag on, for exactly the above reason, but some
          -- we only discover as we go.  Specifically:
          --
          --   * The to/from functions for generic data types
          --
          --   * Top-level variables appearing free in the RHS of an orphan
          --     rule
          --
          --   * Top-level variables appearing free in a TH bracket

        tcg_th_used :: TcRef Bool,
          -- ^ @True@ <=> Template Haskell syntax used.
          --
          -- We need this so that we can generate a dependency on the
          -- Template Haskell package, because the desugarer is going
          -- to emit loads of references to TH symbols.  The reference
          -- is implicit rather than explicit, so we have to zap a
          -- mutable variable.

        tcg_th_splice_used :: TcRef Bool,
          -- ^ @True@ <=> A Template Haskell splice was used.
          --
          -- Splices disable recompilation avoidance (see #481)

        tcg_dfun_n  :: TcRef OccSet,
          -- ^ Allows us to choose unique DFun names.
        tcg_merged :: [(Module, Fingerprint)],
          -- ^ The requirements we merged with; we always have to recompile
          -- if any of these changed.


        -- The next fields accumulate the payload of the module
        -- The binds, rules and foreign-decl fields are collected
        -- initially in un-zonked form and are finally zonked in tcRnSrcDecls

        tcg_rn_exports :: Maybe [Located (IE Name)],
        tcg_rn_imports :: [LImportDecl Name],
                -- Keep the renamed imports regardless.  They are not
                -- voluminous and are needed if you want to report unused imports

        tcg_rn_decls :: Maybe (HsGroup Name),
          -- ^ Renamed decls, maybe.  @Nothing@ <=> Don't retain renamed
          -- decls.

        tcg_dependent_files :: TcRef [FilePath], -- ^ dependencies from addDependentFile

#ifdef ETA_REPL
        tcg_th_topdecls :: TcRef [LHsDecl RdrName],
        -- ^ Top-level declarations from addTopDecls

        tcg_th_topnames :: TcRef NameSet,
        -- ^ Exact names bound in top-level declarations in tcg_th_topdecls

        tcg_th_modfinalizers :: TcRef [TcM ()],
        -- ^ Template Haskell module finalizers.
        --
        -- They are computations in the @TcM@ monad rather than @Q@ because we
        -- set them to use particular local environments.

        tcg_th_state :: TcRef (Map TypeRep Dynamic),
        tcg_th_remote_state :: TcRef (Maybe (ForeignRef (IORef ()))), -- QState
        -- ^ Template Haskell state
#endif /* ETA_REPL */

        tcg_ev_binds  :: Bag EvBind,        -- Top-level evidence bindings

        -- Things defined in this module, or (in GHCi)
        -- in the declarations for a single GHCi command.
        -- For the latter, see Note [The interactive package] in HscTypes
        tcg_binds     :: LHsBinds Id,       -- Value bindings in this module
        tcg_sigs      :: NameSet,           -- ...Top-level names that *lack* a signature
        tcg_imp_specs :: [LTcSpecPrag],     -- ...SPECIALISE prags for imported Ids
        tcg_warns     :: Warnings,          -- ...Warnings and deprecations
        tcg_anns      :: [Annotation],      -- ...Annotations
        tcg_tcs       :: [TyCon],           -- ...TyCons and Classes
        tcg_insts     :: [ClsInst],         -- ...Instances
        tcg_fam_insts :: [FamInst],         -- ...Family instances
        tcg_rules     :: [LRuleDecl Id],    -- ...Rules
        tcg_fords     :: [LForeignDecl Id], -- ...Foreign import & exports
        tcg_vects     :: [LVectDecl Id],    -- ...Vectorisation declarations
        tcg_patsyns   :: [PatSyn],          -- ...Pattern synonyms

        tcg_doc_hdr   :: Maybe LHsDocString, -- ^ Maybe Haddock header docs
        tcg_hpc       :: !AnyHpcUsage,        -- ^ @True@ if any part of the
                                             --  prog uses hpc instrumentation.
          -- NB. BangPattern is to fix a leak, see #15111
        tcg_self_boot :: SelfBootInfo,       -- ^ Whether this module has a
                                             -- corresponding hi-boot file
        tcg_main      :: Maybe Name,         -- ^ The Name of the main
                                             -- function, if this module is
                                             -- the main module.
        tcg_safeInfer :: TcRef Bool,         -- Has the typechecker
                                             -- inferred this module
                                             -- as -XSafe (Safe Haskell)

        -- | A list of user-defined plugins for the constraint solver.
        tcg_tc_plugins :: [TcPluginSolver],

        tcg_top_loc :: RealSrcSpan,
        -- ^ The RealSrcSpan this module came from
        tcg_static_wc :: TcRef WantedConstraints
          -- ^ Wanted constraints of static forms.
        -- See Note [Constraints in static forms].
    }

-- Note [Constraints in static forms]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- When a static form produces constraints like
--
-- f :: StaticPtr (Bool -> String)
-- f = static show
--
-- we collect them in tcg_static_wc and resolve them at the end
-- of type checking. They need to be resolved separately because
-- we don't want to resolve them in the context of the enclosing
-- expression. Consider
--
-- g :: Show a => StaticPtr (a -> String)
-- g = static show
--
-- If the @Show a0@ constraint that the body of the static form produces was
-- resolved in the context of the enclosing expression, then the body of the
-- static form wouldn't be closed because the Show dictionary would come from
-- g's context instead of coming from the top level.
--

instance ContainsModule TcGblEnv where
    extractModule env = tcg_semantic_mod env

data RecFieldEnv
  = RecFields (NameEnv [Name])  -- Maps a constructor name *in this module*
                                -- to the fields for that constructor
              NameSet           -- Set of all fields declared *in this module*;
                                -- used to suppress name-shadowing complaints
                                -- when using record wild cards
                                -- E.g.  let fld = e in C {..}
        -- This is used when dealing with ".." notation in record
        -- construction and pattern matching.
        -- The FieldEnv deals *only* with constructors defined in *this*
        -- module.  For imported modules, we get the same info from the
        -- TypeEnv

data SelfBootInfo
  = NoSelfBoot    -- No corresponding hi-boot file
  | SelfBoot
       { sb_mds :: ModDetails   -- There was a hi-boot file,
       , sb_tcs :: NameSet }    -- and these Ids
  -- We need this info to compute a safe approximation to
  -- recursive loops, to avoid infinite inlinings

{-
Note [Tracking unused binding and imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We gather two sorts of usage information
 * tcg_dus (defs/uses)
      Records *defined* Names (local, top-level)
          and *used*    Names (local or imported)

      Used (a) to report "defined but not used"
               (see RnNames.reportUnusedNames)
           (b) to generate version-tracking usage info in interface
               files (see MkIface.mkUsedNames)
   This usage info is mainly gathered by the renamer's
   gathering of free-variables

 * tcg_used_rdrnames
      Records used *imported* (not locally-defined) RdrNames
      Used only to report unused import declarations
      Notice that they are RdrNames, not Names, so we can
      tell whether the reference was qualified or unqualified, which
      is esssential in deciding whether a particular import decl
      is unnecessary.  This info isn't present in Names.


************************************************************************
*                                                                      *
                The local typechecker environment
*                                                                      *
************************************************************************

Note [The Global-Env/Local-Env story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During type checking, we keep in the tcg_type_env
        * All types and classes
        * All Ids derived from types and classes (constructors, selectors)

At the end of type checking, we zonk the local bindings,
and as we do so we add to the tcg_type_env
        * Locally defined top-level Ids

Why?  Because they are now Ids not TcIds.  This final GlobalEnv is
        a) fed back (via the knot) to typechecking the
           unfoldings of interface signatures
        b) used in the ModDetails of this module
-}

data TcLclEnv           -- Changes as we move inside an expression
                        -- Discarded after typecheck/rename; not passed on to desugarer
  = TcLclEnv {
        tcl_loc        :: RealSrcSpan,     -- Source span
        tcl_ctxt       :: [ErrCtxt],       -- Error context, innermost on top
        tcl_tclvl      :: TcLevel,         -- Birthplace for new unification variables

        tcl_th_ctxt    :: ThStage,         -- Template Haskell context
        tcl_th_bndrs   :: ThBindEnv,       -- Binding level of in-scope Names
                                           -- defined in this module (not imported)

        tcl_arrow_ctxt :: ArrowCtxt,       -- Arrow-notation context

        tcl_rdr :: LocalRdrEnv,         -- Local name envt
                -- Maintained during renaming, of course, but also during
                -- type checking, solely so that when renaming a Template-Haskell
                -- splice we have the right environment for the renamer.
                --
                --   Does *not* include global name envt; may shadow it
                --   Includes both ordinary variables and type variables;
                --   they are kept distinct because tyvar have a different
                --   occurrence contructor (Name.TvOcc)
                -- We still need the unsullied global name env so that
                --   we can look up record field names

        tcl_env  :: TcTypeEnv,    -- The local type environment:
                                  -- Ids and TyVars defined in this module

        tcl_bndrs :: [TcIdBinder],   -- Stack of locally-bound Ids, innermost on top
                                     -- Used only for error reporting

        tcl_tidy :: TidyEnv,      -- Used for tidying types; contains all
                                  -- in-scope type variables (but not term variables)

        tcl_tyvars :: TcRef TcTyVarSet, -- The "global tyvars"
                        -- Namely, the in-scope TyVars bound in tcl_env,
                        -- plus the tyvars mentioned in the types of Ids bound
                        -- in tcl_lenv.
                        -- Why mutable? see notes with tcGetGlobalTyVars

        tcl_lie  :: TcRef WantedConstraints,    -- Place to accumulate type constraints
        tcl_errs :: TcRef Messages              -- Place to accumulate errors
    }

type TcTypeEnv = NameEnv TcTyThing

type ThBindEnv = NameEnv (TopLevelFlag, ThLevel)
   -- Domain = all Ids bound in this module (ie not imported)
   -- The TopLevelFlag tells if the binding is syntactically top level.
   -- We need to know this, because the cross-stage persistence story allows
   -- cross-stage at arbitrary types if the Id is bound at top level.
   --
   -- Nota bene: a ThLevel of 'outerLevel' is *not* the same as being
   -- bound at top level!  See Note [Template Haskell levels] in TcSplice

data TcIdBinder
  = TcIdBndr
       TcId
       TopLevelFlag    -- Tells whether the bindind is syntactically top-level
                       -- (The monomorphic Ids for a recursive group count
                       --  as not-top-level for this purpose.)

{- Note [Given Insts]
   ~~~~~~~~~~~~~~~~~~
Because of GADTs, we have to pass inwards the Insts provided by type signatures
and existential contexts. Consider
        data T a where { T1 :: b -> b -> T [b] }
        f :: Eq a => T a -> Bool
        f (T1 x y) = [x]==[y]

The constructor T1 binds an existential variable 'b', and we need Eq [b].
Well, we have it, because Eq a refines to Eq [b], but we can only spot that if we
pass it inwards.

-}

-- | Type alias for 'IORef'; the convention is we'll use this for mutable
-- bits of data in 'TcGblEnv' which are updated during typechecking and
-- returned at the end.
type TcRef a     = IORef a
-- ToDo: when should I refer to it as a 'TcId' instead of an 'Id'?
type TcId        = Id
type TcIdSet     = IdSet

---------------------------
-- Template Haskell stages and levels
---------------------------

data SpliceType = Typed | Untyped

data ThStage    -- See Note [Template Haskell state diagram] in TcSplice
  = Splice SpliceType -- Inside a top-level splice
                      -- This code will be run *at compile time*;
                      --   the result replaces the splice
                      -- Binding level = 0

  | RunSplice (TcRef [ForeignRef ()])
      -- Set when running a splice, i.e. NOT when renaming or typechecking the
      -- Haskell code for the splice. See Note [RunSplice ThLevel].
      --
      -- Contains a list of mod finalizers collected while executing the splice.
      --
      -- 'addModFinalizer' inserts finalizers here, and from here they are taken
      -- to construct an @HsSpliced@ annotation for untyped splices. See Note
      -- [Delaying modFinalizers in untyped splices] in "RnSplice".
      --
      -- For typed splices, the typechecker takes finalizers from here and
      -- inserts them in the list of finalizers in the global environment.
      --
      -- See Note [Collecting modFinalizers in typed splices] in "TcSplice".

  | Comp        -- Ordinary Haskell code
                -- Binding level = 1

  | Brack                       -- Inside brackets
      ThStage                   --   Enclosing stage
      PendingStuff

data PendingStuff
  = RnPendingUntyped              -- Renaming the inside of an *untyped* bracket
      (TcRef [PendingRnSplice])   -- Pending splices in here

  | RnPendingTyped                -- Renaming the inside of a *typed* bracket

  | TcPending                     -- Typechecking the inside of a typed bracket
      (TcRef [PendingTcSplice])   --   Accumulate pending splices here
      (TcRef WantedConstraints)   --     and type constraints here

topStage, topAnnStage, topSpliceStage :: ThStage
topStage       = Comp
topAnnStage    = Splice Untyped
topSpliceStage = Splice Untyped

instance Outputable ThStage where
   ppr (Splice _)    = text "Splice"
   ppr (RunSplice _) = text "RunSplice"
   ppr Comp          = text "Comp"
   ppr (Brack s _)   = text "Brack" <> parens (ppr s)

type ThLevel = Int
    -- NB: see Note [Template Haskell levels] in TcSplice
    -- Incremented when going inside a bracket,
    -- decremented when going inside a splice
    -- NB: ThLevel is one greater than the 'n' in Fig 2 of the
    --     original "Template meta-programming for Haskell" paper

impLevel, outerLevel :: ThLevel
impLevel = 0    -- Imported things; they can be used inside a top level splice
outerLevel = 1  -- Things defined outside brackets

thLevel :: ThStage -> ThLevel
thLevel (Splice _)  = 0
thLevel (RunSplice _) =
    -- See Note [RunSplice ThLevel].
    panic "thLevel: called when running a splice"
thLevel Comp        = 1
thLevel (Brack s _) = thLevel s + 1

{- Node [RunSplice ThLevel]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The 'RunSplice' stage is set when executing a splice, and only when running a
splice. In particular it is not set when the splice is renamed or typechecked.

'RunSplice' is needed to provide a reference where 'addModFinalizer' can insert
the finalizer (see Note [Delaying modFinalizers in untyped splices]), and
'addModFinalizer' runs when doing Q things. Therefore, It doesn't make sense to
set 'RunSplice' when renaming or typechecking the splice, where 'Splice', 'Brak'
or 'Comp' are used instead.

-}

---------------------------
-- Arrow-notation context
---------------------------

{- Note [Escaping the arrow scope]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In arrow notation, a variable bound by a proc (or enclosed let/kappa)
is not in scope to the left of an arrow tail (-<) or the head of (|..|).
For example

        proc x -> (e1 -< e2)

Here, x is not in scope in e1, but it is in scope in e2.  This can get
a bit complicated:

        let x = 3 in
        proc y -> (proc z -> e1) -< e2

Here, x and z are in scope in e1, but y is not.

We implement this by
recording the environment when passing a proc (using newArrowScope),
and returning to that (using escapeArrowScope) on the left of -< and the
head of (|..|).

All this can be dealt with by the *renamer*. But the type checker needs
to be involved too.  Example (arrowfail001)
  class Foo a where foo :: a -> ()
  data Bar = forall a. Foo a => Bar a
  get :: Bar -> ()
  get = proc x -> case x of Bar a -> foo -< a
Here the call of 'foo' gives rise to a (Foo a) constraint that should not
be captured by the pattern match on 'Bar'.  Rather it should join the
constraints from further out.  So we must capture the constraint bag
from further out in the ArrowCtxt that we push inwards.
-}

data ArrowCtxt   -- Note [Escaping the arrow scope]
  = NoArrowCtxt
  | ArrowCtxt LocalRdrEnv (TcRef WantedConstraints)


---------------------------
-- TcTyThing
---------------------------

data TcTyThing
  = AGlobal TyThing             -- Used only in the return type of a lookup

  | ATcId   {           -- Ids defined in this module; may not be fully zonked
        tct_id     :: TcId,
        tct_closed :: TopLevelFlag }   -- See Note [Bindings with closed types]

  | ATyVar  Name TcTyVar        -- The type variable to which the lexically scoped type
                                -- variable is bound. We only need the Name
                                -- for error-message purposes; it is the corresponding
                                -- Name in the domain of the envt

  | AThing  TcKind   -- Used temporarily, during kind checking, for the
                     -- tycons and clases in this recursive group
                     -- Can be a mono-kind or a poly-kind; in TcTyClsDcls see
                     -- Note [Type checking recursive type and class declarations]

  | APromotionErr PromotionErr

data PromotionErr
  = TyConPE          -- TyCon used in a kind before we are ready
                     --     data T :: T -> * where ...
  | ClassPE          -- Ditto Class

  | FamDataConPE     -- Data constructor for a data family
                     -- See Note [AFamDataCon: not promoting data family constructors] in TcRnDriver

  | RecDataConPE     -- Data constructor in a recursive loop
                     -- See Note [ARecDataCon: recusion and promoting data constructors] in TcTyClsDecls
  | NoDataKinds      -- -XDataKinds not enabled

instance Outputable TcTyThing where     -- Debugging only
   ppr (AGlobal g)      = pprTyThing g
   ppr elt@(ATcId {})   = text "Identifier" <>
                          brackets (ppr (tct_id elt) <> dcolon
                                 <> ppr (varType (tct_id elt)) <> comma
                                 <+> ppr (tct_closed elt))
   ppr (ATyVar n tv)    = text "Type variable" <+> quotes (ppr n) <+> equals <+> ppr tv
   ppr (AThing k)       = text "AThing" <+> ppr k
   ppr (APromotionErr err) = text "APromotionErr" <+> ppr err

instance Outputable PromotionErr where
  ppr ClassPE      = text "ClassPE"
  ppr TyConPE      = text "TyConPE"
  ppr FamDataConPE = text "FamDataConPE"
  ppr RecDataConPE = text "RecDataConPE"
  ppr NoDataKinds  = text "NoDataKinds"

pprTcTyThingCategory :: TcTyThing -> SDoc
pprTcTyThingCategory (AGlobal thing)    = pprTyThingCategory thing
pprTcTyThingCategory (ATyVar {})        = ptext (sLit "Type variable")
pprTcTyThingCategory (ATcId {})         = ptext (sLit "Local identifier")
pprTcTyThingCategory (AThing {})        = ptext (sLit "Kinded thing")
pprTcTyThingCategory (APromotionErr pe) = pprPECategory pe

pprPECategory :: PromotionErr -> SDoc
pprPECategory ClassPE      = ptext (sLit "Class")
pprPECategory TyConPE      = ptext (sLit "Type constructor")
pprPECategory FamDataConPE = ptext (sLit "Data constructor")
pprPECategory RecDataConPE = ptext (sLit "Data constructor")
pprPECategory NoDataKinds  = ptext (sLit "Data constructor")

{-
Note [Bindings with closed types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  f x = let g ys = map not ys
        in ...

Can we generalise 'g' under the OutsideIn algorithm?  Yes,
because all g's free variables are top-level; that is they themselves
have no free type variables, and it is the type variables in the
environment that makes things tricky for OutsideIn generalisation.

Definition:

   A variable is "closed", and has tct_closed set to TopLevel,
      iff
   a) all its free variables are imported, or are themselves closed
   b) generalisation is not restricted by the monomorphism restriction

Under OutsideIn we are free to generalise a closed let-binding.
This is an extension compared to the JFP paper on OutsideIn, which
used "top-level" as a proxy for "closed".  (It's not a good proxy
anyway -- the MR can make a top-level binding with a free type
variable.)

Note that:
  * A top-level binding may not be closed, if it suffer from the MR

  * A nested binding may be closed (eg 'g' in the example we started with)
    Indeed, that's the point; whether a function is defined at top level
    or nested is orthogonal to the question of whether or not it is closed

  * A binding may be non-closed because it mentions a lexically scoped
    *type variable*  Eg
        f :: forall a. blah
        f x = let g y = ...(y::a)...
-}

type ErrCtxt = (Bool, TidyEnv -> TcM (TidyEnv, ContextElement))
        -- Monadic so that we have a chance
        -- to deal with bound type variables just before error
        -- message construction

        -- Bool:  True <=> this is a landmark context; do not
        --                 discard it when trimming for display

{-
************************************************************************
*                                                                      *
        Operations over ImportAvails
*                                                                      *
************************************************************************
-}

-- | 'ImportAvails' summarises what was imported from where, irrespective of
-- whether the imported things are actually used or not.  It is used:
--
--  * when processing the export list,
--
--  * when constructing usage info for the interface file,
--
--  * to identify the list of directly imported modules for initialisation
--    purposes and for optimised overlap checking of family instances,
--
--  * when figuring out what things are really unused
--
data ImportAvails
   = ImportAvails {
        imp_mods :: ImportedMods,
          --      = ModuleEnv [(ModuleName, Bool, SrcSpan, Bool)],
          -- ^ Domain is all directly-imported modules
          -- The 'ModuleName' is what the module was imported as, e.g. in
          -- @
          --     import Foo as Bar
          -- @
          -- it is @Bar@.
          --
          -- The 'Bool' means:
          --
          --  - @True@ => import was @import Foo ()@
          --
          --  - @False@ => import was some other form
          --
          -- Used
          --
          --   (a) to help construct the usage information in the interface
          --       file; if we import something we need to recompile if the
          --       export version changes
          --
          --   (b) to specify what child modules to initialise
          --
          -- We need a full ModuleEnv rather than a ModuleNameEnv here,
          -- because we might be importing modules of the same name from
          -- different packages. (currently not the case, but might be in the
          -- future).

        imp_dep_mods :: ModuleNameEnv (ModuleName, IsBootInterface),
          -- ^ Home-package modules needed by the module being compiled
          --
          -- It doesn't matter whether any of these dependencies
          -- are actually /used/ when compiling the module; they
          -- are listed if they are below it at all.  For
          -- example, suppose M imports A which imports X.  Then
          -- compiling M might not need to consult X.hi, but X
          -- is still listed in M's dependencies.

        imp_dep_pkgs :: Set InstalledUnitId,
          -- ^ Packages needed by the module being compiled, whether directly,
          -- or via other modules in this package, or via modules imported
          -- from other packages.

        imp_trust_pkgs :: Set InstalledUnitId,
          -- ^ This is strictly a subset of imp_dep_pkgs and records the
          -- packages the current module needs to trust for Safe Haskell
          -- compilation to succeed. A package is required to be trusted if
          -- we are dependent on a trustworthy module in that package.
          -- While perhaps making imp_dep_pkgs a tuple of (UnitId, Bool)
          -- where True for the bool indicates the package is required to be
          -- trusted is the more logical  design, doing so complicates a lot
          -- of code not concerned with Safe Haskell.
          -- See Note [RnNames . Tracking Trust Transitively]

        imp_trust_own_pkg :: Bool,
          -- ^ Do we require that our own package is trusted?
          -- This is to handle efficiently the case where a Safe module imports
          -- a Trustworthy module that resides in the same package as it.
          -- See Note [RnNames . Trust Own Package]

        imp_orphs :: [Module],
          -- ^ Orphan modules below us in the import tree (and maybe including
          -- us for imported modules)

        imp_finsts :: [Module]
          -- ^ Family instance modules below us in the import tree (and maybe
          -- including us for imported modules)
      }

mkModDeps :: [(ModuleName, IsBootInterface)]
          -> ModuleNameEnv (ModuleName, IsBootInterface)
mkModDeps deps = foldl add emptyUFM deps
               where
                 add env elt@(m,_) = addToUFM env m elt

modDepsElts
 :: ModuleNameEnv (ModuleName, IsBootInterface)
 -> [(ModuleName, IsBootInterface)]
modDepsElts = sort . nonDetEltsUFM
 -- It's OK to use nonDetEltsUFM here because sorting by module names
 -- restores determinism

emptyImportAvails :: ImportAvails
emptyImportAvails = ImportAvails { imp_mods          = emptyModuleEnv,
                                   imp_dep_mods      = emptyUFM,
                                   imp_dep_pkgs      = S.empty,
                                   imp_trust_pkgs    = S.empty,
                                   imp_trust_own_pkg = False,
                                   imp_orphs         = [],
                                   imp_finsts        = [] }

-- | Union two ImportAvails
--
-- This function is a key part of Import handling, basically
-- for each import we create a separate ImportAvails structure
-- and then union them all together with this function.
plusImportAvails ::  ImportAvails ->  ImportAvails ->  ImportAvails
plusImportAvails
  (ImportAvails { imp_mods = mods1,
                  imp_dep_mods = dmods1, imp_dep_pkgs = dpkgs1,
                  imp_trust_pkgs = tpkgs1, imp_trust_own_pkg = tself1,
                  imp_orphs = orphs1, imp_finsts = finsts1 })
  (ImportAvails { imp_mods = mods2,
                  imp_dep_mods = dmods2, imp_dep_pkgs = dpkgs2,
                  imp_trust_pkgs = tpkgs2, imp_trust_own_pkg = tself2,
                  imp_orphs = orphs2, imp_finsts = finsts2 })
  = ImportAvails { imp_mods          = plusModuleEnv_C (++) mods1 mods2,
                   imp_dep_mods      = plusUFM_C plus_mod_dep dmods1 dmods2,
                   imp_dep_pkgs      = dpkgs1 `S.union` dpkgs2,
                   imp_trust_pkgs    = tpkgs1 `S.union` tpkgs2,
                   imp_trust_own_pkg = tself1 || tself2,
                   imp_orphs         = orphs1 `unionLists` orphs2,
                   imp_finsts        = finsts1 `unionLists` finsts2 }
  where
    plus_mod_dep (m1, boot1) (_, boot2)
        = --WARN( not (m1 == m2), (ppr m1 <+> ppr m2) $$ (ppr boot1 <+> ppr boot2) )
                -- Check mod-names match
          (m1, boot1 && boot2) -- If either side can "see" a non-hi-boot interface, use that

{-
************************************************************************
*                                                                      *
\subsection{Where from}
*                                                                      *
************************************************************************

The @WhereFrom@ type controls where the renamer looks for an interface file
-}

data WhereFrom
  = ImportByUser IsBootInterface        -- Ordinary user import (perhaps {-# SOURCE #-})
  | ImportBySystem                      -- Non user import.
  | ImportByPlugin                      -- Importing a plugin;
                                        -- See Note [Care with plugin imports] in LoadIface

instance Outputable WhereFrom where
  ppr (ImportByUser is_boot) | is_boot     = ptext (sLit "{- SOURCE -}")
                             | otherwise   = empty
  ppr ImportBySystem                       = ptext (sLit "{- SYSTEM -}")
  ppr ImportByPlugin                       = ptext (sLit "{- PLUGIN -}")

{-
************************************************************************
*                                                                      *
*                       Canonical constraints                          *
*                                                                      *
*   These are the constraints the low-level simplifier works with      *
*                                                                      *
************************************************************************
-}

-- The syntax of xi types:
-- xi ::= a | T xis | xis -> xis | ... | forall a. tau
-- Two important notes:
--      (i) No type families, unless we are under a ForAll
--      (ii) Note that xi types can contain unexpanded type synonyms;
--           however, the (transitive) expansions of those type synonyms
--           will not contain any type functions, unless we are under a ForAll.
-- We enforce the structure of Xi types when we flatten (TcCanonical)

type Xi = Type       -- In many comments, "xi" ranges over Xi

type Cts = Bag Ct

data Ct
  -- Atomic canonical constraints
  = CDictCan {  -- e.g.  Num xi
      cc_ev     :: CtEvidence, -- See Note [Ct/evidence invariant]
      cc_class  :: Class,
      cc_tyargs :: [Xi]        -- cc_tyargs are function-free, hence Xi
    }

  | CIrredEvCan {  -- These stand for yet-unusable predicates
      cc_ev :: CtEvidence   -- See Note [Ct/evidence invariant]
        -- The ctev_pred of the evidence is
        -- of form   (tv xi1 xi2 ... xin)
        --      or   (tv1 ~ ty2)   where the CTyEqCan  kind invariant fails
        --      or   (F tys ~ ty)  where the CFunEqCan kind invariant fails
        -- See Note [CIrredEvCan constraints]
    }

  | CTyEqCan {  -- tv ~ rhs
       -- Invariants:
       --   * See Note [Applying the inert substitution] in TcFlatten
       --   * tv not in tvs(rhs)   (occurs check)
       --   * If tv is a TauTv, then rhs has no foralls
       --       (this avoids substituting a forall for the tyvar in other types)
       --   * typeKind ty `subKind` typeKind tv
       --       See Note [Kind orientation for CTyEqCan]
       --   * rhs is not necessarily function-free,
       --       but it has no top-level function.
       --     E.g. a ~ [F b]  is fine
       --     but  a ~ F b    is not
       --   * If the equality is representational, rhs has no top-level newtype
       --     See Note [No top-level newtypes on RHS of representational
       --     equalities] in TcCanonical
       --   * If rhs is also a tv, then it is oriented to give best chance of
       --     unification happening; eg if rhs is touchable then lhs is too
      cc_ev     :: CtEvidence, -- See Note [Ct/evidence invariant]
      cc_tyvar  :: TcTyVar,
      cc_rhs    :: TcType,     -- Not necessarily function-free (hence not Xi)
                               -- See invariants above
      cc_eq_rel :: EqRel
    }

  | CFunEqCan {  -- F xis ~ fsk
       -- Invariants:
       --   * isTypeFamilyTyCon cc_fun
       --   * typeKind (F xis) = tyVarKind fsk
       --   * always Nominal role
       --   * always Given or Wanted, never Derived
      cc_ev     :: CtEvidence,  -- See Note [Ct/evidence invariant]
      cc_fun    :: TyCon,       -- A type function

      cc_tyargs :: [Xi],        -- cc_tyargs are function-free (hence Xi)
        -- Either under-saturated or exactly saturated
        --    *never* over-saturated (because if so
        --    we should have decomposed)

      cc_fsk    :: TcTyVar  -- [Given]  always a FlatSkol skolem
                            -- [Wanted] always a FlatMetaTv unification variable
        -- See Note [The flattening story] in TcFlatten
    }

  | CNonCanonical {        -- See Note [NonCanonical Semantics]
      cc_ev  :: CtEvidence
    }

  | CHoleCan {             -- Treated as an "insoluble" constraint
                           -- See Note [Insoluble constraints]
      cc_ev   :: CtEvidence,
      cc_occ  :: OccName,   -- The name of this hole
      cc_hole :: HoleSort   -- The sort of this hole (expr, type, ...)
    }

-- | Used to indicate which sort of hole we have.
data HoleSort = ExprHole  -- ^ A hole in an expression (TypedHoles)
              | TypeHole  -- ^ A hole in a type (PartialTypeSignatures)

{-
Note [Kind orientation for CTyEqCan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given an equality (t:* ~ s:Open), we can't solve it by updating t:=s,
ragardless of how touchable 't' is, because the kinds don't work.

Instead we absolutely must re-orient it. Reason: if that gets into the
inert set we'll start replacing t's by s's, and that might make a
kind-correct type into a kind error.  After re-orienting,
we may be able to solve by updating s:=t.

Hence in a CTyEqCan, (t:k1 ~ xi:k2) we require that k2 is a subkind of k1.

If the two have incompatible kinds, we just don't use a CTyEqCan at all.
See Note [Equalities with incompatible kinds] in TcCanonical

We can't require *equal* kinds, because
     * wanted constraints don't necessarily have identical kinds
               eg   alpha::? ~ Int
     * a solved wanted constraint becomes a given

Note [Kind orientation for CFunEqCan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For (F xis ~ rhs) we require that kind(lhs) is a subkind of kind(rhs).
This really only maters when rhs is an Open type variable (since only type
variables have Open kinds):
   F ty ~ (a:Open)
which can happen, say, from
      f :: F a b
      f = undefined   -- The a:Open comes from instantiating 'undefined'

Note that the kind invariant is maintained by rewriting.
Eg wanted1 rewrites wanted2; if both were compatible kinds before,
   wanted2 will be afterwards.  Similarly givens.

Caveat:
  - Givens from higher-rank, such as:
          type family T b :: * -> * -> *
          type instance T Bool = (->)

          f :: forall a. ((T a ~ (->)) => ...) -> a -> ...
          flop = f (...) True
     Whereas we would be able to apply the type instance, we would not be able to
     use the given (T Bool ~ (->)) in the body of 'flop'


Note [CIrredEvCan constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CIrredEvCan constraints are used for constraints that are "stuck"
   - we can't solve them (yet)
   - we can't use them to solve other constraints
   - but they may become soluble if we substitute for some
     of the type variables in the constraint

Example 1:  (c Int), where c :: * -> Constraint.  We can't do anything
            with this yet, but if later c := Num, *then* we can solve it

Example 2:  a ~ b, where a :: *, b :: k, where k is a kind variable
            We don't want to use this to substitute 'b' for 'a', in case
            'k' is subequently unifed with (say) *->*, because then
            we'd have ill-kinded types floating about.  Rather we want
            to defer using the equality altogether until 'k' get resolved.

Note [Ct/evidence invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If  ct :: Ct, then extra fields of 'ct' cache precisely the ctev_pred field
of (cc_ev ct), and is fully rewritten wrt the substitution.   Eg for CDictCan,
   ctev_pred (cc_ev ct) = (cc_class ct) (cc_tyargs ct)
This holds by construction; look at the unique place where CDictCan is
built (in TcCanonical).

In contrast, the type of the evidence *term* (ccev_evtm or ctev_evar) in
the evidence may *not* be fully zonked; we are careful not to look at it
during constraint solving.  See Note [Evidence field of CtEvidence]
-}

mkNonCanonical :: CtEvidence -> Ct
mkNonCanonical ev = CNonCanonical { cc_ev = ev }

mkNonCanonicalCt :: Ct -> Ct
mkNonCanonicalCt ct = CNonCanonical { cc_ev = cc_ev ct }

ctEvidence :: Ct -> CtEvidence
ctEvidence = cc_ev

ctLoc :: Ct -> CtLoc
ctLoc = ctEvLoc . ctEvidence

ctPred :: Ct -> PredType
-- See Note [Ct/evidence invariant]
ctPred ct = ctEvPred (cc_ev ct)

-- | Get the flavour of the given 'Ct'
ctFlavour :: Ct -> CtFlavour
ctFlavour = ctEvFlavour . ctEvidence

-- | Get the equality relation for the given 'Ct'
ctEqRel :: Ct -> EqRel
ctEqRel = ctEvEqRel . ctEvidence

dropDerivedWC :: WantedConstraints -> WantedConstraints
-- See Note [Dropping derived constraints]
dropDerivedWC wc@(WC { wc_simple = simples })
  = wc { wc_simple  = filterBag isWantedCt simples }
    -- The wc_impl implications are already (recursively) filtered

{-
Note [Dropping derived constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we discard derived constraints at the end of constraint solving;
see dropDerivedWC.  For example
 * If we have an unsolved (Ord a), we don't want to complain about
   an unsolved (Eq a) as well.

But we keep Derived *insoluble* constraints because they indicate a solid,
comprehensible error.  Particularly:

 * Insolubles Givens indicate unreachable code

 * Insoluble kind equalities (e.g. [D] * ~ (* -> *)) may arise from
   a type equality a ~ Int#, say

 * Insoluble derived wanted equalities (e.g. [D] Int ~ Bool) may
   arise from functional dependency interactions.  We are careful
   to keep a good CtOrigin on such constraints (FunDepOrigin1, FunDepOrigin2)
   so that we can produce a good error message (Trac #9612)

Since we leave these Derived constraints in the residual WantedConstraints,
we must filter them out when we re-process the WantedConstraint,
in TcSimplify.solve_wanteds.


************************************************************************
*                                                                      *
                    CtEvidence
         The "flavor" of a canonical constraint
*                                                                      *
************************************************************************
-}

isWantedCt :: Ct -> Bool
isWantedCt = isWanted . cc_ev

isGivenCt :: Ct -> Bool
isGivenCt = isGiven . cc_ev

isDerivedCt :: Ct -> Bool
isDerivedCt = isDerived . cc_ev

isCTyEqCan :: Ct -> Bool
isCTyEqCan (CTyEqCan {})  = True
isCTyEqCan (CFunEqCan {}) = False
isCTyEqCan _              = False

isCDictCan_Maybe :: Ct -> Maybe Class
isCDictCan_Maybe (CDictCan {cc_class = cls })  = Just cls
isCDictCan_Maybe _              = Nothing

isCIrredEvCan :: Ct -> Bool
isCIrredEvCan (CIrredEvCan {}) = True
isCIrredEvCan _                = False

isCFunEqCan_maybe :: Ct -> Maybe (TyCon, [Type])
isCFunEqCan_maybe (CFunEqCan { cc_fun = tc, cc_tyargs = xis }) = Just (tc, xis)
isCFunEqCan_maybe _ = Nothing

isCFunEqCan :: Ct -> Bool
isCFunEqCan (CFunEqCan {}) = True
isCFunEqCan _ = False

isCNonCanonical :: Ct -> Bool
isCNonCanonical (CNonCanonical {}) = True
isCNonCanonical _ = False

isHoleCt:: Ct -> Bool
isHoleCt (CHoleCan {}) = True
isHoleCt _ = False

-- TODO: Finish backport
-- isOutOfScopeCt :: Ct -> Bool
-- -- We treat expression holes representing out-of-scope variables a bit
-- -- differently when it comes to error reporting
-- isOutOfScopeCt (CHoleCan { cc_hole = ExprHole (OutOfScope {}) }) = True
-- isOutOfScopeCt _ = False

isTypedHoleCt :: Ct -> Bool
isTypedHoleCt (CHoleCan { cc_hole = ExprHole }) = True
isTypedHoleCt _ = False

-- | The following constraints are considered to be a custom type error:
--    1. TypeError msg
--    2. TypeError msg ~ Something  (and the other way around)
--    3. C (TypeError msg)          (for any parameter of class constraint)
getUserTypeErrorMsg :: Ct -> Maybe (Kind, Type)
getUserTypeErrorMsg ct
  | Just (_,t1,t2) <- getEqPredTys_maybe ctT    = oneOf [t1,t2]
  | Just (_,ts)    <- getClassPredTys_maybe ctT = oneOf ts
  | otherwise                                   = isUserErrorTy ctT
  where
  ctT       = ctPred ct
  oneOf xs  = msum (map isUserErrorTy xs)

isUserTypeErrorCt :: Ct -> Bool
isUserTypeErrorCt ct = case getUserTypeErrorMsg ct of
                         Just _ -> True
                         _      -> False

isPartialTypeSigCt :: Ct -> Bool
isPartialTypeSigCt (CHoleCan { cc_hole = TypeHole }) = True
isPartialTypeSigCt _ = False

instance Outputable Ct where
  ppr ct = ppr (cc_ev ct) <+> parens (text ct_sort)
         where ct_sort = case ct of
                           CTyEqCan {}      -> "CTyEqCan"
                           CFunEqCan {}     -> "CFunEqCan"
                           CNonCanonical {} -> "CNonCanonical"
                           CDictCan {}      -> "CDictCan"
                           CIrredEvCan {}   -> "CIrredEvCan"
                           CHoleCan {}      -> "CHoleCan"

singleCt :: Ct -> Cts
singleCt = unitBag

andCts :: Cts -> Cts -> Cts
andCts = unionBags

listToCts :: [Ct] -> Cts
listToCts = listToBag

ctsElts :: Cts -> [Ct]
ctsElts = bagToList

consCts :: Ct -> Cts -> Cts
consCts = consBag

snocCts :: Cts -> Ct -> Cts
snocCts = snocBag

extendCtsList :: Cts -> [Ct] -> Cts
extendCtsList cts xs | null xs   = cts
                     | otherwise = cts `unionBags` listToBag xs

andManyCts :: [Cts] -> Cts
andManyCts = unionManyBags

emptyCts :: Cts
emptyCts = emptyBag

isEmptyCts :: Cts -> Bool
isEmptyCts = isEmptyBag

pprCts :: Cts -> SDoc
pprCts cts = vcat (map ppr (bagToList cts))

{-
************************************************************************
*                                                                      *
                Wanted constraints
     These are forced to be in TcRnTypes because
           TcLclEnv mentions WantedConstraints
           WantedConstraint mentions CtLoc
           CtLoc mentions ErrCtxt
           ErrCtxt mentions TcM
*                                                                      *
v%************************************************************************
-}

data WantedConstraints
  = WC { wc_simple :: Cts              -- Unsolved constraints, all wanted
       , wc_impl   :: Bag Implication
       , wc_insol  :: Cts              -- Insoluble constraints, can be
                                       -- wanted, given, or derived
                                       -- See Note [Insoluble constraints]
    }

emptyWC :: WantedConstraints
emptyWC = WC { wc_simple = emptyBag, wc_impl = emptyBag, wc_insol = emptyBag }

mkSimpleWC :: [Ct] -> WantedConstraints
mkSimpleWC cts
  = WC { wc_simple = listToBag cts, wc_impl = emptyBag, wc_insol = emptyBag }

mkImplicWC :: Bag Implication -> WantedConstraints
mkImplicWC implic
  = WC { wc_simple = emptyBag, wc_impl = implic, wc_insol = emptyBag }

isEmptyWC :: WantedConstraints -> Bool
isEmptyWC (WC { wc_simple = f, wc_impl = i, wc_insol = n })
  = isEmptyBag f && isEmptyBag i && isEmptyBag n

insolubleWC :: WantedConstraints -> Bool
-- True if there are any insoluble constraints in the wanted bag. Ignore
-- constraints arising from PartialTypeSignatures to solve as much of the
-- constraints as possible before reporting the holes.
insolubleWC wc = not (isEmptyBag (filterBag (not . isPartialTypeSigCt)
                                  (wc_insol wc)))
               || anyBag ic_insol (wc_impl wc)

andWC :: WantedConstraints -> WantedConstraints -> WantedConstraints
andWC (WC { wc_simple = f1, wc_impl = i1, wc_insol = n1 })
      (WC { wc_simple = f2, wc_impl = i2, wc_insol = n2 })
  = WC { wc_simple = f1 `unionBags` f2
       , wc_impl   = i1 `unionBags` i2
       , wc_insol  = n1 `unionBags` n2 }

unionsWC :: [WantedConstraints] -> WantedConstraints
unionsWC = foldr andWC emptyWC

addSimples :: WantedConstraints -> Bag Ct -> WantedConstraints
addSimples wc cts
  = wc { wc_simple = wc_simple wc `unionBags` cts }

addImplics :: WantedConstraints -> Bag Implication -> WantedConstraints
addImplics wc implic = wc { wc_impl = wc_impl wc `unionBags` implic }

addInsols :: WantedConstraints -> Bag Ct -> WantedConstraints
addInsols wc cts
  = wc { wc_insol = wc_insol wc `unionBags` cts }

insolublesOnly :: WantedConstraints -> WantedConstraints
-- Keep only the insolubles
insolublesOnly (WC { wc_insol = insols, wc_impl = implics })
  = WC { wc_simple = emptyBag
       , wc_insol  = insols
       , wc_impl = mapBag implic_insols_only implics }
   where
     implic_insols_only implic
       = implic { ic_wanted = insolublesOnly (ic_wanted implic) }

-- insolubleWantedCt :: Ct -> Bool
-- -- Definitely insoluble, in particular /excluding/ type-hole constraints
-- insolubleWantedCt ct
--   | isGivenCt ct     = False              -- See Note [Given insolubles]
--   | isHoleCt ct      = isOutOfScopeCt ct  -- See Note [Insoluble holes]
--   | insolubleEqCt ct = True
--   | otherwise        = False

-- insolubleEqCt :: Ct -> Bool
-- -- Returns True of /equality/ constraints
-- -- that are /definitely/ insoluble
-- -- It won't detect some definite errors like
-- --       F a ~ T (F a)
-- -- where F is a type family, which actually has an occurs check
-- --
-- -- The function is tuned for application /after/ constraint solving
-- --       i.e. assuming canonicalisation has been done
-- -- E.g.  It'll reply True  for     a ~ [a]
-- --               but False for   [a] ~ a
-- -- and
-- --                   True for  Int ~ F a Int
-- --               but False for  Maybe Int ~ F a Int Int
-- --               (where F is an arity-1 type function)
-- insolubleEqCt (CIrredCan { cc_insol = insol }) = insol
-- insolubleEqCt _                                = False

instance Outputable WantedConstraints where
  ppr (WC {wc_simple = s, wc_impl = i, wc_insol = n})
   = ptext (sLit "WC") <+> braces (vcat
        [ ppr_bag (ptext (sLit "wc_simple")) s
        , ppr_bag (ptext (sLit "wc_insol")) n
        , ppr_bag (ptext (sLit "wc_impl")) i ])

ppr_bag :: Outputable a => SDoc -> Bag a -> SDoc
ppr_bag doc bag
 | isEmptyBag bag = empty
 | otherwise      = hang (doc <+> equals)
                       2 (foldrBag (($$) . ppr) empty bag)

{- Note [Given insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (Trac #14325, comment:)
    class (a~b) => C a b

    foo :: C a c => a -> c
    foo x = x

    hm3 :: C (f b) b => b -> f b
    hm3 x = foo x

In the RHS of hm3, from the [G] C (f b) b we get the insoluble
[G] f b ~# b.  Then we also get an unsolved [W] C b (f b).
Residual implication looks like
    forall b. C (f b) b => [G] f b ~# b
                           [W] C f (f b)

We do /not/ want to set the implication status to IC_Insoluble,
because that'll suppress reports of [W] C b (f b).  But we
may not report the insoluble [G] f b ~# b either (see Note [Given errors]
in TcErrors), so we may fail to report anything at all!  Yikes.

Bottom line: insolubleWC (called in TcSimplify.setImplicationStatus)
             should ignore givens even if they are insoluble.

Note [Insoluble holes]
~~~~~~~~~~~~~~~~~~~~~~
Hole constraints that ARE NOT treated as truly insoluble:
  a) type holes, arising from PartialTypeSignatures,
  b) "true" expression holes arising from TypedHoles

An "expression hole" or "type hole" constraint isn't really an error
at all; it's a report saying "_ :: Int" here.  But an out-of-scope
variable masquerading as expression holes IS treated as truly
insoluble, so that it trumps other errors during error reporting.
Yuk!

************************************************************************
*                                                                      *
                Implication constraints
*                                                                      *
************************************************************************
-}

data Implication
  = Implic {
      ic_tclvl :: TcLevel, -- TcLevel: unification variables
                                -- free in the environment

      ic_skols  :: [TcTyVar],    -- Introduced skolems
      ic_info  :: SkolemInfo,    -- See Note [Skolems in an implication]
                                 -- See Note [Shadowing in a constraint]

      ic_given  :: [EvVar],      -- Given evidence variables
                                 --   (order does not matter)
                                 -- See Invariant (GivenInv) in TcType

      ic_no_eqs :: Bool,         -- True  <=> ic_givens have no equalities, for sure
                                 -- False <=> ic_givens might have equalities

      ic_env   :: TcLclEnv,      -- Gives the source location and error context
                                 -- for the implicatdion, and hence for all the
                                 -- given evidence variables

      ic_wanted :: WantedConstraints,  -- The wanted
      ic_insol  :: Bool,               -- True iff insolubleWC ic_wanted is true

      ic_binds  :: EvBindsVar   -- Points to the place to fill in the
                                -- abstraction and bindings
    }

instance Outputable Implication where
  ppr (Implic { ic_tclvl = tclvl, ic_skols = skols
              , ic_given = given, ic_no_eqs = no_eqs
              , ic_wanted = wanted, ic_insol = insol
              , ic_binds = binds, ic_info = info })
   = hang (ptext (sLit "Implic") <+> lbrace)
        2 (sep [ ptext (sLit "TcLevel =") <+> ppr tclvl
               , ptext (sLit "Skolems =") <+> pprTvBndrs skols
               , ptext (sLit "No-eqs =") <+> ppr no_eqs
               , ptext (sLit "Insol =") <+> ppr insol
               , hang (ptext (sLit "Given ="))  2 (pprEvVars given)
               , hang (ptext (sLit "Wanted =")) 2 (ppr wanted)
               , ptext (sLit "Binds =") <+> ppr binds
               , pprSkolInfo info ] <+> rbrace)

{-
Note [Shadowing in a constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We assume NO SHADOWING in a constraint.  Specifically
 * The unification variables are all implicitly quantified at top
   level, and are all unique
 * The skolem varibles bound in ic_skols are all freah when the
   implication is created.
So we can safely substitute. For example, if we have
   forall a.  a~Int => ...(forall b. ...a...)...
we can push the (a~Int) constraint inwards in the "givens" without
worrying that 'b' might clash.

Note [Skolems in an implication]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The skolems in an implication are not there to perform a skolem escape
check.  That happens because all the environment variables are in the
untouchables, and therefore cannot be unified with anything at all,
let alone the skolems.

Instead, ic_skols is used only when considering floating a constraint
outside the implication in TcSimplify.floatEqualities or
TcSimplify.approximateImplications

Note [Insoluble constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some of the errors that we get during canonicalization are best
reported when all constraints have been simplified as much as
possible. For instance, assume that during simplification the
following constraints arise:

 [Wanted]   F alpha ~  uf1
 [Wanted]   beta ~ uf1 beta

When canonicalizing the wanted (beta ~ uf1 beta), if we eagerly fail
we will simply see a message:
    'Can't construct the infinite type  beta ~ uf1 beta'
and the user has no idea what the uf1 variable is.

Instead our plan is that we will NOT fail immediately, but:
    (1) Record the "frozen" error in the ic_insols field
    (2) Isolate the offending constraint from the rest of the inerts
    (3) Keep on simplifying/canonicalizing

At the end, we will hopefully have substituted uf1 := F alpha, and we
will be able to report a more informative error:
    'Can't construct the infinite type beta ~ F alpha beta'

Insoluble constraints *do* include Derived constraints. For example,
a functional dependency might give rise to [D] Int ~ Bool, and we must
report that.  If insolubles did not contain Deriveds, reportErrors would
never see it.


************************************************************************
*                                                                      *
            Pretty printing
*                                                                      *
************************************************************************
-}

pprEvVars :: [EvVar] -> SDoc    -- Print with their types
pprEvVars ev_vars = vcat (map pprEvVarWithType ev_vars)

pprEvVarTheta :: [EvVar] -> SDoc
pprEvVarTheta ev_vars = pprTheta (map evVarPred ev_vars)

pprEvVarWithType :: EvVar -> SDoc
pprEvVarWithType v = ppr v <+> dcolon <+> pprType (evVarPred v)

{-
************************************************************************
*                                                                      *
            CtEvidence
*                                                                      *
************************************************************************

Note [Evidence field of CtEvidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During constraint solving we never look at the type of ctev_evtm, or
ctev_evar; instead we look at the cte_pred field.  The evtm/evar field
may be un-zonked.
-}

data CtEvidence
  = CtGiven { ctev_pred :: TcPredType      -- See Note [Ct/evidence invariant]
            , ctev_evtm :: EvTerm          -- See Note [Evidence field of CtEvidence]
            , ctev_loc  :: CtLoc }
    -- Truly given, not depending on subgoals
    -- NB: Spontaneous unifications belong here

  | CtWanted { ctev_pred :: TcPredType     -- See Note [Ct/evidence invariant]
             , ctev_evar :: EvVar          -- See Note [Evidence field of CtEvidence]
             , ctev_loc  :: CtLoc }
    -- Wanted goal

  | CtDerived { ctev_pred :: TcPredType
              , ctev_loc  :: CtLoc }
    -- A goal that we don't really have to solve and can't immediately
    -- rewrite anything other than a derived (there's no evidence!)
    -- but if we do manage to solve it may help in solving other goals.

ctEvPred :: CtEvidence -> TcPredType
-- The predicate of a flavor
ctEvPred = ctev_pred

ctEvLoc :: CtEvidence -> CtLoc
ctEvLoc = ctev_loc

-- | Get the equality relation relevant for a 'CtEvidence'
ctEvEqRel :: CtEvidence -> EqRel
ctEvEqRel = predTypeEqRel . ctEvPred

-- | Get the role relevant for a 'CtEvidence'
ctEvRole :: CtEvidence -> Role
ctEvRole = eqRelRole . ctEvEqRel

ctEvTerm :: CtEvidence -> EvTerm
ctEvTerm (CtGiven   { ctev_evtm = tm }) = tm
ctEvTerm (CtWanted  { ctev_evar = ev }) = EvId ev
ctEvTerm ctev@(CtDerived {}) = pprPanic "ctEvTerm: derived constraint cannot have id"
                                      (ppr ctev)

ctEvCoercion :: CtEvidence -> TcCoercion
-- ctEvCoercion ev = evTermCoercion (ctEvTerm ev)
ctEvCoercion (CtGiven   { ctev_evtm = tm }) = evTermCoercion tm
ctEvCoercion (CtWanted  { ctev_evar = v })  = mkTcCoVarCo v
ctEvCoercion ctev@(CtDerived {}) = pprPanic "ctEvCoercion: derived constraint cannot have id"
                                      (ppr ctev)

ctEvId :: CtEvidence -> TcId
ctEvId (CtWanted  { ctev_evar = ev }) = ev
ctEvId ctev = pprPanic "ctEvId:" (ppr ctev)

instance Outputable CtEvidence where
  ppr fl = case fl of
             CtGiven {}   -> ptext (sLit "[G]") <+> ppr (ctev_evtm fl) <+> ppr_pty
             CtWanted {}  -> ptext (sLit "[W]") <+> ppr (ctev_evar fl) <+> ppr_pty
             CtDerived {} -> ptext (sLit "[D]") <+> text "_" <+> ppr_pty
         where ppr_pty = dcolon <+> ppr (ctEvPred fl)

isWanted :: CtEvidence -> Bool
isWanted (CtWanted {}) = True
isWanted _ = False

isGiven :: CtEvidence -> Bool
isGiven (CtGiven {})  = True
isGiven _ = False

isDerived :: CtEvidence -> Bool
isDerived (CtDerived {}) = True
isDerived _              = False

{-
%************************************************************************
%*                                                                      *
            CtFlavour
%*                                                                      *
%************************************************************************

Just an enum type that tracks whether a constraint is wanted, derived,
or given, when we need to separate that info from the constraint itself.

-}

data CtFlavour = Given | Wanted | Derived
  deriving Eq

instance Outputable CtFlavour where
  ppr Given   = text "[G]"
  ppr Wanted  = text "[W]"
  ppr Derived = text "[D]"

ctEvFlavour :: CtEvidence -> CtFlavour
ctEvFlavour (CtWanted {})  = Wanted
ctEvFlavour (CtGiven {})   = Given
ctEvFlavour (CtDerived {}) = Derived

{-
************************************************************************
*                                                                      *
            SubGoalDepth
*                                                                      *
************************************************************************

Note [SubGoalDepth]
~~~~~~~~~~~~~~~~~~~
The 'SubGoalCounter' takes care of stopping the constraint solver from looping.
Because of the different use-cases of regular constaints and type function
applications, there are two independent counters. Therefore, this datatype is
abstract. See Note [WorkList]

Each counter starts at zero and increases.

* The "dictionary constraint counter" counts the depth of type class
  instance declarations.  Example:
     [W] d{7} : Eq [Int]
  That is d's dictionary-constraint depth is 7.  If we use the instance
     $dfEqList :: Eq a => Eq [a]
  to simplify it, we get
     d{7} = $dfEqList d'{8}
  where d'{8} : Eq Int, and d' has dictionary-constraint depth 8.

  For civilised (decidable) instance declarations, each increase of
  depth removes a type constructor from the type, so the depth never
  gets big; i.e. is bounded by the structural depth of the type.

  The flag -fcontext-stack=n (not very well named!) fixes the maximium
  level.

* The "type function reduction counter" does the same thing when resolving
* qualities involving type functions. Example:
  Assume we have a wanted at depth 7:
    [W] d{7} : F () ~ a
  If thre is an type function equation "F () = Int", this would be rewritten to
    [W] d{8} : Int ~ a
  and remembered as having depth 8.

  Again, without UndecidableInstances, this counter is bounded, but without it
  can resolve things ad infinitum. Hence there is a maximum level. But we use a
  different maximum, as we expect possibly many more type function reductions
  in sensible programs than type class constraints.

  The flag -ftype-function-depth=n fixes the maximium level.
-}

data SubGoalCounter = CountConstraints | CountTyFunApps

data SubGoalDepth  -- See Note [SubGoalDepth]
   = SubGoalDepth
         {-# UNPACK #-} !Int      -- Dictionary constraints
         {-# UNPACK #-} !Int      -- Type function reductions
  deriving (Eq, Ord)

instance Outputable SubGoalDepth where
 ppr (SubGoalDepth c f) =  angleBrackets $
        char 'C' <> colon <> int c <> comma <>
        char 'F' <> colon <> int f

initialSubGoalDepth :: SubGoalDepth
initialSubGoalDepth = SubGoalDepth 0 0

maxSubGoalDepth :: DynFlags -> SubGoalDepth
maxSubGoalDepth dflags = SubGoalDepth (ctxtStkDepth dflags) (tyFunStkDepth dflags)

bumpSubGoalDepth :: SubGoalCounter -> SubGoalDepth -> SubGoalDepth
bumpSubGoalDepth CountConstraints (SubGoalDepth c f) = SubGoalDepth (c+1) f
bumpSubGoalDepth CountTyFunApps   (SubGoalDepth c f) = SubGoalDepth c (f+1)

subGoalCounterValue :: SubGoalCounter -> SubGoalDepth -> Int
subGoalCounterValue CountConstraints (SubGoalDepth c _) = c
subGoalCounterValue CountTyFunApps   (SubGoalDepth _ f) = f

subGoalDepthExceeded :: SubGoalDepth -> SubGoalDepth -> Maybe SubGoalCounter
subGoalDepthExceeded (SubGoalDepth mc mf) (SubGoalDepth c f)
        | c > mc    = Just CountConstraints
        | f > mf    = Just CountTyFunApps
        | otherwise = Nothing

-- | Checks whether the evidence can be used to solve a goal with the given minimum depth
-- See Note [Preventing recursive dictionaries]
ctEvCheckDepth :: Class -> CtLoc -> CtEvidence -> Bool
ctEvCheckDepth cls target ev
  | isWanted ev
  , cls == coercibleClass  -- The restriction applies only to Coercible
  = ctLocDepth target <= ctLocDepth (ctEvLoc ev)
  | otherwise = True

{-
Note [Preventing recursive dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NB: this will go away when we start treating Coercible as an equality.

We have some classes where it is not very useful to build recursive
dictionaries (Coercible, at the moment). So we need the constraint solver to
prevent that. We conservatively ensure this property using the subgoal depth of
the constraints: When solving a Coercible constraint at depth d, we do not
consider evidence from a depth <= d as suitable.

Therefore we need to record the minimum depth allowed to solve a CtWanted. This
is done in the SubGoalDepth field of CtWanted. Most code now uses mkCtWanted,
which initializes it to initialSubGoalDepth (i.e. 0); but when requesting a
Coercible instance (requestCoercible in TcInteract), we bump the current depth
by one and use that.

There are two spots where wanted contraints attempted to be solved
using existing constraints: lookupInertDict and lookupSolvedDict in
TcSMonad.  Both use ctEvCheckDepth to make the check. That function
ensures that a Given constraint can always be used to solve a goal
(i.e. they are at depth infinity, for our purposes)


************************************************************************
*                                                                      *
            CtLoc
*                                                                      *
************************************************************************

The 'CtLoc' gives information about where a constraint came from.
This is important for decent error message reporting because
dictionaries don't appear in the original source code.
type will evolve...
-}

data CtLoc = CtLoc { ctl_origin :: CtOrigin
                   , ctl_env    :: TcLclEnv
                   , ctl_depth  :: !SubGoalDepth }
  -- The TcLclEnv includes particularly
  --    source location:  tcl_loc   :: RealSrcSpan
  --    context:          tcl_ctxt  :: [ErrCtxt]
  --    binder stack:     tcl_bndrs :: [TcIdBinders]
  --    level:            tcl_tclvl :: TcLevel

mkGivenLoc :: TcLevel -> SkolemInfo -> TcLclEnv -> CtLoc
mkGivenLoc tclvl skol_info env
  = CtLoc { ctl_origin = GivenOrigin skol_info
          , ctl_env    = env { tcl_tclvl = tclvl }
          , ctl_depth  = initialSubGoalDepth }

ctLocEnv :: CtLoc -> TcLclEnv
ctLocEnv = ctl_env

ctLocDepth :: CtLoc -> SubGoalDepth
ctLocDepth = ctl_depth

ctLocOrigin :: CtLoc -> CtOrigin
ctLocOrigin = ctl_origin

ctLocSpan :: CtLoc -> RealSrcSpan
ctLocSpan (CtLoc { ctl_env = lcl}) = tcl_loc lcl

setCtLocSpan :: CtLoc -> RealSrcSpan -> CtLoc
setCtLocSpan ctl@(CtLoc { ctl_env = lcl }) loc = setCtLocEnv ctl (lcl { tcl_loc = loc })

bumpCtLocDepth :: SubGoalCounter -> CtLoc -> CtLoc
bumpCtLocDepth cnt loc@(CtLoc { ctl_depth = d }) = loc { ctl_depth = bumpSubGoalDepth cnt d }

setCtLocOrigin :: CtLoc -> CtOrigin -> CtLoc
setCtLocOrigin ctl orig = ctl { ctl_origin = orig }

setCtLocEnv :: CtLoc -> TcLclEnv -> CtLoc
setCtLocEnv ctl env = ctl { ctl_env = env }

pushErrCtxt :: CtOrigin -> ErrCtxt -> CtLoc -> CtLoc
pushErrCtxt o err loc@(CtLoc { ctl_env = lcl })
  = loc { ctl_origin = o, ctl_env = lcl { tcl_ctxt = err : tcl_ctxt lcl } }

pushErrCtxtSameOrigin :: ErrCtxt -> CtLoc -> CtLoc
-- Just add information w/o updating the origin!
pushErrCtxtSameOrigin err loc@(CtLoc { ctl_env = lcl })
  = loc { ctl_env = lcl { tcl_ctxt = err : tcl_ctxt lcl } }

pprArising :: CtOrigin -> SDoc
-- Used for the main, top-level error message
-- We've done special processing for TypeEq and FunDep origins
pprArising (TypeEqOrigin {}) = empty
pprArising orig              = pprCtOrigin orig

pprArisingAt :: CtLoc -> SDoc
pprArisingAt (CtLoc { ctl_origin = o, ctl_env = lcl})
  = sep [ pprCtOrigin o
        , text "at" <+> ppr (tcl_loc lcl)]

{-
************************************************************************
*                                                                      *
                SkolemInfo
*                                                                      *
************************************************************************
-}

-- SkolemInfo gives the origin of *given* constraints
--   a) type variables are skolemised
--   b) an implication constraint is generated
data SkolemInfo
  = SigSkol UserTypeCtxt        -- A skolem that is created by instantiating
            Type                -- a programmer-supplied type signature
                                -- Location of the binding site is on the TyVar

        -- The rest are for non-scoped skolems
  | ClsSkol Class       -- Bound at a class decl
  | InstSkol            -- Bound at an instance decl
  | DataSkol            -- Bound at a data type declaration
  | FamInstSkol         -- Bound at a family instance decl
  | PatSkol             -- An existential type variable bound by a pattern for
      ConLike           -- a data constructor with an existential type.
      (HsMatchContext Name)
             -- e.g.   data T = forall a. Eq a => MkT a
             --        f (MkT x) = ...
             -- The pattern MkT x will allocate an existential type
             -- variable for 'a'.

  | ArrowSkol           -- An arrow form (see TcArrows)

  | IPSkol [HsIPName]   -- Binding site of an implicit parameter

  | RuleSkol RuleName   -- The LHS of a RULE

  | InferSkol [(Name,TcType)]
                        -- We have inferred a type for these (mutually-recursivive)
                        -- polymorphic Ids, and are now checking that their RHS
                        -- constraints are satisfied.

  | BracketSkol         -- Template Haskell bracket

  | UnifyForAllSkol     -- We are unifying two for-all types
       [TcTyVar]        -- The instantiated skolem variables
       TcType           -- The instantiated type *inside* the forall

  | UnkSkol             -- Unhelpful info (until I improve it)

instance Outputable SkolemInfo where
  ppr = pprSkolInfo

pprSkolInfo :: SkolemInfo -> SDoc
-- Complete the sentence "is a rigid type variable bound by..."
pprSkolInfo (SigSkol (FunSigCtxt f) ty)
                            = hang (ptext (sLit "the type signature for"))
                                 2 (pprPrefixOcc f <+> dcolon <+> ppr ty)
pprSkolInfo (SigSkol cx ty) = hang (pprUserTypeCtxt cx <> colon)
                                 2 (ppr ty)
pprSkolInfo (IPSkol ips)    = ptext (sLit "the implicit-parameter binding") <> plural ips <+> ptext (sLit "for")
                              <+> pprWithCommas ppr ips
pprSkolInfo (ClsSkol cls)   = ptext (sLit "the class declaration for") <+> quotes (ppr cls)
pprSkolInfo InstSkol        = ptext (sLit "the instance declaration")
pprSkolInfo DataSkol        = ptext (sLit "the data type declaration")
pprSkolInfo FamInstSkol     = ptext (sLit "the family instance declaration")
pprSkolInfo BracketSkol     = ptext (sLit "a Template Haskell bracket")
pprSkolInfo (RuleSkol name) = ptext (sLit "the RULE") <+> doubleQuotes (ftext name)
pprSkolInfo ArrowSkol       = ptext (sLit "the arrow form")
pprSkolInfo (PatSkol cl mc) = case cl of
    RealDataCon dc -> sep [ ptext (sLit "a pattern with constructor")
                          , nest 2 $ ppr dc <+> dcolon
                            <+> pprType (dataConUserType dc) <> comma
                            -- pprType prints forall's regardless of -fprint-explict-foralls
                            -- which is what we want here, since we might be saying
                            -- type variable 't' is bound by ...
                          , ptext (sLit "in") <+> pprMatchContext mc ]
    PatSynCon ps -> sep [ ptext (sLit "a pattern with pattern synonym")
                        , nest 2 $ ppr ps <+> dcolon
                          <+> pprType (patSynType ps) <> comma
                        , ptext (sLit "in") <+> pprMatchContext mc ]
pprSkolInfo (InferSkol ids) = sep [ ptext (sLit "the inferred type of")
                                  , vcat [ ppr name <+> dcolon <+> ppr ty
                                         | (name,ty) <- ids ]]
pprSkolInfo (UnifyForAllSkol tvs ty) = ptext (sLit "the type") <+> ppr (mkForAllTys tvs ty)

-- UnkSkol
-- For type variables the others are dealt with by pprSkolTvBinding.
-- For Insts, these cases should not happen
pprSkolInfo UnkSkol = {-WARN( True, text "pprSkolInfo: UnkSkol" )-} ptext (sLit "UnkSkol")

{-
************************************************************************
*                                                                      *
            CtOrigin
*                                                                      *
************************************************************************
-}

data CtOrigin
  = GivenOrigin SkolemInfo

  -- All the others are for *wanted* constraints
  | OccurrenceOf Name           -- Occurrence of an overloaded identifier
  | AppOrigin                   -- An application of some kind

  | SpecPragOrigin Name         -- Specialisation pragma for identifier

  | TypeEqOrigin { uo_actual   :: TcType
                 , uo_expected :: TcType }
  | KindEqOrigin
      TcType TcType             -- A kind equality arising from unifying these two types
      CtOrigin                  -- originally arising from this
  | CoercibleOrigin TcType TcType  -- a Coercible constraint

  | IPOccOrigin  HsIPName       -- Occurrence of an implicit parameter
  | OverLabelOrigin FastString -- Occurrence of an overloaded label

  | LiteralOrigin (HsOverLit Name)      -- Occurrence of a literal
  | NegateOrigin                        -- Occurrence of syntactic negation

  | ArithSeqOrigin (ArithSeqInfo Name) -- [x..], [x..y] etc
  | PArrSeqOrigin  (ArithSeqInfo Name) -- [:x..y:] and [:x,y..z:]
  | SectionOrigin
  | TupleOrigin                        -- (..,..)
  | ExprSigOrigin       -- e :: ty
  | PatSigOrigin        -- p :: ty
  | PatOrigin           -- Instantiating a polytyped pattern at a constructor
  | RecordUpdOrigin
  | ViewPatOrigin

  | ScOrigin            -- Typechecking superclasses of an instance declaration
  | DerivOrigin         -- Typechecking deriving
  | DerivOriginDC DataCon Int
                        -- Checking constraints arising from this data con and field index
  | DerivOriginCoerce Id Type Type
                        -- DerivOriginCoerce id ty1 ty2: Trying to coerce class method `id` from
                        -- `ty1` to `ty2`.
  | StandAloneDerivOrigin -- Typechecking stand-alone deriving
  | DefaultOrigin       -- Typechecking a default decl
  | DoOrigin            -- Arising from a do expression
  | MCompOrigin         -- Arising from a monad comprehension
  | IfOrigin            -- Arising from an if statement
  | ProcOrigin          -- Arising from a proc expression
  | AnnOrigin           -- An annotation

  | FunDepOrigin1       -- A functional dependency from combining
        PredType CtLoc      -- This constraint arising from ...
        PredType CtLoc      -- and this constraint arising from ...

  | FunDepOrigin2       -- A functional dependency from combining
        PredType CtOrigin   -- This constraint arising from ...
        PredType SrcSpan    -- and this instance
        -- We only need a CtOrigin on the first, because the location
        -- is pinned on the entire error message

  | HoleOrigin
  | UnboundOccurrenceOf RdrName
  | ListOrigin          -- An overloaded list
  | StaticOrigin        -- A static form
  | InstProvidedOrigin Module ClsInst
        -- Skolem variable arose when we were testing if an instance
        -- is solvable or not.

ctoHerald :: SDoc
ctoHerald = ptext (sLit "arising from")

pprCtOrigin :: CtOrigin -> SDoc

pprCtOrigin (GivenOrigin sk) = ctoHerald <+> ppr sk

pprCtOrigin (FunDepOrigin1 pred1 loc1 pred2 loc2)
  = hang (ctoHerald <+> ptext (sLit "a functional dependency between constraints:"))
       2 (vcat [ hang (quotes (ppr pred1)) 2 (pprArisingAt loc1)
               , hang (quotes (ppr pred2)) 2 (pprArisingAt loc2) ])

pprCtOrigin (FunDepOrigin2 pred1 orig1 pred2 loc2)
  = hang (ctoHerald <+> ptext (sLit "a functional dependency between:"))
       2 (vcat [ hang (ptext (sLit "constraint") <+> quotes (ppr pred1))
                    2 (pprArising orig1 )
               , hang (ptext (sLit "instance") <+> quotes (ppr pred2))
                    2 (ptext (sLit "at") <+> ppr loc2) ])

pprCtOrigin (KindEqOrigin t1 t2 _)
  = hang (ctoHerald <+> ptext (sLit "a kind equality arising from"))
       2 (sep [ppr t1, char '~', ppr t2])

pprCtOrigin (UnboundOccurrenceOf name)
  = ctoHerald <+> ptext (sLit "an undeclared identifier") <+> quotes (ppr name)

pprCtOrigin (DerivOriginDC dc n)
  = hang (ctoHerald <+> ptext (sLit "the") <+> speakNth n
          <+> ptext (sLit "field of") <+> quotes (ppr dc))
       2 (parens (ptext (sLit "type") <+> quotes (ppr ty)))
  where
    ty = dataConOrigArgTys dc !! (n-1)

pprCtOrigin (DerivOriginCoerce meth ty1 ty2)
  = hang (ctoHerald <+> ptext (sLit "the coercion of the method") <+> quotes (ppr meth))
       2 (sep [ text "from type" <+> quotes (ppr ty1)
              , nest 2 $ text "to type" <+> quotes (ppr ty2) ])

pprCtOrigin (CoercibleOrigin ty1 ty2)
  = hang (ctoHerald <+> text "trying to show that the representations of")
       2 (quotes (ppr ty1) <+> text "and" $$
          quotes (ppr ty2) <+> text "are the same")

pprCtOrigin (InstProvidedOrigin mod cls_inst)
  = vcat [ text "arising when attempting to show that"
         , ppr cls_inst
         , text "is provided by" <+> quotes (ppr mod)]

pprCtOrigin simple_origin
  = ctoHerald <+> pprCtO simple_origin

----------------
pprCtO :: CtOrigin -> SDoc  -- Ones that are short one-liners
pprCtO (OccurrenceOf name)   = hsep [ptext (sLit "a use of"), quotes (ppr name)]
pprCtO AppOrigin             = ptext (sLit "an application")
pprCtO (SpecPragOrigin name) = hsep [ptext (sLit "a specialisation pragma for"), quotes (ppr name)]
pprCtO (IPOccOrigin name)    = hsep [ptext (sLit "a use of implicit parameter"), quotes (ppr name)]
pprCtO (OverLabelOrigin l)   = hsep [ptext (sLit "the overloaded label")
                                    ,quotes (char '#' <> ppr l)]
pprCtO RecordUpdOrigin       = ptext (sLit "a record update")
pprCtO ExprSigOrigin         = ptext (sLit "an expression type signature")
pprCtO PatSigOrigin          = ptext (sLit "a pattern type signature")
pprCtO PatOrigin             = ptext (sLit "a pattern")
pprCtO ViewPatOrigin         = ptext (sLit "a view pattern")
pprCtO IfOrigin              = ptext (sLit "an if statement")
pprCtO (LiteralOrigin lit)   = hsep [ptext (sLit "the literal"), quotes (ppr lit)]
pprCtO (ArithSeqOrigin seq)  = hsep [ptext (sLit "the arithmetic sequence"), quotes (ppr seq)]
pprCtO (PArrSeqOrigin seq)   = hsep [ptext (sLit "the parallel array sequence"), quotes (ppr seq)]
pprCtO SectionOrigin         = ptext (sLit "an operator section")
pprCtO TupleOrigin           = ptext (sLit "a tuple")
pprCtO NegateOrigin          = ptext (sLit "a use of syntactic negation")
pprCtO ScOrigin              = ptext (sLit "the superclasses of an instance declaration")
pprCtO DerivOrigin           = ptext (sLit "the 'deriving' clause of a data type declaration")
pprCtO StandAloneDerivOrigin = ptext (sLit "a 'deriving' declaration")
pprCtO DefaultOrigin         = ptext (sLit "a 'default' declaration")
pprCtO DoOrigin              = ptext (sLit "a do statement")
pprCtO MCompOrigin           = ptext (sLit "a statement in a monad comprehension")
pprCtO ProcOrigin            = ptext (sLit "a proc expression")
pprCtO (TypeEqOrigin t1 t2)  = ptext (sLit "a type equality") <+> sep [ppr t1, char '~', ppr t2]
pprCtO AnnOrigin             = ptext (sLit "an annotation")
pprCtO HoleOrigin            = ptext (sLit "a use of") <+> quotes (ptext $ sLit "_")
pprCtO ListOrigin            = ptext (sLit "an overloaded list")
pprCtO StaticOrigin          = ptext (sLit "a static form")
pprCtO _                     = panic "pprCtOrigin"

{-
Constraint Solver Plugins
-------------------------
-}

type TcPluginSolver = [Ct]    -- given
                   -> [Ct]    -- derived
                   -> [Ct]    -- wanted
                   -> TcPluginM TcPluginResult

newtype TcPluginM a = TcPluginM (TcM a)

instance Functor     TcPluginM where
  fmap = liftM

instance Applicative TcPluginM where
  pure  = return
  (<*>) = ap

instance Monad TcPluginM where
  return x = TcPluginM (return x)
  fail x   = TcPluginM (fail x)
  TcPluginM m >>= k =
    TcPluginM (do a <- m
                  let TcPluginM m1 = k a
                  m1)

runTcPluginM :: TcPluginM a -> TcM a
runTcPluginM (TcPluginM m) = m

-- | This function provides an escape for direct access to
-- the 'TcM` monad.  It should not be used lightly, and
-- the provided 'TcPluginM' API should be favoured instead.
unsafeTcPluginTcM :: TcM a -> TcPluginM a
unsafeTcPluginTcM = TcPluginM

data TcPlugin = forall s. TcPlugin
  { tcPluginInit  :: TcPluginM s
    -- ^ Initialize plugin, when entering type-checker.

  , tcPluginSolve :: s -> TcPluginSolver
    -- ^ Solve some constraints.
    -- TODO: WRITE MORE DETAILS ON HOW THIS WORKS.

  , tcPluginStop  :: s -> TcPluginM ()
   -- ^ Clean up after the plugin, when exiting the type-checker.
  }

data TcPluginResult
  = TcPluginContradiction [Ct]
    -- ^ The plugin found a contradiction.
    -- The returned constraints are removed from the inert set,
    -- and recorded as insoluable.

  | TcPluginOk [(EvTerm,Ct)] [Ct]
    -- ^ The first field is for constraints that were solved.
    -- These are removed from the inert set,
    -- and the evidence for them is recorded.
    -- The second field contains new work, that should be processed by
    -- the constraint solver.

data ContextElement
  = FunctionResultCtxt Bool Int Int (HsExpr Name) Type Type Type Type
  | FunctionArgumentCtxt (LHsExpr Name) (LHsExpr Name) Int
  | SyntaxNameCtxt (HsExpr Name) Type CtLoc
  | FunctionCtxt HeraldContext Arity Int Type Type
  | PolymorphicCtxt Type Type
  | AnnotationTcCtxt (AnnDecl Name)
  | AmbiguityCheckCtxt UserTypeCtxt Type Bool
  | ThetaCtxt UserTypeCtxt ThetaType
  | SignatureCtxt SignatureContext
  | TypeCtxt (HsType Name)
  | PatternSigCtxt Type Type
  | KindCtxt (LHsKind Name)
  | PatternCtxt (Pat Name)
  | ExportCtxt Bool Bool Name Type
  | SpecCtxt (Sig Name)
  | forall thing. (Outputable thing) => VectorCtxt thing
  | TypeSignatureCtxt Name SDoc
  | PatMonoBindsCtxt SDoc
  | MatchCtxt SDoc SDoc
  | ListComprehensionCtxt SDoc
  | StatementCtxt SDoc SDoc
  | CommandCtxt (HsCmd Name)
  | ExpressionCtxt (LHsExpr Name)
  | StaticCtxt (LHsExpr Name)
  | FieldCtxt Name
  | DeclarationCtxt (TyClDecl Name)
  | ForeignDeclarationCtxt (ForeignDecl Name)
  | DefaultDeclarationCtxt
  | RuleCtxt FastString
  | DataConstructorCtxt DataCon
  | DataConstructorsCtxt [Located Name]
  | ClassCtxt Var Type
  | FamilyInstCtxt SDoc Name
  | ClosedTypeFamilyCtxt TyCon
  | AddTypeCtxt TyThing
  | RoleCtxt Name
  | StandaloneCtxt (LHsType Name)
  | DeriveInstCtxt Type
  | InstDeclarationCtxt1 (LHsType Name)
  | InstDeclarationCtxt2 Type
  | MethSigCtxt Name Type Type
  | SpecInstSigCtxt (Sig Name)
  | DerivedInstCtxt Id Class [Type]
  | MainCtxt SDoc
  | QuotationCtxt (HsBracket Name)
  | SpliceDocCtxt (HsSplice Name)
  | SpliceResultCtxt (LHsExpr Name)
  | ReifyCtxt TH.Name [TH.Type]
  --- Renamer error
  | DuplicatedAndShadowedCtxt (HsMatchContext Name)
  | AnnotationRnCtxt (AnnDecl RdrName)
  | QuasiQuoteCtxt (HsBracket RdrName)
  | SpliceCtxt (HsSplice RdrName)

data HowMuch = TooFew | TooMuch
  deriving Show

data HeraldContext
  = ArgumentOfDollarContext
  | OperatorContext (LHsExpr Name)
  | LambdaCaseContext (HsExpr Name)
  | EquationContext Name
  | LambdaExprContext (MatchGroup Name (LHsExpr Name))
  | FunctionApplicationContext (LHsExpr Name)

instance Outputable HeraldContext where
  ppr ArgumentOfDollarContext = text "The first argument of ($) takes"
  ppr (OperatorContext op) = text "The operator" <+> quotes (ppr op) <+> ptext (sLit "takes")
  ppr (LambdaCaseContext e) = sep [ text "The function" <+> quotes (ppr e), text "requires"]
  ppr (EquationContext fun_name)
      = text "The equation(s) for"
               <+> quotes (ppr fun_name) <+> text "have"
  ppr (LambdaExprContext match)
      = sep [ ptext (sLit "The lambda expression")
                           <+> quotes (pprSetDepth (PartWay 1) $
                               pprMatches (LambdaExpr :: HsMatchContext Name) match),
                          -- The pprSetDepth makes the abstraction print briefly
                  ptext (sLit "has")]
  ppr (FunctionApplicationContext fun)
     = sep [ text "The function" <+> quotes (ppr fun), text "is applied to"]

instance Outputable ContextElement where
  ppr (FunctionResultCtxt has_args n_fun n_env fun res_fun res_env _ _)
     | n_fun == n_env = empty
     | n_fun > n_env
     , not_fun res_env = text "Probable cause:" <+> quotes (ppr fun)
                         <+> text "is applied to too few arguments"
     | has_args
     , not_fun res_fun = text "Possible cause:" <+> quotes (ppr fun)
                         <+> text "is applied to too many arguments"
     | otherwise       = empty  -- Never suggest that a naked variable is
                                           -- applied to too many args!
     where not_fun ty   -- ty is definitely not an arrow type,
                    -- and cannot conceivably become one
             = case tcSplitTyConApp_maybe ty of
                 Just (tc, _) -> isAlgTyCon tc
                 Nothing      -> False
  ppr (FunctionArgumentCtxt fun arg arg_no)
     = hang (hsep [ text "In the", speakNth arg_no, text "argument of",
                       quotes (ppr fun) <> text ", namely"]) 2 (quotes (ppr arg))
  ppr (SyntaxNameCtxt name ty inst_loc)
    = vcat [ text "When checking that" <+> quotes (ppr name)
         <+> text "(needed by a syntactic construct)"
       , nest 2 (text "has the required type:" <+> ppr ty)
       , nest 2 (pprArisingAt inst_loc) ]
  ppr (FunctionCtxt herald arity n_args orig_ty ty)
    = ppr herald <+> speakNOf arity (text "argument") <> comma $$
        if n_args == arity
          then text "its type is" <+> quotes (pprType orig_ty) <>
               comma $$
               text "it is specialized to" <+> quotes (pprType ty)
          else sep [text "but its type" <+> quotes (pprType ty),
                    if n_args == 0 then text "has none"
                    else text "has only" <+> speakN n_args]
  ppr (PolymorphicCtxt ty_actual ty_expected)
    = vcat [ hang (text "When checking that:")
                  4 (ppr ty_actual)
           , nest 2 (hang (text "is more polymorphic than:")
                  2 (ppr ty_expected)) ]
  ppr (AnnotationTcCtxt ann) = hang (text "In the annotation:") 2 (ppr ann)
  ppr (AmbiguityCheckCtxt ctxt tidy_ty allow_ambiguous) =
    mk_msg tidy_ty $$ ppWhen (not allow_ambiguous) ambig_msg
    where mk_msg ty = ppr $ SignatureContext ctxt (text "the ambiguity check for") (ppr ty)
          ambig_msg = text "To defer the ambiguity check to use sites, enable AllowAmbiguousTypes"
  ppr (ThetaCtxt ctxt theta)
      = vcat [text "In the context:" <+> pprTheta theta,
              text "While checking" <+> pprUserTypeCtxt ctxt ]
  ppr (SignatureCtxt ctxt) = ppr ctxt
  ppr (TypeCtxt ty) = text "In the type" <+> quotes (ppr ty)
  ppr (PatternSigCtxt sig_ty res_ty)
     = vcat [ hang (text "When checking that the pattern signature:") 4 (ppr sig_ty)
                   , nest 2 (hang (text "fits the type of its context:") 2 (ppr res_ty)) ]
  ppr (KindCtxt k) = (text "In the kind" <+> quotes (ppr k))
  ppr (PatternCtxt pat) = hang (text "In the pattern:") 2 (ppr pat)
  ppr (ExportCtxt inferred want_ambig poly_name ty)
      = vcat [ text "When checking that" <+> quotes (ppr poly_name)
                         <+> text "has the" <+> what <+> text "type"
                       , nest 2 (ppr poly_name <+> dcolon <+> ppr ty)
                       , ppWhen want_ambig $
                         text "Probable cause: the inferred type is ambiguous" ]
      where what | inferred  = text "inferred"
                 | otherwise = text "specified"
  ppr (SpecCtxt prag) = hang (text "In the SPECIALISE pragma") 2 (ppr prag)
  ppr (VectorCtxt thing) = text "When checking the vectorisation declaration for" <+> ppr thing
  ppr (TypeSignatureCtxt name sdoc)
     = sep [ text "In" <+> pprUserTypeCtxt (FunSigCtxt name) <> colon
           , nest 2 sdoc ]
  ppr (PatMonoBindsCtxt sdoc) = hang (text "In a pattern binding:") 2 sdoc
  ppr (MatchCtxt doc1 doc2)
      = hang (text "In" <+> doc1 <> colon) 4 doc2
  ppr (ListComprehensionCtxt sdoc) = hang (text "In the expression:") 2 sdoc
  ppr (StatementCtxt sdoc1 sdoc2) = hang (text "In a stmt of" <+> sdoc1 <> colon)
          2 sdoc2
  ppr (CommandCtxt cmd) = text "In the command:" <+> ppr cmd
  ppr (ExpressionCtxt expr) = hang (text "In the expression:") 2 (ppr expr)
  ppr (StaticCtxt expr) = (hang (text "In the body of a static form:") 2 (ppr expr))
  ppr (FieldCtxt field_name) = text "In the" <+> quotes (ppr field_name) <+> text "field of a record"
  ppr (DeclarationCtxt decl) = hsep [text "In the", pprTyClDeclFlavour decl,
                        text "declaration for", quotes (ppr (tcdName decl))]
  ppr (ForeignDeclarationCtxt fo) = hang (text "When checking declaration:") 2 (ppr fo)
  ppr DefaultDeclarationCtxt = text "When checking the types in a default declaration"
  ppr (RuleCtxt name) = text "When checking the transformation rule" <+>
                  doubleQuotes (ftext name)
  ppr (DataConstructorCtxt con) = text "In the definition of data constructor" <+> quotes (ppr con)
  ppr (DataConstructorsCtxt cons) = dataConCtxtName cons
    where dataConCtxtName [con]
             = text "In the definition of data constructor" <+> quotes (ppr con)
          dataConCtxtName con
             = text "In the definition of data constructors" <+> interpp'SP con
  ppr (ClassCtxt sel_id tau)
     = sep [text "When checking the class method:",
                                   nest 2 (pprPrefixOcc sel_id <+> dcolon <+> ppr tau)]
  ppr (FamilyInstCtxt flavour tycon)
     = hsep [text "In the" <+> flavour <+> text "declaration for", quotes (ppr tycon)]
  ppr (ClosedTypeFamilyCtxt tc)
     = text "In the equations for closed type family" <+> quotes (ppr tc)
  ppr (AddTypeCtxt thing) = hsep [ text "In the", flav
              , text "declaration for", quotes (ppr name) ]
      where
        name = getName thing
        flav = case thing of
                 ATyCon tc
                    | isClassTyCon tc       -> ptext (sLit "class")
                    | isTypeFamilyTyCon tc  -> ptext (sLit "type family")
                    | isDataFamilyTyCon tc  -> ptext (sLit "data family")
                    | isTypeSynonymTyCon tc -> ptext (sLit "type")
                    | isNewTyCon tc         -> ptext (sLit "newtype")
                    | isDataTyCon tc        -> ptext (sLit "data")

                 _ -> pprTrace "addTyThingCtxt strange" (ppr thing)
                      empty
  ppr (RoleCtxt name) = text "while checking a role annotation for" <+> quotes (ppr name)
  ppr (StandaloneCtxt ty) = hang (text "In the stand-alone deriving instance for")
                         2 (quotes (ppr ty))
  ppr (DeriveInstCtxt pred) = text "When deriving the instance for" <+> parens (ppr pred)
  ppr (InstDeclarationCtxt1 hs_inst_ty)
      = inst_decl_ctxt (case unLoc hs_inst_ty of
                            HsForAllTy _ _ _ _ (L _ ty') -> ppr ty'
                            _                            -> ppr hs_inst_ty)
  ppr (InstDeclarationCtxt2 dfun_ty)
      = inst_decl_ctxt (ppr (mkClassPred cls tys))
      where
        (_,_,cls,tys) = tcSplitDFunTy dfun_ty
  ppr (MethSigCtxt sel_name sig_ty meth_ty)
      = hang (text "When checking that instance signature for" <+> quotes (ppr sel_name))
                     2 (vcat [ text "is more general than its signature in the class"
                             , text "Instance sig:" <+> ppr sig_ty
                             , text "   Class sig:" <+> ppr meth_ty ])
  ppr (SpecInstSigCtxt prag)
      = hang (text "In the SPECIALISE pragma") 2 (ppr prag)
  ppr (DerivedInstCtxt sel_id clas tys)
      = vcat [ text "When typechecking the code for " <+> quotes (ppr sel_id)
             , nest 2 (text "in a derived instance for" <+> quotes (pprClassPred clas tys) <> colon)
             , nest 2 $ text "To see the code I am typechecking, use -ddump-deriv" ]
  ppr (MainCtxt sdoc) = text "When checking the type of the" <+> sdoc
  ppr (QuotationCtxt br_body)
      = hang (text "In the Template Haskell quotation") 2 (ppr br_body)
  ppr (SpliceDocCtxt splice)
      = hang (text "In the Template Haskell splice") 2 (pprSplice splice)
  ppr (SpliceResultCtxt expr)
      = sep [ text "In the result of the splice:"
            , nest 2 (char '$' <> ppr expr)
            , text "To see what the splice expanded to, use -ddump-splices"]
  ppr (ReifyCtxt th_nm th_tys)
      = (text "In the argument of reifyInstances:"
                    <+> ppr_th th_nm <+> sep (map ppr_th th_tys))
  --- Renamer errors
  ppr (DuplicatedAndShadowedCtxt ctxt) = text "In" <+> pprMatchContext ctxt
  ppr (AnnotationRnCtxt ann) = hang (text "In the annotation:") 2 (ppr ann)
  ppr (QuasiQuoteCtxt br_body) = hang (text "In the Template Haskell quotation")
         2 (ppr br_body)
  ppr (SpliceCtxt splice)
    = hang (text "In the" <+> what) 2 (ppr splice)
        where
          what = case splice of
                   HsUntypedSplice {} -> text "untyped splice:"
                   HsTypedSplice   {} -> text "typed splice:"
                   HsQuasiQuote    {} -> text "quasi-quotation:"
                   HsSpliced       {} -> text "spliced expression:"

data SignatureContext = SignatureContext UserTypeCtxt SDoc SDoc

instance Outputable SignatureContext where
  ppr (SignatureContext ctxt extra pp_ty)
    = sep [ ptext (sLit "In") <+> extra <+> pprUserTypeCtxt ctxt <> colon
          , nest 2 (pp_sig ctxt) ]
      where
        pp_sig (FunSigCtxt n)  = pp_n_colon n
        pp_sig (ConArgCtxt n)  = pp_n_colon n
        pp_sig (ForSigCtxt n)  = pp_n_colon n
        pp_sig _               = pp_ty
        pp_n_colon n = pprPrefixOcc n <+> dcolon <+> pp_ty

data TypeError
   = FunctionalDepsError [ClsInst]
   | DuplicateInstError [ClsInst]
   | FamilyInstError [FamInst]
   | ArityError String Name Int Int
   | ForAllTypeError Rank Type
   | BadIPPredError PredType
   | EqPredTypeError PredType
   | PredTyVarError PredType
   | PredTupleError PredType
   | PredIrredError PredType
   | PredIrredBadCtxtError PredType
   | InstTypeError Class [Type] SDoc
   | MalformedInstanceTypeError SDoc
   | PredUndecError PredType SDoc
   | BadATError Name Name
   | WrongATArgError Type Type
   | NestedTypeFamilyError Type
   | VariableMultipleOccurenceError Type [TyVar]
   | SmallerAppMsgError Type
   | FamilyPatError TyCon [TyVar] [Type]
   | TypeFamInstIllegalError Type
   | IllegalDerivingItemError (HsType Name)
   | MalformedInstanceError
   | UnexpectedStrictAnnotationError (HsType Name)
   | IllegalConstraintError (HsType Name)
   | UnexpectedTypeSpliceError (HsType Name)
   | TooManyTypeArgumentsError SDoc
   | KindVariableTypeError TcTyVar
   | DataConstructorUnPromoError DataCon
   | BadKindSignatureError Kind
   | PatternBindSignatureError [(Name, TcTyVar)]
   | BadPatterSignatureError TcType [TyVar]
   | NotKindConstructorError (HsKind Name)
   | KindNotAppliedError Name
   | ExpectedLiftedTypeError (HsType Name)
   | ExpectedUnLiftedTypeError (HsType Name)
   | KindOccurCheckError SDoc (HsType Name) Type
   | ExpectingArgumentError SDoc (HsType Name) Type Int
   | KindMisMatchError SDoc (HsType Name) Type
   | DataKindsTypeError Name
   | TypeVariableUsedTypeError TyVar
   | KindVariablePositionError Name
   | TypeVariableUsedKindError Name
   | TypeConstructorUsedKind Name
   | OfKindError TyCon String
   | PromotionError Name PromotionErr
   | CouldNotMatchKindError Type Type
   | GADTTypeFamiliesError
   | BadFieldConstructorError ConLike Name
   | ExistentialLetPatError
   | ExistentialLazyPatError
   | ExistentialProcPatError
   | LazyUnliftedPatError (Pat Name)
   | BadBootDeclarationError
   | RecursivePatternSynonymError (LHsBinds Name)
   | DuplicateVectorisationError Name
   | VectoriseScalarError
   | StrictBindError String Bool [LHsBind Name]
   | PolyBindError [LHsBind Name]
   | UnliftedMustBeBangError [LHsBind Name]
   | CheckArgumentsError Name (LMatch Name (LHsExpr Name)) [LMatch Name (LHsExpr Name)]
   | IfThenElseError
   | ArrowCommandTypeError (HsCmd Name)
   | WrongNumberOfParmsError Arity
   | BiDirectionalError (LPat Name)
   | NonBiDirectionalError Name
   | AsPatternsDefinitionError (Pat Name)
   | TemplateHaskellPatSynError (Pat Name)
   | NKPatSynError (Pat Name)
   | CannotHandleTypeArgError (LHsType Name)
   | NotRecordSelectorError Name
   | BadFieldTypeError [Name]
   | BadFieldTypesError [(Name,TcType)]
   | TagToEnumError
   | IdentifierExpectedError TcTyThing
   | NaughtyRecordSelectorError TcId
   | BadCallTagToEnumError TcType SDoc
   | PolySpliceError Id
   | MissingStrictFieldError DataCon [FieldLabel]
   | BadMethodError Name Name
   | BadGenericMethodError Name Name
   | BadDefaultMethodError Id (Sig Name)
   | GHCForeignImportPrimError
   | SafeUnsafeAnnoError
   | IllegalForeignTypeError SDoc SDoc
   | ForeignJavaError SDoc
   | DuplicateDefaultError [Located (DefaultDecl Name)]
   | PolyDefaultError (LHsType Name)
   | BadDefaultTypeError Type [Class]
   | BadSignatureTypeError Name
   | MoreThanOneDeclarationError (Located Name)
   | WrongKindOfFamilyError TyCon
   | WrongNumberOfParamsError Arity
   | WrongTypeFamilyError Name Name
   | BadGADTDeclarationError Name
   | BadStupidThetaError Name
   | NewtypeConError Name Int
   | EmptyConDeclarationError Name
   | ClosedTypeFamilyError
   | ResultTypeMisMatchError Name DataCon DataCon
   | FieldTypeMisMatchError Name DataCon DataCon
   | BadDataConTypeError DataCon Type Type
   | BadExistentialError DataCon
   | BadGADTKindConError DataCon
   | BadBangTypeError Int SDoc DataCon
   | NewtypeFieldError DataCon Int
   | NewtypeStrictError DataCon
   | NewtypeConTypeError SDoc DataCon
   | ClassArityError Class Int
   | ClassFunDepsError Class
   | NoClassTyVarError Class SDoc
   | ErrMsgTypeError Name
   | NeedXRoleAnnotationsError TyCon
   | WrongNoOfRolesError [Located (Maybe Role)] [TyVar] (LRoleAnnotDecl Name)
   | IncoherentRolesError
   | BadRoleAnnotationError Name Role Role
   | RoleInterfaceInternalError SDoc
   | TypeSynDeclCycleError [LTyClDecl Name]
   | RecClassError [TyCon]
   | IllegalRoleAnnotDeclError (Located Name)
   | MakeDerivSpecsError
   | DerivingNullaryError
   | DerivingThingError Bool Class [Type] Type MsgDoc
   | DerivingKindError TyCon Class [Type] Kind
   | DerivingEtaError Class [Type] Type
   | GenericInstSafeHaskellError (InstInfo Name)
   | TypeableDoesNotSupportError
   | BadBootFamInstDeclError
   | AssocInClassError (Located Name)
   | NotFamilyTypeError TyCon
   | NotOpenFamilyError TyCon
   | TooFewParamsError Arity
   | MisplacedInstSigError Name (LHsType Name)
   | BadFamInstDeclError (Located Name)
   | AddTopDeclsError
   | BadBootDeclError SDoc
   | InstMisMatchError Bool ClsInst
   | BootMisMatchError Bool SDoc TyThing TyThing
   | NoMainMsgError SDoc
   | CheckMainExportedError SDoc
   | EtaREPLUnliftedError Id
   | AmbiguousTypeError
   | CannotFindTypeError String
   | NotInScopeTypeError RdrName
   | IllegalPolyTypeError Type
   | THTypeError SDoc
   | THExceptionError SDoc
   | THUserError SDoc
   | CheckTopDeclError
   | BindNameTypeError RdrName
   | ReifyInstancesError Type
   | NotInScopeTHError TH.Name
   | NotInEnvError Name
   | NoRolesAssociatedError TcTyThing
   | NoTemplateHaskellError LitString SDoc
   | CvtHsTypeError SDoc
   -- Renamer
   | OriginalBindingError RdrName
   | BadQualifiedError RdrName
   | DeclarationError RdrName
   | ExactNameError Name
   | DuplicateNameError Name
   | UnknownSubordinateError SDoc RdrName
   | AccompanyingBindingError Bool SDoc RdrName
   | NotInScopeError SDoc RdrName SDoc (Maybe SDoc)
   | NameClashError RdrName [GlobalRdrElt]
   | DuplicateNamesError SDoc [SrcSpan]
   | TupleSizeError Int
   | BadInstanceError (LHsType RdrName)
   | OperatorError RdrName (HsType RdrName) (LHsType RdrName)
   | RecordSyntaxError (HsType RdrName)
   | DataKindsError Bool (HsType RdrName)
   | NegativeLiteralError (HsType RdrName)
   | BadKindBinderError SDoc [RdrName]
   | OverlappingKindError SDoc [RdrName]
   | BadSignatureError Bool SDoc (HsType RdrName)
   | SectionPrecedenceError (Name, Fixity) (Name, Fixity) (HsExpr RdrName)
   | PrecedenceParseError (Name, Fixity) (Name, Fixity)
   | BadViewPatError (Pat RdrName)
   | DuplicateFieldError SDoc [RdrName]
   | EmptyRecordError
   | NamedFieldPunsError (Located RdrName)
   | DotDotRecordError
   | RecordWildCardsError SDoc
   | BadDotDotConError Name
   | BogusCharacterError Char
   | PackageImportError
   | ModuleCycleError ModuleName
   | SafeEtaError
   | ClassNotFoundError
   | ClassTransitiveError String
   | BindingsBootFileError (LHsBindsLR Name RdrName)
   | MultipleFixityDeclError SrcSpan RdrName
   | LocalPatterSynonymError (Located RdrName)
   | PatternSynonymError
   | DefaultSignatureError (Sig RdrName)
   | EmptyCaseError (HsMatchContext Name)
   | ResultSignatureError (HsType RdrName) SDoc
   | MethodBindError (HsBindLR RdrName RdrName)
   | MethodPatternSynonymError (HsBindLR RdrName RdrName)
   | DuplicateSignatureError [(Located RdrName, Sig RdrName)] RdrName (Sig RdrName)
   | DuplicateMinimalError [LSig RdrName]
   | DuplicateWarningError SrcSpan RdrName
   | DuplicateRoleAnnotationError [LRoleAnnotDecl RdrName]
   | OrphanRoleError (RoleAnnotDecl Name)
   | BadAssociatedTypeErrors [Name]
   | BadImplicitSpliceError
   | BadConstructorError SDoc
   | StandaloneDeriveError
   | BadRuleVarError FastString Name
   | VectorisePragmaError
   | MisplacedSignatureError (Sig Name)
   | IllegalImportError
   | IllegalDataConError RdrName
   | IllegalQualNameError RdrName
   | ExportItemError (IE RdrName)
   | MultipleDeclarationError Name [Name]
   | SimpleDeclarationError [Name]
   | TemplateHaskellExtensionError (HsExpr RdrName)
   | TemplateHaskellSpliceError (LHsExpr RdrName)
   | SectionClosedError (HsExpr RdrName)
   | StaticInterpretedModeError
   | StaticFormSpliceError (HsExpr RdrName)
   | TopLevelBindingsError (HsExpr RdrName) [Name]
   | ArrowCommandExprError (HsExpr RdrName)
   | ParallelListCompError [Name]
   | ImplicitParameterError SDoc SDoc
   | EmptyParStmtError
   | EmptyTransStmtError
   | EmptyListCtxtError (HsStmtContext Name)
   | LastStatementError (HsStmtContext Name) SDoc
   | CheckStatementError SDoc (HsStmtContext Name) SDoc
   | TupleSectionsError
   | SectionParenthesesError (HsExpr RdrName)
   | PatternSyntaxExpressionError (HsExpr RdrName)
   | BadReexportBootThingError PprStyle Bool Name Name
   | HiModuleInterfaceError SDoc InstalledModule
   | UserTypeError SDoc
   | ConstraintSynError Type
   | ExportListMergeError AvailInfo AvailInfo
   | UNameError Name Name
   | MissingBootThingError Bool Name String
   | HsigFileExportedError OccName Module
   | InterfaceFileNotFoundError Name [TyThing]
   | SourceImportError Module
   | HomeModError InstalledModule ModLocation
   | BadIfaceFileError String SDoc
   | LookupInstanceError SDoc
   | NotExactError
   | CheckWellStagedError SDoc ThLevel ThLevel
   | StageRestrictionError SDoc
   | GhcInternalError Name SDoc
   | WrongThingError String TcTyThing Name
   | ModuleDependsOnItselfError Module
   | CannotFindBootFileError Module SDoc
   | RelaxedPolyRecError
   | BadRuleLhsError FastString (LHsExpr Name) (HsExpr Name)
   | BadGadtStupidThetaError
   | IllegalUntypedSpliceError
   | IllegalTypedSpliceError
   | IllegalUntypedBracketError
   | IllegalTypedBracketError
   | IllegalBracketError
   | SafeEtaAnnotationError
   | QuotedNameStageError (HsBracket RdrName)
   | CannotFindInterfaceError SDoc
   | CannotFindModuleError SDoc
   | CountConstraintsError Int Type
   | CountTyFunAppsError Int Type
   | KindError Kind
   | UnliftedArgError Type
   | UbxArgTyErr Type
   | FailTHError String SDoc
   | ModuleNotImportedError ModuleName
   | ModuleDoesNotExportError ModIface ImpDeclSpec (IE RdrName)
   | BadImportItemDataConError OccName ModIface ImpDeclSpec (IE RdrName)
   | ExportClashError GlobalRdrEnv Name Name (IE RdrName) (IE RdrName)

instance Outputable TypeError where
   ppr (FunctionalDepsError ispecs)
       = hang herald 2 (pprInstances ispecs)
    where herald = text "Functional dependencies conflict between instance declarations:"

   ppr (DuplicateInstError ispecs)
      = hang herald 2 (pprInstances ispecs)
    where herald = text "Duplicate instance declarations:"

   ppr (FamilyInstError sorted)
      = hang herald
         2 (vcat [ pprCoAxBranchHdr (famInstAxiom fi) 0
                 | fi <- sorted ])
    where herald = text "Conflicting family instance declarations:"

   ppr (ArityError kind name n m)
       = hsep [ text kind, quotes (ppr name), text "should have",
                n_arguments <> comma, text "but has been given",
                if m==0 then text "none" else int m]
         where
             n_arguments | n == 0 = text "no arguments"
                         | n == 1 = text "1 argument"
                         | True   = hsep [int n, text "arguments"]

   ppr (ForAllTypeError rank ty)
       = vcat [ hang (text "Illegal polymorphic or qualified type:") 2 (ppr ty)
              , suggestion ]
      where
        suggestion = case rank of
                       LimitedRank {} -> text "Perhaps you intended to use RankNTypes or Rank2Types"
                       MonoType d     -> d
                       _              -> empty -- Polytype is always illegal
   ppr (BadIPPredError pred)
       = text "Illegal implicit parameter" <+> quotes (ppr pred)
   ppr (EqPredTypeError pred)
       = text "Illegal equational constraint" <+> pprType pred
                           $$
                           parens (text "Use GADTs or TypeFamilies to permit this")
   ppr (PredTyVarError pred)
       = hang (text "Non type-variable argument")
                               2 (text "in the constraint:" <+> pprType pred) $$ how_to_allow
         where how_to_allow = parens (text "Use FlexibleContexts to permit this")
   ppr (PredTupleError pred)
       = hang (text "Illegal tuple constraint:" <+> pprType pred)
                               2 (parens constraintKindsMsg)
   ppr (PredIrredError pred)
       = hang (text "Illegal constraint:" <+> pprType pred)
                               2 (parens constraintKindsMsg)
   ppr (PredIrredBadCtxtError pred)
       = hang (text "Illegal constraint" <+> quotes (pprType pred)
                                        <+> text "in a superclass/instance context")
                                      2 (parens undecidableMsg)
   ppr (InstTypeError cls tys msg)
       = hang (hang (text "Illegal instance declaration for")
                  2 (quotes (pprClassPred cls tys)))
            2 msg
   ppr (MalformedInstanceTypeError sdoc)
       = text "Malformed instance head:" <+> sdoc
   ppr (PredUndecError pred msg)
       = sep [msg, nest 2 (text "in the constraint:" <+> pprType pred)] $$ parens undecidableMsg
   ppr (BadATError clas op)
       = hsep [text "Class", quotes (ppr clas),
               text "does not have an associated type", quotes (ppr op)]
   ppr (WrongATArgError ty instTy)
       = sep [ ptext (sLit "Type indexes must match class instance head")
           , ptext (sLit "Found") <+> quotes (ppr ty)
             <+> ptext (sLit "but expected") <+> quotes (ppr instTy)
           ]
   ppr (NestedTypeFamilyError famInst)
       = famInstUndecErr famInst nestedMsg $$ parens undecidableMsg
   ppr (VariableMultipleOccurenceError famInst bad_tvs)
       = famInstUndecErr famInst (nomoreMsg bad_tvs) $$ parens undecidableMsg
   ppr (SmallerAppMsgError famInst)
      = famInstUndecErr famInst smallerAppMsg $$ parens undecidableMsg
   ppr (FamilyPatError fam_tc tvs pats)
      = hang (text "Family instance purports to bind type variable" <> plural tvs
              <+> pprQuotedList tvs)
           2 (hang (text "but the real LHS (expanding synonyms) is:")
                 2 (pprTypeApp fam_tc (map expandTypeSynonyms pats) <+> text "= ..."))
   ppr (TypeFamInstIllegalError ty)
       = hang (text "Illegal type synonym family application in instance" <>
              colon) 2 $
           ppr ty
   ppr (IllegalDerivingItemError hs_ty)
       = text "Illegal deriving item" <+> quotes (ppr hs_ty)
   ppr MalformedInstanceError = text "Malformed instance type"
   ppr (UnexpectedStrictAnnotationError ty)
       = text "Unexpected strictness annotation:" <+> ppr ty
   ppr (IllegalConstraintError hs_ty)
       = (hang (text "Illegal constraint:") 2 (ppr hs_ty))
   ppr (UnexpectedTypeSpliceError ty)
       = text "Unexpected type splice:" <+> ppr ty
   ppr (TooManyTypeArgumentsError the_fun)
       = quotes the_fun <+> text "is applied to too many type arguments"
   ppr (KindVariableTypeError tv)
       = text "Kind variable" <+> quotes (ppr tv) <+> text "used as a type"
   ppr (DataConstructorUnPromoError dc)
       = text "Data constructor" <+> quotes (ppr dc)
                                  <+> text "comes from an un-promotable type"
                                  <+> quotes (ppr (dataConTyCon dc))
   ppr (BadKindSignatureError kind)
       = hang (text "Kind signature on data type declaration has non-* return kind") 2 (ppr kind)
   ppr (PatternBindSignatureError sig_tvs)
       = hang (ptext (sLit "You cannot bind scoped type variable") <> plural sig_tvs
              <+> pprQuotedList (map fst sig_tvs))
           2 (ptext (sLit "in a pattern binding signature"))
   ppr (BadPatterSignatureError sig_ty bad_tvs)
       = vcat [ fsep [text "The type variable" <> plural bad_tvs,
                      quotes (pprWithCommas ppr bad_tvs),
                      text "should be bound by the pattern signature" <+> quotes (ppr sig_ty),
                      text "but are actually discarded by a type synonym" ]
              , text "To fix this, expand the type synonym"
              , text "[Note: I hope to lift this restriction in due course]" ]
   ppr (NotKindConstructorError ki)
       = (quotes (ppr ki) <+> text "is not a kind constructor")
   ppr (KindNotAppliedError name)
       = text "Kind" <+> ppr name <+> text "cannot be applied"
   ppr (ExpectedLiftedTypeError ty)
       = text "Expecting a lifted type, but" <+> quotes (ppr ty) <+> text "is unlifted"
   ppr (ExpectedUnLiftedTypeError ty)
       = text "Expecting an unlifted type, but" <+> quotes (ppr ty)
           <+> text "is lifted"
   ppr (KindOccurCheckError sdoc ty tidy_act_kind) = text "Kind occurs check"
        $$ more_info sdoc ty tidy_act_kind

   ppr (ExpectingArgumentError sdoc ty tidy_act_kind n_diff_as)
      = vcat [ text "Expecting" <+>
               speakN n_diff_as <+> text "more argument"
               <> (if n_diff_as > 1 then char 's' else empty)
               <+> text "to" <+> quotes (ppr ty)
             , more_info sdoc ty tidy_act_kind ]
   ppr (KindMisMatchError sdoc ty tidy_act_kind )
       = more_info sdoc ty tidy_act_kind
   ppr (DataKindsTypeError name )
       = hang (text "Illegal kind:" <+> quotes (ppr name))
            2 (text "Perhaps you intended to use DataKinds")
   ppr (TypeVariableUsedTypeError kind_var)
       = (text "Type variable" <+> quotes (ppr kind_var) <+> text "used as a kind")
   ppr (KindVariablePositionError name)
       = (text "Kind variable" <+> quotes (ppr name)
                      <+> text "cannot appear in a function position")
   ppr (TypeVariableUsedKindError name)
      = (text "Type variable" <+> quotes (ppr name)
                     <+> text "used in a kind")
   ppr (TypeConstructorUsedKind name)
      = (hang (text "Type constructor" <+> quotes (ppr name)
                           <+> text "used in a kind")
                        2 (text "inside its own recursive group"))
   ppr (OfKindError tc msg)
      = (quotes (ppr tc) <+> text "of kind" <+> quotes (ppr (tyConKind tc)) <+> text msg)
   ppr (PromotionError name err)
      = (hang (pprPECategory err <+> quotes (ppr name) <+> text "cannot be used here")
                       2 (parens reason))
         where
           reason = case err of
                      FamDataConPE -> text "it comes from a data family instance"
                      NoDataKinds  -> text "Perhaps you intended to use DataKinds"
                      _            -> text "it is defined and used in the same recursive group"
   ppr (CouldNotMatchKindError ki1' ki2')
       = hang (text "Couldn't match kind")
                 2 (sep [quotes (ppr ki1'), text "against", quotes (ppr ki2')])
   ppr GADTTypeFamiliesError
      = text "A pattern match on a GADT requires the" <+>
       text "GADTs or TypeFamilies language extension"
   ppr (BadFieldConstructorError con field)
       = hsep [text "Constructor" <+> quotes (ppr con),
               text "does not have field", quotes (ppr field)]
   ppr ExistentialLazyPatError
      = hang (text "An existential or GADT data constructor cannot be used")
           2 (text "inside a lazy (~) pattern")
   ppr ExistentialProcPatError
      = text "Proc patterns cannot use existential or GADT data constructors"
   ppr ExistentialLetPatError
      = vcat [text "My brain just exploded",
              text "I can't handle pattern bindings for existential or GADT data constructors.",
              text "Instead, use a case-expression, or do-notation, to unpack the constructor."]
   ppr (LazyUnliftedPatError pat)
       = hang (text "A lazy (~) pattern cannot contain unlifted types:") 2 (ppr pat)
   ppr BadBootDeclarationError = text "Illegal declarations in an hs-boot file"
   ppr (RecursivePatternSynonymError binds)
       = hang (text "Recursive pattern synonym definition with following bindings:")
          2 (vcat $ map pprLBind . bagToList $ binds)
      where
        pprLoc loc  = parens (text "defined at" <+> ppr loc)
        pprLBind (L loc bind) = pprWithCommas ppr (collectHsBindBinders bind) <+>
                                pprLoc loc
   ppr (DuplicateVectorisationError first)
        = text "Duplicate vectorisation declarations for" <+> ppr first
   ppr VectoriseScalarError = text "VECTORISE SCALAR type constructor must be nullary"
   ppr (StrictBindError flavour unlifted_bndrs binds)
       = hang (text flavour <+> msg <+> text "aren't allowed:")
            2 (vcat (map ppr binds))
       where
         msg | unlifted_bndrs = text "bindings for unlifted types"
             | otherwise      = text "bang-pattern or unboxed-tuple bindings"
   ppr (PolyBindError binds)
       = hang (text "You can't mix polymorphic and unlifted bindings")
            2 (vcat [vcat (map ppr binds),
                     text "Probable fix: add a type signature"])
   ppr (UnliftedMustBeBangError binds)
       = hang (text "Pattern bindings containing unlifted types should use an outermost bang pattern:")
            2 (vcat (map ppr binds))
   ppr (CheckArgumentsError fun match1 bad_matches)
       = vcat [text "Equations for" <+> quotes (ppr fun) <+>
                             text "have different numbers of arguments",
                           nest 2 (ppr (getLoc match1)),
                           nest 2 (ppr (getLoc (head bad_matches)))]
   ppr IfThenElseError = text "Predicate type of `ifThenElse' depends on result type"
   ppr (ArrowCommandTypeError cmd)
       = vcat [text "The expression", nest 2 (ppr cmd),
                           text "was found where an arrow command was expected"]
   ppr (WrongNumberOfParmsError ty_arity)
       = ptext (sLit "Number of pattern synonym arguments doesn't match type; expected")
         <+> ppr ty_arity
   ppr (BiDirectionalError lpat)
       = hang (text "Right-hand side of bidirectional pattern synonym cannot be used as an expression")
            2 (ppr lpat)
   ppr (NonBiDirectionalError name)
       = text "non-bidirectional pattern synonym"
          <+> quotes (ppr name) <+> text "used in an expression"
   ppr (AsPatternsDefinitionError pat)
       = hang (text "Pattern synonym definition cannot contain as-patterns (@):")
          2 (ppr pat)
   ppr (TemplateHaskellPatSynError pat)
       = hang (text "Pattern synonym definition cannot contain Template Haskell:")
          2 (ppr pat)
   ppr (NKPatSynError pat)
       = hang (text "Pattern synonym definition cannot contain n+k-pattern:")
          2 (ppr pat)
   ppr (CannotHandleTypeArgError ty)
       = text "Can't handle type argument:" <+> ppr ty
   ppr (NotRecordSelectorError field)
       = hsep [quotes (ppr field), text "is not a record selector"]
   ppr (BadFieldTypeError conflictingFields)
       = hang (text "No constructor has all these fields:")
            2 (pprQuotedList conflictingFields)
   ppr (BadFieldTypesError prs)
       = hang (text "Record update for insufficiently polymorphic field"
                              <> plural prs <> colon)
            2 (vcat [ ppr f <+> dcolon <+> ppr ty | (f,ty) <- prs ])
   ppr TagToEnumError = text "tagToEnum# must appear applied to one argument"
   ppr (IdentifierExpectedError thing)
       = ppr thing <+> text "used where a value identifier was expected"
   ppr (NaughtyRecordSelectorError sel_id)
       = text "Cannot use record selector" <+> quotes (ppr sel_id) <+>
         text "as a function due to escaped type variables" $$
         text "Probable fix: use pattern-matching syntax instead"
   ppr (BadCallTagToEnumError ty what)
       = hang (text "Bad call to tagToEnum#" <+> text "at type" <+> ppr ty) 2 what
   ppr (PolySpliceError id)
      = text "Can't splice the polymorphic local variable" <+> quotes (ppr id)
   ppr (MissingStrictFieldError con fields)
      = text "Constructor" <+> quotes (ppr con) <+>
               text "does not have the required strict field(s)" <> rest
       where
         rest | null fields = empty  -- Happens for non-record constructors
                                                -- with strict fields
              | otherwise   = colon <+> pprWithCommas ppr fields
   ppr (BadMethodError clas op)
       = hsep [text "Class", quotes (ppr clas),
               text "does not have a method", quotes (ppr op)]
   ppr (BadGenericMethodError clas op)
       = hsep [text "Class", quotes (ppr clas),
               text "has a generic-default signature without a binding", quotes (ppr op)]
   ppr (BadDefaultMethodError sel_id prag)
       = (text "The" <+> hsSigDoc prag <+> text "for default method"
                   <+> quotes (ppr sel_id)
                   <+> text "lacks an accompanying binding")
   ppr GHCForeignImportPrimError
      = text "Use GHCForeignImportPrim to allow `foreign import prim'."
   ppr SafeUnsafeAnnoError
      = text $ "The safe/unsafe annotation should not be used with "
       ++ "`foreign import prim'."
   ppr (IllegalForeignTypeError argOrRes extra)
       = hang msg 2 extra
       where
         msg = hsep [ text "Unacceptable", argOrRes
                    , text "type in foreign declaration:"]
   ppr (ForeignJavaError sdoc) = sdoc
   ppr (DuplicateDefaultError dup_things)
       = hang (text "Multiple default declarations")
            2 (vcat (map pp dup_things))
       where
         pp (L locn (DefaultDecl _)) = text "here was another default declaration" <+> ppr locn
   ppr (PolyDefaultError ty)
        = hang (text "Illegal polymorphic type in default declaration" <> colon) 2 (ppr ty)
   ppr (BadDefaultTypeError ty deflt_clss)
        = hang (text "The default type" <+> quotes (ppr ty) <+> text "is not an instance of")
             2 (foldr1 (\a b -> a <+> text "or" <+> b) (map (quotes. ppr) deflt_clss))
   ppr (BadSignatureTypeError tc_name)
        = vcat [ text "Illegal kind signature" <+>
                 quotes (ppr tc_name)
               , nest 2 (parens $ text "Use KindSignatures to allow kind signatures") ]
   ppr (MoreThanOneDeclarationError tfe_tycon)
       = text "More than one default declaration for" <+> ppr tfe_tycon
   ppr (WrongKindOfFamilyError family)
       = text "Wrong category of family instance; declaration was for a"
         <+> kindOfFamily
       where
         kindOfFamily | isTypeFamilyTyCon family = text "type family"
                      | isDataFamilyTyCon family = text "data family"
                      | otherwise = pprPanic "wrongKindOfFamily" (ppr family)
   ppr (WrongNumberOfParamsError max_args)
       = text "Number of parameters must match family declaration; expected"
         <+> ppr max_args
   ppr (WrongTypeFamilyError fam_tc_name eqn_tc_name)
      = hang (text "Mismatched type name in type family instance.")
           2 (vcat [ text "Expected:" <+> ppr fam_tc_name
                   , text "  Actual:" <+> ppr eqn_tc_name ])
   ppr (BadGADTDeclarationError tc_name)
       = vcat [ text "Illegal generalised algebraic data declaration for" <+> quotes (ppr tc_name)
              , nest 2 (parens $ text "Use GADTs to allow GADTs") ]
   ppr (BadStupidThetaError tc_name)
      = text "A data type declared in GADT style cannot have a context:" <+> quotes (ppr tc_name)
   ppr (NewtypeConError tycon n)
      = sep [text "A newtype must have exactly one constructor,",
             nest 2 $ text "but" <+> quotes (ppr tycon) <+> text "has" <+> speakN n ]
   ppr (EmptyConDeclarationError tycon)
      = sep [quotes (ppr tycon) <+> text "has no constructors",
             nest 2 $ text "(EmptyDataDecls permits this)"]
   ppr ClosedTypeFamilyError
          = text "You may omit the equations in a closed type family" $$
                text "only in a .hs-boot file"
   ppr (ResultTypeMisMatchError field_name con1 con2)
         = vcat [sep [text "Constructors" <+> ppr con1 <+> text "and" <+> ppr con2,
                      text "have a common field" <+> quotes (ppr field_name) <> comma],
                 nest 2 $ text "but have different result types"]
   ppr (FieldTypeMisMatchError field_name con1 con2)
         = sep [text "Constructors" <+> ppr con1 <+> text "and" <+> ppr con2,
                text "give different types for field", quotes (ppr field_name)]
   ppr (BadDataConTypeError data_con res_ty_tmpl actual_res_ty)
        = hang (text "Data constructor" <+> quotes (ppr data_con) <+>
                text "returns type" <+> quotes (ppr actual_res_ty))
             2 (text "instead of an instance of its parent type" <+> quotes (ppr res_ty_tmpl))
   ppr (BadExistentialError con)
       = hang (text "Data constructor" <+> quotes (ppr con) <+>
               text "has existential type variables, a context, or a specialised result type")
            2 (vcat [ ppr con <+> dcolon <+> ppr (dataConUserType con)
                    , parens $ text "Use ExistentialQuantification or GADTs to allow this" ])
   ppr (BadGADTKindConError data_con)
       = hang (text "Data constructor" <+> quotes (ppr data_con)
               <+> text "cannot be GADT-like in its *kind* arguments")
            2 (ppr data_con <+> dcolon <+> ppr (dataConUserType data_con))
   ppr (BadBangTypeError n herald con)
       = hang herald 2 (text "on the" <+> speakNth n
                        <+> text "argument of" <+> quotes (ppr con))
   ppr (NewtypeFieldError con_name n_flds)
       = sep [text "The constructor of a newtype must have exactly one field",
              nest 2 $ text "but" <+> quotes (ppr con_name) <+> text "has" <+> speakN n_flds]
   ppr (NewtypeStrictError con)
       = sep [text "A newtype constructor cannot have a strictness annotation,",
              nest 2 $ text "but" <+> quotes (ppr con) <+> text "does"]
   ppr (NewtypeConTypeError msg con)
       = (msg $$ ppr con <+> dcolon <+> ppr (dataConUserType con))
   ppr (ClassArityError cls n)
       = vcat [text howMany <+> text "parameters for class" <+> quotes (ppr cls),
             parens (text "Use MultiParamTypeClasses to allow" <+>
                             text allowWhat <+> text "classes")]
      where (howMany, allowWhat)
             | n == 0    = ("No", "no-parameter")
             | otherwise = ("Too many", "multi-parameter")
   ppr (ClassFunDepsError cls)
       = vcat [text "Fundeps in class" <+> quotes (ppr cls),
               parens (text "Use FunctionalDependencies to allow fundeps")]
   ppr (NoClassTyVarError clas what)
       = sep [text "The" <+> what,
              text "mentions none of the type or kind variables of the class" <+>
                     quotes (ppr clas <+> hsep (map ppr (classTyVars clas)))]
   ppr (ErrMsgTypeError tc_name)
       = hang (text "Illegal family declaration for" <+> quotes (ppr tc_name))
                    2 (text "Use TypeFamilies to allow indexed type families")
   ppr (NeedXRoleAnnotationsError tc)
       = text "Illegal role annotation for" <+> ppr tc <> char ';' $$
         text "did you intend to use RoleAnnotations?"
   ppr (WrongNoOfRolesError annots tyvars d)
       = hang (text "Wrong number of roles listed in role annotation;" $$
               text "Expected" <+> (ppr $ length tyvars) <> comma <+>
               text "got" <+> (ppr $ length annots) <> colon)
            2 (ppr d)
   ppr IncoherentRolesError
        = (text "Roles other than" <+> quotes (text "nominal") <+>
           text "for class parameters can lead to incoherence.") $$
          (text "Use IncoherentInstances to allow this; bad role found")
   ppr (BadRoleAnnotationError var annot inferred)
        = hang (text "Role mismatch on variable" <+> ppr var <> colon)
             2 (sep [ text "Annotation says", ppr annot
                    , text "but role", ppr inferred
                    , text "is required" ])
   ppr (RoleInterfaceInternalError doc)
        = vcat [text "Internal error in role inference:",
                doc,
                text "Please report this as a GHC bug: http://www.haskell.org/ghc/reportabug"]
   ppr (TypeSynDeclCycleError sorted_decls)
       = (sep [text "Cycle in type synonym declarations:",
                    nest 2 (vcat (map ppr_decl sorted_decls))])
       where ppr_decl (L loc decl) = ppr loc <> colon <+> ppr decl
   ppr (RecClassError cycles)
       = (sep [text "Cycle in class declaration (via superclasses):",
                      nest 2 (hsep (intersperse (text "->") (map ppr cycles)))])
   ppr (IllegalRoleAnnotDeclError tycon)
       = (text "Illegal role annotation for" <+> ppr tycon <> char ';' $$
          text "they are allowed only for datatypes and classes.")
   ppr MakeDerivSpecsError
      = (hang (text "Deriving not permitted in hs-boot file")
            2 (text "Use an instance declaration instead"))
   ppr DerivingNullaryError = text "Cannot derive instances for nullary classes"
   ppr (DerivingThingError newtype_deriving clas tys ty why)
       = sep [(hang (text "Can't make a derived instance of")
                  2 (quotes (ppr pred))
               $$ nest 2 extra) <> colon,
              nest 2 why]
       where
         extra | newtype_deriving = text "(even with cunning newtype deriving)"
               | otherwise        = empty
         pred = mkClassPred clas (tys ++ [ty])
   ppr (DerivingKindError tc cls cls_tys cls_kind)
       = hang (text "Cannot derive well-kinded instance of form"
                     <+> quotes (pprClassPred cls cls_tys <+> parens (ppr tc <+> text "...")))
            2 (text "Class" <+> quotes (ppr cls)
                 <+> text "expects an argument of kind" <+> quotes (pprKind cls_kind))
   ppr (DerivingEtaError cls cls_tys inst_ty)
       = sep [text "Cannot eta-reduce to an instance of form",
              nest 2 (text "instance (...) =>"
                     <+> pprClassPred cls (cls_tys ++ [inst_ty]))]
   ppr (GenericInstSafeHaskellError i)
       = hang (text "Generic instances can only be derived in Safe Haskell." $+$
               text "Replace the following instance:") 2 (pprInstanceHdr (iSpec i))
   ppr TypeableDoesNotSupportError
       = text "Class `Typeable` does not support user-specified instances."
   ppr BadBootFamInstDeclError = text "Illegal family instance in hs-boot file"
   ppr (AssocInClassError name)
       = text "Associated type" <+> quotes (ppr name) <+>
         text "must be inside a class instance"
   ppr (NotFamilyTypeError tycon)
      = vcat [ text "Illegal family instance for" <+> quotes (ppr tycon)
             , nest 2 $ parens (ppr tycon <+> text "is not an indexed type family")]
   ppr (NotOpenFamilyError tc)
       = text "Illegal instance for closed family" <+> quotes (ppr tc)
   ppr (TooFewParamsError arity)
       = text "Family instance has too few parameters; expected" <+>
         ppr arity
   ppr (MisplacedInstSigError name hs_ty)
      = vcat [ hang (text "Illegal type signature in instance declaration:")
                  2 (hang (pprPrefixName name)
                  2 (dcolon <+> ppr hs_ty))
                  , text "(Use InstanceSigs to allow this)" ]
   ppr (BadFamInstDeclError tc_name)
       = vcat [ text "Illegal family instance for" <+>
                quotes (ppr tc_name)
              , nest 2 (parens $ text "Use TypeFamilies to allow indexed type families") ]
   ppr AddTopDeclsError
      = text "Declaration splices are not permitted inside top-level declarations added with addTopDecls"
   ppr (BadBootDeclError doc)
      = doc
   ppr (InstMisMatchError is_boot inst)
      = hang (ppr inst)
           2 (text "is defined in the" <+>
            (if is_boot then text "hs-boot" else text "hsig")
           <+> text "file, but not in the module itself")
   ppr (BootMisMatchError is_boot extra_info real_thing boot_thing)
      = vcat [ppr real_thing <+>
              text "has conflicting definitions in the module",
              text "and its" <+>
                (if is_boot then text "hs-boot file"
                           else text "hsig file"),
               text "Main module:" <+> pprTyThing real_thing,
              (if is_boot
                then text "Boot file:  "
                else text "Hsig file: ")
                <+> pprTyThing boot_thing,
              extra_info]
   ppr (NoMainMsgError sdoc) = sdoc
   ppr (CheckMainExportedError sdoc) = sdoc
   ppr (EtaREPLUnliftedError id)
       = (sep [text "Eta REPL can't bind a variable of unlifted type:",
               nest 2 (ppr id <+> dcolon <+> ppr (idType id))])
   ppr AmbiguousTypeError = text "Ambigous type!"
   ppr (CannotFindTypeError ty) = text ("Can't find type:" ++ ty)
   ppr (NotInScopeTypeError rdr_name)
       = text "Not in scope:" <+> quotes (ppr rdr_name)
   ppr (IllegalPolyTypeError ty)
       = vcat [ text "Illegal polytype:" <+> ppr ty
              , text "The type of a Typed Template Haskell expression must" <+>
                text "not have any quantification." ]
   ppr (THTypeError sdoc) = sdoc
   ppr (THExceptionError sdoc) = sdoc
   ppr (THUserError sdoc) = sdoc
   ppr CheckTopDeclError
     = text "Only function, value, annotation, and foreign import declarations may be added with addTopDecl"
   ppr (BindNameTypeError name)
     = hang (text "The binder" <+> quotes (ppr name) <+> ptext (sLit "is not a NameU."))
        2 (text "Probable cause: you used mkName instead of newName to generate a binding.")
   ppr (ReifyInstancesError ty)
       = hang (text "reifyInstances:" <+> quotes (ppr ty))
                          2 (text "is not a class constraint or type family application")
   ppr (NotInScopeTHError th_name)
       = quotes (text (TH.pprint th_name)) <+>
                            text "is not in scope at a reify"
   ppr (NotInEnvError name)
       = quotes (ppr name) <+> text "is not in the type environment at a reify"
   ppr (NoRolesAssociatedError thing)
      = text "No roles associated with" <+> (ppr thing)
   ppr (NoTemplateHaskellError s d)
      = (hsep [text "Can't represent" <+> ptext s <+>
                                      text "in Template Haskell:",
                                   nest 2 d])
   ppr (CvtHsTypeError sdoc) = sdoc
   -- Renamer

   ppr (OriginalBindingError name)
      = text "Illegal binding of built-in syntax:" <+> ppr (rdrNameOcc name)

   ppr (BadQualifiedError rdr_name)
      = text "Qualified name in binding position:" <+> ppr rdr_name

   ppr (DeclarationError n)
      = hang (text "Illegal declaration of a type or class operator" <+> quotes (ppr n))
           2 (text "Use TypeOperators to declare operators in type and declarations")
   ppr (ExactNameError name)
      = hang (text "The exact Name" <+> quotes (ppr name) <+> text "is not in scope")
          2 (vcat [ text "Probable cause: you used a unique Template Haskell name (NameU), "
                  , text "perhaps via newName, but did not bind it"
                  , text "If that's it, then -ddump-splices might be useful" ])

   ppr (DuplicateNameError name)
      = hang (text "Duplicate exact Name" <+> quotes (ppr $ nameOccName name))
          2 (vcat [ text "Probable cause: you used a unique Template Haskell name (NameU), "
                  , text "perhaps via newName, but bound it multiple times"
                  , text "If that's it, then -ddump-splices might be useful" ])
   ppr (UnknownSubordinateError doc op)
      = quotes (ppr op) <+> text "is not a (visible)" <+> doc

   ppr (AccompanyingBindingError local what rdr_name)
       = (sep [ text "The" <+> what <+> text "for" <+> quotes (ppr rdr_name)
              , nest 2 $ text "lacks an accompanying binding"] $$ nest 2 msg)
       where msg
               | local = parens $ text "The" <+> what <+> text "must be given where"
                               <+> quotes (ppr rdr_name) <+> text "is declared"
               | otherwise = empty

   ppr (NotInScopeError what rdr_name extra suggestions)
       = vcat [ hang (text "Not in scope:")
                    2 (what <+> quotes (ppr rdr_name))
               , extra' ] $$ extra $$ fromMaybe empty suggestions
        where
          extra' | rdr_name == forall_tv_RDR = perhapsForallMsg
                 | otherwise                 = empty

   ppr (NameClashError rdr_name gres)
       = (vcat [text "Ambiguous occurrence" <+> quotes (ppr rdr_name),
                      text "It could refer to" <+> vcat (msg1 : msgs)])
        where
          (np1:nps) = gres
          msg1 = text "either" <+> mk_ref np1
          msgs = [text "    or" <+> mk_ref np | np <- nps]
          mk_ref gre = sep [quotes (ppr (gre_name gre)) <> comma, pprNameProvenance gre]

   ppr (DuplicateNamesError sdoc locs)
       = vcat [text "Conflicting definitions for" <+> quotes sdoc,
             locations]
        where locations = text "Bound at:" <+> vcat (map ppr (sort locs))
   ppr (TupleSizeError tup_size)
       = (sep [ptext (sLit "A") <+> int tup_size <> text "-tuple is too large for GHC",
                      nest 2 (parens (text "max size is" <+> int mAX_TUPLE_SIZE)),
                      nest 2 (text "Workaround: use nested tuples or define a data type")])

   ppr (BadInstanceError ty)
       = ptext (sLit "Malformed instance:") <+> ppr ty
   ppr (OperatorError op ty ty1)
       = hang (ptext (sLit "Illegal operator") <+> quotes (ppr op) <+> ptext (sLit "in type") <+> quotes (ppr ty))
              2 extra
       where
         extra | op == dot_tv_RDR && forall_head ty1
               = perhapsForallMsg
               | otherwise
               = text "Use TypeOperators to allow operators in types"

         forall_head (L _ (HsTyVar tv))   = tv == forall_tv_RDR
         forall_head (L _ (HsAppTy ty _)) = forall_head ty
         forall_head _other               = False

   ppr (RecordSyntaxError ty) = (hang (text "Record syntax is illegal here:") 2 (ppr ty))

   ppr (DataKindsError is_type thing)
        = hang (text "Illegal" <+> what <> colon <+> quotes (ppr thing))
             2 (text "Perhaps you intended to use DataKinds")
        where
          what | is_type   = text "type"
               | otherwise = text "kind"
   ppr (NegativeLiteralError tyLit)
        = text "Illegal literal in type (type literals must not be negative):" <+> ppr tyLit
   ppr (BadKindBinderError doc kvs)
       = vcat [ hang (text "Unexpected kind variable" <> plural kvs
                      <+> pprQuotedList kvs)
                   2 (text "Perhaps you intended to use PolyKinds")
              , doc ]
   ppr (OverlappingKindError doc kvs)
       = vcat [ text "Kind variable" <> plural kvs <+>
                text "also used as type variable" <> plural kvs
                <> colon <+> pprQuotedList kvs
              , doc ]
   ppr (BadSignatureError is_type doc ty)
        = vcat [ hang (text "Illegal" <+> what
                     <+> text "signature:" <+> quotes (ppr ty))
                     2 (text "Perhaps you intended to use" <+> flag)
               , doc ]
      where
        what | is_type   = text "type"
             | otherwise = text "kind"
        flag | is_type   = text "ScopedTypeVariables"
             | otherwise = text "KindSignatures"
   ppr (SectionPrecedenceError op arg_op section)
       = vcat [text "The operator" <+> ppr_opfix op <+> text "of a section",
              nest 4 (sep [text "must have lower precedence than that of the operand,",
                           nest 2 (text "namely" <+> ppr_opfix arg_op)]),
              nest 4 (text "in the section:" <+> quotes (ppr section))]

   ppr (PrecedenceParseError op1 op2)
       = hang (text "Precedence parsing error")
           4 (hsep [text "cannot mix", ppr_opfix op1, text "and",
                    ppr_opfix op2,
                    text "in the same infix expression"])
   ppr (BadViewPatError pat)
      = vcat [text "Illegal view pattern: " <+> ppr pat,
              text "Use ViewPatterns to enable view patterns"]

   ppr (DuplicateFieldError ctxt dups)
       = hsep [text "duplicate field name",
               quotes (ppr (head dups)),
               text "in record", ctxt]

   ppr EmptyRecordError = text "Empty record update"

   ppr (NamedFieldPunsError fld)
       = vcat [text "Illegal use of punning for field" <+> quotes (ppr fld),
               text "Use NamedFieldPuns to permit this"]
   ppr DotDotRecordError = text "You cannot use `..' in a record update"

   ppr (RecordWildCardsError ctxt)
        = vcat [text "Illegal `..' in record" <+> ctxt,
                text "Use RecordWildCards to permit this"]

   ppr (BadDotDotConError con)
        = vcat [ text "Illegal `..' notation for constructor" <+> quotes (ppr con)
               , nest 2 (text "The constructor has no labelled fields") ]
   ppr (BogusCharacterError c)
       = text "character literal out of range: '\\" <> char c  <> char '\''
   ppr PackageImportError
       = text "Package-qualified imports are not enabled; use PackageImports"
   ppr (ModuleCycleError imp_mod_name)
       = text "A module cannot import itself:" <+> ppr imp_mod_name
   ppr SafeEtaError
      = (text "safe import can't be used as Safe Eta isn't on!"
           $+$ text "please enable Safe Eta through either Safe, Trustworthy or Unsafe")
   ppr ClassNotFoundError = text "Unable to find class in the classpath."
   ppr (ClassTransitiveError notFound)
      = text $ "Class '" ++ notFound ++ "' was a transitive dependency"
            ++ " of one of your java imports and was not found on the classpath."
   ppr (BindingsBootFileError mbinds)
       = hang (text "Bindings in hs-boot files are not allowed") 2 (ppr mbinds)
   ppr (MultipleFixityDeclError loc rdr_name)
       = vcat [text "Multiple fixity declarations for" <+> quotes (ppr rdr_name),
               text "also at " <+> ppr loc]
   ppr (LocalPatterSynonymError rdrname)
      = hang (text "Illegal pattern synonym declaration for" <+> quotes (ppr rdrname))
           2 (text "Pattern synonym declarations are only valid at top level")
   ppr PatternSynonymError
       = hang (text "Illegal pattern synonym declaration")
            2 (text "Use -XPatternSynonyms to enable this extension")
   ppr (DefaultSignatureError sig)
      = vcat [ hang (text "Unexpected default signature:")
               2 (ppr sig)
               , text "Use DefaultSignatures to enable default signatures" ]
   ppr (EmptyCaseError ctxt)
       = hang (text "Empty list of alternatives in" <+> pp_ctxt)
                              2 (text "Use EmptyCase to allow this")
         where
           pp_ctxt = case ctxt of
                       CaseAlt    -> text "case expression"
                       LambdaExpr -> text "\\case expression"
                       _          -> text "(unexpected)" <+> pprMatchContextNoun ctxt
   ppr (ResultSignatureError ty doc)
      = vcat [ text "Illegal result type signature" <+> quotes (ppr ty)
             , nest 2 $ text
                    "Result signatures are no longer supported in pattern matches"
             , doc ]
   ppr (MethodBindError mbind)
       = hang (text "Pattern bindings (except simple variables) not allowed in instance declarations")
             2 (ppr mbind)
   ppr (MethodPatternSynonymError mbind)
       = hang (text "Pattern synonyms not allowed in class/instance declarations")
             2 (ppr mbind)
   ppr (DuplicateSignatureError pairs name sig)
       = vcat [ text "Duplicate" <+> what_it_is
             <> text "s for" <+> quotes (ppr name)
              , text "at" <+> vcat (map ppr $ sort $ map (getLoc . fst) pairs) ]
     where
       what_it_is = hsSigDoc sig
   ppr (DuplicateMinimalError sigs)
       = vcat [ text "Multiple minimal complete definitions"
              , text "at" <+> vcat (map ppr $ sort $ map getLoc sigs)
              , text "Combine alternative minimal complete definitions with `|'" ]
   ppr (DuplicateWarningError loc rdr_name)
       = vcat [text "Multiple warning declarations for" <+> quotes (ppr rdr_name),
               text "also at " <+> ppr loc]
   ppr (DuplicateRoleAnnotationError sorted_list)
       = hang (text "Duplicate role annotations for" <+>
             quotes (ppr $ roleAnnotDeclName first_decl) <> colon)
          2 (vcat $ map pp_role_annot sorted_list)
       where
         (L _loc first_decl : _) = sorted_list

         pp_role_annot (L loc decl) = hang (ppr decl)
                                         4 (text "-- written at" <+> ppr loc)

         _cmp_annot (L loc1 _) (L loc2 _) = loc1 `compare` loc2
   ppr (OrphanRoleError decl)
       = hang (text "Role annotation for a type previously declared:")
          2 (ppr decl) $$
         parens (text "The role annotation must be given where" <+>
                 quotes (ppr $ roleAnnotDeclName decl) <+>
                 text "is declared.")
   ppr (BadAssociatedTypeErrors ns)
       = (hang (text "The RHS of an associated type declaration mentions"
            <+> pprWithCommas (quotes . ppr) ns)
             2 (text "All such variables must be bound on the LHS"))
   ppr BadImplicitSpliceError
      = text "Parse error: naked expression at top level"
         $$ text "Perhaps you intended to use TemplateHaskell"
   ppr (BadConstructorError doc)
       = text "Malformed constructor signature" $$ doc
   ppr StandaloneDeriveError
       = hang (text "Illegal standalone deriving declaration")
            2 (text "Use StandaloneDeriving to enable this extension")
   ppr (BadRuleVarError name var)
       = sep [text "Rule" <+> doubleQuotes (ftext name) <> colon,
              text "Forall'd variable" <+> quotes (ppr var) <+>
              text "does not appear on left hand side"]
   ppr VectorisePragmaError
        = vcat [ text "IMPLEMENTATION RESTRICTION: right-hand side of a VECTORISE pragma"
               , text "must be an identifier" ]
   ppr (MisplacedSignatureError sig)
       = sep [text "Misplaced" <+> hsSigDoc sig <> colon, ppr sig]
   ppr IllegalImportError = text "Illegal import item"
   ppr (IllegalDataConError name)
       = hsep [text "Illegal data constructor name", quotes (ppr name)]
   ppr (IllegalQualNameError rdr)
       = hang (text "Illegal qualified name in import item:") 2 (ppr rdr)
   ppr (ExportItemError export_item)
       = sep [ text "The export item" <+> quotes (ppr export_item),
          text "attempts to export constructors or class methods that are not visible here" ]
   ppr (MultipleDeclarationError name sorted_names)
       = vcat [text "Multiple declarations of" <+>
                quotes (ppr (nameOccName name)),
                -- NB. print the OccName, not the Name, because the
                -- latter might not be in scope in the RdrEnv and so will
                -- be printed qualified.
             text "Declared at:" <+>
                      vcat (map (ppr . nameSrcLoc) sorted_names)]

   ppr (SimpleDeclarationError sorted_names)
       = vcat [text "Multiple declarations with names differing only in case.",
               text "Declared at:" <+>
                      vcat (map (ppr . nameSrcLoc) sorted_names)]
   ppr (TemplateHaskellExtensionError e)
       = ( vcat [ text "Syntax error on" <+> ppr e
                  , text ("Perhaps you intended to use TemplateHaskell"
                          ++ " or TemplateHaskellQuotes") ] )
   ppr (TemplateHaskellSpliceError other_op)
       = (vcat [ hang (text "Infix application with a non-variable operator:")
                             2 (ppr other_op)
                      , text "(Probably resulting from a Template Haskell splice)" ])
   ppr (SectionClosedError expr)
       = hang (text "A section must be enclosed in parentheses")
            2 (text "thus:" <+> (parens (ppr expr)))
   ppr StaticInterpretedModeError
        = sep [ text "The static form is not supported in interpreted mode."
              , text "Please use -fobject-code." ]
   ppr (StaticFormSpliceError e)
       = sep [ text "static forms cannot be used in splices:"
              , nest 2 $ ppr e ]
   ppr (TopLevelBindingsError e fvNonGlobal)
       = cat [ text $ "Only identifiers of top-level bindings can "
                    ++ "appear in the body of the static form:"
             , nest 2 $ ppr e
             , text "but the following identifiers were found instead:"
             , nest 2 $ vcat $ map ppr fvNonGlobal
             ]
   ppr (ArrowCommandExprError e)
       = (vcat [ text "Arrow command found where an expression was expected:"
                            , nest 2 (ppr e) ])
   ppr (ParallelListCompError vs)
       = (text "Duplicate binding in parallel list comprehension for:"
                       <+> quotes (ppr (head vs)))
   ppr (ImplicitParameterError what sdoc)
       = hang (text "Implicit-parameter bindings illegal in" <+> what)
              2 sdoc
   ppr EmptyParStmtError
        = text "Empty statement group in parallel comprehension"
   ppr EmptyTransStmtError
       = text "Empty statement group preceding 'group' or 'then'"
   ppr (EmptyListCtxtError ctxt)
       = text "Empty" <+> pprStmtContext ctxt
   ppr (LastStatementError ctxt sdoc)
       = hang last_error 2 sdoc
        where last_error = text  "The last statement in" <+> pprAStmtContext ctxt
                        <+> text "must be an expression"
   ppr (CheckStatementError sdoc ctxt extra)
       = sep [ text "Unexpected" <+> sdoc <+> text "statement"
             , text "in" <+> pprAStmtContext ctxt ] $$ extra
   ppr TupleSectionsError = text "Illegal tuple section: use TupleSections"
   ppr (SectionParenthesesError expr)
       = hang (text "A section must be enclosed in parentheses")
            2 (text "thus:" <+> (parens (ppr expr)))
   ppr (PatternSyntaxExpressionError e)
       = (sep [text "Pattern syntax in expression context:",
                                       nest 4 (ppr e)])
   ppr (BadReexportBootThingError style is_boot name name' )
       = withPprStyle style $
          vcat [ text "The" <+> (if is_boot then text "hs-boot" else text "hsig")
            <+> text "file (re)exports" <+> quotes (ppr name)
              , text "but the implementing module exports a different identifier" <+> quotes (ppr name')
             ]
   ppr (HiModuleInterfaceError err isig_mod)
       = hang (text "Could not find hi interface for signature" <+>
             quotes (ppr isig_mod) <> colon) 4 err
   ppr (UserTypeError sdoc) = sdoc
   ppr (ConstraintSynError kind)
       = hang (text "Illegal constraint synonym of kind:" <+> quotes (ppr kind))
                                  2 (parens constraintKindsMsg)
   ppr (ExportListMergeError a1 a2)
       = text "While merging export lists, could not combine"
                                  <+> ppr a1 <+> text "with" <+> ppr a2
                                  <+> parens (text "one is a type, the other is a plain identifier")
   ppr (UNameError n1 n2)
       = (text "While merging export lists, could not unify"
                            <+> ppr n1 <+> text "with" <+> ppr n2 $$ extra)
        where
          extra | isHoleName n1 || isHoleName n2
                = text "Neither name variable originates from the current signature."
                | otherwise
                = empty
   ppr (MissingBootThingError is_boot name what)
       = ppr name <+> text "is exported by the" <+>
                   (if is_boot then text "hs-boot" else text "hsig")
                   <+> text "file, but not"
                   <+> text what <+> text "the module"
   ppr (HsigFileExportedError occ impl_mod)
       = quotes (ppr occ)
               <+> text "is exported by the hsig file, but not exported the module"
               <+> quotes (ppr impl_mod)
   ppr (InterfaceFileNotFoundError name ty_things)
        = ifPprDebug (found_things_msg $$ empty)
                                $$ not_found_msg
          where
            not_found_msg = hang (text "Can't find interface-file declaration for" <+>
                            pprNameSpace (occNameSpace (nameOccName name)) <+> ppr name)
                   2 (vcat [text "Probable cause: bug in .hi-boot file, or inconsistent .hi file",
                            text "Use -ddump-if-trace to get an idea of which file caused the error"])
            found_things_msg =
                    hang (text "Found the following declarations in" <+> ppr (nameModule name) <> colon)
                       2 (vcat (map ppr ty_things))
   ppr (SourceImportError mod)
       = hang (text "You cannot {-# SOURCE #-} import a module from another package")
            2 (text "but" <+> quotes (ppr mod) <+> text "is from package"
               <+> quotes (ppr (moduleUnitId mod)))
   ppr (HomeModError mod location)
       = text "attempting to use module " <> quotes (ppr mod)
         <> (case ml_hs_file location of
                Just file -> space <> parens (text file)
                Nothing   -> empty)
         <+> text "which is not loaded"
   ppr (BadIfaceFileError file err)
       = vcat [text "Bad interface file:" <+> text file,
               nest 4 err]
   ppr (LookupInstanceError err)
       = text "Couldn't match instance:" <+> err
   ppr NotExactError = text "Not an exact match (i.e., some variables get instantiated)"
   ppr (CheckWellStagedError pp_thing bind_lvl use_lvl)
       = text "Stage error:" <+> pp_thing <+>
           hsep   [text "is bound at stage" <+> ppr bind_lvl,
                   text "but used at stage" <+> ppr use_lvl]
   ppr (StageRestrictionError pp_thing)
        = sep [ text "GHC stage restriction:"
            , nest 2 (vcat [ pp_thing <+> text "is used in a top-level splice or annotation,"
                           , text "and must be imported, not defined locally"])]
   ppr (GhcInternalError name sdoc)
       = vcat [text "GHC internal error:" <+> quotes (ppr name) <+>
               text "is not in scope during type checking, but it passed the renamer",
               text "tcl_env of environment:" <+> sdoc]
   ppr (WrongThingError expected thing name)
       = (pprTcTyThingCategory thing <+> quotes (ppr name) <+>
                     ptext (sLit "used as a") <+> text expected)
   ppr (ModuleDependsOnItselfError mod)
       = text "Circular imports: module" <+> quotes (ppr mod)
           <+> text "depends on itself"
   ppr (CannotFindBootFileError mod err)
       = hang (text "Could not find hi-boot interface for" <+>
                             quotes (ppr mod) <> colon) 4 err
   ppr RelaxedPolyRecError
      = (vcat [text "Contexts differ in length",
          nest 2 $ parens $ text "Use RelaxedPolyRec to allow this"])
   ppr (BadRuleLhsError name lhs bad_e)
      = sep [text "Rule" <+> ftext name <> colon,
             nest 4 (vcat [text "Illegal expression:" <+> ppr bad_e,
                           text "in left-hand side:" <+> ppr lhs])]
        $$
        ptext (sLit "LHS must be of form (f e1 .. en) where f is not forall'd")
   ppr BadGadtStupidThetaError
      = vcat [text "No context is allowed on a GADT-style data declaration",
           text "(You can put a context on each contructor, though.)"]
   ppr IllegalUntypedSpliceError = text "Untyped splices may not appear in typed brackets"
   ppr IllegalTypedSpliceError = text "Typed splices may not appear in untyped brackets"
   ppr IllegalUntypedBracketError = text "Untyped brackets may only appear in untyped splices."
   ppr IllegalTypedBracketError = text "Typed brackets may only appear in typed splices."
   ppr IllegalBracketError
      = text "Template Haskell brackets cannot be nested" <+>
            text "(without intervening splices)"
   ppr SafeEtaAnnotationError = vcat [ text "Annotations are not compatible with Safe Eta."]
   ppr (QuotedNameStageError br)
       = sep [ text "Stage error: the non-top-level quoted name" <+> ppr br
             , text "must be used at the same stage at which it is bound" ]
   ppr (CannotFindInterfaceError sdoc) = sdoc
   ppr (CannotFindModuleError sdoc) = sdoc
   ppr (CountConstraintsError value tidy_pred)
       = hang msg 2 (ppr tidy_pred)
       where
         msg = vcat [ text "Context reduction stack overflow; size =" <+> int value
                    , text "Use -fcontext-stack=N to increase stack size to N" ]
   ppr (CountTyFunAppsError value tidy_pred)
       = hang msg 2 (ppr tidy_pred)
       where
         msg = vcat [ text "Type function application stack overflow; size =" <+> int value
                    , text "Use -ftype-function-depth=N to increase stack size to N" ]
   ppr (KindError kind )
       = sep [text "Expecting an ordinary type, but found a type of kind", ppr kind]
   ppr (UnliftedArgError ty)
       = sep [text "Illegal unlifted type:", ppr ty]
   ppr (UbxArgTyErr ty)
       = sep [text "Illegal unboxed tuple type as function argument:", ppr ty]
   ppr (FailTHError what sdoc)
        = (vcat [ hang (char 'A' <+> text what
                                 <+> text "requires GHC with interpreter support:")
                                  2 sdoc
                                  , text "Perhaps you are using a stage-1 compiler?" ])
   ppr (ModuleNotImportedError mod)
       = text "The export item `module" <+> ppr mod <> text "' is not imported"
   ppr (ModuleDoesNotExportError iface decl_spec ie)
         = sep [text "Module", quotes (ppr (is_mod decl_spec)), source_import,
                text "does not export", quotes (ppr ie)]
         where
           source_import | mi_boot iface = text "(hi-boot interface)"
                         | otherwise     = empty
   ppr (BadImportItemDataConError dataType_occ iface decl_spec ie)
      = vcat [ text "In module"
                 <+> quotes (ppr (is_mod decl_spec))
                 <+> source_import <> colon
             , nest 2 $ quotes datacon
                 <+> text "is a data constructor of"
                 <+> quotes dataType
             , text "To import it use"
             , nest 2 $ quotes (text "import")
                 <+> ppr (is_mod decl_spec)
                 <> parens_sp (dataType <> parens_sp datacon)
             , ptext (sLit "or")
             , nest 2 $ quotes (text "import")
                 <+> ppr (is_mod decl_spec)
                 <> parens_sp (dataType <> text "(..)")
             ]
      where
        datacon_occ = rdrNameOcc $ ieName ie
        datacon = parenSymOcc datacon_occ (ppr datacon_occ)
        dataType = parenSymOcc dataType_occ (ppr dataType_occ)
        source_import | mi_boot iface = text "(hi-boot interface)"
                      | otherwise     = empty
        parens_sp d = parens (space <> d <> space)  -- T( f,g )
   ppr (ExportClashError global_env name1 name2 ie1 ie2)
        = vcat [ text "Conflicting exports for" <+> quotes (ppr occ) <> colon
               , ppr_export ie1' name1'
               , ppr_export ie2' name2' ]
        where
          occ = nameOccName name1
          ppr_export ie name = nest 3 (hang (quotes (ppr ie) <+> text "exports" <+>
                                             quotes (ppr name))
                                          2 (pprNameProvenance (get_gre name)))

          -- get_gre finds a GRE for the Name, so that we can show its provenance
          get_gre name
              = case lookupGRE_Name global_env name of
                   (gre:_) -> gre
                   []      -> pprPanic "exportClashErr" (ppr name)
          get_loc name = greSrcSpan (get_gre name)
          (name1', ie1', name2', ie2') = if get_loc name1 < get_loc name2
                                         then (name1, ie1, name2, ie2)
                                         else (name2, ie2, name1, ie1)

pprSigCtxt :: UserTypeCtxt -> SDoc -> SDoc -> ContextElement
-- (pprSigCtxt ctxt <extra> <type>)
-- prints    In <extra> the type signature for 'f':
--              f :: <type>
-- The <extra> is either empty or "the ambiguity check for"
pprSigCtxt ctxt extra pp_ty = SignatureCtxt $ SignatureContext ctxt extra pp_ty

pprMatchInCtxt :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
               => HsMatchContext idL -> Match idR body -> ContextElement
pprMatchInCtxt ctxt match  = MatchCtxt (pprMatchContext ctxt) (pprMatch ctxt match)

pprStmtInCtxt :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
               => HsStmtContext idL -> StmtLR idL idR body -> ContextElement
pprStmtInCtxt ctxt (LastStmt e _ _)
  | isListCompExpr ctxt      -- For [ e | .. ], do not mutter about "stmts"
  = ListComprehensionCtxt (ppr e)

pprStmtInCtxt ctxt stmt
  = StatementCtxt (pprAStmtContext ctxt) (ppr_stmt stmt)
  where
    -- For Group and Transform Stmts, don't print the nested stmts!
    ppr_stmt (TransStmt { trS_by = by, trS_using = using
                        , trS_form = form }) = pprTransStmt by using form
    ppr_stmt stmt = pprStmt stmt

inst_decl_ctxt :: SDoc -> SDoc
inst_decl_ctxt doc = hang (text "In the instance declaration for")
                        2 (quotes doc)

ppr_th :: TH.Ppr a => a -> SDoc
ppr_th x = text (TH.pprint x)

perhapsForallMsg :: SDoc
perhapsForallMsg
  = vcat [ ptext (sLit "Perhaps you intended to use ExplicitForAll or similar flag")
         , ptext (sLit "to enable explicit-forall syntax: forall <tvs>. <type>")]

ppr_opfix :: (Name, Fixity) -> SDoc
ppr_opfix (op, fixity) = pp_op <+> brackets (ppr fixity)
  where
    pp_op | op == negateName = ptext (sLit "prefix `-'")
          | otherwise        = quotes (ppr op)

-- the SrcSpan that pprNameProvenance prints out depends on whether
-- the Name is defined locally or not: for a local definition the
-- definition site is used, otherwise the location of the import
-- declaration.  We want to sort the export locations in
-- exportClashErr by this SrcSpan, we need to extract it:
greSrcSpan :: GlobalRdrElt -> SrcSpan
greSrcSpan gre
  | Imported (is:_) <- gre_prov gre = is_dloc (is_decl is)
  | otherwise                       = name_span
  where
    name_span = nameSrcSpan (gre_name gre)

undecidableMsg, constraintKindsMsg :: SDoc
undecidableMsg     = ptext (sLit "Use UndecidableInstances to permit this")
constraintKindsMsg = ptext (sLit "Use ConstraintKinds to permit this")

pprUserTypeErrorTy :: Type -> TypeError
pprUserTypeErrorTy ty = UserTypeError $ pprUserTypeErrorTy' ty

pprUserTypeErrorTy' :: Type -> SDoc
pprUserTypeErrorTy' ty =
  case splitTyConApp_maybe ty of

    -- Text "Something"
    Just (tc,[txt])
      | tyConName tc == typeErrorTextDataConName
      , Just str <- isStrLitTy txt -> ftext str

    -- ShowType t
    Just (tc,[_k,t])
      | tyConName tc == typeErrorShowTypeDataConName -> ppr t

    -- t1 :<>: t2
    Just (tc,[t1,t2])
      | tyConName tc == typeErrorAppendDataConName ->
        pprUserTypeErrorTy' t1 <> pprUserTypeErrorTy' t2

    -- t1 :$$: t2
    Just (tc,[t1,t2])
      | tyConName tc == typeErrorVAppendDataConName ->
        pprUserTypeErrorTy' t1 $$ pprUserTypeErrorTy' t2

    -- An uneavaluated type function
    _ -> ppr ty

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

famInstUndecErr :: Type -> SDoc -> SDoc
famInstUndecErr ty msg
  = sep [msg,
         nest 2 (ptext (sLit "in the type family application:") <+>
                 pprType ty)]

nestedMsg, smallerAppMsg :: SDoc
nestedMsg     = text "Nested type family application"
smallerAppMsg = text "Application is no smaller than the instance head"

nomoreMsg :: [TcTyVar] -> SDoc
nomoreMsg tvs
  = sep [ text "Variable" <> plural tvs <+> quotes (pprWithCommas ppr tvs)
        , (if isSingleton tvs then text "occurs"
                                  else text "occur")
          <+> text "more often than in the instance head" ]

more_info :: SDoc -> HsType Name -> Type -> SDoc
more_info sdoc ty tidy_act_kind = sep [ sdoc <> comma
              , nest 2 $ text "but" <+> quotes (ppr ty)
                <+> text "has kind" <+> quotes (pprKind tidy_act_kind)]

data InstInfo a
  = InstInfo {
      iSpec   :: ClsInst,        -- Includes the dfun id.  Its forall'd type
      iBinds  :: InstBindings a   -- variables scope over the stuff in InstBindings!
    }

iDFunId :: InstInfo a -> DFunId
iDFunId info = instanceDFunId (iSpec info)

data InstBindings a
  = InstBindings
      { ib_tyvars  :: [Name]        -- Names of the tyvars from the instance head
                                    -- that are lexically in scope in the bindings

      , ib_binds   :: (LHsBinds a)  -- Bindings for the instance methods

      , ib_pragmas :: [LSig a]      -- User pragmas recorded for generating
                                    -- specialised instances

      , ib_extensions :: [LangExt.Extension] -- Any extra extensions that should
                                         -- be enabled when type-checking this
                                         -- instance; needed for
                                         -- GeneralizedNewtypeDeriving

      , ib_derived :: Bool
           -- True <=> This code was generated by GHC from a deriving clause
           --          or standalone deriving declaration
           -- Used only to improve error messages
      }

instance OutputableBndr a => Outputable (InstInfo a) where
    ppr = pprInstInfoDetails

pprInstInfoDetails :: OutputableBndr a => InstInfo a -> SDoc
pprInstInfoDetails info
   = hang (pprInstanceHdr (iSpec info) <+> ptext (sLit "where"))
        2 (details (iBinds info))
  where
    details (InstBindings { ib_binds = b }) = pprLHsBinds b
