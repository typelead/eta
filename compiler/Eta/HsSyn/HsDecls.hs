{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable,
             DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Abstract syntax of global declarations.
--
-- Definitions for: @SynDecl@ and @ConDecl@, @ClassDecl@,
-- @InstDecl@, @DefaultDecl@ and @ForeignDecl@.
module Eta.HsSyn.HsDecls (
  -- * Toplevel declarations
  HsDecl(..), LHsDecl, HsDataDefn(..),
  -- ** Class or type declarations
  TyClDecl(..), LTyClDecl,
  TyClGroup(..), tyClGroupConcat, mkTyClGroup,
  isClassDecl, isDataDecl, isSynDecl, tcdName,
  isFamilyDecl, isTypeFamilyDecl, isDataFamilyDecl,
  isOpenTypeFamilyInfo, isClosedTypeFamilyInfo,
  tyFamInstDeclName, tyFamInstDeclLName,
  countTyClDecls, pprTyClDeclFlavour,
  tyClDeclLName, tyClDeclTyVars,
  hsDeclHasCusk, famDeclHasCusk,
  FamilyDecl(..), LFamilyDecl,

  -- ** Instance declarations
  InstDecl(..), LInstDecl, NewOrData(..), FamilyInfo(..),
  TyFamInstDecl(..), LTyFamInstDecl, instDeclDataFamInsts,
  DataFamInstDecl(..), LDataFamInstDecl, pprDataFamInstFlavour,
  TyFamEqn(..), TyFamInstEqn, LTyFamInstEqn, TyFamDefltEqn, LTyFamDefltEqn,
  HsTyPats,
  LClsInstDecl, ClsInstDecl(..),

  -- ** Standalone deriving declarations
  DerivDecl(..), LDerivDecl,
  -- ** @RULE@ declarations
  LRuleDecls,RuleDecls(..),RuleDecl(..), LRuleDecl, RuleBndr(..),LRuleBndr,
  collectRuleBndrSigTys,
  flattenRuleDecls,
  -- ** @VECTORISE@ declarations
  VectDecl(..), LVectDecl,
  lvectDeclName, lvectInstDecl,
  -- ** @default@ declarations
  DefaultDecl(..), LDefaultDecl,
  -- ** Template haskell declaration splice
  SpliceExplicitFlag(..),
  SpliceDecl(..), LSpliceDecl,
  -- ** Foreign function interface declarations
  ForeignDecl(..), LForeignDecl, ForeignImport(..), ForeignExport(..),
  noForeignImportCoercionYet, noForeignExportCoercionYet, isForeignExportSuper,
  CImportSpec(..),
  -- ** Data-constructor declarations
  ConDecl(..), getConNames, LConDecl, ResType(..),
  HsConDeclDetails, hsConDeclArgTys,
  -- ** Document comments
  DocDecl(..), LDocDecl, docDeclDoc,
  -- ** Deprecations
  WarnDecl(..),  LWarnDecl,
  WarnDecls(..), LWarnDecls,
  -- ** Annotations
  AnnDecl(..), LAnnDecl,
  AnnProvenance(..), annProvenanceName_maybe,
  -- ** Role annotations
  RoleAnnotDecl(..), LRoleAnnotDecl, roleAnnotDeclName,

  -- * Grouping
  HsGroup(..),  emptyRdrGroup, emptyRnGroup, appendGroups

    ) where

-- friends:
import {-# SOURCE #-}   Eta.HsSyn.HsExpr( LHsExpr, HsExpr, HsSplice, pprExpr,
                                          pprSpliceDecl )
        -- Because Expr imports Decls via HsBracket

import Eta.HsSyn.HsBinds
import Eta.HsSyn.HsPat
import Eta.HsSyn.HsTypes
import Eta.HsSyn.HsDoc
import Eta.Types.TyCon
import Eta.BasicTypes.Name
import Eta.BasicTypes.BasicTypes
import Eta.Types.Coercion
import Eta.Prelude.ForeignCall
import Eta.HsSyn.PlaceHolder ( PostTc,PostRn,PlaceHolder(..),DataId )
import Eta.BasicTypes.NameSet
import Eta.BasicTypes.Interop

-- others:
import Eta.Types.InstEnv
import Eta.Types.Class
import Eta.Utils.Outputable
import Eta.Utils.Util
import Eta.BasicTypes.SrcLoc
import Eta.Utils.FastString

import Eta.Utils.Bag
import Data.Data        hiding (TyCon,Fixity)
import Data.List
#if __GLASGOW_HASKELL__ < 709
import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
#endif
import Data.Maybe

{-
************************************************************************
*                                                                      *
\subsection[HsDecl]{Declarations}
*                                                                      *
************************************************************************
-}

type LHsDecl id = Located (HsDecl id)
        -- ^ When in a list this may have
        --
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi'
        --

-- For details on above see note [Api annotations] in ApiAnnotation

-- | A Haskell Declaration
data HsDecl id
  = TyClD       (TyClDecl id)     -- ^ A type or class declaration.
  | InstD       (InstDecl  id)    -- ^ An instance declaration.
  | DerivD      (DerivDecl id)
  | ValD        (HsBind id)
  | SigD        (Sig id)
  | DefD        (DefaultDecl id)
  | ForD        (ForeignDecl id)
  | WarningD    (WarnDecls id)
  | AnnD        (AnnDecl id)
  | RuleD       (RuleDecls id)
  | VectD       (VectDecl id)
  | SpliceD     (SpliceDecl id)  -- Includes quasi-quotes
  | DocD        (DocDecl)
  | RoleAnnotD  (RoleAnnotDecl id)
  deriving (Typeable)
deriving instance (DataId id) => Data (HsDecl id)


-- NB: all top-level fixity decls are contained EITHER
-- EITHER SigDs
-- OR     in the ClassDecls in TyClDs
--
-- The former covers
--      a) data constructors
--      b) class methods (but they can be also done in the
--              signatures of class decls)
--      c) imported functions (that have an IfacSig)
--      d) top level decls
--
-- The latter is for class methods only

-- | A 'HsDecl' is categorised into a 'HsGroup' before being
-- fed to the renamer.
data HsGroup id
  = HsGroup {
        hs_valds   :: HsValBinds id,
        hs_splcds  :: [LSpliceDecl id],

        hs_tyclds  :: [TyClGroup id],
                -- A list of mutually-recursive groups
                -- No family-instances here; they are in hs_instds
                -- Parser generates a singleton list;
                -- renamer does dependency analysis

        hs_instds  :: [LInstDecl id],
                -- Both class and family instance declarations in here

        hs_derivds :: [LDerivDecl id],

        hs_fixds   :: [LFixitySig id],
                -- Snaffled out of both top-level fixity signatures,
                -- and those in class declarations

        hs_defds   :: [LDefaultDecl id],
        hs_fords   :: [LForeignDecl id],
        hs_warnds  :: [LWarnDecls id],
        hs_annds   :: [LAnnDecl id],
        hs_ruleds  :: [LRuleDecls id],
        hs_vects   :: [LVectDecl id],

        hs_docs    :: [LDocDecl]
  } deriving (Typeable)
deriving instance (DataId id) => Data (HsGroup id)

emptyGroup, emptyRdrGroup, emptyRnGroup :: HsGroup a
emptyRdrGroup = emptyGroup { hs_valds = emptyValBindsIn }
emptyRnGroup  = emptyGroup { hs_valds = emptyValBindsOut }

emptyGroup = HsGroup { hs_tyclds = [], hs_instds = [],
                       hs_derivds = [],
                       hs_fixds = [], hs_defds = [], hs_annds = [],
                       hs_fords = [], hs_warnds = [], hs_ruleds = [], hs_vects = [],
                       hs_valds = error "emptyGroup hs_valds: Can't happen",
                       hs_splcds = [],
                       hs_docs = [] }

appendGroups :: HsGroup a -> HsGroup a -> HsGroup a
appendGroups
    HsGroup {
        hs_valds   = val_groups1,
        hs_splcds  = spliceds1,
        hs_tyclds  = tyclds1,
        hs_instds  = instds1,
        hs_derivds = derivds1,
        hs_fixds   = fixds1,
        hs_defds   = defds1,
        hs_annds   = annds1,
        hs_fords   = fords1,
        hs_warnds  = warnds1,
        hs_ruleds  = rulds1,
        hs_vects   = vects1,
        hs_docs    = docs1 }
    HsGroup {
        hs_valds   = val_groups2,
        hs_splcds  = spliceds2,
        hs_tyclds  = tyclds2,
        hs_instds  = instds2,
        hs_derivds = derivds2,
        hs_fixds   = fixds2,
        hs_defds   = defds2,
        hs_annds   = annds2,
        hs_fords   = fords2,
        hs_warnds  = warnds2,
        hs_ruleds  = rulds2,
        hs_vects   = vects2,
        hs_docs    = docs2 }
  =
    HsGroup {
        hs_valds   = val_groups1 `plusHsValBinds` val_groups2,
        hs_splcds  = spliceds1 ++ spliceds2,
        hs_tyclds  = tyclds1   ++ tyclds2,
        hs_instds  = instds1   ++ instds2,
        hs_derivds = derivds1  ++ derivds2,
        hs_fixds   = fixds1    ++ fixds2,
        hs_annds   = annds1    ++ annds2,
        hs_defds   = defds1    ++ defds2,
        hs_fords   = fords1    ++ fords2,
        hs_warnds  = warnds1   ++ warnds2,
        hs_ruleds  = rulds1    ++ rulds2,
        hs_vects   = vects1    ++ vects2,
        hs_docs    = docs1     ++ docs2 }

instance OutputableBndr name => Outputable (HsDecl name) where
    ppr (TyClD dcl)             = ppr dcl
    ppr (ValD binds)            = ppr binds
    ppr (DefD def)              = ppr def
    ppr (InstD inst)            = ppr inst
    ppr (DerivD deriv)          = ppr deriv
    ppr (ForD fd)               = ppr fd
    ppr (SigD sd)               = ppr sd
    ppr (RuleD rd)              = ppr rd
    ppr (VectD vect)            = ppr vect
    ppr (WarningD wd)           = ppr wd
    ppr (AnnD ad)               = ppr ad
    ppr (SpliceD dd)            = ppr dd
    ppr (DocD doc)              = ppr doc
    ppr (RoleAnnotD ra)         = ppr ra

instance OutputableBndr name => Outputable (HsGroup name) where
    ppr (HsGroup { hs_valds   = val_decls,
                   hs_tyclds  = tycl_decls,
                   hs_instds  = inst_decls,
                   hs_derivds = deriv_decls,
                   hs_fixds   = fix_decls,
                   hs_warnds  = deprec_decls,
                   hs_annds   = ann_decls,
                   hs_fords   = foreign_decls,
                   hs_defds   = default_decls,
                   hs_ruleds  = rule_decls,
                   hs_vects   = vect_decls })
        = vcat_mb empty
            [ppr_ds fix_decls, ppr_ds default_decls,
             ppr_ds deprec_decls, ppr_ds ann_decls,
             ppr_ds rule_decls,
             ppr_ds vect_decls,
             if isEmptyValBinds val_decls
                then Nothing
                else Just (ppr val_decls),
             ppr_ds (tyClGroupConcat tycl_decls),
             ppr_ds inst_decls,
             ppr_ds deriv_decls,
             ppr_ds foreign_decls]
        where
          ppr_ds :: Outputable a => [a] -> Maybe SDoc
          ppr_ds [] = Nothing
          ppr_ds ds = Just (vcat (map ppr ds))

          vcat_mb :: SDoc -> [Maybe SDoc] -> SDoc
          -- Concatenate vertically with white-space between non-blanks
          vcat_mb _    []             = empty
          vcat_mb gap (Nothing : ds) = vcat_mb gap ds
          vcat_mb gap (Just d  : ds) = gap $$ d $$ vcat_mb blankLine ds

type LSpliceDecl name = Located (SpliceDecl name)
data SpliceDecl id
  = SpliceDecl                  -- Top level splice
        (Located (HsSplice id))
        SpliceExplicitFlag
    deriving (Typeable)
deriving instance (DataId id) => Data (SpliceDecl id)

instance OutputableBndr name => Outputable (SpliceDecl name) where
   ppr (SpliceDecl (L _ e) f) = pprSpliceDecl e f

{-
************************************************************************
*                                                                      *
\subsection[SynDecl]{@data@, @newtype@ or @type@ (synonym) type declaration}
*                                                                      *
************************************************************************

                --------------------------------
                        THE NAMING STORY
                --------------------------------

Here is the story about the implicit names that go with type, class,
and instance decls.  It's a bit tricky, so pay attention!

"Implicit" (or "system") binders
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Each data type decl defines
        a worker name for each constructor
        to-T and from-T convertors
  Each class decl defines
        a tycon for the class
        a data constructor for that tycon
        the worker for that constructor
        a selector for each superclass

All have occurrence names that are derived uniquely from their parent
declaration.

None of these get separate definitions in an interface file; they are
fully defined by the data or class decl.  But they may *occur* in
interface files, of course.  Any such occurrence must haul in the
relevant type or class decl.

Plan of attack:
 - Ensure they "point to" the parent data/class decl
   when loading that decl from an interface file
   (See RnHiFiles.getSysBinders)

 - When typechecking the decl, we build the implicit TyCons and Ids.
   When doing so we look them up in the name cache (RnEnv.lookupSysName),
   to ensure correct module and provenance is set

These are the two places that we have to conjure up the magic derived
names.  (The actual magic is in OccName.mkWorkerOcc, etc.)

Default methods
~~~~~~~~~~~~~~~
 - Occurrence name is derived uniquely from the method name
   E.g. $dmmax

 - If there is a default method name at all, it's recorded in
   the ClassOpSig (in HsBinds), in the DefMeth field.
   (DefMeth is defined in Class.lhs)

Source-code class decls and interface-code class decls are treated subtly
differently, which has given me a great deal of confusion over the years.
Here's the deal.  (We distinguish the two cases because source-code decls
have (Just binds) in the tcdMeths field, whereas interface decls have Nothing.

In *source-code* class declarations:

 - When parsing, every ClassOpSig gets a DefMeth with a suitable RdrName
   This is done by RdrHsSyn.mkClassOpSigDM

 - The renamer renames it to a Name

 - During typechecking, we generate a binding for each $dm for
   which there's a programmer-supplied default method:
        class Foo a where
          op1 :: <type>
          op2 :: <type>
          op1 = ...
   We generate a binding for $dmop1 but not for $dmop2.
   The Class for Foo has a NoDefMeth for op2 and a DefMeth for op1.
   The Name for $dmop2 is simply discarded.

In *interface-file* class declarations:
  - When parsing, we see if there's an explicit programmer-supplied default method
    because there's an '=' sign to indicate it:
        class Foo a where
          op1 = :: <type>       -- NB the '='
          op2   :: <type>
    We use this info to generate a DefMeth with a suitable RdrName for op1,
    and a NoDefMeth for op2
  - The interface file has a separate definition for $dmop1, with unfolding etc.
  - The renamer renames it to a Name.
  - The renamer treats $dmop1 as a free variable of the declaration, so that
    the binding for $dmop1 will be sucked in.  (See RnHsSyn.tyClDeclFVs)
    This doesn't happen for source code class decls, because they *bind* the default method.

Dictionary functions
~~~~~~~~~~~~~~~~~~~~
Each instance declaration gives rise to one dictionary function binding.

The type checker makes up new source-code instance declarations
(e.g. from 'deriving' or generic default methods --- see
TcInstDcls.tcInstDecls1).  So we can't generate the names for
dictionary functions in advance (we don't know how many we need).

On the other hand for interface-file instance declarations, the decl
specifies the name of the dictionary function, and it has a binding elsewhere
in the interface file:
        instance {Eq Int} = dEqInt
        dEqInt :: {Eq Int} <pragma info>

So again we treat source code and interface file code slightly differently.

Source code:
  - Source code instance decls have a Nothing in the (Maybe name) field
    (see data InstDecl below)

  - The typechecker makes up a Local name for the dict fun for any source-code
    instance decl, whether it comes from a source-code instance decl, or whether
    the instance decl is derived from some other construct (e.g. 'deriving').

  - The occurrence name it chooses is derived from the instance decl (just for
    documentation really) --- e.g. dNumInt.  Two dict funs may share a common
    occurrence name, but will have different uniques.  E.g.
        instance Foo [Int]  where ...
        instance Foo [Bool] where ...
    These might both be dFooList

  - The CoreTidy phase externalises the name, and ensures the occurrence name is
    unique (this isn't special to dict funs).  So we'd get dFooList and dFooList1.

  - We can take this relaxed approach (changing the occurrence name later)
    because dict fun Ids are not captured in a TyCon or Class (unlike default
    methods, say).  Instead, they are kept separately in the InstEnv.  This
    makes it easy to adjust them after compiling a module.  (Once we've finished
    compiling that module, they don't change any more.)


Interface file code:
  - The instance decl gives the dict fun name, so the InstDecl has a (Just name)
    in the (Maybe name) field.

  - RnHsSyn.instDeclFVs treats the dict fun name as free in the decl, so that we
    suck in the dfun binding
-}

type LTyClDecl name = Located (TyClDecl name)

-- | A type or class declaration.
data TyClDecl name
  = -- | @type/data family T :: *->*@
    --
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
    --             'ApiAnnotation.AnnData',
    --             'ApiAnnotation.AnnFamily','ApiAnnotation.AnnDcolon',
    --             'ApiAnnotation.AnnWhere',
    --             'ApiAnnotation.AnnOpen','ApiAnnotation.AnnDcolon',
    --             'ApiAnnotation.AnnClose'

    -- For details on above see note [Api annotations] in ApiAnnotation
    FamDecl { tcdFam :: FamilyDecl name }

  | -- | @type@ declaration
    --
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
    --             'ApiAnnotation.AnnEqual',

    -- For details on above see note [Api annotations] in ApiAnnotation
    SynDecl { tcdLName  :: Located name            -- ^ Type constructor
            , tcdTyVars :: LHsTyVarBndrs name      -- ^ Type variables; for an associated type
                                                  --   these include outer binders
            , tcdRhs    :: LHsType name            -- ^ RHS of type declaration
            , tcdFVs    :: PostRn name NameSet }

  | -- | @data@ declaration
    --
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnData',
    --              'ApiAnnotation.AnnFamily',
    --              'ApiAnnotation.AnnNewType',
    --              'ApiAnnotation.AnnNewType','ApiAnnotation.AnnDcolon'
    --              'ApiAnnotation.AnnWhere',

    -- For details on above see note [Api annotations] in ApiAnnotation
    DataDecl { tcdLName    :: Located name        -- ^ Type constructor
             , tcdTyVars   :: LHsTyVarBndrs name  -- ^ Type variables; for an associated type
                                                  --   these include outer binders
                                                  -- Eg  class T a where
                                                  --       type F a :: *
                                                  --       type F a = a -> a
                                                  -- Here the type decl for 'f' includes 'a'
                                                  -- in its tcdTyVars
             , tcdDataDefn :: HsDataDefn name
             , tcdFVs      :: PostRn name NameSet }

  | ClassDecl { tcdCtxt    :: LHsContext name,          -- ^ Context...
                tcdLName   :: Located name,             -- ^ Name of the class
                tcdTyVars  :: LHsTyVarBndrs name,       -- ^ Class type variables
                tcdFDs     :: [Located (FunDep (Located name))],
                                                        -- ^ Functional deps
                tcdSigs    :: [LSig name],              -- ^ Methods' signatures
                tcdMeths   :: LHsBinds name,            -- ^ Default methods
                tcdATs     :: [LFamilyDecl name],       -- ^ Associated types;
                tcdATDefs  :: [LTyFamDefltEqn name],    -- ^ Associated type defaults
                tcdDocs    :: [LDocDecl],               -- ^ Haddock docs
                tcdFVs     :: PostRn name NameSet
    }
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnClass',
        --           'ApiAnnotation.AnnWhere','ApiAnnotation.AnnOpen',
        --           'ApiAnnotation.AnnClose'
        --   - The tcdFDs will have 'ApiAnnotation.AnnVbar',
        --                          'ApiAnnotation.AnnComma'
        --                          'ApiAnnotation.AnnRarrow'

        -- For details on above see note [Api annotations] in ApiAnnotation

  deriving (Typeable)
deriving instance (DataId id) => Data (TyClDecl id)

 -- This is used in TcTyClsDecls to represent
 -- strongly connected components of decls
 -- No family instances in here
 -- The role annotations must be grouped with their decls for the
 -- type-checker to infer roles correctly
data TyClGroup name
  = TyClGroup { group_tyclds :: [LTyClDecl name]
              , group_roles  :: [LRoleAnnotDecl name] }
    deriving (Typeable)
deriving instance (DataId id) => Data (TyClGroup id)

tyClGroupConcat :: [TyClGroup name] -> [LTyClDecl name]
tyClGroupConcat = concatMap group_tyclds

mkTyClGroup :: [LTyClDecl name] -> TyClGroup name
mkTyClGroup decls = TyClGroup { group_tyclds = decls, group_roles = [] }

type LFamilyDecl name = Located (FamilyDecl name)
data FamilyDecl name = FamilyDecl
  { fdInfo    :: FamilyInfo name            -- type or data, closed or open
  , fdLName   :: Located name               -- type constructor
  , fdTyVars  :: LHsTyVarBndrs name         -- type variables
  , fdKindSig :: Maybe (LHsKind name) }     -- result kind
  deriving( Typeable )
deriving instance (DataId id) => Data (FamilyDecl id)

data FamilyInfo name
  = DataFamily
  | OpenTypeFamily
     -- this list might be empty, if we're in an hs-boot file and the user
     -- said "type family Foo x where .."
  | ClosedTypeFamily [LTyFamInstEqn name]
  deriving( Typeable )
deriving instance (DataId name) => Data (FamilyInfo name)

{-
------------------------------
Simple classifiers
-}

-- | @True@ <=> argument is a @data@\/@newtype@
-- declaration.
isDataDecl :: TyClDecl name -> Bool
isDataDecl (DataDecl {}) = True
isDataDecl _other        = False

-- | type or type instance declaration
isSynDecl :: TyClDecl name -> Bool
isSynDecl (SynDecl {})   = True
isSynDecl _other        = False

-- | type class
isClassDecl :: TyClDecl name -> Bool
isClassDecl (ClassDecl {}) = True
isClassDecl _              = False

-- | type/data family declaration
isFamilyDecl :: TyClDecl name -> Bool
isFamilyDecl (FamDecl {})  = True
isFamilyDecl _other        = False

-- | type family declaration
isTypeFamilyDecl :: TyClDecl name -> Bool
isTypeFamilyDecl (FamDecl (FamilyDecl { fdInfo = info })) = case info of
  OpenTypeFamily      -> True
  ClosedTypeFamily {} -> True
  _                   -> False
isTypeFamilyDecl _ = False

-- | open type family info
isOpenTypeFamilyInfo :: FamilyInfo name -> Bool
isOpenTypeFamilyInfo OpenTypeFamily = True
isOpenTypeFamilyInfo _              = False

-- | closed type family info
isClosedTypeFamilyInfo :: FamilyInfo name -> Bool
isClosedTypeFamilyInfo (ClosedTypeFamily {}) = True
isClosedTypeFamilyInfo _                     = False

-- | data family declaration
isDataFamilyDecl :: TyClDecl name -> Bool
isDataFamilyDecl (FamDecl (FamilyDecl { fdInfo = DataFamily })) = True
isDataFamilyDecl _other      = False

-- Dealing with names

tyFamInstDeclName :: OutputableBndr name
                  => TyFamInstDecl name -> name
tyFamInstDeclName = unLoc . tyFamInstDeclLName

tyFamInstDeclLName :: OutputableBndr name
                   => TyFamInstDecl name -> Located name
tyFamInstDeclLName (TyFamInstDecl { tfid_eqn =
                     (L _ (TyFamEqn { tfe_tycon = ln })) })
  = ln

tyClDeclLName :: TyClDecl name -> Located name
tyClDeclLName (FamDecl { tcdFam = FamilyDecl { fdLName = ln } }) = ln
tyClDeclLName decl = tcdLName decl

tcdName :: TyClDecl name -> name
tcdName = unLoc . tyClDeclLName

tyClDeclTyVars :: OutputableBndr name => TyClDecl name -> LHsTyVarBndrs name
tyClDeclTyVars (FamDecl { tcdFam = FamilyDecl { fdTyVars = tvs } }) = tvs
tyClDeclTyVars d = tcdTyVars d

countTyClDecls :: [TyClDecl name] -> (Int, Int, Int, Int, Int)
        -- class, synonym decls, data, newtype, family decls
countTyClDecls decls
 = (count isClassDecl    decls,
    count isSynDecl      decls,  -- excluding...
    count isDataTy       decls,  -- ...family...
    count isNewTy        decls,  -- ...instances
    count isFamilyDecl   decls)
 where
   isDataTy DataDecl{ tcdDataDefn = HsDataDefn { dd_ND = DataType } } = True
   isDataTy _                                                       = False

   isNewTy DataDecl{ tcdDataDefn = HsDataDefn { dd_ND = NewType } } = True
   isNewTy _                                                      = False

-- | Does this declaration have a complete, user-supplied kind signature?
-- See Note [Complete user-supplied kind signatures]
hsDeclHasCusk :: TyClDecl name -> Bool
hsDeclHasCusk (FamDecl { tcdFam = fam_decl }) = famDeclHasCusk fam_decl
hsDeclHasCusk (SynDecl { tcdTyVars = tyvars, tcdRhs = rhs })
  = hsTvbAllKinded tyvars && rhs_annotated rhs
  where
    rhs_annotated (L _ ty) = case ty of
      HsParTy lty  -> rhs_annotated lty
      HsKindSig {} -> True
      _            -> False
hsDeclHasCusk (DataDecl { tcdTyVars = tyvars })  = hsTvbAllKinded tyvars
hsDeclHasCusk (ClassDecl { tcdTyVars = tyvars }) = hsTvbAllKinded tyvars

-- | Does this family declaration have a complete, user-supplied kind signature?
famDeclHasCusk :: FamilyDecl name -> Bool
famDeclHasCusk (FamilyDecl { fdInfo = ClosedTypeFamily _
                           , fdTyVars = tyvars
                           , fdKindSig = m_sig })
  = hsTvbAllKinded tyvars && isJust m_sig
famDeclHasCusk _ = True  -- all open families have CUSKs!

{-
Note [Complete user-supplied kind signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We kind-check declarations differently if they have a complete, user-supplied
kind signature (CUSK). This is because we can safely generalise a CUSKed
declaration before checking all of the others, supporting polymorphic recursion.
See https://ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindInference#Proposednewstrategy
and #9200 for lots of discussion of how we got here.

A declaration has a CUSK if we can know its complete kind without doing any inference,
at all. Here are the rules:

 - A class or datatype is said to have a CUSK if and only if all of its type
variables are annotated. Its result kind is, by construction, Constraint or *
respectively.

 - A type synonym has a CUSK if and only if all of its type variables and its
RHS are annotated with kinds.

 - A closed type family is said to have a CUSK if and only if all of its type
variables and its return type are annotated.

 - An open type family always has a CUSK -- unannotated type variables (and return type) default to *.
-}

instance OutputableBndr name
              => Outputable (TyClDecl name) where

    ppr (FamDecl { tcdFam = decl }) = ppr decl
    ppr (SynDecl { tcdLName = ltycon, tcdTyVars = tyvars, tcdRhs = rhs })
      = hang (ptext (sLit "type") <+>
              pp_vanilla_decl_head ltycon tyvars [] <+> equals)
          4 (ppr rhs)

    ppr (DataDecl { tcdLName = ltycon, tcdTyVars = tyvars, tcdDataDefn = defn })
      = pp_data_defn (pp_vanilla_decl_head ltycon tyvars) defn

    ppr (ClassDecl {tcdCtxt = context, tcdLName = lclas, tcdTyVars = tyvars,
                    tcdFDs  = fds,
                    tcdSigs = sigs, tcdMeths = methods,
                    tcdATs = ats, tcdATDefs = at_defs})
      | null sigs && isEmptyBag methods && null ats && null at_defs -- No "where" part
      = top_matter

      | otherwise       -- Laid out
      = vcat [ top_matter <+> ptext (sLit "where")
             , nest 2 $ pprDeclList (map ppr ats ++
                                     map ppr_fam_deflt_eqn at_defs ++
                                     pprLHsBindsForUser methods sigs) ]
      where
        top_matter = ptext (sLit "class")
                     <+> pp_vanilla_decl_head lclas tyvars (unLoc context)
                     <+> pprFundeps (map unLoc fds)

instance OutputableBndr name => Outputable (TyClGroup name) where
  ppr (TyClGroup { group_tyclds = tyclds, group_roles = roles })
    = ppr tyclds $$
      ppr roles

instance (OutputableBndr name) => Outputable (FamilyDecl name) where
  ppr (FamilyDecl { fdInfo = info, fdLName = ltycon,
                    fdTyVars = tyvars, fdKindSig = mb_kind})
      = vcat [ pprFlavour info <+> pp_vanilla_decl_head ltycon tyvars [] <+> pp_kind <+> pp_where
             , nest 2 $ pp_eqns ]
        where
          pp_kind = case mb_kind of
                      Nothing   -> empty
                      Just kind -> dcolon <+> ppr kind
          (pp_where, pp_eqns) = case info of
            ClosedTypeFamily eqns -> ( ptext (sLit "where")
                                     , if null eqns
                                       then ptext (sLit "..")
                                       else vcat $ map ppr_fam_inst_eqn eqns )
            _                     -> (empty, empty)

pprFlavour :: FamilyInfo name -> SDoc
pprFlavour DataFamily            = ptext (sLit "data family")
pprFlavour OpenTypeFamily        = ptext (sLit "type family")
pprFlavour (ClosedTypeFamily {}) = ptext (sLit "type family")

instance Outputable (FamilyInfo name) where
  ppr = pprFlavour

pp_vanilla_decl_head :: OutputableBndr name
   => Located name
   -> LHsTyVarBndrs name
   -> HsContext name
   -> SDoc
pp_vanilla_decl_head thing tyvars context
 = hsep [pprHsContext context, pprPrefixOcc (unLoc thing), ppr tyvars]

pp_fam_inst_lhs :: OutputableBndr name
   => Located name
   -> HsTyPats name
   -> HsContext name
   -> SDoc
pp_fam_inst_lhs thing (HsWB { hswb_cts = typats }) context -- explicit type patterns
   = hsep [ pprHsContext context, pprPrefixOcc (unLoc thing)
          , hsep (map (pprParendHsType.unLoc) typats)]

pprTyClDeclFlavour :: TyClDecl a -> SDoc
pprTyClDeclFlavour (ClassDecl {})   = ptext (sLit "class")
pprTyClDeclFlavour (SynDecl {})     = ptext (sLit "type")
pprTyClDeclFlavour (FamDecl { tcdFam = FamilyDecl { fdInfo = info }})
  = pprFlavour info
pprTyClDeclFlavour (DataDecl { tcdDataDefn = HsDataDefn { dd_ND = nd } })
  = ppr nd

{-
************************************************************************
*                                                                      *
\subsection[ConDecl]{A data-constructor declaration}
*                                                                      *
************************************************************************
-}

data HsDataDefn name   -- The payload of a data type defn
                       -- Used *both* for vanilla data declarations,
                       --       *and* for data family instances
  = -- | Declares a data type or newtype, giving its constructors
    -- @
    --  data/newtype T a = <constrs>
    --  data/newtype instance T [a] = <constrs>
    -- @
    HsDataDefn { dd_ND       :: NewOrData,
                 dd_ctxt     :: LHsContext name,           -- ^ Context
                 dd_metaData :: (Maybe (Located CType), [JavaAnnotation name]),
                 dd_kindSig  :: Maybe (LHsKind name),
                     -- ^ Optional kind signature.
                     --
                     -- @(Just k)@ for a GADT-style @data@,
                     -- or @data instance@ decl, with explicit kind sig
                     --
                     -- Always @Nothing@ for H98-syntax decls

                 dd_cons   :: [LConDecl name],
                     -- ^ Data constructors
                     --
                     -- For @data T a = T1 | T2 a@
                     --   the 'LConDecl's all have 'ResTyH98'.
                     -- For @data T a where { T1 :: T a }@
                     --   the 'LConDecls' all have 'ResTyGADT'.

                 dd_derivs :: Maybe (Located [LHsType name])
                     -- ^ Derivings; @Nothing@ => not specified,
                     --              @Just []@ => derive exactly what is asked
                     --
                     -- These "types" must be of form
                     -- @
                     --      forall ab. C ty1 ty2
                     -- @
                     -- Typically the foralls and ty args are empty, but they
                     -- are non-empty for the newtype-deriving case
                     --
                     --  - 'ApiAnnotation.AnnKeywordId' :
                     --       'ApiAnnotation.AnnDeriving',
                     --       'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'

             -- For details on above see note [Api annotations] in ApiAnnotation
   }
    deriving( Typeable )
deriving instance (DataId id) => Data (HsDataDefn id)

data NewOrData
  = NewType                     -- ^ @newtype Blah ...@
  | DataType                    -- ^ @data Blah ...@
  deriving( Eq, Data, Typeable )                -- Needed because Demand derives Eq

type LConDecl name = Located (ConDecl name)
      -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi' when
      --   in a GADT constructor list

  -- For details on above see note [Api annotations] in ApiAnnotation

-- |
--
-- @
-- data T b = forall a. Eq a => MkT a b
--   MkT :: forall b a. Eq a => MkT a b
--
-- data T b where
--      MkT1 :: Int -> T Int
--
-- data T = Int `MkT` Int
--        | MkT2
--
-- data T a where
--      Int `MkT` Int :: T Int
-- @
--
-- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen',
--            'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnCLose',
--            'ApiAnnotation.AnnEqual','ApiAnnotation.AnnVbar',
--            'ApiAnnotation.AnnDarrow','ApiAnnotation.AnnDarrow',
--            'ApiAnnotation.AnnForall','ApiAnnotation.AnnDot'

-- For details on above see note [Api annotations] in ApiAnnotation
data ConDecl name
  = ConDecl
    { con_names     :: [Located name]
        -- ^ Constructor names.  This is used for the DataCon itself, and for
        -- the user-callable wrapper Id.
        -- It is a list to deal with GADT constructors of the form
        --   T1, T2, T3 :: <payload>
    , con_explicit  :: HsExplicitFlag
        -- ^ Is there an user-written forall? (cf. 'HsTypes.HsForAllTy')

    , con_qvars     :: LHsTyVarBndrs name
        -- ^ Type variables.  Depending on 'con_res' this describes the
        -- following entities
        --
        --  - ResTyH98:  the constructor's *existential* type variables
        --  - ResTyGADT: *all* the constructor's quantified type variables
        --
        -- If con_explicit is Implicit, then con_qvars is irrelevant
        -- until after renaming.

    , con_cxt       :: LHsContext name
        -- ^ The context.  This /does not/ include the \"stupid theta\" which
        -- lives only in the 'TyData' decl.

    , con_details   :: HsConDeclDetails name
        -- ^ The main payload

    , con_res       :: ResType (LHsType name)
        -- ^ Result type of the constructor

    , con_doc       :: Maybe LHsDocString
        -- ^ A possible Haddock comment.

    , con_anns      :: [JavaAnnotation name]
        -- ^ A possible Haddock comment.

    , con_old_rec :: Bool
        -- ^ TEMPORARY field; True <=> user has employed now-deprecated syntax for
        --                             GADT-style record decl   C { blah } :: T a b
        -- Remove this when we no longer parse this stuff, and hence do not
        -- need to report deprecated use
    } deriving (Typeable)
deriving instance (DataId name) => Data (ConDecl name)

getConNames :: ConDecl name -> [Located name]
getConNames ConDecl { con_names = names } = names

type HsConDeclDetails name
   = HsConDetails (LBangType name) (Located [LConDeclField name])

hsConDeclArgTys :: HsConDeclDetails name -> [LBangType name]
hsConDeclArgTys (PrefixCon tys)    = tys
hsConDeclArgTys (InfixCon ty1 ty2) = [ty1,ty2]
hsConDeclArgTys (RecCon flds)      = map (cd_fld_type . unLoc) (unLoc flds)

data ResType ty
   = ResTyH98             -- Constructor was declared using Haskell 98 syntax
   | ResTyGADT SrcSpan ty -- Constructor was declared using GADT-style syntax,
                          --      and here is its result type, and the SrcSpan
                          --      of the original sigtype, for API Annotations
   deriving (Data, Typeable)

instance Outputable ty => Outputable (ResType ty) where
         -- Debugging only
   ppr ResTyH98         = ptext (sLit "ResTyH98")
   ppr (ResTyGADT _ ty) = ptext (sLit "ResTyGADT") <+> ppr ty

pp_data_defn :: OutputableBndr name
                  => (HsContext name -> SDoc)   -- Printing the header
                  -> HsDataDefn name
                  -> SDoc
pp_data_defn pp_hdr (HsDataDefn { dd_ND = new_or_data, dd_ctxt = L _ context
                                , dd_kindSig = mb_sig
                                , dd_cons = condecls, dd_derivs = derivings })
  | null condecls
  = ppr new_or_data <+> pp_hdr context <+> pp_sig

  | otherwise
  = hang (ppr new_or_data <+> pp_hdr context <+> pp_sig)
       2 (pp_condecls condecls $$ pp_derivings)
  where
    pp_sig = case mb_sig of
               Nothing   -> empty
               Just kind -> dcolon <+> ppr kind
    pp_derivings = case derivings of
                     Nothing       -> empty
                     Just (L _ ds) -> hsep [ptext (sLit "deriving"),
                                            parens (interpp'SP ds)]

instance OutputableBndr name => Outputable (HsDataDefn name) where
   ppr d = pp_data_defn (\_ -> ptext (sLit "Naked HsDataDefn")) d

instance Outputable NewOrData where
  ppr NewType  = ptext (sLit "newtype")
  ppr DataType = ptext (sLit "data")

pp_condecls :: OutputableBndr name => [LConDecl name] -> SDoc
pp_condecls cs@(L _ ConDecl{ con_res = ResTyGADT _ _ } : _) -- In GADT syntax
  = hang (ptext (sLit "where")) 2 (vcat (map ppr cs))
pp_condecls cs                    -- In H98 syntax
  = equals <+> sep (punctuate (ptext (sLit " |")) (map ppr cs))

instance (OutputableBndr name) => Outputable (ConDecl name) where
    ppr = pprConDecl

pprConDecl :: OutputableBndr name => ConDecl name -> SDoc
pprConDecl (ConDecl { con_names = [L _ con]  -- NB: non-GADT means 1 con
                    , con_explicit = expl, con_qvars = tvs
                    , con_cxt = cxt, con_details = details
                    , con_res = ResTyH98, con_doc = doc })
  = sep [ppr_mbDoc doc, pprHsForAll expl tvs cxt, ppr_details details]
  where
    ppr_details (InfixCon t1 t2) = hsep [ppr t1, pprInfixOcc con, ppr t2]
    ppr_details (PrefixCon tys)  = hsep (pprPrefixOcc con
                                   : map (pprParendHsType . unLoc) tys)
    ppr_details (RecCon fields)  = pprPrefixOcc con
                                 <+> pprConDeclFields (unLoc fields)

pprConDecl (ConDecl { con_names = cons, con_explicit = expl, con_qvars = tvs
                    , con_cxt = cxt, con_details = PrefixCon arg_tys
                    , con_res = ResTyGADT _ res_ty })
  = ppr_con_names cons <+> dcolon <+>
    sep [pprHsForAll expl tvs cxt, ppr (foldr mk_fun_ty res_ty arg_tys)]
  where
    mk_fun_ty a b = noLoc (HsFunTy a b)

pprConDecl (ConDecl { con_names = cons, con_explicit = expl, con_qvars = tvs
                    , con_cxt = cxt, con_details = RecCon fields
                    , con_res = ResTyGADT _ res_ty })
  = sep [ppr_con_names cons <+> dcolon <+> pprHsForAll expl tvs cxt,
         pprConDeclFields (unLoc fields) <+> arrow <+> ppr res_ty]

pprConDecl decl@(ConDecl { con_details = InfixCon ty1 ty2, con_res = ResTyGADT {} })
  = pprConDecl (decl { con_details = PrefixCon [ty1,ty2] })
        -- In GADT syntax we don't allow infix constructors
        -- so if we ever trip over one (albeit I can't see how that
        -- can happen) print it like a prefix one

-- this fallthrough would happen with a non-GADT-syntax ConDecl with more
-- than one constructor, which should indeed be impossible
pprConDecl (ConDecl { con_names = cons }) = pprPanic "pprConDecl" (ppr cons)

ppr_con_names :: (OutputableBndr name) => [Located name] -> SDoc
ppr_con_names = pprWithCommas (pprPrefixOcc . unLoc)

{-
************************************************************************
*                                                                      *
                Instance declarations
*                                                                      *
************************************************************************

Note [Type family instance declarations in HsSyn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The data type TyFamEqn represents one equation of a type family instance.
It is parameterised over its tfe_pats field:

 * An ordinary type family instance declaration looks like this in source Haskell
      type instance T [a] Int = a -> a
   (or something similar for a closed family)
   It is represented by a TyFamInstEqn, with *type* in the tfe_pats field.

 * On the other hand, the *default instance* of an associated type looksl like
   this in source Haskell
      class C a where
        type T a b
        type T a b = a -> b   -- The default instance
   It is represented by a TyFamDefltEqn, with *type variables8 in the tfe_pats field.
-}

----------------- Type synonym family instances -------------
type LTyFamInstEqn  name = Located (TyFamInstEqn  name)
  -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi'
  --   when in a list

-- For details on above see note [Api annotations] in ApiAnnotation

type LTyFamDefltEqn name = Located (TyFamDefltEqn name)

type HsTyPats name = HsWithBndrs name [LHsType name]
            -- ^ Type patterns (with kind and type bndrs)
            -- See Note [Family instance declaration binders]

type TyFamInstEqn  name = TyFamEqn name (HsTyPats name)
type TyFamDefltEqn name = TyFamEqn name (LHsTyVarBndrs name)
  -- See Note [Type family instance declarations in HsSyn]

-- | One equation in a type family instance declaration
-- See Note [Type family instance declarations in HsSyn]
data TyFamEqn name pats
  = TyFamEqn
       { tfe_tycon :: Located name
       , tfe_pats  :: pats
       , tfe_rhs   :: LHsType name }
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnEqual'

    -- For details on above see note [Api annotations] in ApiAnnotation
  deriving( Typeable )
deriving instance (DataId name, Data pats) => Data (TyFamEqn name pats)

type LTyFamInstDecl name = Located (TyFamInstDecl name)
data TyFamInstDecl name
  = TyFamInstDecl
       { tfid_eqn  :: LTyFamInstEqn name
       , tfid_fvs  :: PostRn name NameSet }
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
    --           'ApiAnnotation.AnnInstance',

    -- For details on above see note [Api annotations] in ApiAnnotation
  deriving( Typeable )
deriving instance (DataId name) => Data (TyFamInstDecl name)

----------------- Data family instances -------------

type LDataFamInstDecl name = Located (DataFamInstDecl name)
data DataFamInstDecl name
  = DataFamInstDecl
       { dfid_tycon :: Located name
       , dfid_pats  :: HsTyPats name      -- LHS
       , dfid_defn  :: HsDataDefn  name   -- RHS
       , dfid_fvs   :: PostRn name NameSet } -- Free vars for
                                             -- dependency analysis
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnData',
    --           'ApiAnnotation.AnnNewType','ApiAnnotation.AnnInstance',
    --           'ApiAnnotation.AnnDcolon'
    --           'ApiAnnotation.AnnWhere','ApiAnnotation.AnnOpen',
    --           'ApiAnnotation.AnnClose'

    -- For details on above see note [Api annotations] in ApiAnnotation
  deriving( Typeable )
deriving instance (DataId name) => Data (DataFamInstDecl name)


----------------- Class instances -------------

type LClsInstDecl name = Located (ClsInstDecl name)
data ClsInstDecl name
  = ClsInstDecl
      { cid_poly_ty :: LHsType name    -- Context => Class Instance-type
                                       -- Using a polytype means that the renamer conveniently
                                       -- figures out the quantified type variables for us.
      , cid_binds         :: LHsBinds name           -- Class methods
      , cid_sigs          :: [LSig name]             -- User-supplied pragmatic info
      , cid_tyfam_insts   :: [LTyFamInstDecl name]   -- Type family instances
      , cid_datafam_insts :: [LDataFamInstDecl name] -- Data family instances
      , cid_overlap_mode  :: Maybe (Located OverlapMode)
         -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
         --                                    'ApiAnnotation.AnnClose',

        -- For details on above see note [Api annotations] in ApiAnnotation
      }
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnInstance',
    --           'ApiAnnotation.AnnWhere',
    --           'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose',

    -- For details on above see note [Api annotations] in ApiAnnotation
  deriving (Typeable)
deriving instance (DataId id) => Data (ClsInstDecl id)


----------------- Instances of all kinds -------------

type LInstDecl name = Located (InstDecl name)
data InstDecl name  -- Both class and family instances
  = ClsInstD
      { cid_inst  :: ClsInstDecl name }
  | DataFamInstD              -- data family instance
      { dfid_inst :: DataFamInstDecl name }
  | TyFamInstD              -- type family instance
      { tfid_inst :: TyFamInstDecl name }
  deriving (Typeable)
deriving instance (DataId id) => Data (InstDecl id)

{-
Note [Family instance declaration binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A {Ty|Data}FamInstDecl is a data/type family instance declaration
the pats field is LHS patterns, and the tvs of the HsBSig
tvs are fv(pat_tys), *including* ones that are already in scope

   Eg   class C s t where
          type F t p :: *
        instance C w (a,b) where
          type F (a,b) x = x->a
   The tcdTyVars of the F decl are {a,b,x}, even though the F decl
   is nested inside the 'instance' decl.

   However after the renamer, the uniques will match up:
        instance C w7 (a8,b9) where
          type F (a8,b9) x10 = x10->a8
   so that we can compare the type patter in the 'instance' decl and
   in the associated 'type' decl
-}

instance (OutputableBndr name) => Outputable (TyFamInstDecl name) where
  ppr = pprTyFamInstDecl TopLevel

pprTyFamInstDecl :: OutputableBndr name => TopLevelFlag -> TyFamInstDecl name -> SDoc
pprTyFamInstDecl top_lvl (TyFamInstDecl { tfid_eqn = eqn })
   = ptext (sLit "type") <+> ppr_instance_keyword top_lvl <+> ppr_fam_inst_eqn eqn

ppr_instance_keyword :: TopLevelFlag -> SDoc
ppr_instance_keyword TopLevel    = ptext (sLit "instance")
ppr_instance_keyword NotTopLevel = empty

ppr_fam_inst_eqn :: OutputableBndr name => LTyFamInstEqn name -> SDoc
ppr_fam_inst_eqn (L _ (TyFamEqn { tfe_tycon = tycon
                                , tfe_pats  = pats
                                , tfe_rhs   = rhs }))
    = pp_fam_inst_lhs tycon pats [] <+> equals <+> ppr rhs

ppr_fam_deflt_eqn :: OutputableBndr name => LTyFamDefltEqn name -> SDoc
ppr_fam_deflt_eqn (L _ (TyFamEqn { tfe_tycon = tycon
                                 , tfe_pats  = tvs
                                 , tfe_rhs   = rhs }))
    = pp_vanilla_decl_head tycon tvs [] <+> equals <+> ppr rhs

instance (OutputableBndr name) => Outputable (DataFamInstDecl name) where
  ppr = pprDataFamInstDecl TopLevel

pprDataFamInstDecl :: OutputableBndr name => TopLevelFlag -> DataFamInstDecl name -> SDoc
pprDataFamInstDecl top_lvl (DataFamInstDecl { dfid_tycon = tycon
                                            , dfid_pats  = pats
                                            , dfid_defn  = defn })
  = pp_data_defn pp_hdr defn
  where
    pp_hdr ctxt = ppr_instance_keyword top_lvl <+> pp_fam_inst_lhs tycon pats ctxt

pprDataFamInstFlavour :: DataFamInstDecl name -> SDoc
pprDataFamInstFlavour (DataFamInstDecl { dfid_defn = (HsDataDefn { dd_ND = nd }) })
  = ppr nd

instance (OutputableBndr name) => Outputable (ClsInstDecl name) where
    ppr (ClsInstDecl { cid_poly_ty = inst_ty, cid_binds = binds
                     , cid_sigs = sigs, cid_tyfam_insts = ats
                     , cid_overlap_mode = mbOverlap
                     , cid_datafam_insts = adts })
      | null sigs, null ats, null adts, isEmptyBag binds  -- No "where" part
      = top_matter

      | otherwise       -- Laid out
      = vcat [ top_matter <+> ptext (sLit "where")
             , nest 2 $ pprDeclList $
               map (pprTyFamInstDecl NotTopLevel . unLoc)   ats ++
               map (pprDataFamInstDecl NotTopLevel . unLoc) adts ++
               pprLHsBindsForUser binds sigs ]
      where
        top_matter = ptext (sLit "instance") <+> ppOverlapPragma mbOverlap
                                             <+> ppr inst_ty

ppOverlapPragma :: Maybe (Located OverlapMode) -> SDoc
ppOverlapPragma mb =
  case mb of
    Nothing           -> empty
    Just (L _ (NoOverlap _))    -> ptext (sLit "{-# NO_OVERLAP #-}")
    Just (L _ (Overlappable _)) -> ptext (sLit "{-# OVERLAPPABLE #-}")
    Just (L _ (Overlapping _))  -> ptext (sLit "{-# OVERLAPPING #-}")
    Just (L _ (Overlaps _))     -> ptext (sLit "{-# OVERLAPS #-}")
    Just (L _ (Incoherent _))   -> ptext (sLit "{-# INCOHERENT #-}")




instance (OutputableBndr name) => Outputable (InstDecl name) where
    ppr (ClsInstD     { cid_inst  = decl }) = ppr decl
    ppr (TyFamInstD   { tfid_inst = decl }) = ppr decl
    ppr (DataFamInstD { dfid_inst = decl }) = ppr decl

-- Extract the declarations of associated data types from an instance

instDeclDataFamInsts :: [LInstDecl name] -> [DataFamInstDecl name]
instDeclDataFamInsts inst_decls
  = concatMap do_one inst_decls
  where
    do_one (L _ (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = fam_insts } }))
      = map unLoc fam_insts
    do_one (L _ (DataFamInstD { dfid_inst = fam_inst }))      = [fam_inst]
    do_one (L _ (TyFamInstD {}))                              = []

{-
************************************************************************
*                                                                      *
\subsection[DerivDecl]{A stand-alone instance deriving declaration}
*                                                                      *
************************************************************************
-}

type LDerivDecl name = Located (DerivDecl name)

data DerivDecl name = DerivDecl
        { deriv_type :: LHsType name
        , deriv_overlap_mode :: Maybe (Located OverlapMode)
         -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
         --                                    'ApiAnnotation.AnnClose',
         --                                    'ApiAnnotation.AnnDeriving',
         --                                    'ApiAnnotation.AnnInstance'

  -- For details on above see note [Api annotations] in ApiAnnotation
        }
  deriving (Typeable)
deriving instance (DataId name) => Data (DerivDecl name)

instance (OutputableBndr name) => Outputable (DerivDecl name) where
    ppr (DerivDecl ty o)
        = hsep [ptext (sLit "deriving instance"), ppOverlapPragma o, ppr ty]

{-
************************************************************************
*                                                                      *
\subsection[DefaultDecl]{A @default@ declaration}
*                                                                      *
************************************************************************

There can only be one default declaration per module, but it is hard
for the parser to check that; we pass them all through in the abstract
syntax, and that restriction must be checked in the front end.
-}

type LDefaultDecl name = Located (DefaultDecl name)

data DefaultDecl name
  = DefaultDecl [LHsType name]
        -- ^ - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnDefault',
        --          'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
  deriving (Typeable)
deriving instance (DataId name) => Data (DefaultDecl name)

instance (OutputableBndr name)
              => Outputable (DefaultDecl name) where

    ppr (DefaultDecl tys)
      = ptext (sLit "default") <+> parens (interpp'SP tys)

{-
************************************************************************
*                                                                      *
\subsection{Foreign function interface declaration}
*                                                                      *
************************************************************************
-}

-- foreign declarations are distinguished as to whether they define or use a
-- Haskell name
--
--  * the Boolean value indicates whether the pre-standard deprecated syntax
--   has been used
--
type LForeignDecl name = Located (ForeignDecl name)

data ForeignDecl name
  = ForeignImport (Located name) -- defines this name
                  (LHsType name) -- sig_ty
                  (PostTc name Coercion) -- rep_ty ~ sig_ty
                  ForeignImport
  | ForeignExport (Located name) -- uses this name
                  (LHsType name) -- sig_ty
                  (PostTc name Coercion)  -- sig_ty ~ rep_ty
                  ForeignExport
        -- ^
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnForeign',
        --           'ApiAnnotation.AnnImport','ApiAnnotation.AnnExport',
        --           'ApiAnnotation.AnnDcolon'

        -- For details on above see note [Api annotations] in ApiAnnotation
  deriving (Typeable)
deriving instance (DataId name) => Data (ForeignDecl name)
{-
    In both ForeignImport and ForeignExport:
        sig_ty is the type given in the Haskell code
        rep_ty is the representation for this type, i.e. with newtypes
               coerced away and type functions evaluated.
    Thus if the declaration is valid, then rep_ty will only use types
    such as Int and IO that we know how to make foreign calls with.
-}

noForeignImportCoercionYet :: PlaceHolder
noForeignImportCoercionYet = PlaceHolder

noForeignExportCoercionYet :: PlaceHolder
noForeignExportCoercionYet = PlaceHolder

isForeignExportSuper :: ForeignExport -> Bool
isForeignExportSuper (CExport (L _ (CExportStatic labelString _)) _)
  = isJust (stripPrefix "@super" (unpackFS labelString))

-- Specification Of an imported external entity in dependence on the calling
-- convention
--
data ForeignImport = -- import of a C entity
                     --
                     --  * the two strings specifying a header file or library
                     --   may be empty, which indicates the absence of a
                     --   header or object specification (both are not used
                     --   in the case of `CWrapper' and when `CFunction'
                     --   has a dynamic target)
                     --
                     --  * the calling convention is irrelevant for code
                     --   generation in the case of `CLabel', but is needed
                     --   for pretty printing
                     --
                     --  * `Safety' is irrelevant for `CLabel' and `CWrapper'
                     --
                     CImport  (Located CCallConv) -- ccall or stdcall
                              (Located Safety)  -- interruptible, safe or unsafe
                              (Maybe Header)       -- name of C header
                              CImportSpec          -- details of the C entity
                              (Located SourceText) -- original source text for
                                                   -- the C entity
  deriving (Data, Typeable)

-- details of an external C entity
--
data CImportSpec = CLabel    CLabelString     -- import address of a C label
                 | CFunction CCallTarget      -- static or dynamic function
                 | CWrapper  CLabelString     -- wrapper to expose closures
                             Bool             -- Abstract class?
  deriving (Data, Typeable)

-- specification of an externally exported entity in dependence on the calling
-- convention
--
data ForeignExport = CExport  (Located CExportSpec) -- contains the calling
                                                    -- convention
                              (Located SourceText)  -- original source text for
                                                    -- the C entity
  deriving (Data, Typeable)

-- pretty printing of foreign declarations
--

instance OutputableBndr name => Outputable (ForeignDecl name) where
  ppr (ForeignImport n ty _ fimport) =
    hang (ptext (sLit "foreign import") <+> ppr fimport <+> ppr n)
       2 (dcolon <+> ppr ty)
  ppr (ForeignExport n ty _ fexport) =
    hang (ptext (sLit "foreign export") <+> ppr fexport <+> ppr n)
       2 (dcolon <+> ppr ty)

instance Outputable ForeignImport where
  ppr (CImport  cconv safety mHeader spec _) =
    ppr cconv <+> ppr safety <+>
    char '"' <> pprCEntity spec <> char '"'
    where
      pp_hdr = case mHeader of
               Nothing -> empty
               Just (Header header) -> ftext header

      pprCEntity (CLabel lbl) =
        ptext (sLit "static") <+> pp_hdr <+> char '&' <> ppr lbl
      pprCEntity (CFunction (StaticTarget lbl _ isFun)) =
            pp_hdr
        <+> (if isFun then empty else ptext (sLit "value"))
        <+> ppr lbl
      pprCEntity (CFunction (DynamicTarget)) =
        ptext (sLit "dynamic")
      pprCEntity (CWrapper target isAbstract) =
        ptext (sLit "@wrapper") <+> ppr target <+> ppr isAbstract

instance Outputable ForeignExport where
  ppr (CExport  (L _ (CExportStatic lbl cconv)) _) =
    ppr cconv <+> char '"' <> ppr lbl <> char '"'

{-
************************************************************************
*                                                                      *
\subsection{Transformation rules}
*                                                                      *
************************************************************************
-}

type LRuleDecls name = Located (RuleDecls name)

  -- Note [Pragma source text] in BasicTypes
data RuleDecls name = HsRules { rds_src   :: SourceText
                              , rds_rules :: [LRuleDecl name] }
  deriving (Typeable)
deriving instance (DataId name) => Data (RuleDecls name)

type LRuleDecl name = Located (RuleDecl name)

data RuleDecl name
  = HsRule                      -- Source rule
        (Located RuleName)      -- Rule name
        Activation
        [LRuleBndr name]        -- Forall'd vars; after typechecking this
                                --   includes tyvars
        (Located (HsExpr name)) -- LHS
        (PostRn name NameSet)   -- Free-vars from the LHS
        (Located (HsExpr name)) -- RHS
        (PostRn name NameSet)   -- Free-vars from the RHS
        -- ^
        --  - 'ApiAnnotation.AnnKeywordId' :
        --           'ApiAnnotation.AnnOpen','ApiAnnotation.AnnTilde',
        --           'ApiAnnotation.AnnVal',
        --           'ApiAnnotation.AnnClose',
        --           'ApiAnnotation.AnnForall','ApiAnnotation.AnnDot',
        --           'ApiAnnotation.AnnEqual',

        -- For details on above see note [Api annotations] in ApiAnnotation
  deriving (Typeable)
deriving instance (DataId name) => Data (RuleDecl name)

flattenRuleDecls :: [LRuleDecls name] -> [LRuleDecl name]
flattenRuleDecls decls = concatMap (rds_rules . unLoc) decls

type LRuleBndr name = Located (RuleBndr name)
data RuleBndr name
  = RuleBndr (Located name)
  | RuleBndrSig (Located name) (HsWithBndrs name (LHsType name))
        -- ^
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --     'ApiAnnotation.AnnDcolon','ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
  deriving (Typeable)
deriving instance (DataId name) => Data (RuleBndr name)

collectRuleBndrSigTys :: [RuleBndr name] -> [HsWithBndrs name (LHsType name)]
collectRuleBndrSigTys bndrs = [ty | RuleBndrSig _ ty <- bndrs]

instance OutputableBndr name => Outputable (RuleDecls name) where
  ppr (HsRules _ rules) = ppr rules

instance OutputableBndr name => Outputable (RuleDecl name) where
  ppr (HsRule name act ns lhs _fv_lhs rhs _fv_rhs)
        = sep [text "{-# RULES" <+> doubleQuotes (ftext $ unLoc name)
                                <+> ppr act,
               nest 4 (pp_forall <+> pprExpr (unLoc lhs)),
               nest 4 (equals <+> pprExpr (unLoc rhs) <+> text "#-}") ]
        where
          pp_forall | null ns   = empty
                    | otherwise = forAllLit <+> fsep (map ppr ns) <> dot

instance OutputableBndr name => Outputable (RuleBndr name) where
   ppr (RuleBndr name) = ppr name
   ppr (RuleBndrSig name ty) = ppr name <> dcolon <> ppr ty

{-
************************************************************************
*                                                                      *
\subsection{Vectorisation declarations}
*                                                                      *
************************************************************************

A vectorisation pragma, one of

  {-# VECTORISE f = closure1 g (scalar_map g) #-}
  {-# VECTORISE SCALAR f #-}
  {-# NOVECTORISE f #-}

  {-# VECTORISE type T = ty #-}
  {-# VECTORISE SCALAR type T #-}
-}

type LVectDecl name = Located (VectDecl name)

data VectDecl name
  = HsVect
      SourceText   -- Note [Pragma source text] in BasicTypes
      (Located name)
      (LHsExpr name)
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --           'ApiAnnotation.AnnEqual','ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | HsNoVect
      SourceText   -- Note [Pragma source text] in BasicTypes
      (Located name)
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --                                    'ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | HsVectTypeIn                -- pre type-checking
      SourceText                -- Note [Pragma source text] in BasicTypes
      Bool                      -- 'TRUE' => SCALAR declaration
      (Located name)
      (Maybe (Located name))    -- 'Nothing' => no right-hand side
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --           'ApiAnnotation.AnnType','ApiAnnotation.AnnClose',
        --           'ApiAnnotation.AnnEqual'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | HsVectTypeOut               -- post type-checking
      Bool                      -- 'TRUE' => SCALAR declaration
      TyCon
      (Maybe TyCon)             -- 'Nothing' => no right-hand side
  | HsVectClassIn               -- pre type-checking
      SourceText                -- Note [Pragma source text] in BasicTypes
      (Located name)
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --           'ApiAnnotation.AnnClass','ApiAnnotation.AnnClose',

       -- For details on above see note [Api annotations] in ApiAnnotation
  | HsVectClassOut              -- post type-checking
      Class
  | HsVectInstIn                -- pre type-checking (always SCALAR)  !!!FIXME: should be superfluous now
      (LHsType name)
  | HsVectInstOut               -- post type-checking (always SCALAR) !!!FIXME: should be superfluous now
      ClsInst
  deriving (Typeable)
deriving instance (DataId name) => Data (VectDecl name)

lvectDeclName :: NamedThing name => LVectDecl name -> Name
lvectDeclName (L _ (HsVect _       (L _ name) _))    = getName name
lvectDeclName (L _ (HsNoVect _     (L _ name)))      = getName name
lvectDeclName (L _ (HsVectTypeIn _  _ (L _ name) _)) = getName name
lvectDeclName (L _ (HsVectTypeOut  _ tycon _))       = getName tycon
lvectDeclName (L _ (HsVectClassIn _ (L _ name)))     = getName name
lvectDeclName (L _ (HsVectClassOut cls))             = getName cls
lvectDeclName (L _ (HsVectInstIn _))
  = panic "HsDecls.lvectDeclName: HsVectInstIn"
lvectDeclName (L _ (HsVectInstOut  _))
  = panic "HsDecls.lvectDeclName: HsVectInstOut"

lvectInstDecl :: LVectDecl name -> Bool
lvectInstDecl (L _ (HsVectInstIn _))  = True
lvectInstDecl (L _ (HsVectInstOut _)) = True
lvectInstDecl _                       = False

instance OutputableBndr name => Outputable (VectDecl name) where
  ppr (HsVect _ v rhs)
    = sep [text "{-# VECTORISE" <+> ppr v,
           nest 4 $
             pprExpr (unLoc rhs) <+> text "#-}" ]
  ppr (HsNoVect _ v)
    = sep [text "{-# NOVECTORISE" <+> ppr v <+> text "#-}" ]
  ppr (HsVectTypeIn _ False t Nothing)
    = sep [text "{-# VECTORISE type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeIn _ False t (Just t'))
    = sep [text "{-# VECTORISE type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectTypeIn _ True t Nothing)
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeIn _ True t (Just t'))
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectTypeOut False t Nothing)
    = sep [text "{-# VECTORISE type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeOut False t (Just t'))
    = sep [text "{-# VECTORISE type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectTypeOut True t Nothing)
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeOut True t (Just t'))
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectClassIn _ c)
    = sep [text "{-# VECTORISE class" <+> ppr c <+> text "#-}" ]
  ppr (HsVectClassOut c)
    = sep [text "{-# VECTORISE class" <+> ppr c <+> text "#-}" ]
  ppr (HsVectInstIn ty)
    = sep [text "{-# VECTORISE SCALAR instance" <+> ppr ty <+> text "#-}" ]
  ppr (HsVectInstOut i)
    = sep [text "{-# VECTORISE SCALAR instance" <+> ppr i <+> text "#-}" ]

{-
************************************************************************
*                                                                      *
\subsection[DocDecl]{Document comments}
*                                                                      *
************************************************************************
-}

type LDocDecl = Located (DocDecl)

data DocDecl
  = DocCommentNext HsDocString
  | DocCommentPrev HsDocString
  | DocCommentNamed String HsDocString
  | DocGroup Int HsDocString
  deriving (Data, Typeable)

-- Okay, I need to reconstruct the document comments, but for now:
instance Outputable DocDecl where
  ppr _ = text "<document comment>"

docDeclDoc :: DocDecl -> HsDocString
docDeclDoc (DocCommentNext d) = d
docDeclDoc (DocCommentPrev d) = d
docDeclDoc (DocCommentNamed _ d) = d
docDeclDoc (DocGroup _ d) = d

{-
************************************************************************
*                                                                      *
\subsection[DeprecDecl]{Deprecations}
*                                                                      *
************************************************************************

We use exported entities for things to deprecate.
-}


type LWarnDecls name = Located (WarnDecls name)

 -- Note [Pragma source text] in BasicTypes
data WarnDecls name = Warnings { wd_src :: SourceText
                               , wd_warnings :: [LWarnDecl name]
                               }
  deriving (Data, Typeable)


type LWarnDecl name = Located (WarnDecl name)

data WarnDecl name = Warning [Located name] WarningTxt
  deriving (Data, Typeable)

instance OutputableBndr name => Outputable (WarnDecls name) where
    ppr (Warnings _ decls) = ppr decls

instance OutputableBndr name => Outputable (WarnDecl name) where
    ppr (Warning thing txt)
      = hsep [text "{-# DEPRECATED", ppr thing, doubleQuotes (ppr txt), text "#-}"]

{-
************************************************************************
*                                                                      *
\subsection[AnnDecl]{Annotations}
*                                                                      *
************************************************************************
-}

type LAnnDecl name = Located (AnnDecl name)

data AnnDecl name = HsAnnotation
                      SourceText -- Note [Pragma source text] in BasicTypes
                      (AnnProvenance name) (Located (HsExpr name))
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
      --           'ApiAnnotation.AnnType'
      --           'ApiAnnotation.AnnModule'
      --           'ApiAnnotation.AnnClose'

      -- For details on above see note [Api annotations] in ApiAnnotation
  deriving (Typeable)
deriving instance (DataId name) => Data (AnnDecl name)

instance (OutputableBndr name) => Outputable (AnnDecl name) where
    ppr (HsAnnotation _ provenance expr)
      = hsep [text "{-#", pprAnnProvenance provenance, pprExpr (unLoc expr), text "#-}"]

data AnnProvenance name = ValueAnnProvenance (Located name)
                        | TypeAnnProvenance (Located name)
                        | ModuleAnnProvenance
  deriving (Data, Typeable, Functor)
deriving instance Foldable    AnnProvenance
deriving instance Traversable AnnProvenance

annProvenanceName_maybe :: AnnProvenance name -> Maybe name
annProvenanceName_maybe (ValueAnnProvenance (L _ name)) = Just name
annProvenanceName_maybe (TypeAnnProvenance (L _ name))  = Just name
annProvenanceName_maybe ModuleAnnProvenance       = Nothing

pprAnnProvenance :: OutputableBndr name => AnnProvenance name -> SDoc
pprAnnProvenance ModuleAnnProvenance       = ptext (sLit "ANN module")
pprAnnProvenance (ValueAnnProvenance (L _ name))
  = ptext (sLit "ANN") <+> ppr name
pprAnnProvenance (TypeAnnProvenance (L _ name))
  = ptext (sLit "ANN type") <+> ppr name

{-
************************************************************************
*                                                                      *
\subsection[RoleAnnot]{Role annotations}
*                                                                      *
************************************************************************
-}

type LRoleAnnotDecl name = Located (RoleAnnotDecl name)

-- See #8185 for more info about why role annotations are
-- top-level declarations
data RoleAnnotDecl name
  = RoleAnnotDecl (Located name)         -- type constructor
                  [Located (Maybe Role)] -- optional annotations
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
      --           'ApiAnnotation.AnnRole'

      -- For details on above see note [Api annotations] in ApiAnnotation
  deriving (Data, Typeable)

instance OutputableBndr name => Outputable (RoleAnnotDecl name) where
  ppr (RoleAnnotDecl ltycon roles)
    = ptext (sLit "type role") <+> ppr ltycon <+>
      hsep (map (pp_role . unLoc) roles)
    where
      pp_role Nothing  = underscore
      pp_role (Just r) = ppr r

roleAnnotDeclName :: RoleAnnotDecl name -> name
roleAnnotDeclName (RoleAnnotDecl (L _ name) _) = name
