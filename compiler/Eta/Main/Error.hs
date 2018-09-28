{-# LANGUAGE ExistentialQuantification #-}

module Eta.Main.Error (
      ErrMsg(..), WarnMsg,
      Messages, ErrorMessages, WarningMessages, unionMessages,
      pprErrMsgBag, pprErrMsgBagWithLoc,
      pprLocErrMsg, makeIntoWarning, isWarning, sortMsgBag,

      errorsFound, emptyMessages, isEmptyMessages,
      mkErrMsg, mkPlainErrMsg, mkLongErrMsg, mkWarnMsg, mkPlainWarnMsg,
      mkLongWarnMsg, printBagOfErrors, isWarnMsgFatal,

      pprSigCtxt, pprMatchInCtxt, pprStmtInCtxt, perhapsForallMsg,
      pprUserTypeErrorTy, pprUserTypeErrorTy', Rank(..),
      rankZeroMonoType, tyConArgMonoType, synArgMonoType,
      funArgResRank, forAllAllowed, InstInfo(..),
      iDFunId, pprInstInfoDetails, InstBindings(..),

      PromotionErr(..), pprPECategory,

      ContextElement(..), HowMuch(..), TypeError(..),
      HeraldContext(..), mkEqInfoMsg, misMatchMsg, pprFunctionHerald,
      MisMatchType(..), mkAmbigMsgTvs, pprHoleError, pp_with_type, greSrcSpan

) where

import Eta.Main.Constants ( mAX_TUPLE_SIZE )
import Eta.Main.ErrUtils
import Data.Ord
import Eta.BasicTypes.SrcLoc
import Eta.BasicTypes.VarSet
import Eta.Utils.Outputable
import Eta.TypeCheck.TcType
import Eta.Main.DynFlags
import Data.List (sortBy)
import Eta.Types.InstEnv
import Eta.Types.Kind
import Eta.Types.FamInstEnv
import Eta.Utils.Util
import Eta.BasicTypes.RdrName
import Eta.BasicTypes.Name
import Eta.BasicTypes.Avail
import Eta.BasicTypes.Var
import Eta.BasicTypes.Module
import Eta.BasicTypes.BasicTypes
import Eta.BasicTypes.Id
import Eta.Utils.Bag
import Eta.Utils.FastString
import Eta.HsSyn.HsSyn
import Eta.Prelude.PrelNames (forall_tv_RDR, dot_tv_RDR, negateName,
                              typeErrorVAppendDataConName,
                              typeErrorTextDataConName,
                              typeErrorShowTypeDataConName,
                              typeErrorAppendDataConName)
import Eta.Types.TyCon
import Eta.Types.Coercion (pprCoAxBranchHdr)
import Eta.Types.Type
import Eta.BasicTypes.DataCon  ( DataCon, dataConUserType, dataConTyCon )
import Eta.Types.Class    ( Class, classTyVars )
import Eta.BasicTypes.ConLike  ( ConLike(..) )
import qualified Language.Eta.Meta as TH
import qualified Eta.LanguageExtensions as LangExt
import Data.List     ( sort, intersperse, partition )
import Data.Maybe    ( isJust )
import Data.Set ( Set )

-- -----------------------------------------------------------------------------
-- Basic error messages: just render a message with a source location.

type Messages        = (WarningMessages, ErrorMessages)
type WarningMessages = Bag WarnMsg
type ErrorMessages   = Bag ErrMsg

unionMessages :: Messages -> Messages -> Messages
unionMessages (warns1, errs1) (warns2, errs2) =
  (warns1 `unionBags` warns2, errs1 `unionBags` errs2)

data ErrMsg = ErrMsg {
        errMsgSpan        :: SrcSpan,  -- Location of source that triggered the error message
        errMsgContext     :: PrintUnqualified, -- Print fully qualified name/just the name
        errMsgShortDoc    :: TypeError,   -- Core error message
        errMsgShortString :: String, -- Contains the same text as errMsgShortDoc
        errMsgExtraInfo   :: [ContextElement], -- Contains the context of error message
        errMsgSeverity    :: Severity, -- The relative importance of error message
        errMsgReason      :: WarnReason -- Context of the warning. Will be set to
                                        -- NoReason if its not a warning
        }
        -- The SrcSpan is used for sorting errors into line-number order

type WarnMsg = ErrMsg

instance Show ErrMsg where
    show em = errMsgShortString em

makeIntoWarning :: WarnReason -> ErrMsg -> ErrMsg
makeIntoWarning reason err = err
    { errMsgSeverity = SevWarning
    , errMsgReason = reason }

isWarning :: ErrMsg -> Bool
isWarning err
  | SevWarning <- errMsgSeverity err = True
  | otherwise                        = False

-- -----------------------------------------------------------------------------
-- Collecting up messages for later ordering and printing.

mk_err_msg :: DynFlags -> Severity -> SrcSpan -> PrintUnqualified -> TypeError
                -> [ContextElement] -> ErrMsg
mk_err_msg  dflags sev locn print_unqual msg extra
 = ErrMsg { errMsgSpan = locn, errMsgContext = print_unqual
          , errMsgShortDoc = msg , errMsgShortString = showSDoc dflags (ppr msg)
          , errMsgExtraInfo = extra
          , errMsgSeverity = sev
          , errMsgReason = NoReason }

mkLongErrMsg  :: DynFlags -> SrcSpan -> PrintUnqualified -> TypeError -> [ContextElement] -> ErrMsg
-- A long (multi-line) error message
mkErrMsg      :: DynFlags -> SrcSpan -> PrintUnqualified -> TypeError                     -> ErrMsg
-- A short (one-line) error message
mkPlainErrMsg :: DynFlags -> SrcSpan ->                     TypeError                     -> ErrMsg
-- Variant that doesn't care about qualified/unqualified names

mkLongErrMsg   dflags locn unqual msg extra = mk_err_msg dflags SevError   locn unqual        msg extra
mkErrMsg       dflags locn unqual msg       = mk_err_msg dflags SevError   locn unqual        msg []
mkPlainErrMsg  dflags locn        msg       = mk_err_msg dflags SevError   locn alwaysQualify msg []

----------------

mkWarnMsg      :: DynFlags -> SrcSpan -> PrintUnqualified -> MsgDoc           -> ErrMsg
mkLongWarnMsg  :: DynFlags -> SrcSpan -> PrintUnqualified -> MsgDoc -> MsgDoc -> ErrMsg
mkPlainWarnMsg :: DynFlags -> SrcSpan                               -> MsgDoc -> ErrMsg

mkLongWarnMsg  dflags locn unqual msg extra = mk_warn_msg dflags SevWarning locn unqual        msg extra
mkWarnMsg      dflags locn unqual msg       = mk_warn_msg dflags SevWarning locn unqual        msg empty
mkPlainWarnMsg dflags locn        msg       = mk_warn_msg dflags SevWarning locn alwaysQualify msg empty

mk_warn_msg :: DynFlags -> Severity -> SrcSpan -> PrintUnqualified -> MsgDoc
                -> MsgDoc -> ErrMsg
mk_warn_msg  dflags sev locn print_unqual msg extra
 = ErrMsg { errMsgSpan = locn, errMsgContext = print_unqual
          , errMsgShortDoc = GeneralWarningSDoc msg
          , errMsgShortString = showSDoc dflags msg
          , errMsgExtraInfo = [WarningCtxt extra]
          , errMsgSeverity = sev
          , errMsgReason = NoReason }

emptyMessages :: Messages
emptyMessages = (emptyBag, emptyBag)

isEmptyMessages :: Messages -> Bool
isEmptyMessages (warns, errs) = isEmptyBag warns && isEmptyBag errs

errorsFound :: DynFlags -> Messages -> Bool
errorsFound _dflags (_warns, errs) = not (isEmptyBag errs)

printBagOfErrors :: DynFlags -> Bag ErrMsg -> IO ()
printBagOfErrors dflags bag_of_errors
  | isEmptyBag bag_of_errors = return ()
  | otherwise = printMsgBag dflags bag_of_errors

pprErrMsgBag :: Bag ErrMsg -> [SDoc]
pprErrMsgBag bag
  = [ sdocWithDynFlags $ \dflags ->
      let style = mkErrStyle dflags unqual
      in withPprStyle style (ppr d $$ vcat (map ppr e))
    | ErrMsg { errMsgShortDoc  = d,
               errMsgExtraInfo = e,
               errMsgContext   = unqual } <- sortMsgBag bag ]

pprErrMsgBagWithLoc :: Bag ErrMsg -> [SDoc]
pprErrMsgBagWithLoc bag = [ pprLocErrMsg item | item <- sortMsgBag bag ]

pprLocErrMsg :: ErrMsg -> SDoc
pprLocErrMsg (ErrMsg { errMsgSpan      = s
                     , errMsgShortDoc  = d
                     , errMsgExtraInfo = e
                     , errMsgSeverity  = sev
                     , errMsgContext   = unqual })
  = sdocWithDynFlags $ \dflags ->
    withPprStyle (mkErrStyle dflags unqual) (mkLocMessage sev s (ppr d $$ vcat (map ppr e)))

printMsgBag :: DynFlags -> Bag ErrMsg -> IO ()
printMsgBag dflags bag
  = sequence_ [ let style = mkErrStyle dflags unqual
                in putLogMsg dflags reason sev s style (ppr d $$ vcat (map ppr e))
              | ErrMsg { errMsgSpan      = s,
                         errMsgShortDoc  = d,
                         errMsgSeverity  = sev,
                         errMsgReason    = reason,
                         errMsgExtraInfo = e,
                         errMsgContext   = unqual } <- sortMsgBag bag ]

sortMsgBag :: Bag ErrMsg -> [ErrMsg]
sortMsgBag bag = sortBy (comparing errMsgSpan) $ bagToList bag

-- | Checks if given 'WarnMsg' is a fatal warning.
isWarnMsgFatal :: DynFlags -> WarnMsg -> Maybe (Maybe WarningFlag)
isWarnMsgFatal dflags ErrMsg{errMsgReason = Reason wflag}
  = if wopt_fatal wflag dflags
      then Just (Just wflag)
      else Nothing
isWarnMsgFatal dflags _
  = if gopt Opt_WarnIsError dflags
      then Just Nothing
      else Nothing

data ContextElement
  = FunctionResultCtxt Bool Int Int (HsExpr Name) Type Int Type Type Type Type
  | FunctionArgumentCtxt (LHsExpr Name) (Maybe TcTauType) (LHsExpr Name) Int
  | SyntaxNameCtxt (HsExpr Name) Type SDoc
  | FunctionCtxt HeraldContext Arity Int (Maybe [LHsExpr Name]) Type Type
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
  | WarningCtxt SDoc

instance Show ContextElement where
  show (FunctionResultCtxt {}) = "FunctionResultCtxt"
  show (FunctionArgumentCtxt {}) = "FunctionArgumentCtxt"
  show (SyntaxNameCtxt {}) = "SyntaxNameCtxt"
  show (FunctionCtxt {}) = "FunctionCtxt"
  show (PolymorphicCtxt {}) = "PolymorphicCtxt"
  show (AnnotationTcCtxt {}) = "AnnotationTcCtxt"
  show (AmbiguityCheckCtxt {}) = "AmbiguityCheckCtxt"
  show (ThetaCtxt {}) = "ThetaCtxt"
  show (SignatureCtxt {}) = "SignatureCtxt"
  show (TypeCtxt {}) = "TypeCtxt"
  show (PatternSigCtxt {}) = "PatternSigCtxt"
  show (KindCtxt {}) = "KindCtxt"
  show (PatternCtxt {}) = "PatternCtxt"
  show (ExportCtxt {}) = "ExportCtxt"
  show (SpecCtxt {}) = "SpecCtxt"
  show (VectorCtxt {}) = "VectorCtxt"
  show (TypeSignatureCtxt {}) = "TypeSignatureCtxt"
  show (PatMonoBindsCtxt {}) = "PatMonoBindsCtxt"
  show (MatchCtxt {}) = "MatchCtxt"
  show (ListComprehensionCtxt {}) = "ListComprehensionCtxt"
  show (StatementCtxt {}) = "StatementCtxt"
  show (CommandCtxt {}) = "CommandCtxt"
  show (ExpressionCtxt {}) = "ExpressionCtxt"
  show (StaticCtxt {}) = "StaticCtxt"
  show (FieldCtxt {}) = "FieldCtxt"
  show (DeclarationCtxt {}) = "DeclarationCtxt"
  show (ForeignDeclarationCtxt {}) = "ForeignDeclarationCtxt"
  show (DefaultDeclarationCtxt {}) = "DefaultDeclarationCtxt"
  show (RuleCtxt {}) = "RuleCtxt"
  show (DataConstructorCtxt {}) = "DataConstructorCtxt"
  show (DataConstructorsCtxt {}) = "DataConstructorsCtxt"
  show (ClassCtxt {}) = "ClassCtxt"
  show (FamilyInstCtxt {}) = "FamilyInstCtxt"
  show (ClosedTypeFamilyCtxt {}) = "ClosedTypeFamilyCtxt"
  show (AddTypeCtxt {}) = "AddTypeCtxt"
  show (RoleCtxt {}) = "RoleCtxt"
  show (StandaloneCtxt {}) = "StandaloneCtxt"
  show (DeriveInstCtxt {}) = "DeriveInstCtxt"
  show (InstDeclarationCtxt1 {}) = "InstDeclarationCtxt1"
  show (InstDeclarationCtxt2 {}) = "InstDeclarationCtxt2"
  show (MethSigCtxt {}) = "MethSigCtxt"
  show (SpecInstSigCtxt {}) = "SpecInstSigCtxt"
  show (DerivedInstCtxt {}) = "DerivedInstCtxt"
  show (MainCtxt {}) = "MainCtxt"
  show (QuotationCtxt {}) = "QuotationCtxt"
  show (SpliceDocCtxt {}) = "SpliceDocCtxt"
  show (SpliceResultCtxt {}) = "SpliceResultCtxt"
  show (ReifyCtxt {}) = "ReifyCtxt"
    --- Renamer error
  show (DuplicatedAndShadowedCtxt {}) = "DuplicatedAndShadowedCtxt"
  show (AnnotationRnCtxt {}) = "AnnotationRnCtxt"
  show (QuasiQuoteCtxt {}) = "QuasiQuoteCtxt"
  show (SpliceCtxt {}) = "SpliceCtxt"
  show (WarningCtxt {}) = "WarningCtxt"

data HowMuch = TooFew | TooMuch
  deriving Show

data HeraldContext
  = ArgumentOfDollarContext
  | OperatorContext (LHsExpr Name)
  | LambdaCaseContext (HsExpr Name)
  | EquationContext Name
  | LambdaExprContext (MatchGroup Name (LHsExpr Name))
  | FunctionApplicationContext (LHsExpr Name)

pprFunctionHerald :: HeraldContext -> SDoc
pprFunctionHerald ArgumentOfDollarContext                = text "($)"
pprFunctionHerald (OperatorContext expr)                 = ppr expr
pprFunctionHerald (LambdaCaseContext expr_name)          = ppr expr_name
pprFunctionHerald (EquationContext name)                 = ppr name
pprFunctionHerald (LambdaExprContext expr_name)          =
  (pprSetDepth (PartWay 1) $ pprMatches (LambdaExpr :: HsMatchContext Name) expr_name)
pprFunctionHerald (FunctionApplicationContext expr_name) = ppr expr_name

instance Outputable HeraldContext where
  ppr ArgumentOfDollarContext = text "The first argument of ($) takes"
  ppr (OperatorContext op) = text "The operator" <+> quotes (ppr op) <+> text "takes"
  ppr (LambdaCaseContext e) = sep [ text "The function" <+> quotes (ppr e), text "requires"]
  ppr (EquationContext fun_name)
      = text "The equation(s) for"
               <+> quotes (ppr fun_name) <+> text "have"
  ppr (LambdaExprContext match)
      = sep [ text "The lambda expression"
                           <+> quotes (pprSetDepth (PartWay 1) $
                               pprMatches (LambdaExpr :: HsMatchContext Name) match),
                          -- The pprSetDepth makes the abstraction print briefly
                  text "has"]
  ppr (FunctionApplicationContext fun)
     = sep [ text "The function" <+> quotes (ppr fun), text "is applied to"]

instance Outputable ContextElement where
  ppr (FunctionResultCtxt has_args n_fun n_env fun _ _ res_fun res_env _ _)
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
  ppr (FunctionArgumentCtxt fun _fun_tau arg arg_no)
     = hang (hsep [ text "In the", speakNth arg_no, text "argument of",
                       quotes (ppr fun) <> text ", namely"]) 2 (quotes (ppr arg))
  ppr (SyntaxNameCtxt name ty inst_loc)
    = vcat [ text "When checking that" <+> quotes (ppr name)
         <+> text "(needed by a syntactic construct)"
       , nest 2 (text "has the required type:" <+> ppr ty)
       , nest 2 inst_loc ]
  ppr (FunctionCtxt herald arity n_args _ orig_ty ty)
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
                    | isClassTyCon tc       -> text "class"
                    | isTypeFamilyTyCon tc  -> text "type family"
                    | isDataFamilyTyCon tc  -> text "data family"
                    | isTypeSynonymTyCon tc -> text "type"
                    | isNewTyCon tc         -> text "newtype"
                    | isDataTyCon tc        -> text "data"

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
  ppr (WarningCtxt s) = s

data SignatureContext = SignatureContext UserTypeCtxt SDoc SDoc

instance Outputable SignatureContext where
  ppr (SignatureContext ctxt extra pp_ty)
    = sep [ text "In" <+> extra <+> pprUserTypeCtxt ctxt <> colon
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
   | IdentifierExpectedError SDoc
   | NaughtyRecordSelectorError SDoc
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
   | NoRolesAssociatedError SDoc
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
   | NotInScopeError RdrName Bool [(RdrName, HowInScope)]
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
   | CheckWellStagedError SDoc SDoc SDoc
   | StageRestrictionError SDoc
   | GhcInternalError Name SDoc
   | WrongThingError String SDoc Name
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
   | ModuleDoesNotExportError SDoc ImpDeclSpec (IE RdrName)
   | BadImportItemDataConError OccName SDoc ImpDeclSpec (IE RdrName)
   | ExportClashError GlobalRdrEnv Name Name (IE RdrName) (IE RdrName)
   | GeneralWarningSDoc SDoc
   | DeSugarError SDoc
   | CouldNotDedudeError SDoc SDoc
   | UnfilledHoleError OccName Type Bool Bool SDoc ([TyVar] -> SDoc)
   | OutOfScopeHoleError RdrName Type RealSrcSpan GlobalRdrEnv (Set RealSrcSpan)
                         [(RdrName, HowInScope)]
   | IPError SDoc SDoc
   | TypeMismatchError MisMatchType TcTyVarSet TcType TcType SDoc
   | TypeVariableError MisMatchType SDoc SDoc
   | OccursCheckError  SDoc SDoc SDoc
   | TypeVariableKindMismatchError TyVar Type SDoc
   | UnificationVariableError TyVar Type
   | ImplicationSkolemError SDoc SDoc SDoc
   | ImplicationSkolemEscapeError SDoc SDoc SDoc
   | UntouchableUnificationError SDoc SDoc SDoc SDoc SDoc
   | NoInstanceWithUnifiersError SDoc
   | NoInstanceWithOverlapError SDoc
   | UnsafeOverlappingInstancesError SDoc
   | PartialTypeSignatureError SDoc
   | HscMainError SDoc
   | ParserError SDoc

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
       = sep [ text "Type indexes must match class instance head"
             , text "Found" <+> quotes (ppr ty)
                 <+> text "but expected" <+> quotes (ppr instTy) ]
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
       = hang (text "You cannot bind scoped type variable" <> plural sig_tvs
              <+> pprQuotedList (map fst sig_tvs))
           2 (text "in a pattern binding signature")
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
       = text "Number of pattern synonym arguments doesn't match type; expected"
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
       = thing <+> text "used where a value identifier was expected"
   ppr (NaughtyRecordSelectorError sel_id)
       = text "Cannot use record selector" <+> quotes sel_id <+>
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
     = hang (text "The binder" <+> quotes (ppr name) <+> text "is not a NameU.")
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
      = text "No roles associated with" <+> thing
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

   ppr (NotInScopeError rdr_name is_dk suggest)
       = vcat [ hang (text "Not in scope:")
                    2 (what <+> quotes (ppr rdr_name))
               , extra' ] $$ extra $$ extra_err
        where
          what = pprNonVarNameSpace (occNameSpace (rdrNameOcc rdr_name))
          extra
            | is_dk = text "A data constructor of that name is in scope; did you mean DataKinds?"
            | otherwise = empty
          tried_rdr_name = rdr_name
          tried_ns      = occNameSpace tried_occ
          tried_occ     = rdrNameOcc tried_rdr_name
          extra' | rdr_name == forall_tv_RDR = perhapsForallMsg
                 | otherwise                 = empty
          perhaps = text "Perhaps you meant"
          extra_err = case suggest of
                        []  -> empty
                        [p] -> perhaps <+> pp_item p
                        ps  -> sep [ perhaps <+> text "one of these:"
                                   , nest 2 (pprWithCommas pp_item ps) ]
          pp_item :: (RdrName, HowInScope) -> SDoc
          pp_item (rdr, Left loc) = pp_ns rdr <+> quotes (ppr rdr) <+> loc' -- Locally defined
              where loc' = case loc of
                             UnhelpfulSpan l -> parens (ppr l)
                             RealSrcSpan l -> parens (text "line" <+> int (srcSpanStartLine l))
          pp_item (rdr, Right is) = pp_ns rdr <+> quotes (ppr rdr) <+>   -- Imported
                                    parens (text "imported from" <+> ppr (is_mod is))

          pp_ns :: RdrName -> SDoc
          pp_ns rdr | ns /= tried_ns = pprNameSpace ns
                    | otherwise      = empty
            where ns = rdrNameSpace rdr

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
       = (sep [text "A" <+> int tup_size <> text "-tuple is too large for GHC",
                      nest 2 (parens (text "max size is" <+> int mAX_TUPLE_SIZE)),
                      nest 2 (text "Workaround: use nested tuples or define a data type")])

   ppr (BadInstanceError ty)
       = text "Malformed instance:" <+> ppr ty
   ppr (OperatorError op ty ty1)
       = hang (text "Illegal operator" <+> quotes (ppr op) <+> text "in type" <+> quotes (ppr ty))
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
           hsep   [text "is bound at stage" <+> bind_lvl,
                   text "but used at stage" <+> use_lvl]
   ppr (StageRestrictionError pp_thing)
        = sep [ text "GHC stage restriction:"
            , nest 2 (vcat [ pp_thing <+> text "is used in a top-level splice or annotation,"
                           , text "and must be imported, not defined locally"])]
   ppr (GhcInternalError name sdoc)
       = vcat [text "GHC internal error:" <+> quotes (ppr name) <+>
               text "is not in scope during type checking, but it passed the renamer",
               text "tcl_env of environment:" <+> sdoc]
   ppr (WrongThingError expected thing name)
       = (thing <+> quotes (ppr name) <+>
                     text "used as a" <+> text expected)
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
        text "LHS must be of form (f e1 .. en) where f is not forall'd"
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
   ppr (ModuleDoesNotExportError source_import decl_spec ie)
         = sep [text "Module", quotes (ppr (is_mod decl_spec)), source_import,
                text "does not export", quotes (ppr ie)]

   ppr (BadImportItemDataConError dataType_occ source_import decl_spec ie)
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
             , text "or"
             , nest 2 $ quotes (text "import")
                 <+> ppr (is_mod decl_spec)
                 <> parens_sp (dataType <> text "(..)")
             ]
      where
        datacon_occ = rdrNameOcc $ ieName ie
        datacon = parenSymOcc datacon_occ (ppr datacon_occ)
        dataType = parenSymOcc dataType_occ (ppr dataType_occ)
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

   ppr (GeneralWarningSDoc s) = s
   ppr (DeSugarError s) = s
   ppr (CouldNotDedudeError s e) = s $$ e
   ppr (UnfilledHoleError occ hole_ty is_expr is_type_hole_err binds_doc pprSkols) =
     getDoc $ pprHoleError occ hole_ty is_expr is_type_hole_err binds_doc pprSkols empty
   ppr (OutOfScopeHoleError {}) = text "OutOfScopeHoleError"
   ppr (IPError s e) = s $$ e
   ppr (TypeMismatchError mt tvs ty1 ty2 extra) = vcat [pprMisMatchType mt, mkEqInfoMsg tvs ty1 ty2, extra]
   ppr (TypeVariableError mt e d) = vcat [pprMisMatchType mt, e, d]
   ppr (OccursCheckError occCheckMsg extra2 extra) = occCheckMsg $$ extra2 $$ extra
   ppr (TypeVariableKindMismatchError tv1 ty2 extra) =
     kindErrorMsg (mkTyVarTy tv1) ty2 $$ extra
     where kindErrorMsg :: Type -> Type -> SDoc   -- Types are already tidy
           kindErrorMsg ty1 ty2
             = vcat [ text "Kind incompatibility when matching types:"
                    , nest 2 (vcat [ ppr ty1 <+> dcolon <+> ppr k1
                                   , ppr ty2 <+> dcolon <+> ppr k2 ]) ]
             where
               k1 = typeKind ty1
               k2 = typeKind ty2
   ppr (UnificationVariableError tv1 ty2) =
     vcat [ text "Cannot instantiate unification variable" <+> quotes (ppr tv1)
          , hang (text "with a type involving foralls:") 2 (ppr ty2)
          , nest 2 (text "Perhaps you want ImpredicativeTypes") ]
   ppr (ImplicationSkolemError s e d) = vcat [s, e, d]
   ppr (ImplicationSkolemEscapeError s e d) = s $$ e $$ d
   ppr (UntouchableUnificationError s e d f i) = vcat [s, e, d, f, i]
   ppr (NoInstanceWithUnifiersError s) = s
   ppr (NoInstanceWithOverlapError s) = s
   ppr (UnsafeOverlappingInstancesError s) = s
   ppr (PartialTypeSignatureError s) = s
   ppr (HscMainError s) = s
   ppr (ParserError s) = s

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
  = vcat [ text "Perhaps you intended to use ExplicitForAll or similar flag"
         , text "to enable explicit-forall syntax: forall <tvs>. <type>"]

ppr_opfix :: (Name, Fixity) -> SDoc
ppr_opfix (op, fixity) = pp_op <+> brackets (ppr fixity)
  where
    pp_op | op == negateName = text "prefix `-'"
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
undecidableMsg     = text "Use UndecidableInstances to permit this"
constraintKindsMsg = text "Use ConstraintKinds to permit this"

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
rankZeroMonoType = MonoType (text "Perhaps you intended to use RankNTypes or Rank2Types")
tyConArgMonoType = MonoType (text "Perhaps you intended to use ImpredicativeTypes")
synArgMonoType   = MonoType (text "Perhaps you intended to use LiberalTypeSynonyms")

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
         nest 2 (text "in the type family application:" <+>
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
   = hang (pprInstanceHdr (iSpec info) <+> text "where")
        2 (details (iBinds info))
  where
    details (InstBindings { ib_binds = b }) = pprLHsBinds b

data PromotionErr
  = TyConPE          -- TyCon used in a kind before we are ready
                     --     data T :: T -> * where ...
  | ClassPE          -- Ditto Class

  | FamDataConPE     -- Data constructor for a data family
                     -- See Note [AFamDataCon: not promoting data family constructors] in TcRnDriver

  | RecDataConPE     -- Data constructor in a recursive loop
                     -- See Note [ARecDataCon: recusion and promoting data constructors] in TcTyClsDecls
  | NoDataKinds      -- -XDataKinds not enabled

instance Outputable PromotionErr where
  ppr ClassPE      = text "ClassPE"
  ppr TyConPE      = text "TyConPE"
  ppr FamDataConPE = text "FamDataConPE"
  ppr RecDataConPE = text "RecDataConPE"
  ppr NoDataKinds  = text "NoDataKinds"

pprPECategory :: PromotionErr -> SDoc
pprPECategory ClassPE      = text "Class"
pprPECategory TyConPE      = text "Type constructor"
pprPECategory FamDataConPE = text "Data constructor"
pprPECategory RecDataConPE = text "Data constructor"
pprPECategory NoDataKinds  = text "Data constructor"

--------------------
misMatchMsg :: Maybe SwapFlag -> EqRel -> TcType -> TcType -> SDoc
-- Types are already tidy
-- If oriented then ty1 is actual, ty2 is expected
misMatchMsg oriented eq_rel ty1 ty2
  | Just IsSwapped <- oriented
  = misMatchMsg (Just NotSwapped) eq_rel ty2 ty1
  | Just NotSwapped <- oriented
  = sep [ text "Couldn't match" <+> repr1 <+> text "expected" <+>
          what <+> quotes (ppr ty2)
        , nest (12 + extra_space) $
          text "with" <+> repr2 <+> text "actual" <+> what <+> quotes (ppr ty1)
        , blankLine
        , sameOccExtra ty2 ty1 ]
  | otherwise
  = sep [ text "Couldn't match" <+> repr1 <+> what <+> quotes (ppr ty1)
        , nest (15 + extra_space) $
          text "with" <+> repr2 <+> quotes (ppr ty2)
        , sameOccExtra ty1 ty2 ]
  where
    what | isKind ty1 = text "kind"
         | otherwise  = text "type"

    (repr1, repr2, extra_space) = case eq_rel of
      NomEq  -> (empty, empty, 0)
      ReprEq -> (text "representation of", text "that of", 10)

sameOccExtra :: TcType -> TcType -> SDoc
-- See Note [Disambiguating (X ~ X) errors]
sameOccExtra ty1 ty2
  | Just (tc1, _) <- tcSplitTyConApp_maybe ty1
  , Just (tc2, _) <- tcSplitTyConApp_maybe ty2
  , let n1 = tyConName tc1
        n2 = tyConName tc2
        same_occ = nameOccName n1                  == nameOccName n2
        same_pkg = moduleUnitId (nameModule n1) == moduleUnitId (nameModule n2)
  , n1 /= n2   -- Different Names
  , same_occ   -- but same OccName
  = text "NB:" <+> (ppr_from same_pkg n1 $$ ppr_from same_pkg n2)
  | otherwise
  = empty
  where
    ppr_from same_pkg nm
      | isGoodSrcSpan loc
      = hang (quotes (ppr nm) <+> text "is defined at")
           2 (ppr loc)
      | otherwise  -- Imported things have an UnhelpfulSrcSpan
      = hang (quotes (ppr nm))
           2 (sep [ text "is defined in" <+> quotes (ppr (moduleName mod))
                  , ppUnless (same_pkg || pkg == mainUnitId) $
                    nest 4 $ text "in package" <+> quotes (ppr pkg) ])
       where
         pkg = moduleUnitId mod
         mod = nameModule nm
         loc = nameSrcSpan nm

mkEqInfoMsg :: TcTyVarSet -> TcType -> TcType -> SDoc
-- Report (a) ambiguity if either side is a type function application
--            e.g. F a0 ~ Int
--        (b) warning about injectivity if both sides are the same
--            type function application   F a ~ F b
--            See Note [Non-injective type functions]
mkEqInfoMsg tvs ty1 ty2
 = tyfun_msg $$ ambig_msg
 where
     mb_fun1 = isTyFun_maybe ty1
     mb_fun2 = isTyFun_maybe ty2

     ambig_msg | isJust mb_fun1 || isJust mb_fun2
               = snd (mkAmbigMsgTvs tvs)
               | otherwise = empty

     tyfun_msg | Just tc1 <- mb_fun1
               , Just tc2 <- mb_fun2
               , tc1 == tc2
               = text "NB:" <+> quotes (ppr tc1)
                 <+> text "is a type function, and may not be injective"
               | otherwise = empty

mkAmbigMsgTvs :: TcTyVarSet -> (Bool, SDoc)
mkAmbigMsgTvs tvs
 | null ambig_tkvs = (False, empty)
 | otherwise       = (True,  msg)
 where
   ambig_tkv_set = filterVarSet isAmbiguousTyVar tvs
   ambig_tkvs    = varSetElems ambig_tkv_set
   (ambig_kvs, ambig_tvs) = partition isKindVar ambig_tkvs

   msg | any isRuntimeUnkSkol ambig_tkvs  -- See Note [Runtime skolems]
       =  vcat [ ptext (sLit "Cannot resolve unknown runtime type") <> plural ambig_tvs
                    <+> pprQuotedList ambig_tvs
               , ptext (sLit "Use :print or :force to determine these types")]

       | not (null ambig_tvs)
       = pp_ambig (ptext (sLit "type")) ambig_tvs

       | otherwise  -- All ambiguous kind variabes; suggest -fprint-explicit-kinds
       = vcat [ pp_ambig (ptext (sLit "kind")) ambig_kvs
              , sdocWithDynFlags suggest_explicit_kinds ]

   pp_ambig what tkvs
     = ptext (sLit "The") <+> what <+> ptext (sLit "variable") <> plural tkvs
       <+> pprQuotedList tkvs <+> is_or_are tkvs <+> ptext (sLit "ambiguous")

   is_or_are [_] = text "is"
   is_or_are _   = text "are"

   suggest_explicit_kinds dflags  -- See Note [Suggest -fprint-explicit-kinds]
     | gopt Opt_PrintExplicitKinds dflags = empty
     | otherwise = ptext (sLit "Use -fprint-explicit-kinds to see the kind arguments")


data MisMatchType
  = MisMatch (Maybe SwapFlag) EqRel Type Type
  | CouldNotDeduce SDoc

pprMisMatchType :: MisMatchType -> SDoc
pprMisMatchType (MisMatch oriented eq_rel ty1 ty2) =
  misMatchMsg oriented eq_rel ty1 ty2
pprMisMatchType (CouldNotDeduce s) = s

getDoc :: (String, String, SDoc) -> SDoc
getDoc (_,_,s) = s

pp_with_type :: OccName -> Type -> SDoc
pp_with_type occ ty = hang (pprPrefixOcc occ) 2 (dcolon <+> pprType ty)

pprHoleError :: OccName -> Type -> Bool -> Bool -> SDoc -> ([TyVar] -> SDoc) -> SDoc
             -> (String, String, SDoc)
pprHoleError occ hole_ty is_expr is_type_hole_err binds_doc pprSkols caret =
  ( "TYPED HOLE"
  , "HoleError"
  , caret $+$ hole_msg $+$ binds_doc)
  where hole_kind = typeKind hole_ty
        tyvars    = tyVarsOfTypeList hole_ty

        hole_msg
          | is_expr = vcat [ hang (text "Found hole:")
                                2 (pp_with_type occ hole_ty)
                           , tyvars_msg, expr_hole_hint ]
          | otherwise = vcat [ hang (text "Found type wildcard" <+>
                                     quotes (ppr occ))
                                  2 (text "standing for" <+>
                                     quotes pp_hole_type_with_kind)
                             , tyvars_msg, type_hole_hint ]

        pp_hole_type_with_kind
           | isLiftedTypeKind hole_kind
           = pprType hole_ty
           | otherwise
           = pprType hole_ty <+> dcolon <+> pprKind hole_kind

        tyvars_msg = ppUnless (null tyvars) $
                     text "Where:" <+> (vcat (map loc_msg other_tvs) $$ pprSkols skol_tvs)
           where
             (skol_tvs, other_tvs) = partition is_skol tyvars
             is_skol tv = isSkolemTyVar tv

        type_hole_hint
            | is_type_hole_err
            = text "To use the inferred type, enable PartialTypeSignatures"
            | otherwise
            = empty

        expr_hole_hint                       -- Give hint for, say,   f x = _x
            | lengthFS (occNameFS occ) > 1  -- Don't give this hint for plain "_"
            = text "Or perhaps" <+> quotes (ppr occ)
              <+> text "is mis-spelled, or not in scope"
            | otherwise
            = empty

        loc_msg tv
          | MetaTv {} <- tcTyVarDetails tv =
            quotes (ppr tv) <+> text "is an ambiguous type variable"
          | otherwise = empty -- Skolems dealt with already

isTyFun_maybe :: Type -> Maybe TyCon
isTyFun_maybe ty = case tcSplitTyConApp_maybe ty of
                      Just (tc,_) | isTypeFamilyTyCon tc -> Just tc
                      _ -> Nothing
