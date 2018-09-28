{-# LANGUAGE LambdaCase #-}
module Eta.Main.ErrorReporting (
  renderWarnings, renderErrors
) where

import Eta.Main.Error
import Eta.Main.ErrUtils hiding (getCaretDiagnostic)
import Eta.Utils.Outputable
import Eta.Utils.FastString (unpackFS)
import Eta.Utils.Bag (isEmptyBag)
import Eta.Utils.Util (sortWith)
import Data.Time.Clock.POSIX
import Data.Int
import System.IO.Unsafe
import Eta.BasicTypes.SrcLoc
import Eta.TypeCheck.TcType
import Eta.Types.TypeRep
import System.IO.Error (catchIOError)
import Eta.Utils.StringBuffer (atLine, hGetStringBuffer, len, lexemeToString)
import Eta.Main.DynFlags
import Eta.Utils.PprColor (PprColor)
import qualified Eta.Utils.PprColor as Col
import Eta.BasicTypes.OccName
import Eta.BasicTypes.RdrName
import Data.List (partition)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either (isLeft)
import Data.Maybe
import Eta.Prelude.PrelNames (forall_tv_RDR)

renderWarnings :: WarningMessages -> Maybe SDoc
renderWarnings msgs
 | isEmptyBag msgs = Nothing
 -- TODO: Split up warnings and errors into individual types
 --       Handle warning groups & flags
 | otherwise = Just $ renderErrors msgs

renderErrors :: ErrorMessages -> SDoc
renderErrors msgs
  | isEmptyBag msgs = empty
  | otherwise = nest 1 $
    text "\n" $+$
    colored Col.colLightPurpleFg (greeting <> text "," <+> compliment <> text "!") $+$
    blankLine $+$
    summary $+$
    blankLine $+$
    -- locationInfo $+$
    -- blankLine $+$
    allErrors
  where greeting     = getRandomMessage greetings
        compliment   = getRandomMessage compliments
        summary      = getRandomMessage errorSummary totalErrors Col.colLightPurpleFg
        sortedErrors = sortMsgBag msgs
        _locationInfo = colored (Col.colBold `mappend` Col.colGreyFg)
                               (ftext $ srcSpanLocation $ errMsgSpan $ head sortedErrors)
        totalErrors  = length sortedErrors
        numberErrors = zip [1..] sortedErrors
        allErrors    = foldl combineErrMsgs empty numberErrors
        combineErrMsgs msg (i, err) =
            msg $+$
            blankLine $+$

            pprErrMsg (pprHeading i) err $+$ text "\n"

getRandomMessage :: [a] -> a
getRandomMessage messages =
  messages !! fromIntegral (unsafePerformIO (fmap round getPOSIXTime :: IO Int64)
        `rem` (fromIntegral (length messages)))

greetings :: [SDoc]
greetings = map text ["Greetings", "Hi", "Hey"]

compliments :: [SDoc]
compliments =
  map text ["thank you for your hard work", "keep up the good work",
            "great work so far", "you've written some great code"]

errorSummary :: [Int -> PprColor -> SDoc]
errorSummary = [standard]
  where standard i col
          | i == 1 = prefix <+> colored col (text "one error") <> text "."
          | otherwise = prefix <+> colored col (int i <+> text "errors") <> text "."
        prefix = text "To make sure your program runs properly, I've analyzed it and found"

pprHeading :: Int -> String -> SDoc
pprHeading i heading = colored Col.colLightPurpleFg (int i) <+>
                       hyphens shorter <+>
                       colored (Col.colLightPurpleFg) (text heading) <+>
                       hyphens longer
    where hyphens n = text (replicate n unicodeHorizontalDash)
          shorter = remainingDashes `div` 2
          longer = remainingDashes - shorter

          cols = pprCols unsafeGlobalDynFlags
          remainingDashes = cols - length (show i) - 3 - length heading


pprErrMsg :: (String -> SDoc) -> ErrMsg -> SDoc
pprErrMsg prHeading ErrMsg { errMsgShortDoc  = coreError,
                             errMsgExtraInfo = stackTrace,
                             errMsgContext   = unqual,
                             errMsgSeverity  = sev,
                             errMsgSpan      = span }
  = sdocWithDynFlags $ \dflags ->
      let style = setStyleColored True $ mkErrStyle dflags unqual
      in withPprStyle style (prHeading heading $+$
                             blankLine $+$
                             finalOutput $+$
                             blankLine $+$
                             helpUrlMsg)
  where (heading, mUrlFragment, finalOutput)
          | Just (heading, url, sdoc) <- pprNiceErrMsg caret coreError stackTrace span
          = (heading, Just url, sdoc)
          | otherwise = ("Error",
                         Nothing,
                         nest 1 caret $+$
                         blankLine $+$
                         ppr coreError $$ vcat (map ppr stackTrace))
        caret = unsafePerformIO (getCaretDiagnostic Col.colCyanFg (Col.colBold `mappend` Col.colLightRedFg) sev span)
        helpUrlMsg
         | Just urlFragment <- mUrlFragment
         = text "If you need more help, check out" $+$
           blankLine $+$
           nest 4 (colored Col.colLightPurpleFg (text $ "https://errors.eta-lang.org/" ++ urlFragment))
         | otherwise = text "If you need more help, check out" $+$
                       blankLine $+$
                       nest 4 (colored Col.colLightPurpleFg (text $ "https://eta-lang.org/"))

pprNiceErrMsg :: SDoc -> TypeError -> [ContextElement] -> SrcSpan -> Maybe (String, String, SDoc)
pprNiceErrMsg caret (NotInScopeError rdr_name is_dk suggest) _ctxt _
  = pprOutOfScopeError rdr_name is_dk suggest caret
pprNiceErrMsg caret (TypeMismatchError mt tvs ty1 ty2 extra) ctxt span
  = Just (pprTypeMisMatchError caret mt tvs ty1 ty2 extra ctxt span)
pprNiceErrMsg caret (OutOfScopeHoleError rdr hole_ty err_loc rdr_env splice_locs suggest) _ctxt _span
  = Just $ pprOutOfScopeHoleError rdr hole_ty err_loc rdr_env splice_locs suggest caret
pprNiceErrMsg caret (UnfilledHoleError occ hole_ty is_expr is_type_hole_err binds_doc pprSkols) _ctxt _span
  = Just $ pprHoleError occ hole_ty is_expr is_type_hole_err binds_doc pprSkols caret
pprNiceErrMsg _ _ _ _ = Nothing

pprTypeMisMatchError :: SDoc -> MisMatchType -> TcTyVarSet -> TcType
                     -> TcType -> SDoc -> [ContextElement] -> SrcSpan -> (String, String, SDoc)
pprTypeMisMatchError caret mt tvs ty1 ty2 extra ctxts _span =
  ("FUNCTION TYPE MISMATCH",
   "FunctionTypeMismatch",
   fullMismatchError)
  where fullMismatchError
          | (ctxt:_) <- ctxts
          , FunctionCtxt herald arity n_args args orig_ty _ty <- ctxt
          = vcat [ let arguments n
                         | n == 1    = text "argument"
                         | otherwise = text "arguments"
                       extra = arity - n_args
                   in text "The function below has been given" <+>
                      colored Col.colLightRedFg (int extra <+> text "extra" <+> arguments extra) <> text "."
                 , maybe caret (highlightSource Col.colCyanFg .
                                map (\arg -> (getLoc arg, colored Col.colLightRedFg)) . drop n_args)
                   args
                 , text "I think you might have forgotten to add a" <+> hintColor (text "parenthesis")
                        <+> text "or a" <+> hintColor (text "comma") <> text "."
                 , blankLine
                 , text "The type of the function is shown below."
                 , blankLine
                 , blankLine
                 , nest 2 (pprFunctionHerald herald <+> dcolon <+> ppr orig_ty)
                 ]
          | (ctxt:_) <- ctxts
          , FunctionResultCtxt _has_args n_fun _n_env fun fun_ty no_args _res_fun _res_env _fun_res_ty _env_ty <- ctxt
          = vcat [ text "I found a type mismatch in the function below."
                 , caret
                 , text "The type of" <+> colored Col.colLightRedFg (ppr fun) <+> text "is shown below."
                 , blankLine
                 , blankLine
                 , nest 2 (colored Col.colLightRedFg (ppr fun) <+> dcolon <+>
                           pprHighlightFunTy (\n -> n > no_args && n <= (no_args + n_fun))
                            (colored Col.colRedFg) fun_ty)
                 , blankLine
                 , let n_args
                         | n_fun == 1 = text "argument is"
                         | otherwise = text "arguments are"
                   in text "I think that" <+>
                       colored Col.colYellowFg (int n_fun <+> n_args <+> text "missing")
                        <> text "."
                 ]
          | (ctxt:_) <- ctxts
          , FunctionArgumentCtxt fun fun_tau arg arg_no <- ctxt
          = vcat [ text "I found a type mismatch in the function and argument below."
                  , highlightSource Col.colCyanFg
                      [(getLoc fun, colored Col.colOrangeFg),
                       (getLoc arg, colored Col.colLightRedFg)]
                  , text "The types of the expressions are shown below."
                  , blankLine
                  , blankLine
                  , nest 2 (ppr fun <+> dcolon <+>
                            maybe empty
                            (pprHighlightFunTy (== arg_no) (colored Col.colOrangeFg))
                            fun_tau)
                  , blankLine
                  , blankLine
                  , nest 2 (ppr arg <+> dcolon <+> colored Col.colLightRedFg (ppr ty1))
                  , blankLine
                  , text "You can fix the problem by"
                  , blankLine, blankLine
                    , nest 4 $ char unicodeArrowHead <+> text "checking that you're using the"
                           <+> (hintColor $ text  "right function") <> text "."
                  , blankLine, blankLine
                    , nest 4 $ char unicodeArrowHead <+> text "checking that you're using the"
                           <+> (hintColor $ text "right argument") <> text "." ]
          | (_:ctxt:_) <- ctxts
          , FunctionArgumentCtxt fun fun_tau arg arg_no <- ctxt
          = vcat [ text "I found a type mismatch in the function and argument below."
                 , highlightSource Col.colCyanFg
                     [(getLoc fun, colored Col.colOrangeFg),
                      (getLoc arg, colored Col.colLightRedFg)]
                 , text "The types of the expressions are shown below."
                 , blankLine
                 , blankLine
                 , nest 2 (ppr fun <+> dcolon <+>
                           maybe empty
                           (pprHighlightFunTy (== arg_no) (colored Col.colOrangeFg))
                           fun_tau)
                 , blankLine
                 , blankLine
                 , nest 2 (ppr arg <+> dcolon <+> colored Col.colLightRedFg (ppr ty1))
                 , blankLine
                 , text "You can fix the problem by"
                 , blankLine, blankLine
                   , nest 4 $ char unicodeArrowHead <+> text "checking that you're using the"
                          <+> (hintColor $ text  "right function") <> text "."
                 , blankLine, blankLine
                   , nest 4 $ char unicodeArrowHead <+> text "checking that you're using the"
                          <+> (hintColor $ text "right argument") <> text "." ]
          | otherwise =
                vcat [ text "I found a type mismatch in the code below.",
                       caret,
                       ppr (TypeMismatchError mt tvs ty1 ty2 extra),
                       vcat $ map ppr ctxts,
                       text $ show ctxts ]

pprOutOfScopeError :: RdrName -> Bool
                   -> [(RdrName, HowInScope)]
                   -> SDoc -> Maybe (String, String, SDoc)
pprOutOfScopeError rdr_name is_dk suggest caret =
     Just ("OUT OF SCOPE",
          "OutOfScope",
          vcat [ text "I did not understand what" <+> rdr_ns
                    <+> nameCol (ppr rdr_name)
                    <+> text "means below.",
                 caret,
                 extra',
                 extra,
                 extra_err ])
   where
     locInfoCol = coloredQuotes Col.colEtaFg
     suggestCol = coloredQuotes Col.colLightRedFg
     nameCol = coloredQuotes Col.colLightRedFg

     rdr_ns = pprNonVarNameSpace (occNameSpace (rdrNameOcc rdr_name))
     extra
       | is_dk = text "A data constructor of that name is in scope; did you mean DataKinds?"
       | otherwise = empty
     tried_rdr_name = rdr_name
     tried_ns      = occNameSpace tried_occ
     tried_occ     = rdrNameOcc tried_rdr_name
     extra' | rdr_name == forall_tv_RDR = perhapsForallMsg
            | otherwise                 = empty
     perhaps = text "Did you mean"
     extra_err = case suggest of
                   []  -> vcat [ text "You can help me understand by"
                               , blankLine, blankLine
                               , nest 4 $ char unicodeArrowHead <+> text "checking if there is a"
                                   <+> (hintColor $ text "typo") <> text "."
                               , blankLine, blankLine
                               , nest 4 $ char unicodeArrowHead <+> text "checking if you imported the"
                                 <+> (hintColor $ text "right module") <> text "." ]
                   [p] -> vcat [ perhaps <+> text "this?"
                               , blankLine, blankLine
                               , nest 4 (pp_item p)
                               , blankLine ]
                   ps  -> let (locals, importeds) = partition (\(_, inscope) -> isLeft inscope) ps
                              pprLocals
                                | null locals = empty
                                | otherwise = vcat [ blankLine, blankLine, nest 4 (vcat $ map pp_item locals) ]
                              pprImporteds
                                | null importeds = empty
                                | otherwise = vcat [ blankLine, blankLine, nest 4 (vcat $ map pp_item importeds) ]
                          in vcat [ perhaps <+> text "one of these?",
                                    pprLocals,
                                    pprImporteds ]


     pp_item :: (RdrName, HowInScope) -> SDoc
     -- Locally defined
     pp_item (rdr, Left loc) = locInfoCol loc' <+>
                               suggestion_space <+>
                               pp_ns rdr <+>
                               suggestCol (ppr rdr)

         where loc' = case loc of
                        UnhelpfulSpan l -> parens (ppr l)
                        RealSrcSpan l   ->
                          text "Line" <+> int (srcSpanStartLine l)
     -- Imported
     pp_item (rdr, Right is) =
       locInfoCol (ppr (is_mod is)) <+>
       pp_ns rdr <+> suggestion_space <+> suggestCol (ppr rdr)

     suggestion_space = hsep (replicate 2 space)
     pp_ns :: RdrName -> SDoc
     pp_ns rdr | ns /= tried_ns = pprNameSpace ns
               | otherwise      = empty
       where ns = rdrNameSpace rdr

pprOutOfScopeHoleError :: RdrName -> Type -> RealSrcSpan -> GlobalRdrEnv -> Set RealSrcSpan
                       -> [(RdrName, HowInScope)] -> SDoc -> (String, String, SDoc)
pprOutOfScopeHoleError rdr hole_ty err_loc rdr_env splice_locs _suggestions caret
  = ("OUT OF SCOPE"
    ,"OutOfScope"
    ,out_of_scope_msg $+$
     caret $+$
     vcat match_msgs) -- ++ [ppr suggestions]))
  where match_msgs = mk_match_msgs rdr_env splice_locs
        occ = rdrNameOcc rdr
        boring_type = isTyVarTy hole_ty
        out_of_scope_msg -- Print v :: ty only if the type has structure
          | boring_type = hang herald 2 (ppr occ)
          | otherwise   = hang herald 2 (pp_with_type occ hole_ty)
        herald | isDataOcc occ = text "Data constructor not in scope:"
               | otherwise     = text "Variable not in scope:"

        -- Indicate if the out-of-scope variable exactly (and unambiguously) matches
        -- a top-level binding in a later inter-splice group; see Note [OutOfScope
        -- exact matches]
        mk_match_msgs rdr_env splice_locs
          = let gres = filter isLocalGRE (lookupGlobalRdrEnv rdr_env occ)
            in case gres of
                [gre]
                  |  RealSrcSpan bind_loc <- greSrcSpan gre
                      -- Find splice between the unbound variable and the match; use
                      -- lookupLE, not lookupLT, since match could be in the splice
                  ,  Just th_loc <- Set.lookupLE bind_loc splice_locs
                  ,  err_loc < th_loc
                  -> [mk_bind_scope_msg bind_loc th_loc]
                _ -> []

        mk_bind_scope_msg bind_loc th_loc
          | is_th_bind
          = hang (quotes (ppr occ) <+> parens (text "splice on" <+> th_rng))
              2 (text "is not in scope before line" <+> int th_start_ln)
          | otherwise
          = hang (quotes (ppr occ) <+> bind_rng <+> text "is not in scope")
              2 (text "before the splice on" <+> th_rng)
          where
            bind_rng = parens (text "line" <+> int bind_ln)
            th_rng
              | th_start_ln == th_end_ln = single
              | otherwise                = multi
            single = text "line"  <+> int th_start_ln
            multi  = text "lines" <+> int th_start_ln <> text "-" <> int th_end_ln
            bind_ln     = srcSpanStartLine bind_loc
            th_start_ln = srcSpanStartLine th_loc
            th_end_ln   = srcSpanEndLine   th_loc
            is_th_bind = th_loc `containsSpan` bind_loc

getCaretDiagnostic :: PprColor -> PprColor -> Severity -> SrcSpan -> IO MsgDoc
getCaretDiagnostic _ _ _ (UnhelpfulSpan _) = pure empty
getCaretDiagnostic marginColor sevColor _ (RealSrcSpan span) = do
  caretDiagnostic <$> getSrcLine (srcSpanFile span) row

  where
    getSrcLine fn i =
      getLine i (unpackFS fn)
        `catchIOError` \_ ->
          pure Nothing

    getLine i fn = do
      -- StringBuffer has advantages over readFile:
      -- (a) no lazy IO, otherwise IO exceptions may occur in pure code
      -- (b) always UTF-8, rather than some system-dependent encoding
      --     (Haskell source code must be UTF-8 anyway)
      content <- hGetStringBuffer fn
      case atLine i content of
        Just at_line -> pure $
          case lines (fix <$> lexemeToString at_line (len at_line)) of
            srcLine : _ -> Just srcLine
            _           -> Nothing
        _ -> pure Nothing

    -- allow user to visibly see that their code is incorrectly encoded
    -- (StringBuffer.nextChar uses \0 to represent undecodable characters)
    fix '\0' = '\xfffd'
    fix c    = c

    row = srcSpanStartLine span
    rowStr = show row
    multiline = row /= srcSpanEndLine span

    caretDiagnostic Nothing = empty
    caretDiagnostic (Just srcLineWithNewline) =
      vcat [ blankLine,
             colored marginColor (text marginRow) <>
             text (" " ++ srcLinePre) <>
             colored sevColor (text srcLineSpan) <>
             text (srcLinePost),
             blankLine ]
      where

        -- expand tabs in a device-independent manner #13664
        expandTabs tabWidth i s =
          case s of
            ""        -> ""
            '\t' : cs -> replicate effectiveWidth ' ' ++
                         expandTabs tabWidth (i + effectiveWidth) cs
            c    : cs -> c : expandTabs tabWidth (i + 1) cs
          where effectiveWidth = tabWidth - i `mod` tabWidth

        srcLine = filter (/= '\n') (expandTabs 8 0 srcLineWithNewline)

        start = srcSpanStartCol span - 1
        end | multiline = length srcLine
            | otherwise = srcSpanEndCol span - 1
        width = max 1 (end - start)

        marginRow   = rowStr ++ " |"

        (srcLinePre,  srcLineRest) = splitAt start srcLine
        (srcLineSpan, srcLinePost) = splitAt width srcLineRest

highlightSource :: PprColor -> [(SrcSpan, SDoc -> SDoc)] -> MsgDoc
highlightSource marginColor locs
  | null realLocs = empty
  | otherwise = vcat [ blankLine
                     , highlightedLines
                     , blankLine ]

  where realLocs = mapMaybe (\case (UnhelpfulSpan _,  _) -> Nothing
                                   (RealSrcSpan span, f) -> Just (span, f)) locs
        fullSpan   = foldl1 combineRealSrcSpans $ map fst realLocs
        startLine  = srcSpanStartLine fullSpan
        endLine    = srcSpanEndLine   fullSpan
        fileName   = unpackFS (srcSpanFile fullSpan)
        sortedLocs = sortWith fst realLocs
        highlightedLines = go sortedLocs startLine
        margin n   = colored marginColor (text (show n ++ " |"))
        go [] _ = empty
        go locs0 !n
          | n > endLine = empty
          | otherwise =
            margin n <> text " " <> goLocs lineLocs srcLine 0 $+$
            go (trailingLoc lineLocs locs') (n + 1)
          where (lineLocs, locs') =
                  span (\(s, _) -> srcSpanStartLine s <= n) locs0
                trailingLoc xs ys
                  | length lineLocs > 0
                  , let trail@(s, _) = last xs
                  , srcSpanEndLine s /= n
                  = trail:ys
                  | otherwise = ys
                mSrcLine = unsafePerformIO (getSrcLine fileName n)
                srcLine = fromMaybe (error "highlightSource: srcLine fromMaybe") $
                            fmap expandLine mSrcLine
                goLocs [] line _offset = text line
                goLocs ((span, f):locs) line offset =
                   text srcLinePre <>
                   f (text srcLineSpan) <>
                   goLocs locs srcLinePost (offset + end)
                  where start | n == srcSpanStartLine span =
                                srcSpanStartCol span - 1 - offset
                              | otherwise = 0
                        end | n == srcSpanEndLine span =
                                srcSpanEndCol span - 1 - offset
                            | otherwise = length line
                        width = max 1 (end - start)
                        (srcLinePre,  srcLineRest) = splitAt start line
                        (srcLineSpan, srcLinePost) = splitAt width srcLineRest


getSrcLine :: FilePath -> Int -> IO (Maybe String)
getSrcLine fn i = getLine i fn `catchIOError` \_ -> pure Nothing
  where getLine :: Int -> FilePath -> IO (Maybe String)
        getLine i fn = do
          -- StringBuffer has advantages over readFile:
          -- (a) no lazy IO, otherwise IO exceptions may occur in pure code
          -- (b) always UTF-8, rather than some system-dependent encoding
          --     (Haskell source code must be UTF-8 anyway)
          content <- hGetStringBuffer fn
          case atLine i content of
            Just at_line -> pure $
              case lines (fix <$> lexemeToString at_line (len at_line)) of
                  srcLine : _ -> Just srcLine
                  _           -> Nothing
            _ -> pure Nothing

        -- allow user to visibly see that their code is incorrectly encoded
        -- (StringBuffer.nextChar uses \0 to represent undecodable characters)
        fix '\0' = '\xfffd'
        fix c    = c

expandLine :: String -> String
expandLine srcLineWithNewline =
  filter (/= '\n') (expandTabs 8 0 srcLineWithNewline)
  where -- expand tabs in a device-independent manner #13664
        expandTabs tabWidth i s =
          case s of
            ""        -> ""
            '\t' : cs -> replicate effectiveWidth ' ' ++
                           expandTabs tabWidth (i + effectiveWidth) cs
            c    : cs -> c : expandTabs tabWidth (i + 1) cs
          where effectiveWidth = tabWidth - i `mod` tabWidth

coloredQuotes :: PprColor -> SDoc -> SDoc
coloredQuotes col identifier =
  sdocWithDynFlags $ \dflags ->
    if shouldUseColor dflags
    then colored col identifier
    else quotes identifier

unicodeHorizontalDash :: Char
unicodeHorizontalDash
  | unicode   = '\x2500'
  | otherwise = '-'

unicode :: Bool
unicode = useUnicode unsafeGlobalDynFlags

unicodeArrowHead :: Char
unicodeArrowHead
  | unicode   = '\x27A4'
  | otherwise = '>'

hintColor :: SDoc -> SDoc
hintColor = colored Col.colYellowFg
