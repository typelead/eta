module Eta.Main.ErrorReporting (
  renderWarnings, renderErrors
) where

import Eta.Main.Error
import Eta.Main.ErrUtils hiding (getCaretDiagnostic)
import Eta.Utils.Outputable
import Eta.Utils.Bag (isEmptyBag)
import Data.Time.Clock.POSIX
import Data.Int
import System.IO.Unsafe
import Eta.BasicTypes.SrcLoc
import Eta.Utils.FastString
import System.IO.Error (catchIOError)
import Eta.Utils.StringBuffer (atLine, hGetStringBuffer, len, lexemeToString)
import Eta.Main.DynFlags
import Eta.Utils.PprColor (PprColor)
import qualified Eta.Utils.PprColor as Col
import Eta.BasicTypes.OccName
import Eta.BasicTypes.RdrName
import Data.List (partition)
import Data.Either (isLeft)
import Eta.Prelude.PrelNames (forall_tv_RDR)

renderWarnings :: WarningMessages -> Maybe SDoc
renderWarnings msgs
 | isEmptyBag msgs = Nothing
 | otherwise = Just $ error "renderWarnings"

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
greetings = map text ["Greetings Jo", "Hi Jo", "Hey Jo"]

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
          | Just (heading, url, sdoc) <- pprNiceErrMsg caret coreError stackTrace
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

pprNiceErrMsg :: SDoc -> TypeError -> [ContextElement] -> Maybe (String, String, SDoc)
pprNiceErrMsg caret (NotInScopeError rdr_name is_dk suggest) _ctxt
  = pprOutOfScopeError rdr_name is_dk suggest caret
pprNiceErrMsg _ _ _ = Nothing

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
     hintColor = colored Col.colYellowFg

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
