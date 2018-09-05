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
  | otherwise =
    text "\n" $+$
    greeting <> text "," <+> compliment <> text "!" $+$
    blankLine $+$
    summary $+$
    blankLine $+$
    locationInfo $+$
    blankLine $+$
    allErrors
  where greeting     = getRandomMessage greetings
        compliment   = getRandomMessage compliments
        summary      = getRandomMessage errorSummary totalErrors
        sortedErrors = sortMsgBag msgs
        locationInfo = ftext $ srcSpanLocation $ errMsgSpan $ head sortedErrors
        totalErrors  = length sortedErrors
        numberErrors = zip [1..] sortedErrors
        allErrors    = foldl combineErrMsgs empty numberErrors
        combineErrMsgs msg (i, err) =
            msg $+$
            blankLine $+$
            pprHeading i (text "NAMING ERROR") $+$
            blankLine $+$
            pprErrMsg err $+$ text "\n"

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

errorSummary :: [Int -> SDoc]
errorSummary = [standard]
  where standard i
          | i == 1 = prefix <+> text "one error."
          | otherwise = prefix <+> int i <+> text "errors."
        prefix = text "To make sure your program runs properly, I've analyzed it and found"

pprHeading :: Int -> SDoc -> SDoc
pprHeading i heading = int i <+> hyphens <+> heading <+> hyphens
    where hyphens = text (replicate 25 '-')

pprErrMsg :: ErrMsg -> SDoc
pprErrMsg ErrMsg { errMsgShortDoc  = coreError,
                   errMsgExtraInfo = stackTrace,
                   errMsgContext   = unqual,
                   errMsgSeverity  = sev,
                   errMsgSpan      = span }
  = sdocWithDynFlags $ \dflags ->
      let style = mkErrStyle dflags unqual
      in withPprStyle style finalOutput
  where finalOutput
          | Just sdoc <- pprNiceErrMsg caret coreError stackTrace = sdoc
          | otherwise = caret $+$
                        blankLine $+$
                        ppr coreError $$ vcat (map ppr stackTrace)
        caret = unsafePerformIO (getCaretDiagnostic sev span)

pprNiceErrMsg :: SDoc -> TypeError -> [ContextElement] -> Maybe SDoc
pprNiceErrMsg caret (NotInScopeError rdr_name is_dk suggest) _ctxt
  = Just $ vcat [ text "I did not understand what" <+> rdr_ns
                    <+> coloredQuotes Col.colRedFg (ppr rdr_name)
                    <+> text "means in the following line.",
                  caret,
                  extra',
                  extra,
                  extra_err ]
   where
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
                               , blankLine
                               , nest 4 $ text "checking if there is a typo"
                               , blankLine
                               , nest 4 $ text "check to see if you imported the right module" ]
                   [p] -> vcat [ perhaps
                               , blankLine
                               , nest 4 (pp_item p)
                               , blankLine ]
                   ps  -> let (locals, importeds) = partition (\(_, inscope) -> isLeft inscope) ps
                              pprLocals
                                | null locals = empty
                                | otherwise = nest 4 (vcat $ map pp_item locals)
                              pprImporteds
                                | null importeds = empty
                                | otherwise = nest 4 (vcat $ map pp_item importeds)
                          in vcat [ perhaps <+> text "one of these?",
                                    blankLine,
                                    pprLocals,
                                    blankLine,
                                    pprImporteds ]

     pp_item :: (RdrName, HowInScope) -> SDoc
     -- Locally defined
     pp_item (rdr, Left loc) = coloredQuotes Col.colCyanFg loc' <+>
                               pp_ns rdr <+>
                               coloredQuotes Col.colYellowFg (ppr rdr)

         where loc' = case loc of
                        UnhelpfulSpan l -> parens (ppr l)
                        RealSrcSpan l -> text "Line" <+> int (srcSpanStartLine l)
     -- Imported
     pp_item (rdr, Right is) = coloredQuotes Col.colCyanFg (ppr (is_mod is)) <+>
                               pp_ns rdr <+>
                               coloredQuotes Col.colYellowFg (ppr rdr)

     pp_ns :: RdrName -> SDoc
     pp_ns rdr | ns /= tried_ns = pprNameSpace ns
               | otherwise      = empty
       where ns = rdrNameSpace rdr

pprNiceErrMsg _ _ _ = Nothing

getCaretDiagnostic :: Severity -> SrcSpan -> IO MsgDoc
getCaretDiagnostic _ (UnhelpfulSpan _) = pure empty
getCaretDiagnostic severity (RealSrcSpan span) = do
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
      sdocWithDynFlags $ \ dflags ->
      let sevColor = getSeverityColor severity (colScheme dflags)
          marginColor = Col.sMargin (colScheme dflags)
      in
      text ("\n") <>
      colored marginColor (text marginRow) <>
      text (" " ++ srcLinePre) <>
      colored sevColor (text srcLineSpan) <>
      text (srcLinePost ++ "\n") <>
      blankLine

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

coloredQuotes :: Col.PprColor -> SDoc -> SDoc
coloredQuotes col identifier =
  sdocWithDynFlags $ \dflags ->
    if shouldUseColor dflags
    then colored col identifier
    else quotes identifier
