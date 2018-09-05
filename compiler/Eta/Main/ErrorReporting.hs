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
        allErrors    = foldl (\msg (i, err) ->
                              msg $+$
                              blankLine $+$
                              pprHeading i (text "NAMING ERROR") $+$
                              blankLine $+$
                              unsafePerformIO
                                (getCaretDiagnostic (errMsgSeverity err) (errMsgSpan err)) $+$
                              blankLine $+$
                              pprErrMsg err $+$ text "\n") empty numberErrors

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

pprErrMsg :: ErrMsg -> SDoc
pprErrMsg ErrMsg { errMsgShortDoc  = d,
                   errMsgExtraInfo = e,
                   errMsgContext   = unqual }
  = sdocWithDynFlags $ \dflags ->
      let style = mkErrStyle dflags unqual
      in withPprStyle style (ppr d $$ vcat (map ppr e))

pprHeading :: Int -> SDoc -> SDoc
pprHeading i heading = int i <+> hyphens <+> heading <+> hyphens
    where hyphens = text (replicate 25 '-')

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
