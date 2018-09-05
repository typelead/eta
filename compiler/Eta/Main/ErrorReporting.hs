module Eta.Main.ErrorReporting (
  renderWarnings, renderErrors
) where

import Eta.Main.Error
import Eta.Utils.Outputable
import Eta.Utils.Bag (isEmptyBag)
import Data.Time.Clock.POSIX
import Data.Int
import System.IO.Unsafe
import Eta.BasicTypes.SrcLoc

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
      in withPprStyle style (d $$ e)

pprHeading :: Int -> SDoc -> SDoc
pprHeading i heading = int i <+> hyphens <+> heading <+> hyphens
    where hyphens = text (replicate 25 '-')
