module Eta.Utils.PprColor where

import Data.Maybe
import Data.Monoid
import Eta.Utils.Util (OverridingBool(..), split)

-- | A color\/style for use with 'colored'.
newtype PprColor = PprColor { renderColor :: String }

mapPprColor :: (String -> String) -> PprColor -> PprColor
mapPprColor f (PprColor s) = PprColor (f s)

-- | Allow colors to be combined (e.g. bold + red);
--   In case of conflict, right side takes precedence.
instance Monoid PprColor where
  mempty = PprColor mempty
  PprColor s1 `mappend` PprColor s2 = PprColor (s1 <> s2)

renderColorAfresh :: PprColor -> String
renderColorAfresh c = renderColor (colReset `mappend` c)

codeCustom :: String -> PprColor
codeCustom "" = mempty
codeCustom s  = PprColor ("\27[" ++ s)

clearToEndOfLine :: PprColor
clearToEndOfLine = mapPprColor ("\r" ++) $ codeCustom "K"

colCustom :: String -> PprColor
colCustom s = codeCustom (s ++ "m")

colReset :: PprColor
colReset = colCustom "0"

colBold :: PprColor
colBold = colCustom ";1"

colBlackFg :: PprColor
colBlackFg = colCustom "30"

colRedFg :: PprColor
colRedFg = colCustom "31"

colGreenFg :: PprColor
colGreenFg = colCustom "32"

colYellowFg :: PprColor
colYellowFg = colCustom "33"

colBlueFg :: PprColor
colBlueFg = colCustom "34"

colMagentaFg :: PprColor
colMagentaFg = colCustom "35"

colCyanFg :: PprColor
colCyanFg = colCustom "36"

colWhiteFg :: PprColor
colWhiteFg = colCustom "37"

colEtaFg :: PprColor
colEtaFg = colCustom "38;5;31"

data Scheme =
  Scheme
  { sHeader  :: PprColor
  , sMessage :: PprColor
  , sWarning :: PprColor
  , sError   :: PprColor
  , sFatal   :: PprColor
  , sMargin  :: PprColor
  }

defaultScheme :: Scheme
defaultScheme =
  Scheme
  { sHeader  = mempty
  , sMessage = colBold
  , sWarning = colBold `mappend` colMagentaFg
  , sError   = colBold `mappend` colRedFg
  , sFatal   = colBold `mappend` colRedFg
  , sMargin  = colBold `mappend` colEtaFg
  }

-- | Parse the color scheme from a string (presumably from the @GHC_COLORS@
-- environment variable).
parseScheme :: String -> (OverridingBool, Scheme) -> (OverridingBool, Scheme)
parseScheme "always" (_, cs) = (Always, cs)
parseScheme "auto"   (_, cs) = (Auto,   cs)
parseScheme "never"  (_, cs) = (Never,  cs)
parseScheme input    (b, cs) =
  ( b
  , Scheme
    { sHeader  = fromMaybe (sHeader cs)  (lookup "header" table)
    , sMessage = fromMaybe (sMessage cs) (lookup "message" table)
    , sWarning = fromMaybe (sWarning cs) (lookup "warning" table)
    , sError   = fromMaybe (sError cs)   (lookup "error"   table)
    , sFatal   = fromMaybe (sFatal cs)   (lookup "fatal"   table)
    , sMargin  = fromMaybe (sMargin cs)  (lookup "margin"  table)
    }
  )
  where
    table = do
      w <- split ':' input
      let (k, v') = break (== '=') w
      case v' of
        '=' : v -> return (k, colCustom v)
        _ -> []
