module Eta.Main.ErrorReporting (
  renderWarnings, renderErrors
) where

import Eta.Main.Error
import Eta.Utils.Outputable
import Eta.Utils.Bag (isEmptyBag)

renderWarnings :: WarningMessages -> Maybe SDoc
renderWarnings msgs
 | isEmptyBag msgs = Nothing
 | otherwise = Just $ error "renderWarnings"

renderErrors :: ErrorMessages -> SDoc
renderErrors msgs
  | isEmptyBag msgs = empty
  | otherwise = error "renderErrors"
