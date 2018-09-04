module Eta.Main.ErrorReporting (
  renderWarnings, renderErrors
) where

import Eta.Main.Error
import Eta.Utils.Outputable
import Eta.Utils.Bag (isEmptyBag)

renderWarnings :: WarningMessages -> SDoc
renderWarnings msgs
 | isEmptyBag msgs = empty
 | otherwise = error "renderWarnings"

renderErrors :: ErrorMessages -> SDoc
renderErrors msgs
  | isEmptyBag msgs = empty
  | otherwise = error "renderErrors"
