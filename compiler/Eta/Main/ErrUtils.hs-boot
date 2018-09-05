module Eta.Main.ErrUtils where

import Eta.Utils.Outputable (SDoc, Outputable(..))
import Eta.BasicTypes.SrcLoc (SrcSpan)

data Severity
  = SevOutput
  | SevDump
  | SevInteractive
  | SevInfo
  | SevWarning
  | SevError
  | SevFatal

instance Outputable Severity where

type MsgDoc = SDoc

mkLocMessage :: Severity -> SrcSpan -> MsgDoc -> MsgDoc
mkLocMessageAnn :: Maybe String -> Severity -> SrcSpan -> MsgDoc -> MsgDoc
getCaretDiagnostic :: Severity -> SrcSpan -> IO MsgDoc
mkFullMsg :: SDoc -> SDoc -> SDoc
