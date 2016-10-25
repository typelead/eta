module ETA.Main.ErrUtils where

import ETA.Utils.Outputable (SDoc)
import ETA.BasicTypes.SrcLoc (SrcSpan)

data Severity
  = SevOutput
  | SevDump
  | SevInteractive
  | SevInfo
  | SevWarning
  | SevError
  | SevFatal

type MsgDoc = SDoc

mkLocMessage :: Severity -> SrcSpan -> MsgDoc -> MsgDoc
