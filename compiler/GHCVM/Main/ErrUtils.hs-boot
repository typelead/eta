module ErrUtils where

import GHCVM.Utils.Outputable (SDoc)
import GHCVM.BasicTypes.SrcLoc (SrcSpan)

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
