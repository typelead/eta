module Eta.Main.Error (
      ErrMsg, WarnMsg, errMsgReason, errMsgSeverity,
      Messages, ErrorMessages, WarningMessages, unionMessages,
      errMsgSpan, errMsgContext, errMsgShortDoc, errMsgExtraInfo,
      pprErrMsgBag, pprErrMsgBagWithLoc,
      pprLocErrMsg, makeIntoWarning, isWarning,

      errorsFound, emptyMessages, isEmptyMessages,
      mkErrMsg, mkPlainErrMsg, mkLongErrMsg, mkWarnMsg, mkPlainWarnMsg,
      mkLongWarnMsg, printBagOfErrors, isWarnMsgFatal

) where

import Eta.Main.ErrUtils
import Eta.Utils.Bag              ( Bag, bagToList, isEmptyBag, emptyBag, unionBags )
import Data.Ord
import Eta.BasicTypes.SrcLoc
import Eta.Utils.Outputable
import Eta.Main.DynFlags
import Data.List (sortBy)

-- -----------------------------------------------------------------------------
-- Basic error messages: just render a message with a source location.

type Messages        = (WarningMessages, ErrorMessages)
type WarningMessages = Bag WarnMsg
type ErrorMessages   = Bag ErrMsg

unionMessages :: Messages -> Messages -> Messages
unionMessages (warns1, errs1) (warns2, errs2) =
  (warns1 `unionBags` warns2, errs1 `unionBags` errs2)

data ErrMsg = ErrMsg {
        errMsgSpan        :: SrcSpan,
        errMsgContext     :: PrintUnqualified,
        errMsgShortDoc    :: MsgDoc,   -- errMsgShort* should always
        errMsgShortString :: String, -- contain the same text
        errMsgExtraInfo   :: MsgDoc,
        errMsgSeverity    :: Severity,
        errMsgReason      :: WarnReason
        }
        -- The SrcSpan is used for sorting errors into line-number order

type WarnMsg = ErrMsg

instance Show ErrMsg where
    show em = errMsgShortString em

makeIntoWarning :: WarnReason -> ErrMsg -> ErrMsg
makeIntoWarning reason err = err
    { errMsgSeverity = SevWarning
    , errMsgReason = reason }

isWarning :: ErrMsg -> Bool
isWarning err
  | SevWarning <- errMsgSeverity err = True
  | otherwise                        = False

-- -----------------------------------------------------------------------------
-- Collecting up messages for later ordering and printing.

mk_err_msg :: DynFlags -> Severity -> SrcSpan -> PrintUnqualified -> MsgDoc -> SDoc -> ErrMsg
mk_err_msg  dflags sev locn print_unqual msg extra
 = ErrMsg { errMsgSpan = locn, errMsgContext = print_unqual
          , errMsgShortDoc = msg , errMsgShortString = showSDoc dflags msg
          , errMsgExtraInfo = extra
          , errMsgSeverity = sev
          , errMsgReason = NoReason }

mkLongErrMsg, mkLongWarnMsg   :: DynFlags -> SrcSpan -> PrintUnqualified -> MsgDoc -> MsgDoc -> ErrMsg
-- A long (multi-line) error message
mkErrMsg, mkWarnMsg           :: DynFlags -> SrcSpan -> PrintUnqualified -> MsgDoc            -> ErrMsg
-- A short (one-line) error message
mkPlainErrMsg, mkPlainWarnMsg :: DynFlags -> SrcSpan ->                     MsgDoc            -> ErrMsg
-- Variant that doesn't care about qualified/unqualified names

mkLongErrMsg   dflags locn unqual msg extra = mk_err_msg dflags SevError   locn unqual        msg extra
mkErrMsg       dflags locn unqual msg       = mk_err_msg dflags SevError   locn unqual        msg empty
mkPlainErrMsg  dflags locn        msg       = mk_err_msg dflags SevError   locn alwaysQualify msg empty
mkLongWarnMsg  dflags locn unqual msg extra = mk_err_msg dflags SevWarning locn unqual        msg extra
mkWarnMsg      dflags locn unqual msg       = mk_err_msg dflags SevWarning locn unqual        msg empty
mkPlainWarnMsg dflags locn        msg       = mk_err_msg dflags SevWarning locn alwaysQualify msg empty

----------------
emptyMessages :: Messages
emptyMessages = (emptyBag, emptyBag)

isEmptyMessages :: Messages -> Bool
isEmptyMessages (warns, errs) = isEmptyBag warns && isEmptyBag errs

errorsFound :: DynFlags -> Messages -> Bool
errorsFound _dflags (_warns, errs) = not (isEmptyBag errs)

printBagOfErrors :: DynFlags -> Bag ErrMsg -> IO ()
printBagOfErrors dflags bag_of_errors
  = printMsgBag dflags bag_of_errors

pprErrMsgBag :: Bag ErrMsg -> [SDoc]
pprErrMsgBag bag
  = [ sdocWithDynFlags $ \dflags ->
      let style = mkErrStyle dflags unqual
      in withPprStyle style (d $$ e)
    | ErrMsg { errMsgShortDoc  = d,
               errMsgExtraInfo = e,
               errMsgContext   = unqual } <- sortMsgBag bag ]

pprErrMsgBagWithLoc :: Bag ErrMsg -> [SDoc]
pprErrMsgBagWithLoc bag = [ pprLocErrMsg item | item <- sortMsgBag bag ]

pprLocErrMsg :: ErrMsg -> SDoc
pprLocErrMsg (ErrMsg { errMsgSpan      = s
                     , errMsgShortDoc  = d
                     , errMsgExtraInfo = e
                     , errMsgSeverity  = sev
                     , errMsgContext   = unqual })
  = sdocWithDynFlags $ \dflags ->
    withPprStyle (mkErrStyle dflags unqual) (mkLocMessage sev s (d $$ e))

printMsgBag :: DynFlags -> Bag ErrMsg -> IO ()
printMsgBag dflags bag
  = sequence_ [ let style = mkErrStyle dflags unqual
                in putLogMsg dflags reason sev s style (d $$ e)
              | ErrMsg { errMsgSpan      = s,
                         errMsgShortDoc  = d,
                         errMsgSeverity  = sev,
                         errMsgReason    = reason,
                         errMsgExtraInfo = e,
                         errMsgContext   = unqual } <- sortMsgBag bag ]

sortMsgBag :: Bag ErrMsg -> [ErrMsg]
sortMsgBag bag = sortBy (comparing errMsgSpan) $ bagToList bag

-- | Checks if given 'WarnMsg' is a fatal warning.
isWarnMsgFatal :: DynFlags -> WarnMsg -> Maybe (Maybe WarningFlag)
isWarnMsgFatal dflags ErrMsg{errMsgReason = Reason wflag}
  = if wopt_fatal wflag dflags
      then Just (Just wflag)
      else Nothing
isWarnMsgFatal dflags _
  = if gopt Opt_WarnIsError dflags
      then Just Nothing
      else Nothing
