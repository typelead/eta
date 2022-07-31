{-
(c) The AQUA Project, Glasgow University, 1994-1998

\section[ErrsUtils]{Utilities for error reporting}
-}

{-# LANGUAGE CPP #-}

module Eta.Main.ErrUtils (
        MsgDoc,
        Validity(..), andValid, allValid, isValid, getInvalids,

        ErrMsg, WarnMsg, Severity(..), errMsgSeverity, errMsgReason,
        Messages, ErrorMessages, WarningMessages, unionMessages,
        errMsgSpan, errMsgContext, errMsgShortDoc, errMsgExtraInfo,
        mkLocMessage, mkLocMessageAnn, pprMessageBag, pprErrMsgBag, pprErrMsgBagWithLoc,
        pprLocErrMsg, makeIntoWarning, isWarning,

        errorsFound, emptyMessages, isEmptyMessages,
        mkErrMsg, mkPlainErrMsg, mkLongErrMsg, mkWarnMsg, mkPlainWarnMsg,
        mkLongWarnMsg, printBagOfErrors,

        ghcExit,

        -- * Utilities
        doIfSet, doIfSet_dyn,
        getCaretDiagnostic,

        -- * Dump files
        dumpIfSet, dumpIfSet_dyn, dumpIfSet_dyn_printer,
        mkDumpDoc, dumpSDoc,

        --  * Messages during compilation
        putMsg, printInfoForUser, printOutputForUser,
        logInfo, logOutput, logInteractive,
        errorMsg, warningMsg,
        fatalErrorMsg, fatalErrorMsg', fatalErrorMsg'',
        compilationProgressMsg,
        showPass,
        debugTraceMsg,

        prettyPrintGhcErrors, traceCmd, isWarnMsgFatal
    ) where

#include "HsVersions.h"

import Eta.Utils.Bag              ( Bag, bagToList, isEmptyBag, emptyBag, unionBags )
import Eta.Utils.Exception
import Eta.Utils.Outputable
import Eta.Utils.Panic
import Eta.Utils.FastString (unpackFS)
import Eta.Utils.StringBuffer (atLine, hGetStringBuffer, len, lexemeToString)
import qualified Eta.Utils.PprColor as Col
import Eta.BasicTypes.SrcLoc
import Eta.Main.DynFlags

import System.Directory
import System.Exit      ( ExitCode(..), exitWith )
import System.FilePath  ( takeDirectory, (</>) )
import Data.List
import qualified Data.Set as Set
import Data.IORef
import Data.Ord
import Data.Time
import Control.Monad
import Control.Monad.IO.Class
import System.IO
import System.IO.Error (catchIOError)

-------------------------
type MsgDoc  = SDoc

-------------------------
data Validity
  = IsValid            -- Everything is fine
  | NotValid MsgDoc    -- A problem, and some indication of why

isValid :: Validity -> Bool
isValid IsValid       = True
isValid (NotValid {}) = False

andValid :: Validity -> Validity -> Validity
andValid IsValid v = v
andValid v _       = v

allValid :: [Validity] -> Validity   -- If they aren't all valid, return the first
allValid []       = IsValid
allValid (v : vs) = v `andValid` allValid vs

getInvalids :: [Validity] -> [MsgDoc]
getInvalids vs = [d | NotValid d <- vs]

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

data Severity
  = SevOutput
  | SevDump
  | SevInteractive
  | SevInfo
  | SevWarning
  | SevError
  | SevFatal

instance Show ErrMsg where
    show em = errMsgShortString em

pprMessageBag :: Bag MsgDoc -> SDoc
pprMessageBag msgs = vcat (punctuate blankLine (bagToList msgs))


-- | Make an unannotated error message with location info.
mkLocMessage :: Severity -> SrcSpan -> MsgDoc -> MsgDoc
mkLocMessage = mkLocMessageAnn Nothing

-- | Make a possibly annotated error message with location info.
mkLocMessageAnn
  :: Maybe String                       -- ^ optional annotation
  -> Severity                           -- ^ severity
  -> SrcSpan                            -- ^ location
  -> MsgDoc                             -- ^ message
  -> MsgDoc
  -- Always print the location, even if it is unhelpful.  Error messages
  -- are supposed to be in a standard format, and one without a location
  -- would look strange.  Better to say explicitly "<no location info>".
mkLocMessageAnn ann severity locn msg
    = sdocWithDynFlags $ \dflags ->
      let locn' = if gopt Opt_ErrorSpans dflags
                  then ppr locn
                  else ppr (srcSpanStart locn)

          sevColor = getSeverityColor severity (colScheme dflags)

          -- Add optional information
          optAnn = case ann of
            Nothing -> text ""
            Just i  -> text " [" <> colored sevColor (text i) <> text "]"

          -- Add prefixes, like    Foo.hs:34: warning:
          --                           <the warning message>
          header = locn' <> colon <+>
                   colored sevColor sevText <> optAnn

      in colored (Col.sMessage (colScheme dflags))
                  (hang (colored (Col.sHeader (colScheme dflags)) header) 4
                        msg)

  where
    sevText =
      case severity of
        SevWarning -> text "warning:"
        SevError   -> text "error:"
        SevFatal   -> text "fatal:"
        _          -> empty

getSeverityColor :: Severity -> Col.Scheme -> Col.PprColor
getSeverityColor SevWarning = Col.sWarning
getSeverityColor SevError   = Col.sError
getSeverityColor SevFatal   = Col.sFatal
getSeverityColor _          = const mempty

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
      colored marginColor (text marginSpace) <>
      text ("\n") <>
      colored marginColor (text marginRow) <>
      text (" " ++ srcLinePre) <>
      colored sevColor (text srcLineSpan) <>
      text (srcLinePost ++ "\n") <>
      colored marginColor (text marginSpace) <>
      colored sevColor (text (" " ++ caretLine))

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

        marginWidth = length rowStr
        marginSpace = replicate marginWidth ' ' ++ " |"
        marginRow   = rowStr ++ " |"

        (srcLinePre,  srcLineRest) = splitAt start srcLine
        (srcLineSpan, srcLinePost) = splitAt width srcLineRest

        caretEllipsis | multiline = "..."
                      | otherwise = ""
        caretLine = replicate start ' ' ++ replicate width '^' ++ caretEllipsis

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

ghcExit :: DynFlags -> Int -> IO ()
ghcExit dflags val
  | val == 0  = exitWith ExitSuccess
  | otherwise = do errorMsg dflags (text "\nCompilation had errors\n\n")
                   exitWith (ExitFailure val)

doIfSet :: Bool -> IO () -> IO ()
doIfSet flag action | flag      = action
                    | otherwise = return ()

doIfSet_dyn :: DynFlags -> GeneralFlag -> IO () -> IO()
doIfSet_dyn dflags flag action | gopt flag dflags = action
                               | otherwise        = return ()

-- -----------------------------------------------------------------------------
-- Dumping

dumpIfSet :: DynFlags -> Bool -> String -> SDoc -> IO ()
dumpIfSet dflags flag hdr doc
  | not flag   = return ()
  | otherwise  = putLogMsg dflags
                           NoReason
                           SevDump
                           noSrcSpan
                           (defaultDumpStyle dflags)
                           (mkDumpDoc hdr doc)

-- | a wrapper around 'dumpSDoc'.
-- First check whether the dump flag is set
-- Do nothing if it is unset
dumpIfSet_dyn :: DynFlags -> DumpFlag -> String -> SDoc -> IO ()
dumpIfSet_dyn dflags flag hdr doc
  = when (dopt flag dflags) $ dumpSDoc dflags alwaysQualify flag hdr doc

-- | a wrapper around 'dumpSDoc'.
-- First check whether the dump flag is set
-- Do nothing if it is unset
--
-- Unlike 'dumpIfSet_dyn',
-- has a printer argument but no header argument
dumpIfSet_dyn_printer :: PrintUnqualified
                      -> DynFlags -> DumpFlag -> SDoc -> IO ()
dumpIfSet_dyn_printer printer dflags flag doc
  = when (dopt flag dflags) $ dumpSDoc dflags printer flag "" doc

mkDumpDoc :: String -> SDoc -> SDoc
mkDumpDoc hdr doc
   = vcat [blankLine,
           line <+> text hdr <+> line,
           doc,
           blankLine]
     where
        line = text (replicate 20 '=')


-- | Write out a dump.
--      If --dump-to-file is set then this goes to a file.
--      otherwise emit to stdout.
--
-- When hdr is empty, we print in a more compact format (no separators and
-- blank lines)
--
-- The DumpFlag is used only to choose the filename to use if --dump-to-file is
-- used; it is not used to decide whether to dump the output
dumpSDoc :: DynFlags -> PrintUnqualified -> DumpFlag -> String -> SDoc -> IO ()
dumpSDoc dflags print_unqual flag hdr doc
 = do let mFile = chooseDumpFile dflags flag
          dump_style = mkDumpStyle dflags print_unqual
      case mFile of
            Just fileName
                 -> do
                        let gdref = generatedDumps dflags
                        gd <- readIORef gdref
                        let append = Set.member fileName gd
                            mode = if append then AppendMode else WriteMode
                        when (not append) $
                            writeIORef gdref (Set.insert fileName gd)
                        createDirectoryIfMissing True (takeDirectory fileName)
                        handle <- openFile fileName mode

                        -- We do not want the dump file to be affected by
                        -- environment variables, but instead to always use
                        -- UTF8. See:
                        -- https://gitlab.haskell.org/ghc/ghc/-/issues/10762
                        hSetEncoding handle utf8

                        doc' <- if null hdr
                                then return doc
                                else do t <- getCurrentTime
                                        let d = text (show t)
                                             $$ blankLine
                                             $$ doc
                                        return $ mkDumpDoc hdr d
                        defaultLogActionHPrintDoc dflags handle doc' dump_style
                        hClose handle

            -- write the dump to stdout
            Nothing -> do
              let (doc', severity)
                    | null hdr  = (doc, SevOutput)
                    | otherwise = (mkDumpDoc hdr doc, SevDump)
              putLogMsg dflags NoReason severity noSrcSpan dump_style doc'


-- | Choose where to put a dump file based on DynFlags
--
chooseDumpFile :: DynFlags -> DumpFlag -> Maybe String
chooseDumpFile dflags flag

        | gopt Opt_DumpToFile dflags || flag == Opt_D_th_dec_file
        , Just prefix <- getPrefix
        = Just $ setDir (prefix ++ (beautifyDumpName flag))

        | otherwise
        = Nothing

        where getPrefix
                 -- dump file location is being forced
                 --      by the --ddump-file-prefix flag.
               | Just prefix <- dumpPrefixForce dflags
                  = Just prefix
                 -- dump file location chosen by DriverPipeline.runPipeline
               | Just prefix <- dumpPrefix dflags
                  = Just prefix
                 -- we haven't got a place to put a dump file.
               | otherwise
                  = Nothing
              setDir f = case dumpDir dflags of
                         Just d  -> d </> f
                         Nothing ->       f

-- | Build a nice file name from name of a GeneralFlag constructor
beautifyDumpName :: DumpFlag -> String
beautifyDumpName Opt_D_th_dec_file = "th.hs"
beautifyDumpName flag
 = let str = show flag
       suff = case stripPrefix "Opt_D_" str of
              Just x -> x
              Nothing -> panic ("Bad flag name: " ++ str)
       dash = map (\c -> if c == '_' then '-' else c) suff
   in dash


-- -----------------------------------------------------------------------------
-- Outputting messages from the compiler

-- We want all messages to go through one place, so that we can
-- redirect them if necessary.  For example, when GHC is used as a
-- library we might want to catch all messages that GHC tries to
-- output and do something else with them.

ifVerbose :: DynFlags -> Int -> IO () -> IO ()
ifVerbose dflags val act
  | verbosity dflags >= val = act
  | otherwise               = return ()

errorMsg :: DynFlags -> MsgDoc -> IO ()
errorMsg dflags msg
   = putLogMsg dflags NoReason SevError noSrcSpan (defaultErrStyle dflags) msg

warningMsg :: DynFlags -> MsgDoc -> IO ()
warningMsg dflags msg
   = putLogMsg dflags NoReason SevWarning noSrcSpan (defaultErrStyle dflags) msg

fatalErrorMsg :: DynFlags -> MsgDoc -> IO ()
fatalErrorMsg dflags msg = fatalErrorMsg' (log_action dflags) dflags msg

fatalErrorMsg' :: LogAction -> DynFlags -> MsgDoc -> IO ()
fatalErrorMsg' la dflags msg =
    la dflags NoReason SevFatal noSrcSpan (defaultErrStyle dflags) msg

fatalErrorMsg'' :: FatalMessager -> String -> IO ()
fatalErrorMsg'' fm msg = fm msg

compilationProgressMsg :: DynFlags -> String -> IO ()
compilationProgressMsg dflags msg = ifVerbose dflags 1 printMsg
  where printMsg
          | shouldUseColor dflags =
            logInteractive dflags (setStyleColored True (defaultUserStyle dflags))
              (colored Col.clearToEndOfLine (text msg))
          | otherwise = logOutput dflags (defaultUserStyle dflags) (text msg)

showPass :: DynFlags -> String -> IO ()
showPass dflags what
  = ifVerbose dflags 2 $
    logInfo dflags (defaultUserStyle dflags) (text "***" <+> text what <> colon)

debugTraceMsg :: DynFlags -> Int -> MsgDoc -> IO ()
debugTraceMsg dflags val msg = ifVerbose dflags val $
                               logInfo dflags (defaultDumpStyle dflags) msg

putMsg :: DynFlags -> MsgDoc -> IO ()
putMsg dflags msg = logInfo dflags (defaultUserStyle dflags) msg

printInfoForUser :: DynFlags -> PrintUnqualified -> MsgDoc -> IO ()
printInfoForUser dflags print_unqual msg
  = logInfo dflags (mkUserStyle dflags print_unqual AllTheWay) msg

printOutputForUser :: DynFlags -> PrintUnqualified -> MsgDoc -> IO ()
printOutputForUser dflags print_unqual msg
  = logOutput dflags (mkUserStyle dflags print_unqual AllTheWay) msg

logInfo :: DynFlags -> PprStyle -> MsgDoc -> IO ()
logInfo dflags sty msg = putLogMsg dflags NoReason SevInfo noSrcSpan sty msg

logOutput :: DynFlags -> PprStyle -> MsgDoc -> IO ()
-- Like logInfo but with SevOutput rather then SevInfo
logOutput dflags sty msg = putLogMsg dflags NoReason SevOutput noSrcSpan sty msg

logInteractive :: DynFlags -> PprStyle -> MsgDoc -> IO ()
-- Like logInfo but with SevOutput rather then SevInfo
logInteractive dflags sty msg = putLogMsg dflags NoReason SevInteractive noSrcSpan sty msg

prettyPrintGhcErrors :: ExceptionMonad m => DynFlags -> m a -> m a
prettyPrintGhcErrors dflags
    = ghandle $ \e -> case e of
                      PprPanic str doc ->
                          pprDebugAndThen dflags panic (text str) doc
                      PprSorry str doc ->
                          pprDebugAndThen dflags sorry (text str) doc
                      PprProgramError str doc ->
                          pprDebugAndThen dflags pgmError (text str) doc
                      _ ->
                          liftIO $ throwIO e

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

traceCmd :: DynFlags -> String -> String -> IO a -> IO a
-- trace the command (at two levels of verbosity)
traceCmd dflags phase_name cmd_line action
 = do   { let verb = verbosity dflags
        ; showPass dflags phase_name
        ; debugTraceMsg dflags 3 (text cmd_line)
        ; case flushErr dflags of
              FlushErr io -> io

           -- And run it!
        ; action `catchIO` handle_exn verb
        }
  where
    handle_exn _verb exn = do { debugTraceMsg dflags 2 (char '\n')
                              ; debugTraceMsg dflags 2
                                (text "Failed:"
                                 <+> text cmd_line
                                 <+> text (show exn))
                              ; throwGhcExceptionIO (ProgramError (show exn))}
