{-
(c) The AQUA Project, Glasgow University, 1994-1998

\section[ErrsUtils]{Utilities for error reporting}
-}

{-# LANGUAGE CPP #-}

module Eta.Main.ErrUtils (
        MsgDoc,
        Validity(..), andValid, allValid, isValid, getInvalids,

        Severity(..),
        mkLocMessage, mkLocMessageAnn, pprMessageBag,
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

        prettyPrintGhcErrors, traceCmd,
        mkFullMsg, ParserMessages, emptyParserMessages,
        parserErrorsFound, printBagMsgDoc
    ) where

#include "HsVersions.h"

import Eta.Utils.Bag              ( Bag, bagToList, emptyBag, isEmptyBag )
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
import Data.Time
import Control.Monad
import Control.Monad.IO.Class
import System.IO
import System.IO.Error (catchIOError)
import Eta.Utils.PprColor
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

data Severity
  = SevOutput
  | SevDump
  | SevInteractive
  | SevInfo
  | SevWarning
  | SevError
  | SevFatal
 deriving Show

instance Outputable Severity where
  ppr severity = text (show severity)


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
                  (hang (colored (Col.sHeader (colScheme dflags)) header) 0
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
                        -- https://ghc.haskell.org/trac/ghc/ticket/10762
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

mkFullMsg :: SDoc -> SDoc -> SDoc
mkFullMsg msg caret =
  vcat [ blankLine,
         withBold (text startDashes) <+> colored colEtaFg (text "PROBLEM")
       <+> withBold (text endDashes),
         mainDoc,
         blankLine]

   where maxLength = maximum $ map length $ lines $ showSDoc dflags mainDoc
         mainDoc = vcat [blankLine,
                         text "The expression",
                         nest 4 caret,
                         text "has a type error.",
                         blankLine,
                         nest 4 msg,
                         blankLine ]
         unicode = useUnicode dflags
         dflags = unsafeGlobalDynFlags
         withBold sdoc
           | unicode = sdoc
           | otherwise = colored colBold sdoc
         startDashes
           | unicode = unicodeStartDashes
           | otherwise = "----"
         endDashes
           | unicode = unicodeEndDashes
           | otherwise = replicate (totalDashLength - startLength) '-'
         startLength = length startDashes + 9
         totalDashLength = maxLength + 6
         unicodeStartDashes = replicate 3 unicodeHorizontalDash
         unicodeEndDashes = replicate (totalDashLength - startLength - 1) unicodeHorizontalDash
         unicodeHorizontalDash = '\x2500'

type ParserMessages = (Bag SDoc, Bag SDoc)

emptyParserMessages :: ParserMessages
emptyParserMessages = (emptyBag, emptyBag)

parserErrorsFound :: DynFlags -> ParserMessages -> Bool
parserErrorsFound _dflags (_warns, errs) = not (isEmptyBag errs)

printBagMsgDoc :: DynFlags -> Bag MsgDoc -> IO ()
printBagMsgDoc dflags msgs
  | isEmptyBag msgs = return ()
  | otherwise = putLogMsg dflags NoReason SevWarning noSrcSpan style $ pprMessageBag msgs
  where style = mkErrStyle dflags alwaysQualify
