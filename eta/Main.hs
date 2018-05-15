{-# LANGUAGE CPP, NondecreasingIndentation, TupleSections #-}
module Main (main) where

-- GHC API
import qualified ETA.Main.GHC as GHC
import ETA.Main.GHC               ( Ghc, GhcMonad(..), LoadHowMuch(..) )
import ETA.Main.CmdLineParser
import ETA.Iface.LoadIface        ( showIface, loadUserInterface)
import ETA.Main.HscMain           ( newHscEnv )
import ETA.Main.DriverPipeline
import ETA.Main.DriverMkDepend    ( doMkDependHS )
import ETA.Main.SysTools
import ETA.Main.Constants
import ETA.Main.HscTypes
import ETA.Main.Packages          (pprPackages, pprPackagesSimple)
import ETA.Main.DriverPhases
import ETA.BasicTypes.BasicTypes  (failed)
import ETA.Main.StaticFlags
import ETA.Main.DynFlags
import ETA.Main.ErrUtils
import ETA.Utils.FastString
import ETA.Utils.Outputable
import ETA.BasicTypes.SrcLoc
import ETA.Utils.Util
import ETA.Utils.Metrics hiding (Mode)
import ETA.Utils.Panic
import ETA.Utils.MonadUtils       (liftIO)
#if defined(ETA_REPL)
import Eta.REPL.UI          ( interactiveUI, etaReplWelcomeMsg, defaultEtaReplSettings )
#endif
-- Imports for --abi-hash
import ETA.BasicTypes.Module      ( mkModuleName)
import ETA.Main.Finder            ( findImportedModule, cannotFindInterface )
import ETA.TypeCheck.TcRnMonad    ( initIfaceCheck )
import ETA.Utils.Binary           ( openBinMem, put_, fingerprintBinMem )

-- Standard Libraries
import System.IO
import System.IO.Unsafe
import System.Environment
import System.Exit
import System.FilePath
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

initETA :: IO ()
initETA = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  -- Handle GHC-specific character encoding flags, allowing us to control how
  -- GHC produces output regardless of OS.
  env <- getEnvironment
  case lookup "GHC_CHARENC" env of
    Just "UTF-8" -> do
      hSetEncoding stdout utf8
      hSetEncoding stderr utf8
    _ -> do
      -- Avoid GHC erroring out when trying to display unhandled characters
      hSetTranslit stdout
      hSetTranslit stderr

main :: IO ()
main = do
  initETA
  GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    -- 1. extract the -B flag from the args
    argv0 <- getArgs
    libdir <- findTopDir Nothing
    let (minusB_args, argv1) = partition ("-B" `isPrefixOf`) argv0
        mbMinusB | null minusB_args = Just libdir
                 | otherwise = Just (drop 2 (last minusB_args))

    let argv1' = map (mkGeneralLocated "on the commandline") argv1
    (argv2, staticFlagWarnings) <- parseStaticFlags argv1'

    -- 2. Parse the "mode" flags (--make, --interactive etc.)
    (mode, argv3, modeFlagWarnings) <- parseModeFlags argv2

    let flagWarnings = staticFlagWarnings ++ modeFlagWarnings

    -- If all we want to do is something like showing the version number
    -- then do it now, before we start a GHC session etc. This makes
    -- getting basic information much more resilient.

    -- In particular, if we wait until later before giving the version
    -- number then bootstrapping gets confused, as it tries to find out
    -- what version of GHC it's using before package.conf exists, so
    -- starting the session fails.
    case mode of
        Left preStartupMode ->
          case preStartupMode of
            ShowSupportedExtensions   -> showSupportedExtensions
            ShowVersion               -> showVersion
            ShowNumVersion            -> putStrLn cProjectVersionNumbers
            ShowOptions isInteractive -> showOptions isInteractive
        Right postStartupMode ->
            -- start our GHC session
            GHC.runGhc mbMinusB $ do

              dflags <- GHC.getSessionDynFlags

              case postStartupMode of
                  Left preLoadMode ->
                      liftIO $
                          case preLoadMode of
                              ShowInfo               -> showInfo dflags
                              ShowGhcUsage           -> showEtaUsage  dflags
                              ShowGhciUsage          -> showEtaiUsage dflags
                              PrintWithDynFlags f    -> putStrLn (f dflags)
                  Right postLoadMode ->
                      main' postLoadMode dflags argv3 flagWarnings
    return ()

main' :: PostLoadMode -> DynFlags -> [Located String] -> [Located String]
      -> Ghc ()
main' postLoadMode dflags0 args flagWarnings = do
  -- set the default GhcMode, HscTarget and GhcLink.  The HscTarget
  -- can be further adjusted on a module by module basis, using only
  -- the -fvia-C and -fasm flags.  If the default HscTarget is not
  -- HscC or HscAsm, -fvia-C and -fasm have no effect.
  let dflt_target = hscTarget dflags0
      (mode, lang, link)
         = case postLoadMode of
               DoInteractive   -> (CompManager, HscInterpreted, NoLink)
               DoEval _        -> (CompManager, HscInterpreted, NoLink)
               DoMake          -> (CompManager, dflt_target,    LinkBinary)
               DoMkDependHS    -> (MkDepend,    dflt_target,    LinkBinary)
               DoAbiHash       -> (OneShot,     dflt_target,    LinkBinary)
               _               -> (OneShot,     dflt_target,    LinkBinary)

  let dflags1 = case lang of
                HscInterpreted ->
                    let platform = targetPlatform dflags0
                        dflags0a = updateWays $ dflags0 { ways = interpWays }
                        dflags0b = foldl gopt_set dflags0a
                                 $ concatMap (wayGeneralFlags platform)
                                             interpWays
                        dflags0c = foldl gopt_unset dflags0b
                                 $ concatMap (wayUnsetGeneralFlags platform)
                                             interpWays
                    in dflags0c
                _ ->
                    dflags0
      dflags2 = dflags1{ ghcMode   = mode,
                         hscTarget = lang,
                         ghcLink   = link,
                         verbosity = case postLoadMode of
                                         DoEval _ -> 0
                                         _other   -> 1
                        }

      -- turn on -fimplicit-import-qualified for GHCi now, so that it
      -- can be overriden from the command-line
      -- XXX: this should really be in the interactive DynFlags, but
      -- we don't set that until later in interactiveUI
      dflags3  | DoInteractive <- postLoadMode = imp_qual_enabled
               | DoEval _      <- postLoadMode = imp_qual_enabled
               | otherwise                     = dflags2
        where imp_qual_enabled = dflags2 `gopt_set` Opt_ImplicitImportQualified

        -- The rest of the arguments are "dynamic"
        -- Leftover ones are presumably files
  (dflags4, fileish_args, dynamicFlagWarnings) <- GHC.parseDynamicFlags dflags3 args

  GHC.prettyPrintGhcErrors dflags4 $ do

  let flagWarnings' = flagWarnings ++ dynamicFlagWarnings

  handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $
         liftIO $ handleFlagWarnings dflags4 flagWarnings'

        -- make sure we clean up after ourselves
  GHC.defaultCleanupHandler dflags4 $ do

  liftIO $ showBanner postLoadMode dflags4

  let
     -- To simplify the handling of filepaths, we normalise all filepaths right
     -- away - e.g., for win32 platforms, backslashes are converted
     -- into forward slashes.
    normal_fileish_paths = map (normalise . unLoc) fileish_args
    -- TODO: Clean this up
    (srcs', objs)        = parititionArgs normal_fileish_paths [] []
    srcs                 = srcs' ++ map (\o -> (o, Nothing)) objs
    dflags5              = dflags4

    -- dflags5 = dflags4 { ldInputs = map (FileOption "") objs
    --                                ++ ldInputs dflags4 }

  -- we've finished manipulating the DynFlags, update the session
  _ <- GHC.setSessionDynFlags dflags5
  dflags6 <- GHC.getSessionDynFlags
  hsc_env <- GHC.getSession

        ---------------- Display configuration -----------
  case verbosity dflags6 of
    v | v == 4 -> liftIO $ dumpPackagesSimple dflags6
      | v >= 5 -> liftIO $ dumpPackages dflags6
      | otherwise -> return ()

        ---------------- Final sanity checking -----------
  liftIO $ checkOptions postLoadMode dflags6 srcs objs

  let measure mode io = do
        liftIO $ GHC.startMetrics mode
        r <- io
        liftIO $ GHC.endMetrics
        return r

  ---------------- Do the business -----------
  handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $
    case postLoadMode of
       ShowInterface f        -> liftIO $ doShowIface dflags6 f
       DoMake                 -> measure MakeMode $ doMake srcs
       DoMkDependHS           -> doMkDependHS (map fst srcs)
       StopBefore p           -> measure OneShotMode $ liftIO (oneShot hsc_env p srcs)
       DoInteractive          -> measure InteractiveMode $ liftIO $ putStrLn "Eta REPL not implemented yet"
       DoEval _exprs          -> measure EvalMode $ liftIO $ putStrLn "Eta REPL not implemented yet"
       DoAbiHash              -> abiHash (map fst srcs)
       ShowPackages           -> liftIO $ showPackages dflags6

  liftIO $ dumpFinalStats dflags6

-- -----------------------------------------------------------------------------
-- Splitting arguments into source files and object files.  This is where we
-- interpret the -x <suffix> option, and attach a (Maybe Phase) to each source
-- file indicating the phase specified by the -x option in force, if any.

parititionArgs :: [String] -> [(String, Maybe Phase)] -> [String]
               -> ([(String, Maybe Phase)], [String])
parititionArgs [] srcs objs = (reverse srcs, reverse objs)
parititionArgs ("-x":suff:args) srcs objs
  | "none" <- suff      = parititionArgs args srcs objs
  | StopLn <- phase     = parititionArgs args srcs (slurp ++ objs)
  | otherwise           = parititionArgs rest (these_srcs ++ srcs) objs
        where phase = startPhase suff
              (slurp,rest) = break (== "-x") args
              these_srcs = zip slurp (repeat (Just phase))
parititionArgs (arg:args) srcs objs
  | looksLikeAnInput arg = parititionArgs args ((arg,Nothing):srcs) objs
  | otherwise               = parititionArgs args srcs (arg:objs)

    {-
      We split out the object files (.o, .dll) and add them
      to ldInputs for use by the linker.

      The following things should be considered compilation manager inputs:

       - haskell source files (strings ending in .hs, .lhs or other
         haskellish extension),

       - module names (not forgetting hierarchical module names),

       - things beginning with '-' are flags that were not recognised by
         the flag parser, and we want them to generate errors later in
         checkOptions, so we class them as source files (#5921)

       - and finally we consider everything not containing a '.' to be
         a comp manager input, as shorthand for a .hs or .lhs filename.

      Everything else is considered to be a linker object, and passed
      straight through to the linker.
    -}
looksLikeAnInput :: String -> Bool
looksLikeAnInput m =  isSourceFilename m
                      || looksLikeModuleName m
                      || "-" `isPrefixOf` m
                      || '.' `notElem` m

-- -----------------------------------------------------------------------------
-- Option sanity checks

-- | Ensure sanity of options.
--
-- Throws 'UsageError' or 'CmdLineError' if not.
checkOptions :: PostLoadMode -> DynFlags -> [(String,Maybe Phase)] -> [String] -> IO ()
     -- Final sanity checking before kicking off a compilation (pipeline).
checkOptions mode dflags srcs objs = do
     -- Complain about any unknown flags
   let unknown_opts = [ f | (f@('-':_), _) <- srcs ]
   when (notNull unknown_opts) (unknownFlagsErr unknown_opts)

   when (notNull (filter wayRTSOnly (ways dflags))
         && isInterpretiveMode mode) $
        hPutStrLn stderr "Warning: -debug, -threaded and -ticky are ignored by GHCi"

        -- -prof and --interactive are not a good combination
   when ((filter (not . wayRTSOnly) (ways dflags) /= interpWays)
         && isInterpretiveMode mode) $
      throwGhcException (UsageError
                         "--interactive can't be used with -prof or -unreg.")
        -- -ohi sanity check
   if isJust (outputHi dflags) &&
      (isCompManagerMode mode || srcs `lengthExceeds` 1)
        then throwGhcException (UsageError "-ohi can only be used when compiling a single source file")
        else if srcs `lengthExceeds` 1 && isJust (outputFile dflags)
                 && not (isLinkMode mode)
             then throwGhcException (UsageError "can't apply -o to multiple source files")
             else do
     let not_linking = not (isLinkMode mode) || isNoLink (ghcLink dflags)
     when (not_linking && not (null objs)) $
       hPutStrLn stderr ("Warning: the following files would be used as linker inputs, but linking is not being done: " ++ unwords objs)

     if null srcs && (null objs || not_linking) && needsInputsMode mode
       then throwGhcException (UsageError "no input files")
       else verifyOutputFiles dflags


-- Compiler output options

-- Called to verify that the output files point somewhere valid.
--
-- The assumption is that the directory portion of these output
-- options will have to exist by the time 'verifyOutputFiles'
-- is invoked.
--
-- We create the directories for -odir, -hidir, -outputdir etc. ourselves if
-- they don't exist, so don't check for those here (#2278).
verifyOutputFiles :: DynFlags -> IO ()
verifyOutputFiles dflags = do
  let ofile = outputFile dflags
  when (isJust ofile) $ do
     let fn = fromJust ofile
     flg <- doesDirNameExist fn
     unless flg (nonExistentDir "-o" fn)
  let ohi = outputHi dflags
  when (isJust ohi) $ do
     let hi = fromJust ohi
     flg <- doesDirNameExist hi
     unless flg (nonExistentDir "-ohi" hi)
 where
   nonExistentDir flg dir =
     throwGhcException (CmdLineError ("error: directory portion of " ++
                             show dir ++ " does not exist (used with " ++
                             show flg ++ " option.)"))
-----------------------------------------------------------------------------
-- GHC modes of operation

type Mode = Either PreStartupMode PostStartupMode
type PostStartupMode = Either PreLoadMode PostLoadMode

data PreStartupMode
  = ShowVersion                          -- ghc -V/--version
  | ShowNumVersion                       -- ghc --numeric-version
  | ShowSupportedExtensions              -- ghc --supported-extensions
  | ShowOptions Bool {- isInteractive -} -- ghc --show-options

showVersionMode, showNumVersionMode, showSupportedExtensionsMode, showOptionsMode :: Mode
showVersionMode             = mkPreStartupMode ShowVersion
showNumVersionMode          = mkPreStartupMode ShowNumVersion
showSupportedExtensionsMode = mkPreStartupMode ShowSupportedExtensions
showOptionsMode             = mkPreStartupMode (ShowOptions False)

mkPreStartupMode :: PreStartupMode -> Mode
mkPreStartupMode = Left

isShowVersionMode :: Mode -> Bool
isShowVersionMode (Left ShowVersion) = True
isShowVersionMode _ = False

isShowNumVersionMode :: Mode -> Bool
isShowNumVersionMode (Left ShowNumVersion) = True
isShowNumVersionMode _ = False

data PreLoadMode
  = ShowGhcUsage                           -- ghc -?
  | ShowGhciUsage                          -- ghci -?
  | ShowInfo                               -- ghc --info
  | PrintWithDynFlags (DynFlags -> String) -- ghc --print-foo

showEtaUsageMode, showEtaiUsageMode, showInfoMode :: Mode
showEtaUsageMode = mkPreLoadMode ShowGhcUsage
showEtaiUsageMode = mkPreLoadMode ShowGhciUsage
showInfoMode = mkPreLoadMode ShowInfo

printSetting :: String -> Mode
printSetting k = mkPreLoadMode (PrintWithDynFlags f)
    where f _dflags = fromMaybe (panic ("Setting not found: " ++ show k))
                    $ lookup k compilerInfo

mkPreLoadMode :: PreLoadMode -> Mode
mkPreLoadMode = Right . Left

isShowGhcUsageMode :: Mode -> Bool
isShowGhcUsageMode (Right (Left ShowGhcUsage)) = True
isShowGhcUsageMode _ = False

isShowGhciUsageMode :: Mode -> Bool
isShowGhciUsageMode (Right (Left ShowGhciUsage)) = True
isShowGhciUsageMode _ = False

data PostLoadMode
  = ShowInterface FilePath  -- ghc --show-iface
  | DoMkDependHS            -- ghc -M
  | StopBefore Phase        -- ghc -E | -C | -S
                            -- StopBefore StopLn is the default
  | DoMake                  -- ghc --make
  | DoInteractive           -- ghc --interactive
  | DoEval [String]         -- ghc -e foo -e bar => DoEval ["bar", "foo"]
  | DoAbiHash               -- ghc --abi-hash
  | ShowPackages            -- ghc --show-packages

doMkDependHSMode, doMakeMode, doInteractiveMode,
  doAbiHashMode, showPackagesMode :: Mode
doMkDependHSMode = mkPostLoadMode DoMkDependHS
doMakeMode = mkPostLoadMode DoMake
doInteractiveMode = mkPostLoadMode DoInteractive
doAbiHashMode = mkPostLoadMode DoAbiHash
showPackagesMode = mkPostLoadMode ShowPackages

showInterfaceMode :: FilePath -> Mode
showInterfaceMode fp = mkPostLoadMode (ShowInterface fp)

stopBeforeMode :: Phase -> Mode
stopBeforeMode phase = mkPostLoadMode (StopBefore phase)

doEvalMode :: String -> Mode
doEvalMode str = mkPostLoadMode (DoEval [str])

mkPostLoadMode :: PostLoadMode -> Mode
mkPostLoadMode = Right . Right

isDoInteractiveMode :: Mode -> Bool
isDoInteractiveMode (Right (Right DoInteractive)) = True
isDoInteractiveMode _ = False

isStopLnMode :: Mode -> Bool
isStopLnMode (Right (Right (StopBefore StopLn))) = True
isStopLnMode _ = False

isDoMakeMode :: Mode -> Bool
isDoMakeMode (Right (Right DoMake)) = True
isDoMakeMode _ = False

#ifdef ETA_REPL
isInteractiveMode :: PostLoadMode -> Bool
isInteractiveMode DoInteractive = True
isInteractiveMode _             = False
#endif

-- isInterpretiveMode: byte-code compiler involved
isInterpretiveMode :: PostLoadMode -> Bool
isInterpretiveMode DoInteractive = True
isInterpretiveMode (DoEval _)    = True
isInterpretiveMode _             = False

needsInputsMode :: PostLoadMode -> Bool
needsInputsMode DoMkDependHS    = True
needsInputsMode (StopBefore _)  = True
needsInputsMode DoMake          = True
needsInputsMode _               = False

-- True if we are going to attempt to link in this mode.
-- (we might not actually link, depending on the GhcLink flag)
isLinkMode :: PostLoadMode -> Bool
isLinkMode (StopBefore StopLn) = True
isLinkMode DoMake              = True
isLinkMode DoInteractive       = True
isLinkMode (DoEval _)          = True
isLinkMode _                   = False

isCompManagerMode :: PostLoadMode -> Bool
isCompManagerMode DoMake        = True
isCompManagerMode DoInteractive = True
isCompManagerMode (DoEval _)    = True
isCompManagerMode _             = False

-- -----------------------------------------------------------------------------
-- Parsing the mode flag

parseModeFlags :: [Located String]
               -> IO (Mode,
                      [Located String],
                      [Located String])
parseModeFlags args = do
  let ((leftover, errs1, warns), (mModeFlag, errs2, flags')) =
          runCmdLine (processArgs modeFlags args)
                     (Nothing, [], [])
      mode = case mModeFlag of
             Nothing     -> doMakeMode
             Just (m, _) -> m

  -- See Note [Handling errors when parsing commandline flags]
  unless (null errs1 && null errs2) $ throwGhcException $ errorsToGhcException $
      map ("on the commandline", ) $ map unLoc errs1 ++ errs2

  return (mode, flags' ++ leftover, warns)

type ModeM = CmdLineP (Maybe (Mode, String), [String], [Located String])

modeFlags :: [Flag ModeM]
modeFlags =
  [  ------- help / version ----------------------------------------------
    defFlag "?"                     (PassFlag (setMode showEtaUsageMode))
  , defFlag "-help"                 (PassFlag (setMode showEtaUsageMode))
  , defFlag "V"                     (PassFlag (setMode showVersionMode))
  , defFlag "-version"              (PassFlag (setMode showVersionMode))
  , defFlag "-numeric-version"      (PassFlag (setMode showNumVersionMode))
  , defFlag "-info"                 (PassFlag (setMode showInfoMode))
  , defFlag "-show-options"         (PassFlag (setMode showOptionsMode))
  , defFlag "-supported-languages"  (PassFlag (setMode showSupportedExtensionsMode))
  , defFlag "-supported-extensions" (PassFlag (setMode showSupportedExtensionsMode))
  , defFlag "-show-packages"        (PassFlag (setMode showPackagesMode)) ]
  ++
  [ defFlag k'                      (PassFlag (setMode (printSetting k)))
  | k <- ["Project version",
          "Project Git commit id",
          "Booter version",
          "Stage",
          "Build platform",
          "Host platform",
          "Target platform",
          "Have interpreter",
          "Object splitting supported",
          "Have native code generator",
          "Support SMP",
          "Unregisterised",
          "Tables next to code",
          "RTS ways",
          "Leading underscore",
          "Debug on",
          "LibDir",
          "Global Package DB",
          "C compiler flags",
          "C compiler link flags",
          "ld flags"],
    let k' = "-print-" ++ map (replaceSpace . toLower) k
        replaceSpace ' ' = '-'
        replaceSpace c   = c
  ] ++
      ------- interfaces ----------------------------------------------------
  [ defFlag "-show-iface"  (HasArg (\f -> setMode (showInterfaceMode f)
                                               "--show-iface"))

      ------- primary modes ------------------------------------------------
  , defFlag "c"            (PassFlag (\f -> do setMode (stopBeforeMode StopLn) f
                                               addFlag "-no-link" f))
  , defFlag "M"            (PassFlag (setMode doMkDependHSMode))
  , defFlag "E"            (PassFlag (setMode (stopBeforeMode anyHsc)))
  , defFlag "C"            (PassFlag (setMode (stopBeforeMode HCc)))
  , defFlag "S"            (PassFlag (setMode (stopBeforeMode (As False))))
  , defFlag "-make"        (PassFlag (setMode doMakeMode))
  , defFlag "-interactive" (PassFlag (setMode doInteractiveMode))
  , defFlag "-abi-hash"    (PassFlag (setMode doAbiHashMode))
  , defFlag "e"            (SepArg   (\s -> setMode (doEvalMode s) "-e"))
  ]

setMode :: Mode -> String -> EwM ModeM ()
setMode newMode newFlag = liftEwM $ do
    (mModeFlag, errs, flags') <- getCmdLineState
    let (modeFlag', errs') =
            case mModeFlag of
            Nothing -> ((newMode, newFlag), errs)
            Just (oldMode, oldFlag) ->
                case (oldMode, newMode) of
                    -- -c/--make are allowed together, and mean --make -no-link
                    _ |  isStopLnMode oldMode && isDoMakeMode newMode
                      || isStopLnMode newMode && isDoMakeMode oldMode ->
                      ((doMakeMode, "--make"), [])

                    -- If we have both --help and --interactive then we
                    -- want showEtaiUsage
                    _ | isShowGhcUsageMode oldMode &&
                        isDoInteractiveMode newMode ->
                            ((showEtaiUsageMode, oldFlag), [])
                      | isShowGhcUsageMode newMode &&
                        isDoInteractiveMode oldMode ->
                            ((showEtaiUsageMode, newFlag), [])
                    -- Otherwise, --help/--version/--numeric-version always win
                      | isDominantFlag oldMode -> ((oldMode, oldFlag), [])
                      | isDominantFlag newMode -> ((newMode, newFlag), [])
                    -- We need to accumulate eval flags like "-e foo -e bar"
                    (Right (Right (DoEval esOld)),
                     Right (Right (DoEval [eNew]))) ->
                        ((Right (Right (DoEval (eNew : esOld))), oldFlag),
                         errs)
                    -- Saying e.g. --interactive --interactive is OK
                    _ | oldFlag == newFlag -> ((oldMode, oldFlag), errs)

                    -- --interactive and --show-options are used together
                    (Right (Right DoInteractive), Left (ShowOptions _)) ->
                      ((Left (ShowOptions True),
                        "--interactive --show-options"), errs)
                    (Left (ShowOptions _), Right (Right DoInteractive)) ->
                      ((Left (ShowOptions True),
                        "--show-options --interactive"), errs)
                    -- Otherwise, complain
                    _ -> let err = flagMismatchErr oldFlag newFlag
                         in ((oldMode, oldFlag), err : errs)
    putCmdLineState (Just modeFlag', errs', flags')
  where isDominantFlag f = isShowGhcUsageMode   f ||
                           isShowGhciUsageMode  f ||
                           isShowVersionMode    f ||
                           isShowNumVersionMode f

flagMismatchErr :: String -> String -> String
flagMismatchErr oldFlag newFlag
    = "cannot use `" ++ oldFlag ++  "' with `" ++ newFlag ++ "'"

addFlag :: String -> String -> EwM ModeM ()
addFlag s flag = liftEwM $ do
  (m, e, flags') <- getCmdLineState
  putCmdLineState (m, e, mkGeneralLocated loc s : flags')
    where loc = "addFlag by " ++ flag ++ " on the commandline"

-- ----------------------------------------------------------------------------
-- Run --make mode

warnHscFile :: String -> IO ()
warnHscFile f = putStrLn $ "ERROR: File " ++ f ++ " of unsupported type (.hsc)"

handleHscFiles :: [String] -> Ghc ()
handleHscFiles fs =
  if null fs
  then return ()
  else do
    liftIO $ mapM_ warnHscFile fs
    liftIO $ exitWith (ExitFailure 1)

doMake :: [(String,Maybe Phase)] -> Ghc ()
doMake srcs  = do
  hsc_env <- GHC.getSession
  handleHscFiles $ filter (".hsc" `isSuffixOf`) (map fst srcs)
  if null hs_srcs
  then liftIO (oneShot hsc_env StopLn srcs)
  else do
    o_files <- liftIO $ compileFiles hsc_env StopLn non_hs_srcs
    dflags <- GHC.getSessionDynFlags
    let dflags' = foldr addJarInputs dflags o_files
    _ <- GHC.setSessionDynFlags dflags'
    targets <- mapM (uncurry GHC.guessTarget) hs_srcs
    GHC.setTargets targets
    ok_flag <- GHC.load LoadAllTargets
    when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))
    return ()
  where (hs_srcs, non_hs_srcs) = partition haskellish srcs
        haskellish (f,Nothing) =
          looksLikeModuleName f || isHaskellUserSrcFilename f || '.' `notElem` f
        haskellish (_,Just phase) =
          phase `notElem` [ As True, As False, Cc, Cobjc, Cobjcpp, CmmCpp, Cmm, StopLn ]


-- ---------------------------------------------------------------------------
-- --show-iface mode

doShowIface :: DynFlags -> FilePath -> IO ()
doShowIface dflags file = do
  hsc_env <- newHscEnv dflags
  showIface hsc_env file

-- ---------------------------------------------------------------------------
-- Various banners and verbosity output.

showBanner :: PostLoadMode -> DynFlags -> IO ()
showBanner _postLoadMode dflags = do
   let verb = verbosity dflags

#ifdef ETA_REPL
   -- Show the GHCi banner
   when (isInteractiveMode _postLoadMode && verb >= 1) $ putStrLn etaReplWelcomeMsg
#endif

   -- Display details of the configuration in verbose mode
   when (verb >= 2) $
    do hPutStr stderr cProjectName
       hPutStr stderr ", Version "
       hPutStr stderr cProjectVersion
       hPutStr stderr " booted by GHC version "
       hPutStrLn stderr ghcProjectVersion

-- We print out a Read-friendly string, but a prettier one than the
-- Show instance gives us
showInfo :: DynFlags -> IO ()
showInfo _dflags = do
  let sq x = " [" ++ x ++ "\n ]"
  putStrLn $ sq $ intercalate "\n ," $ map show $ compilerInfo

compilerInfo :: [(String, String)]
compilerInfo = [("Project name", cProjectName),
                ("Project version", cProjectVersion),
                ("Project Git commit id", cProjectGitCommitId),
                ("Project version", cProjectVersion),
                ("LibDir", topDir),
                ("Global Package DB", topDir </> "package.conf.d")
               ] ++ map (,"YES")
                  ["Uses unit IDs"
                  ,"Support thinning and renaming package flags"
                  ,"Support parallel --make"
                  ,"Support reexported-modules"
                  ,"Uses package keys"
                  ,"Requires unified installed package IDs"]
  where topDir = unsafePerformIO (findTopDir Nothing)

showSupportedExtensions :: IO ()
showSupportedExtensions = mapM_ putStrLn supportedLanguagesAndExtensions

showVersion :: IO ()
showVersion = do
  putStrLn (cProjectName ++ ", Version " ++ cProjectVersion ++ gitHashMessage)
  where gitHash | null cProjectGitCommitId = Nothing
                | otherwise = Just cProjectGitCommitId
        gitHashMessage = maybe "" (", Git Revision " ++ ) gitHash

showOptions :: Bool -> IO ()
showOptions isInteractive = putStr (unlines availableOptions)
    where
      availableOptions = flagsForCompletion isInteractive ++
        map ('-':) (concat [
            getFlagNames modeFlags
          , filterUnwantedStatic . getFlagNames $ flagsStatic
          , flagsStaticNames
          ])
      getFlagNames = map flagName
      -- this is a hack to get rid of two unwanted entries that get listed
      -- as static flags. Hopefully this hack will disappear one day together
      -- with static flags
      filterUnwantedStatic      = filter (`notElem`["f", "fno-"])

showEtaUsage :: DynFlags -> IO ()
showEtaUsage = showUsage False

showEtaiUsage :: DynFlags -> IO ()
showEtaiUsage = showUsage True

showUsage :: Bool -> DynFlags -> IO ()
showUsage etaRepl _dflags = putStrLn usage
  where usage = if etaRepl then etaReplUsage else etaUsage

-- TODO: Make this better
etaUsage, etaReplUsage :: String
etaUsage = "Eta v" ++ cProjectVersion ++ "\n\n\
See the Eta User Guide:\n\
https://eta-lang.org/docs/\n"

-- TODO: Make this better
etaReplUsage = "Eta REPL v" ++ cProjectVersion ++ "\n\n\
See the Eta User Guide:\n\
https://eta-lang.org/docs/\n"

dumpFinalStats :: DynFlags -> IO ()
dumpFinalStats dflags =
  when (gopt Opt_D_faststring_stats dflags) $ dumpFastStringStats dflags

dumpFastStringStats :: DynFlags -> IO ()
dumpFastStringStats dflags = do
  buckets <- getFastStringTable
  let (entries, longest, has_z) = countFS 0 0 0 buckets
      msg = text "FastString stats:" $$
            nest 4 (vcat [text "size:           " <+> int (length buckets),
                          text "entries:        " <+> int entries,
                          text "longest chain:  " <+> int longest,
                          text "has z-encoding: " <+> (has_z `pcntOf` entries)
                         ])
        -- we usually get more "has z-encoding" than "z-encoded", because
        -- when we z-encode a string it might hash to the exact same string,
        -- which will is not counted as "z-encoded".  Only strings whose
        -- Z-encoding is different from the original string are counted in
        -- the "z-encoded" total.
  putMsg dflags msg
  where
   x `pcntOf` y = int ((x * 100) `quot` y) <> char '%'

countFS :: Int -> Int -> Int -> [[FastString]] -> (Int, Int, Int)
countFS entries longest has_z [] = (entries, longest, has_z)
countFS entries longest has_z (b:bs) =
  let
        len = length b
        longest' = max len longest
        entries' = entries + len
        has_zs = length (filter hasZEncoding b)
  in
        countFS entries' longest' (has_z + has_zs) bs

showPackages, dumpPackages, dumpPackagesSimple :: DynFlags -> IO ()
showPackages       dflags = putStrLn (showSDoc dflags (pprPackages dflags))
dumpPackages       dflags = putMsg dflags (pprPackages dflags)
dumpPackagesSimple dflags = putMsg dflags (pprPackagesSimple dflags)

-- -----------------------------------------------------------------------------
-- ABI hash support

{-
        ghc --abi-hash Data.Foo System.Bar

Generates a combined hash of the ABI for modules Data.Foo and
System.Bar.  The modules must already be compiled, and appropriate -i
options may be necessary in order to find the .hi files.

This is used by Cabal for generating the InstalledPackageId for a
package.  The InstalledPackageId must change when the visible ABI of
the package chagnes, so during registration Cabal calls ghc --abi-hash
to get a hash of the package's ABI.
-}

-- | Print ABI hash of input modules.
--
-- The resulting hash is the MD5 of the GHC version used (Trac #5328,
-- see 'hiVersion') and of the existing ABI hash from each module (see
-- 'mi_mod_hash').
abiHash :: [String] -- ^ List of module names
        -> Ghc ()
abiHash strs = do
  hsc_env <- getSession
  let dflags = hsc_dflags hsc_env

  liftIO $ do

  let find_it str = do
         let modname = mkModuleName str
         r <- findImportedModule hsc_env modname Nothing
         case r of
           Found _ m -> return m
           _error    -> throwGhcException $ CmdLineError $ showSDoc dflags $
                          cannotFindInterface dflags modname r

  mods <- mapM find_it strs

  let get_iface = loadUserInterface False (text "abiHash")
  ifaces <- initIfaceCheck hsc_env $ mapM get_iface mods

  bh <- openBinMem (3*1024) -- just less than a block
  put_ bh hiVersion
    -- package hashes change when the compiler version changes (for now)
    -- see #5328
  mapM_ (put_ bh . mi_mod_hash) ifaces
  f <- fingerprintBinMem bh

  putStrLn (showPpr dflags f)

-- -----------------------------------------------------------------------------
-- Util

unknownFlagsErr :: [String] -> a
unknownFlagsErr fs = throwGhcException $ UsageError $ concatMap oneError fs
  where
    oneError f =
        "unrecognised flag: " ++ f ++ "\n" ++
        (case fuzzyMatch f (nub allFlags) of
            [] -> ""
            suggs -> "did you mean one of:\n" ++ unlines (map ("  " ++) suggs))
