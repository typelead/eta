{-
-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2001-2003
--
-- Access to system tools: gcc, cp, rm etc
--
-----------------------------------------------------------------------------
-}

{-# LANGUAGE CPP, ScopedTypeVariables #-}

module ETA.Main.SysTools (
        -- Initialisation
        initSysTools,

        -- Interface to system tools
        runUnlit, runCpp, runCc, -- [Option] -> IO ()
        runJavac,
        runPp,                   -- [Option] -> IO ()
        runSplit,                -- [Option] -> IO ()
        runAs, runLink, runLibtool, -- [Option] -> IO ()
        runMkDLL,
        runWindres,
        runLlvmOpt,
        runLlvmLlc,
        runClang,
        figureLlvmVersion,
        readElfSection,

        getLinkerInfo,
        getCompilerInfo,

        linkDynLib,

        askCc,

        copy,
        copyWithHeader,

        Option(..),

        -- frameworks
        getPkgFrameworkOpts,
        getFrameworkOpts,

        findTopDir
 ) where

#include "HsVersions.h"

import ETA.BasicTypes.Module
import ETA.Main.Packages
import ETA.Utils.Outputable
import ETA.Main.FileCleanup
import ETA.Main.ErrUtils
import ETA.Utils.Panic
import ETA.Utils.Platform
import ETA.Utils.Util
import ETA.Main.Constants
import ETA.Main.DynFlags
import ETA.Utils.Exception
import qualified ETA.Utils.Exception as Exception

import Data.IORef
import Data.Maybe
import System.Exit
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error as IO
import System.Directory
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP hiding (char)
import qualified Text.ParserCombinators.ReadP as R
import System.Process
import Control.Concurrent
import ETA.Utils.FastString
import ETA.BasicTypes.SrcLoc           ( SrcLoc, mkSrcLoc, noSrcSpan, mkSrcSpan )

#ifdef mingw32_HOST_OS
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

{-
How GHC finds its files
~~~~~~~~~~~~~~~~~~~~~~~

[Note topdir]

GHC needs various support files (library packages, RTS etc), plus
various auxiliary programs (cp, gcc, etc).  It starts by finding topdir,
the root of GHC's support files

On Unix:
  - ghc always has a shell wrapper that passes a -B<dir> option

On Windows:
  - ghc never has a shell wrapper.
  - we can find the location of the ghc binary, which is
        $topdir/bin/<something>.exe
    where <something> may be "ghc", "ghc-stage2", or similar
  - we strip off the "bin/<something>.exe" to leave $topdir.

from topdir we can find package.conf, ghc-asm, etc.


SysTools.initSysProgs figures out exactly where all the auxiliary programs
are, and initialises mutable variables to make it easy to call them.
To to this, it makes use of definitions in Config.hs, which is a Haskell
file containing variables whose value is figured out by the build system.

Config.hs contains two sorts of things

  cGCC,         The *names* of the programs
  cCPP            e.g.  cGCC = gcc
  cUNLIT                cCPP = gcc -E
  etc           They do *not* include paths


  cUNLIT_DIR   The *path* to the directory containing unlit, split etc
  cSPLIT_DIR   *relative* to the root of the build tree,
                   for use when running *in-place* in a build tree (only)



---------------------------------------------
NOTES for an ALTERNATIVE scheme (i.e *not* what is currently implemented):

Another hair-brained scheme for simplifying the current tool location
nightmare in GHC: Simon originally suggested using another
configuration file along the lines of GCC's specs file - which is fine
except that it means adding code to read yet another configuration
file.  What I didn't notice is that the current package.conf is
general enough to do this:

Package
    {name = "tools",    import_dirs = [],  source_dirs = [],
     library_dirs = [], hs_libraries = [], extra_libraries = [],
     include_dirs = [], c_includes = [],   package_deps = [],
     extra_ghc_opts = ["-pgmc/usr/bin/gcc","-pgml${topdir}/bin/unlit", ... etc.],
     extra_cc_opts = [], extra_ld_opts = []}

Which would have the advantage that we get to collect together in one
place the path-specific package stuff with the path-specific tool
stuff.
                End of NOTES
---------------------------------------------

************************************************************************
*                                                                      *
\subsection{Initialisation}
*                                                                      *
************************************************************************
-}

initSysTools :: Maybe String    -- Maybe TopDir path (without the '-B' prefix)
             -> IO Settings     -- Set all the mutable variables above, holding
                                --      (a) the system programs
                                --      (b) the package-config file
                                --      (c) the GHC usage message
initSysTools mbMinusB
  = do topDir <- findTopDir mbMinusB
       tmpdir <- getTemporaryDirectory
       let platform = Platform { platformWordSize = 4
                          , platformArch = undefined
                          , platformOS = undefined
                          , platformUnregisterised = undefined
                          , platformHasGnuNonexecStack = undefined
                          , platformHasIdentDirective = undefined
                          , platformHasSubsectionsViaSymbols = undefined
                          , platformIsCrossCompiling = undefined
      }
           pkgconfig_path = topDir </> "package.conf.d"
       return $ Settings { sTargetPlatform      = platform
                         , sTmpDir              = normalise tmpdir
                         , sTopDir              = topDir
                         , sSystemPackageConfig = pkgconfig_path
                         , sProgramName         = "eta"
                         , sProjectVersion      = cProjectVersion
                         , sPgm_F               = ""
                         , sPgm_javac           = ("javac",["-verbose"])
                         , sPgm_i               = ""
                         , sOpt_L               = []
                         , sOpt_P               = []
                         , sOpt_F               = []
                         , sOpt_c               = []
                         , sOpt_a               = []
                         , sOpt_l               = []
                         , sOpt_windres         = []
                         , sOpt_lo              = []
                         , sOpt_lc              = []
                         , sOpt_javac           = []
                         , sOpt_i               = []
                         , sGhcUsagePath        = undefined
                         , sGhciUsagePath       = undefined
                         , sRawSettings         = undefined
                         , sExtraGccViaCFlags   = undefined
                         , sLdSupportsCompactUnwind = undefined
                         , sLdSupportsBuildId   = undefined
                         , sLdSupportsFilelist  = undefined
                         , sLdIsGnuLd           = undefined
                         , sPgm_L               = undefined
                         , sPgm_P               = undefined
                         , sPgm_c               = undefined
                         , sPgm_s               = undefined
                         , sPgm_a               = undefined
                         , sPgm_l               = undefined
                         , sPgm_dll             = undefined
                         , sPgm_sysman          = undefined
                         , sPgm_windres         = undefined
                         , sPgm_libtool         = undefined
                         , sPgm_readelf         = undefined
                         , sPgm_lo              = undefined
                         , sPgm_lc              = undefined
                       }

-- returns a Unix-format path (relying on getBaseDir to do so too)
findTopDir :: Maybe String -- Maybe TopDir path (without the '-B' prefix).
           -> IO String    -- TopDir (in Unix format '/' separated)
findTopDir (Just minusb) = return (normalise minusb)
findTopDir _ = do
  appdir <- getAppUserDataDirectory "eta"
  return $ appdir </> cProjectVersionNumbers

{-
************************************************************************
*                                                                      *
\subsection{Running an external program}
*                                                                      *
************************************************************************
-}

runUnlit :: DynFlags -> [Option] -> IO ()
runUnlit dflags args = do
  let prog = pgm_L dflags
      opts = getOpts dflags opt_L
  runSomething dflags "Literate pre-processor" prog
               (map Option opts ++ args)

runCpp :: DynFlags -> [Option] -> IO ()
runCpp dflags args =   do
  let (p,args0) = pgm_P dflags
      args1 = map Option (getOpts dflags opt_P)
      args2 = if gopt Opt_WarnIsError dflags
                 then [Option "-Werror"]
                 else []
  mb_env <- getGccEnv args2
  runSomethingFiltered dflags id  "C pre-processor" p
                       (args0 ++ args1 ++ args2 ++ args) mb_env

runPp :: DynFlags -> [Option] -> IO ()
runPp dflags args =   do
  let prog = pgm_F dflags
      opts = map Option (getOpts dflags opt_F)
  runSomething dflags "Haskell pre-processor" prog (args ++ opts)

runCc :: DynFlags -> [Option] -> IO ()
runCc dflags args =   do
  let (p,args0) = pgm_c dflags
      args1 = map Option (getOpts dflags opt_c)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  runSomethingResponseFile dflags cc_filter "C Compiler" p args2 mb_env
 where
  -- discard some harmless warnings from gcc that we can't turn off
  cc_filter = unlines . doFilter . lines

  {-
  gcc gives warnings in chunks like so:
      In file included from /foo/bar/baz.h:11,
                       from /foo/bar/baz2.h:22,
                       from wibble.c:33:
      /foo/flibble:14: global register variable ...
      /foo/flibble:15: warning: call-clobbered r...
  We break it up into its chunks, remove any call-clobbered register
  warnings from each chunk, and then delete any chunks that we have
  emptied of warnings.
  -}
  doFilter = unChunkWarnings . filterWarnings . chunkWarnings []
  -- We can't assume that the output will start with an "In file inc..."
  -- line, so we start off expecting a list of warnings rather than a
  -- location stack.
  chunkWarnings :: [String] -- The location stack to use for the next
                            -- list of warnings
                -> [String] -- The remaining lines to look at
                -> [([String], [String])]
  chunkWarnings loc_stack [] = [(loc_stack, [])]
  chunkWarnings loc_stack xs
      = case break loc_stack_start xs of
        (warnings, lss:xs') ->
            case span loc_start_continuation xs' of
            (lsc, xs'') ->
                (loc_stack, warnings) : chunkWarnings (lss : lsc) xs''
        _ -> [(loc_stack, xs)]

  filterWarnings :: [([String], [String])] -> [([String], [String])]
  filterWarnings [] = []
  -- If the warnings are already empty then we are probably doing
  -- something wrong, so don't delete anything
  filterWarnings ((xs, []) : zs) = (xs, []) : filterWarnings zs
  filterWarnings ((xs, ys) : zs) = case filter wantedWarning ys of
                                       [] -> filterWarnings zs
                                       ys' -> (xs, ys') : filterWarnings zs

  unChunkWarnings :: [([String], [String])] -> [String]
  unChunkWarnings [] = []
  unChunkWarnings ((xs, ys) : zs) = xs ++ ys ++ unChunkWarnings zs

  loc_stack_start        s = "In file included from " `isPrefixOf` s
  loc_start_continuation s = "                 from " `isPrefixOf` s
  wantedWarning w
   | "warning: call-clobbered register used" `isContainedIn` w = False
   | otherwise = True

-- | Runs `javac` with the given options and returns the list of files generated.
runJavac :: DynFlags -> [String] -> IO [FilePath]
runJavac dflags args = do
  wiredInPkgs <- getPackageLibJars dflags $ map toInstalledUnitId pkgs
  let (prog, args0) = pgm_javac dflags
      opts = getOpts dflags opt_javac
      classPathsAll = wiredInPkgs ++ classPaths dflags
#ifndef mingw32_HOST_OS
      classPathSep = ":"
#else
      classPathSep = ";"
#endif
      classPathFolded = intercalate classPathSep classPathsAll
      classPath = if null classPathsAll
                  then []
                  else ["-cp", classPathFolded ]
      allArgs = ["-verbose"] ++ args0 ++ classPath
                ++ ["-source", "1.7", "-target", "1.7"] ++ opts ++ args
  (exitCode, _stdout, stderr) <-
    readProcessEnvWithExitCode prog allArgs  []

  case exitCode of
    ExitSuccess -> return $ getClassOutputs stderr
    ExitFailure _ -> die . unlines . filter (not . ("[" `isPrefixOf`)) $ lines stderr
  where (pkgs, _) = break (== thisPkg)
                      [rtsUnitId, primUnitId, integerUnitId, baseUnitId]
        thisPkg = thisPackage dflags
        getClassFile str = do
          str' <- breakSubstring  "RegularFileObject[" str
          return $ init . init $ str'
        getClassOutputs str = catMaybes
                            . map getClassFile
                            . filter ( ".class]]" `isInfixOf` )
                            . filter ("RegularFileObject[" `isInfixOf `)
                            $ lines str
        breakSubstring str1 str2 = stripPrefix startStr str2
            where
                startStr = head $ filter (isSuffixOf str1) $ inits str2

isContainedIn :: String -> String -> Bool
xs `isContainedIn` ys = any (xs `isPrefixOf`) (tails ys)

askCc :: DynFlags -> [Option] -> IO String
askCc dflags args = do
  let (p,args0) = pgm_c dflags
      args1 = map Option (getOpts dflags opt_c)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  runSomethingWith dflags "gcc" p args2 $ \real_args ->
    readCreateProcessWithExitCode' (proc p real_args){ env = mb_env }

-- Similar to System.Process.readCreateProcessWithExitCode, but stderr is
-- inherited from the parent process, and output to stderr is not captured.
readCreateProcessWithExitCode'
    :: CreateProcess
    -> IO (ExitCode, String)    -- ^ stdout
readCreateProcessWithExitCode' proc = do
    (_, Just outh, _, pid) <-
        createProcess proc{ std_out = CreatePipe }

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ evaluate (length output) >> putMVar outMVar ()

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, output)

readProcessEnvWithExitCode
    :: String -- ^ program path
    -> [String] -- ^ program args
    -> [(String, String)] -- ^ environment to override
    -> IO (ExitCode, String, String) -- ^ (exit_code, stdout, stderr)
readProcessEnvWithExitCode prog args env_update = do
    current_env <- getEnvironment
    let new_env = env_update ++ [ (k, v)
                                | let overriden_keys = map fst env_update
                                , (k, v) <- current_env
                                , k `notElem` overriden_keys
                                ]
        p       = proc prog args

    (_stdin, Just stdoh, Just stdeh, pid) <-
        createProcess p{ std_out = CreatePipe
                       , std_err = CreatePipe
                       , env     = Just new_env
                       }

    outMVar <- newEmptyMVar
    errMVar <- newEmptyMVar

    _ <- forkIO $ do
        stdo <- hGetContents stdoh
        _ <- evaluate (length stdo)
        putMVar outMVar stdo

    _ <- forkIO $ do
        stde <- hGetContents stdeh
        _ <- evaluate (length stde)
        putMVar errMVar stde

    out <- takeMVar outMVar
    hClose stdoh
    err <- takeMVar errMVar
    hClose stdeh

    ex <- waitForProcess pid

    return (ex, out, err)

-- Don't let gcc localize version info string, #8825
en_locale_env :: [(String, String)]
en_locale_env = [("LANGUAGE", "en")]

-- If the -B<dir> option is set, add <dir> to PATH.  This works around
-- a bug in gcc on Windows Vista where it can't find its auxiliary
-- binaries (see bug #1110).
getGccEnv :: [Option] -> IO (Maybe [(String,String)])
getGccEnv opts =
  if null b_dirs
     then return Nothing
     else do env <- getEnvironment
             return (Just (map mangle_path env))
 where
  (b_dirs, _) = partitionWith get_b_opt opts

  get_b_opt (Option ('-':'B':dir)) = Left dir
  get_b_opt other = Right other

  mangle_path (path,paths) | map toUpper path == "PATH"
        = (path, '\"' : head b_dirs ++ "\";" ++ paths)
  mangle_path other = other

runSplit :: DynFlags -> [Option] -> IO ()
runSplit dflags args = do
  let (p,args0) = pgm_s dflags
  runSomething dflags "Splitter" p (args0++args)

runAs :: DynFlags -> [Option] -> IO ()
runAs dflags args = do
  let (p,args0) = pgm_a dflags
      args1 = map Option (getOpts dflags opt_a)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  runSomethingFiltered dflags id "Assembler" p args2 mb_env

-- | Run the LLVM Optimiser
runLlvmOpt :: DynFlags -> [Option] -> IO ()
runLlvmOpt dflags args = do
  let (p,args0) = pgm_lo dflags
      args1 = map Option (getOpts dflags opt_lo)
  runSomething dflags "LLVM Optimiser" p (args0 ++ args1 ++ args)

-- | Run the LLVM Compiler
runLlvmLlc :: DynFlags -> [Option] -> IO ()
runLlvmLlc dflags args = do
  let (p,args0) = pgm_lc dflags
      args1 = map Option (getOpts dflags opt_lc)
  runSomething dflags "LLVM Compiler" p (args0 ++ args1 ++ args)

-- | Run the clang compiler (used as an assembler for the LLVM
-- backend on OS X as LLVM doesn't support the OS X system
-- assembler)
runClang :: DynFlags -> [Option] -> IO ()
runClang dflags args = do
  -- we simply assume its available on the PATH
  let clang = "clang"
      -- be careful what options we call clang with
      -- see #5903 and #7617 for bugs caused by this.
      (_,args0) = pgm_a dflags
      args1 = map Option (getOpts dflags opt_a)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  Exception.catch (do
        runSomethingFiltered dflags id "Clang (Assembler)" clang args2 mb_env
    )
    (\(err :: SomeException) -> do
        errorMsg dflags $
            text ("Error running clang! you need clang installed to use the" ++
                "LLVM backend") $+$
            text "(or GHC tried to execute clang incorrectly)"
        throwIO err
    )

-- | Figure out which version of LLVM we are running this session
figureLlvmVersion :: DynFlags -> IO (Maybe Int)
figureLlvmVersion dflags = do
  let (pgm,opts) = pgm_lc dflags
      args = filter notNull (map showOpt opts)
      -- we grab the args even though they should be useless just in
      -- case the user is using a customised 'llc' that requires some
      -- of the options they've specified. llc doesn't care what other
      -- options are specified when '-version' is used.
      args' = args ++ ["-version"]
  ver <- catchIO (do
             (pin, pout, perr, _) <- runInteractiveProcess pgm args'
                                             Nothing Nothing
             {- > llc -version
                  Low Level Virtual Machine (http://llvm.org/):
                    llvm version 2.8 (Ubuntu 2.8-0Ubuntu1)
                    ...
             -}
             hSetBinaryMode pout False
             _     <- hGetLine pout
             vline <- hGetLine pout
             v     <- case filter isDigit vline of
                            []      -> fail "no digits!"
                            [x]     -> fail $ "only 1 digit! (" ++ show x ++ ")"
                            (x:y:_) -> return ((read [x,y]) :: Int)
             hClose pin
             hClose pout
             hClose perr
             return $ Just v
            )
            (\err -> do
                debugTraceMsg dflags 2
                    (text "Error (figuring out LLVM version):" <+>
                     text (show err))
                errorMsg dflags $ vcat
                    [ text "Warning:", nest 9 $
                          text "Couldn't figure out LLVM version!" $$
                          text "Make sure you have installed LLVM"]
                return Nothing)
  return ver

{- Note [Windows stack usage]

See: Trac #8870 (and #8834 for related info)

On Windows, occasionally we need to grow the stack. In order to do
this, we would normally just bump the stack pointer - but there's a
catch on Windows.

If the stack pointer is bumped by more than a single page, then the
pages between the initial pointer and the resulting location must be
properly committed by the Windows virtual memory subsystem. This is
only needed in the event we bump by more than one page (i.e 4097 bytes
or more).

Windows compilers solve this by emitting a call to a special function
called _chkstk, which does this committing of the pages for you.

The reason this was causing a segfault was because due to the fact the
new code generator tends to generate larger functions, we needed more
stack space in GHC itself. In the x86 codegen, we needed approximately
~12kb of stack space in one go, which caused the process to segfault,
as the intervening pages were not committed.

In the future, we should do the same thing, to make the problem
completely go away. In the mean time, we're using a workaround: we
instruct the linker to specify the generated PE as having an initial
reserved stack size of 8mb, as well as a initial *committed* stack
size of 8mb. The default committed size was previously only 4k.

Theoretically it's possible to still hit this problem if you request a
stack bump of more than 8mb in one go. But the amount of code
necessary is quite large, and 8mb "should be more than enough for
anyone" right now (he said, before millions of lines of code cried out
in terror).

-}

{- Note [Run-time linker info]

See also: Trac #5240, Trac #6063, Trac #10110

Before 'runLink', we need to be sure to get the relevant information
about the linker we're using at runtime to see if we need any extra
options. For example, GNU ld requires '--reduce-memory-overheads' and
'--hash-size=31' in order to use reasonable amounts of memory (see
trac #5240.) But this isn't supported in GNU gold.

Generally, the linker changing from what was detected at ./configure
time has always been possible using -pgml, but on Linux it can happen
'transparently' by installing packages like binutils-gold, which
change what /usr/bin/ld actually points to.

Clang vs GCC notes:

For gcc, 'gcc -Wl,--version' gives a bunch of output about how to
invoke the linker before the version information string. For 'clang',
the version information for 'ld' is all that's output. For this
reason, we typically need to slurp up all of the standard error output
and look through it.

Other notes:

We cache the LinkerInfo inside DynFlags, since clients may link
multiple times. The definition of LinkerInfo is there to avoid a
circular dependency.

-}

{- Note [ELF needed shared libs]

Some distributions change the link editor's default handling of
ELF DT_NEEDED tags to include only those shared objects that are
needed to resolve undefined symbols. For Template Haskell we need
the last temporary shared library also if it is not needed for the
currently linked temporary shared library. We specify --no-as-needed
to override the default. This flag exists in GNU ld and GNU gold.

The flag is only needed on ELF systems. On Windows (PE) and Mac OS X
(Mach-O) the flag is not needed.

-}

{- Note [Windows static libGCC]

The GCC versions being upgraded to in #10726 are configured with
dynamic linking of libgcc supported. This results in libgcc being
linked dynamically when a shared library is created.

This introduces thus an extra dependency on GCC dll that was not
needed before by shared libraries created with GHC. This is a particular
issue on Windows because you get a non-obvious error due to this missing
dependency. This dependent dll is also not commonly on your path.

For this reason using the static libgcc is preferred as it preserves
the same behaviour that existed before. There are however some very good
reasons to have the shared version as well as described on page 181 of
https://gcc.gnu.org/onlinedocs/gcc-5.2.0/gcc.pdf :

"There are several situations in which an application should use the
 shared ‘libgcc’ instead of the static version. The most common of these
 is when the application wishes to throw and catch exceptions across different
 shared libraries. In that case, each of the libraries as well as the application
 itself should use the shared ‘libgcc’. "

-}

neededLinkArgs :: LinkerInfo -> [Option]
neededLinkArgs (GnuLD o)     = o
neededLinkArgs (GnuGold o)   = o
neededLinkArgs (DarwinLD o)  = o
neededLinkArgs (SolarisLD o) = o
neededLinkArgs UnknownLD     = []

-- Grab linker info and cache it in DynFlags.
getLinkerInfo :: DynFlags -> IO LinkerInfo
getLinkerInfo dflags = do
  info <- readIORef (rtldInfo dflags)
  case info of
    Just v  -> return v
    Nothing -> do
      v <- getLinkerInfo' dflags
      writeIORef (rtldInfo dflags) (Just v)
      return v

-- See Note [Run-time linker info].
getLinkerInfo' :: DynFlags -> IO LinkerInfo
getLinkerInfo' _ = undefined

-- Grab compiler info and cache it in DynFlags.
getCompilerInfo :: DynFlags -> IO CompilerInfo
getCompilerInfo dflags = do
  info <- readIORef (rtccInfo dflags)
  case info of
    Just v  -> return v
    Nothing -> do
      v <- getCompilerInfo' dflags
      writeIORef (rtccInfo dflags) (Just v)
      return v

-- See Note [Run-time linker info].
getCompilerInfo' :: DynFlags -> IO CompilerInfo
getCompilerInfo' dflags = do
  let (pgm,_) = pgm_c dflags
      -- Try to grab the info from the process output.
      parseCompilerInfo _stdo stde _exitc
        -- Regular GCC
        | any ("gcc version" `isPrefixOf`) stde =
          return GCC
        -- Regular clang
        | any ("clang version" `isPrefixOf`) stde =
          return Clang
        -- XCode 5.1 clang
        | any ("Apple LLVM version 5.1" `isPrefixOf`) stde =
          return AppleClang51
        -- XCode 5 clang
        | any ("Apple LLVM version" `isPrefixOf`) stde =
          return AppleClang
        -- XCode 4.1 clang
        | any ("Apple clang version" `isPrefixOf`) stde =
          return AppleClang
         -- Unknown linker.
        | otherwise = fail "invalid -v output, or compiler is unsupported"

  -- Process the executable call
  info <- catchIO (do
                (exitc, stdo, stde) <-
                    readProcessEnvWithExitCode pgm ["-v"] en_locale_env
                -- Split the output by lines to make certain kinds
                -- of processing easier.
                parseCompilerInfo (lines stdo) (lines stde) exitc
            )
            (\err -> do
                debugTraceMsg dflags 2
                    (text "Error (figuring out C compiler information):" <+>
                     text (show err))
                errorMsg dflags $ hang (text "Warning:") 9 $
                  text "Couldn't figure out C compiler information!" $$
                  text "Make sure you're using GNU gcc, or clang"
                return UnknownCC)
  return info

runLink :: DynFlags -> [Option] -> IO ()
runLink _ _ = undefined

runLibtool :: DynFlags -> [Option] -> IO ()
runLibtool dflags args = do
  linkargs <- neededLinkArgs `fmap` getLinkerInfo dflags
  let args1      = map Option (getOpts dflags opt_l)
      args2      = [Option "-static"] ++ args1 ++ args ++ linkargs
      libtool    = pgm_libtool dflags
  mb_env <- getGccEnv args2
  runSomethingFiltered dflags id "Linker" libtool args2 mb_env

runMkDLL :: DynFlags -> [Option] -> IO ()
runMkDLL dflags args = do
  let (p,args0) = pgm_dll dflags
      args1 = args0 ++ args
  mb_env <- getGccEnv (args0++args)
  runSomethingFiltered dflags id "Make DLL" p args1 mb_env

runWindres :: DynFlags -> [Option] -> IO ()
runWindres dflags args = do
  let (gcc, gcc_args) = pgm_c dflags
      windres = pgm_windres dflags
      opts = map Option (getOpts dflags opt_windres)
      quote x = "\"" ++ x ++ "\""
      args' = -- If windres.exe and gcc.exe are in a directory containing
              -- spaces then windres fails to run gcc. We therefore need
              -- to tell it what command to use...
              Option ("--preprocessor=" ++
                      unwords (map quote (gcc :
                                          map showOpt gcc_args ++
                                          map showOpt opts ++
                                          ["-E", "-xc", "-DRC_INVOKED"])))
              -- ...but if we do that then if windres calls popen then
              -- it can't understand the quoting, so we have to use
              -- --use-temp-file so that it interprets it correctly.
              -- See #1828.
            : Option "--use-temp-file"
            : args
  mb_env <- getGccEnv gcc_args
  runSomethingFiltered dflags id "Windres" windres args' mb_env

copy :: DynFlags -> String -> FilePath -> FilePath -> IO ()
copy dflags purpose from to = copyWithHeader dflags purpose Nothing from to

copyWithHeader :: DynFlags -> String -> Maybe String -> FilePath -> FilePath
               -> IO ()
copyWithHeader dflags purpose maybe_header from to = do
  showPass dflags purpose

  hout <- openBinaryFile to   WriteMode
  hin  <- openBinaryFile from ReadMode
  ls <- hGetContents hin -- inefficient, but it'll do for now. ToDo: speed up
  maybe (return ()) (header hout) maybe_header
  hPutStr hout ls
  hClose hout
  hClose hin
 where
  -- write the header string in UTF-8.  The header is something like
  --   {-# LINE "foo.hs" #-}
  -- and we want to make sure a Unicode filename isn't mangled.
  header h str = do
   hSetEncoding h utf8
   hPutStr h str
   hSetBinaryMode h True

-- | read the contents of the named section in an ELF object as a
-- String.
readElfSection :: DynFlags -> String -> FilePath -> IO (Maybe String)
readElfSection dflags section exe = do
  let
     prog = pgm_readelf dflags
     args = [Option "-p", Option section, FileOption "" exe]
  --
  r <- readProcessEnvWithExitCode prog (filter notNull (map showOpt args))
                                  en_locale_env
  case r of
    (ExitSuccess, out, _err) -> return (doFilter (lines out))
    _ -> return Nothing
 where
  doFilter [] = Nothing
  doFilter (s:r) = case readP_to_S parse s of
                    [(p,"")] -> Just p
                    _r       -> doFilter r
   where parse = do
           skipSpaces
           _ <- R.char '['
           skipSpaces
           _ <- string "0]"
           skipSpaces
           munch (const True)

-----------------------------------------------------------------------------
-- Running an external program

runSomething :: DynFlags
             -> String          -- For -v message
             -> String          -- Command name (possibly a full path)
                                --      assumed already dos-ified
             -> [Option]        -- Arguments
                                --      runSomething will dos-ify them
             -> IO ()

runSomething dflags phase_name pgm args =
  runSomethingFiltered dflags id phase_name pgm args Nothing

-- | Run a command, placing the arguments in an external response file.
--
-- This command is used in order to avoid overlong command line arguments on
-- Windows. The command line arguments are first written to an external,
-- temporary response file, and then passed to the linker via @filepath.
-- response files for passing them in. See:
--
--     https://gcc.gnu.org/wiki/Response_Files
--     https://ghc.haskell.org/trac/ghc/ticket/10777
runSomethingResponseFile
  :: DynFlags -> (String->String) -> String -> String -> [Option]
  -> Maybe [(String,String)] -> IO ()

runSomethingResponseFile dflags filter_fn phase_name pgm args mb_env =
    runSomethingWith dflags phase_name pgm args $ \real_args -> do
        fp <- getResponseFile real_args
        let args = ['@':fp]
        r <- builderMainLoop dflags filter_fn pgm args mb_env
        return (r,())
  where
    getResponseFile args = do
      fp <- newTempName dflags TFL_CurrentModule "rsp"
      withFile fp WriteMode $ \h -> do
          hSetEncoding h utf8
          hPutStr h $ unlines $ map escape args
      return fp

    -- Note: Response files have backslash-escaping, double quoting, and are
    -- whitespace separated (some implementations use newline, others any
    -- whitespace character). Therefore, escape any backslashes, newlines, and
    -- double quotes in the argument, and surround the content with double
    -- quotes.
    --
    -- Another possibility that could be considered would be to convert
    -- backslashes in the argument to forward slashes. This would generally do
    -- the right thing, since backslashes in general only appear in arguments
    -- as part of file paths on Windows, and the forward slash is accepted for
    -- those. However, escaping is more reliable, in case somehow a backslash
    -- appears in a non-file.
    escape x = concat
        [ "\""
        , concatMap
            (\c ->
                case c of
                    '\\' -> "\\\\"
                    '\n' -> "\\n"
                    '\"' -> "\\\""
                    _    -> [c])
            x
        , "\""
        ]

runSomethingFiltered
  :: DynFlags -> (String->String) -> String -> String -> [Option]
  -> Maybe [(String,String)] -> IO ()

runSomethingFiltered dflags filter_fn phase_name pgm args mb_env = do
    runSomethingWith dflags phase_name pgm args $ \real_args -> do
        r <- builderMainLoop dflags filter_fn pgm real_args mb_env
        return (r,())

runSomethingWith
  :: DynFlags -> String -> String -> [Option]
  -> ([String] -> IO (ExitCode, a))
  -> IO a

runSomethingWith dflags phase_name pgm args io = do
  let real_args = filter notNull (map showOpt args)
      cmdLine = showCommandForUser pgm real_args
  traceCmd dflags phase_name cmdLine $ handleProc pgm phase_name $ io real_args

handleProc :: String -> String -> IO (ExitCode, r) -> IO r
handleProc pgm phase_name proc = do
    (rc, r) <- proc `catchIO` handler
    case rc of
      ExitSuccess{} -> return r
      ExitFailure n
        -- rawSystem returns (ExitFailure 127) if the exec failed for any
        -- reason (eg. the program doesn't exist).  This is the only clue
        -- we have, but we need to report something to the user because in
        -- the case of a missing program there will otherwise be no output
        -- at all.
       | n == 127  -> does_not_exist
       | otherwise -> throwGhcExceptionIO (PhaseFailed phase_name rc)
  where
    handler err =
       if IO.isDoesNotExistError err
          then does_not_exist
          else IO.ioError err

    does_not_exist = throwGhcExceptionIO (InstallationError ("could not execute: " ++ pgm))


builderMainLoop :: DynFlags -> (String -> String) -> FilePath
                -> [String] -> Maybe [(String, String)]
                -> IO ExitCode
builderMainLoop dflags filter_fn pgm real_args mb_env = do
  chan <- newChan
  (hStdIn, hStdOut, hStdErr, hProcess) <- runInteractiveProcess pgm real_args Nothing mb_env

  -- and run a loop piping the output from the compiler to the log_action in DynFlags
  hSetBuffering hStdOut LineBuffering
  hSetBuffering hStdErr LineBuffering
  _ <- forkIO (readerProc chan hStdOut filter_fn)
  _ <- forkIO (readerProc chan hStdErr filter_fn)
  -- we don't want to finish until 2 streams have been completed
  -- (stdout and stderr)
  -- nor until 1 exit code has been retrieved.
  rc <- loop chan hProcess (2::Integer) (1::Integer) ExitSuccess
  -- after that, we're done here.
  hClose hStdIn
  hClose hStdOut
  hClose hStdErr
  return rc
  where
    -- status starts at zero, and increments each time either
    -- a reader process gets EOF, or the build proc exits.  We wait
    -- for all of these to happen (status==3).
    -- ToDo: we should really have a contingency plan in case any of
    -- the threads dies, such as a timeout.
    loop _    _        0 0 exitcode = return exitcode
    loop chan hProcess t p exitcode = do
      mb_code <- if p > 0
                   then getProcessExitCode hProcess
                   else return Nothing
      case mb_code of
        Just code -> loop chan hProcess t (p-1) code
        Nothing
          | t > 0 -> do
              msg <- readChan chan
              case msg of
                BuildMsg msg -> do
                  log_action dflags dflags SevInfo noSrcSpan defaultUserStyle msg
                  loop chan hProcess t p exitcode
                BuildError loc msg -> do
                  log_action dflags dflags SevError (mkSrcSpan loc loc) defaultUserStyle msg
                  loop chan hProcess t p exitcode
                EOF ->
                  loop chan hProcess (t-1) p exitcode
          | otherwise -> loop chan hProcess t p exitcode

readerProc :: Chan BuildMessage -> Handle -> (String -> String) -> IO ()
readerProc chan hdl filter_fn =
    (do str <- hGetContents hdl
        loop (linesPlatform (filter_fn str)) Nothing)
    `finally`
       writeChan chan EOF
        -- ToDo: check errors more carefully
        -- ToDo: in the future, the filter should be implemented as
        -- a stream transformer.
    where
        loop []     Nothing    = return ()
        loop []     (Just err) = writeChan chan err
        loop (l:ls) in_err     =
                case in_err of
                  Just err@(BuildError srcLoc msg)
                    | leading_whitespace l -> do
                        loop ls (Just (BuildError srcLoc (msg $$ text l)))
                    | otherwise -> do
                        writeChan chan err
                        checkError l ls
                  Nothing -> do
                        checkError l ls
                  _ -> panic "readerProc/loop"

        checkError l ls
           = case parseError l of
                Nothing -> do
                    writeChan chan (BuildMsg (text l))
                    loop ls Nothing
                Just (file, lineNum, colNum, msg) -> do
                    let srcLoc = mkSrcLoc (mkFastString file) lineNum colNum
                    loop ls (Just (BuildError srcLoc (text msg)))

        leading_whitespace []    = False
        leading_whitespace (x:_) = isSpace x

parseError :: String -> Maybe (String, Int, Int, String)
parseError s0 = case breakColon s0 of
                Just (filename, s1) ->
                    case breakIntColon s1 of
                    Just (lineNum, s2) ->
                        case breakIntColon s2 of
                        Just (columnNum, s3) ->
                            Just (filename, lineNum, columnNum, s3)
                        Nothing ->
                            Just (filename, lineNum, 0, s2)
                    Nothing -> Nothing
                Nothing -> Nothing

breakColon :: String -> Maybe (String, String)
breakColon xs = case break (':' ==) xs of
                    (ys, _:zs) -> Just (ys, zs)
                    _ -> Nothing

breakIntColon :: String -> Maybe (Int, String)
breakIntColon xs = case break (':' ==) xs of
                       (ys, _:zs)
                        | not (null ys) && all isAscii ys && all isDigit ys ->
                           Just (read ys, zs)
                       _ -> Nothing

data BuildMessage
  = BuildMsg   !SDoc
  | BuildError !SrcLoc !SDoc
  | EOF

{-
************************************************************************
*                                                                      *
\subsection{Support code}
*                                                                      *
************************************************************************
-}

-----------------------------------------------------------------------------
-- Define       getBaseDir     :: IO (Maybe String)

-- getBaseDir :: IO (Maybe String)
-- #if defined(mingw32_HOST_OS)
-- -- Assuming we are running ghc, accessed by path  $(stuff)/bin/ghc.exe,
-- -- return the path $(stuff)/lib.
-- getBaseDir = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.
--   where
--     try_size size = allocaArray (fromIntegral size) $ \buf -> do
--         ret <- c_GetModuleFileName nullPtr buf size
--         case ret of
--           0 -> return Nothing
--           _ | ret < size -> fmap (Just . rootDir) $ peekCWString buf
--             | otherwise  -> try_size (size * 2)
--
--     rootDir s = case splitFileName $ normalise s of
--                 (d, ghc_exe)
--                  | lower ghc_exe `elem` ["ghc.exe",
--                                          "ghc-stage1.exe",
--                                          "ghc-stage2.exe",
--                                          "ghc-stage3.exe"] ->
--                     case splitFileName $ takeDirectory d of
--                     -- ghc is in $topdir/bin/ghc.exe
--                     (d', bin) | lower bin == "bin" -> takeDirectory d' </> "lib"
--                     _ -> fail
--                 _ -> fail
--         where fail = panic ("can't decompose ghc.exe path: " ++ show s)
--               lower = map toLower
--
-- foreign import WINDOWS_CCONV unsafe "windows.h GetModuleFileNameW"
--   c_GetModuleFileName :: Ptr () -> CWString -> Word32 -> IO Word32
-- #else
-- getBaseDir = return Nothing
-- #endif

-- Divvy up text stream into lines, taking platform dependent
-- line termination into account.
linesPlatform :: String -> [String]
#if !defined(mingw32_HOST_OS)
linesPlatform ls = lines ls
#else
linesPlatform "" = []
linesPlatform xs =
  case lineBreak xs of
    (as,xs1) -> as : linesPlatform xs1
  where
   lineBreak "" = ("","")
   lineBreak ('\r':'\n':xs) = ([],xs)
   lineBreak ('\n':xs) = ([],xs)
   lineBreak (x:xs) = let (as,bs) = lineBreak xs in (x:as,bs)

#endif

linkDynLib :: DynFlags -> [String] -> [InstalledUnitId] -> IO ()
linkDynLib _ _ _ = undefined

getPkgFrameworkOpts :: DynFlags -> Platform -> [InstalledUnitId] -> IO [String]
getPkgFrameworkOpts dflags platform dep_packages
  | platformUsesFrameworks platform = do
    pkg_framework_path_opts <- do
        pkg_framework_paths <- getPackageFrameworkPath dflags dep_packages
        return $ map ("-F" ++) pkg_framework_paths

    pkg_framework_opts <- do
        pkg_frameworks <- getPackageFrameworks dflags dep_packages
        return $ concat [ ["-framework", fw] | fw <- pkg_frameworks ]

    return (pkg_framework_path_opts ++ pkg_framework_opts)

  | otherwise = return []

getFrameworkOpts :: DynFlags -> Platform -> [String]
getFrameworkOpts dflags platform
  | platformUsesFrameworks platform = framework_path_opts ++ framework_opts
  | otherwise = []
  where
    framework_paths     = frameworkPaths dflags
    framework_path_opts = map ("-F" ++) framework_paths

    frameworks     = cmdlineFrameworks dflags
    -- reverse because they're added in reverse order from the cmd line:
    framework_opts = concat [ ["-framework", fw]
                            | fw <- reverse frameworks ]
