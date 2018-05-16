{-# LANGUAGE CPP, NamedFieldPuns, NondecreasingIndentation, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

-----------------------------------------------------------------------------
--
-- GHC Driver
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module ETA.Main.DriverPipeline (
        -- Run a series of compilation steps in a pipeline, for a
        -- collection of source files.
   oneShot,

        -- Interfaces for the compilation manager (interpreted/batch-mode)
   preprocess,
   compileOne, compileOne',
   compileFiles,
   link,

        -- Exports for hooks to override runPhase and link
   PhasePlus(..), CompPipeline(..), PipeEnv(..), PipeState(..),
   phaseOutputFilename, getPipeState, getPipeEnv,
   hscPostBackendPhase, getLocation, setModLocation, setDynFlags,
   runPhase, jarFileName,
   linkingNeeded, writeInterfaceOnlyMode,
   compressionMethod
  ) where

-- import ETA.Core.CoreSyn (CoreProgram)
-- import ETA.StgSyn.StgSyn (StgBinding)
-- import ETA.Profiling.CostCentre (CollectedCCs)
-- import ETA.SimplStg.SimplStg         ( stg2stg )
-- import ETA.StgSyn.CoreToStg        ( coreToStg )
-- import ETA.Core.CorePrep         ( corePrepPgm )
import ETA.Main.SysTools
import ETA.Main.Constants
import qualified ETA.Main.SysTools as SysTools
-- import ETA.Types.TyCon ( isDataTyCon )
-- import ETA.BasicTypes.NameEnv

-- import ETA.CodeGen.Main
import ETA.CodeGen.Name
import ETA.Debug
import ETA.CodeGen.Rts
-- import ETA.Parser.Parse
import ETA.Utils.JAR
import ETA.Util
import Codec.JVM
import ETA.Main.FileCleanup
import ETA.Iface.IfaceSyn
import ETA.Prelude.ForeignCall
import ETA.Main.PipelineMonad
import ETA.Main.Packages
import ETA.Main.HeaderInfo
import ETA.Main.DriverPhases
import ETA.Main.HscMain
import ETA.Main.Finder
import ETA.Main.HscTypes hiding ( Hsc )
-- import ETA.Utils.Outputable   hiding ((<>))
import ETA.BasicTypes.Module
import ETA.Utils.UniqFM           ( eltsUFM )
import ETA.Main.ErrUtils
import ETA.Main.DynFlags
import ETA.Utils.Panic
import ETA.Utils.Util
import ETA.Utils.StringBuffer     ( hGetStringBuffer )
import ETA.BasicTypes.BasicTypes       ( SuccessFlag(..) )
-- import ETA.Utils.Maybes           ( expectJust )
import ETA.BasicTypes.OccName
import ETA.BasicTypes.SrcLoc
import ETA.Utils.FastString
-- import LlvmCodeGen      ( llvmFixupAsm )
import ETA.Utils.MonadUtils
-- import ETA.Utils.Platform
import ETA.TypeCheck.TcRnTypes
import ETA.Main.Hooks

import ETA.Utils.Exception
import ETA.Utils.Fingerprint
-- import qualified ETA.Utils.Exception as Exception
-- import Data.IORef       ( readIORef )
import System.Directory
import System.FilePath
-- import System.IO
import System.PosixCompat.Files (fileExist, touchFile)
import Control.Monad hiding (void)
import Data.Foldable    (fold)
import Data.List        ( partition, nub, union , (\\) )
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Int
-- import System.Environment
-- import Data.Char
-- import Data.List (isPrefixOf)
import Control.Arrow((&&&))
import Data.ByteString (ByteString)
import Data.Time
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Language.Preprocessor.Unlit
import qualified Hpp.CmdLine as Hpp
import qualified Hpp.Types   as Hpp

#include "HsVersions.h"

-- ---------------------------------------------------------------------------
-- Pre-process

-- | Just preprocess a file, put the result in a temp. file (used by the
-- compilation manager during the summary phase).
--
-- We return the augmented DynFlags, because they contain the result
-- of slurping in the OPTIONS pragmas

preprocess :: HscEnv
           -> (FilePath, Maybe Phase) -- ^ filename and starting phase
           -> IO (DynFlags, FilePath)
preprocess hsc_env (filename, mb_phase) =
  ASSERT2(isJust mb_phase || isHaskellSrcFilename filename, text filename)
  runPipeline anyHsc hsc_env (filename, fmap RealPhase mb_phase)
        Nothing Temporary Nothing{-no ModLocation-} Nothing{-no stub-}

-- ---------------------------------------------------------------------------

-- | Compile
--
-- Compile a single module, under the control of the compilation manager.
--
-- This is the interface between the compilation manager and the
-- compiler proper (hsc), where we deal with tedious details like
-- reading the OPTIONS pragma from the source file, converting the
-- C or assembly that GHC produces into an object file, and compiling
-- FFI stub files.
--
-- NB.  No old interface can also mean that the source has changed.

compileOne :: HscEnv
           -> ModSummary      -- ^ summary for module being compiled
           -> Int             -- ^ module N ...
           -> Int             -- ^ ... of M
           -> Maybe ModIface  -- ^ old interface, if we have one
           -> Maybe Linkable  -- ^ old linkable, if we have one
           -> SourceModified
           -> IO HomeModInfo   -- ^ the complete HomeModInfo, if successful

compileOne = compileOne' Nothing (Just batchMsg)

compileOne' :: Maybe TcGblEnv
            -> Maybe Messager
            -> HscEnv
            -> ModSummary      -- ^ summary for module being compiled
            -> Int             -- ^ module N ...
            -> Int             -- ^ ... of M
            -> Maybe ModIface  -- ^ old interface, if we have one
            -> Maybe Linkable  -- ^ old linkable, if we have one
            -> SourceModified
            -> IO HomeModInfo   -- ^ the complete HomeModInfo, if successful

compileOne' m_tc_result mHscMessage
            hsc_env0 summary mod_index nmods mb_old_iface maybe_old_linkable
            source_modified0
 = do
   let dflags0     = ms_hspp_opts summary
       this_mod    = ms_mod summary
       src_flavour = ms_hsc_src summary
       location    = ms_location summary
       input_fn    = expectJust "compile:hs" (ml_hs_file location)
       input_fnpp  = ms_hspp_file summary
       mod_graph   = hsc_mod_graph hsc_env0
       needsTH     = any (xopt Opt_TemplateHaskell . ms_hspp_opts) mod_graph
       needsQQ     = any (xopt Opt_QuasiQuotes     . ms_hspp_opts) mod_graph
       needsLinker = needsTH || needsQQ
       isDynWay    = any (== WayDyn) (ways dflags0)
       isProfWay   = any (== WayProf) (ways dflags0)
   -- #8180 - when using TemplateHaskell, switch on -dynamic-too so
   -- the linker can correctly load the object files.
   let dflags1 = if needsLinker && dynamicGhc && not isDynWay && not isProfWay
                  then gopt_set dflags0 Opt_BuildDynamicToo
                  else dflags0

   debugTraceMsg dflags1 2 (text "compile: input file" <+> text input_fnpp)

   let basename = dropExtension input_fn

  -- We add the directory in which the .hs files resides) to the import path.
  -- This is needed when we try to compile the .hc file later, if it
  -- imports a _stub.h file that we created here.
   let current_dir = takeDirectory basename
       old_paths   = includePaths dflags1
       dflags      = dflags1 { includePaths = current_dir : old_paths }
       hsc_env     = hsc_env0 {hsc_dflags = dflags}

   -- Figure out what lang we're generating
   let hsc_lang = hscTarget dflags
   -- ... and what the next phase should be
   let next_phase = hscPostBackendPhase dflags src_flavour hsc_lang
   -- ... and what file to generate the output into
   output_fn <- getOutputFilename next_phase
                        Temporary basename dflags next_phase (Just location)

   -- -fforce-recomp should also work with --make
   let force_recomp = gopt Opt_ForceRecomp dflags
       source_modified
         | force_recomp = SourceModified
         | otherwise = source_modified0
       object_filename = ml_obj_file location

   let always_do_basic_recompilation_check = case hsc_lang of
                                             HscInterpreted -> True
                                             _ -> False

   e <- genericHscCompileGetFrontendResult
            always_do_basic_recompilation_check
            m_tc_result mHscMessage
            hsc_env summary source_modified mb_old_iface (mod_index, nmods)

   case e of
       Left iface ->
           do details <- genModDetails hsc_env iface
              MASSERT(isJust maybe_old_linkable)
              return (HomeModInfo{ hm_details  = details,
                                   hm_iface    = iface,
                                   hm_linkable = maybe_old_linkable })

       Right (tc_result, mb_old_hash) ->
           -- run the compiler
           case hsc_lang of
               HscInterpreted ->
                   case ms_hsc_src summary of
                   t | isHsBootOrSig t ->
                       do (iface, _changed, details) <- hscSimpleIface hsc_env tc_result mb_old_hash
                          return (HomeModInfo{ hm_details  = details,
                                               hm_iface    = iface,
                                               hm_linkable = maybe_old_linkable })
                   _ -> do guts0 <- hscDesugar hsc_env summary tc_result
                           guts <- hscSimplify hsc_env guts0
                           (iface, _changed, details, cgguts) <- hscNormalIface hsc_env guts mb_old_hash
                           (hasStub, comp_bc, modBreaks) <- hscInteractive hsc_env cgguts summary

                           stub_o <- case hasStub of
                                     Nothing -> return []
                                     Just stub_c -> do
                                         stub_o <- compileStub hsc_env stub_c
                                         return [DotO stub_o]

                           let hs_unlinked = [BCOs comp_bc modBreaks]
                               unlinked_time = ms_hs_date summary
                             -- Why do we use the timestamp of the source file here,
                             -- rather than the current time?  This works better in
                             -- the case where the local clock is out of sync
                             -- with the filesystem's clock.  It's just as accurate:
                             -- if the source is modified, then the linkable will
                             -- be out of date.
                           let linkable = LM unlinked_time this_mod
                                          (hs_unlinked ++ stub_o)

                           return (HomeModInfo{ hm_details  = details,
                                                hm_iface    = iface,
                                                hm_linkable = Just linkable })
               HscNothing ->
                   do (iface, changed, details) <- hscSimpleIface hsc_env tc_result mb_old_hash
                      when (gopt Opt_WriteInterface dflags) $
                         hscWriteIface dflags iface changed summary
                      let linkable = if isHsBootOrSig src_flavour
                                     then maybe_old_linkable
                                     else Just (LM (ms_hs_date summary) this_mod [])
                      return (HomeModInfo{ hm_details  = details,
                                           hm_iface    = iface,
                                           hm_linkable = linkable })

               _ ->
                   case ms_hsc_src summary of
                   HsBootFile ->
                       do (iface, changed, details) <- hscSimpleIface hsc_env tc_result mb_old_hash
                          hscWriteIface dflags iface changed summary
                          touchObjectFile dflags object_filename
                          return (HomeModInfo{ hm_details  = details,
                                               hm_iface    = iface,
                                               hm_linkable = maybe_old_linkable })

                   HsigFile ->
                       do (iface, changed, details) <-
                                    hscSimpleIface hsc_env tc_result mb_old_hash
                          hscWriteIface dflags iface changed summary

                          -- #10660: Use the pipeline instead of calling
                          -- compileEmptyStub directly, so -dynamic-too gets
                          -- handled properly
                          let mod_name = ms_mod_name summary
                          _ <- runPipeline StopLn hsc_env
                                            (output_fn,
                                             Just (HscOut src_flavour mod_name HscUpdateSig))
                                            (Just basename)
                                            Persistent
                                            (Just location)
                                            Nothing

                          -- Same as Hs
                          o_time <- getModificationUTCTime object_filename
                          let linkable =
                                  LM o_time this_mod [DotO object_filename]

                          return (HomeModInfo{ hm_details  = details,
                                               hm_iface    = iface,
                                               hm_linkable = Just linkable })

                   HsSrcFile ->
                        do guts0 <- hscDesugar hsc_env summary tc_result
                           guts <- hscSimplify hsc_env guts0
                           (iface, changed, details, cgguts) <- hscNormalIface hsc_env guts mb_old_hash
                           hscWriteIface dflags iface changed summary

                           -- We're in --make mode: finish the compilation pipeline.
                           let mod_name = ms_mod_name summary
                           _ <- runPipeline StopLn hsc_env
                                             (output_fn,
                                              Just (HscOut src_flavour mod_name (HscRecomp cgguts summary)))
                                             (Just basename)
                                             Persistent
                                             (Just location)
                                             Nothing
                                 -- The object filename comes from the ModLocation
                           o_time <- getModificationUTCTime object_filename
                           let linkable = LM o_time this_mod [DotO object_filename]

                           return (HomeModInfo{ hm_details  = details,
                                                hm_iface    = iface,
                                                hm_linkable = Just linkable })

-----------------------------------------------------------------------------
-- stub .h and .c files (for foreign export support)

-- The _stub.c file is derived from the haskell source file, possibly taking
-- into account the -stubdir option.
--
-- The object file created by compiling the _stub.c file is put into a
-- temporary file, which will be later combined with the main .o file
-- (see the MergeStubs phase).

compileStub :: HscEnv -> FilePath -> IO FilePath
compileStub hsc_env stub_c = do
        (_, stub_o) <- runPipeline StopLn hsc_env (stub_c,Nothing)  Nothing
                                   Temporary Nothing{-no ModLocation-} Nothing

        return stub_o

compileEmptyStub :: DynFlags -> HscEnv -> FilePath -> ModLocation -> IO ()
compileEmptyStub dflags hsc_env basename location = do
  -- To maintain the invariant that every Haskell file
  -- compiles to object code, we make an empty (but
  -- valid) stub object file for signatures
  empty_stub <- newTempName dflags TFL_CurrentModule "c"
  writeFile empty_stub ""
  _ <- runPipeline StopLn hsc_env
                  (empty_stub, Nothing)
                  (Just basename)
                  Persistent
                  (Just location)
                  Nothing
  return ()

-- ---------------------------------------------------------------------------
-- Link

link :: GhcLink                 -- interactive or batch
     -> DynFlags                -- dynamic flags
     -> Bool                    -- attempt linking in batch mode?
     -> HomePackageTable        -- what to link
     -> IO SuccessFlag

link LinkInMemory _ _ _ = error "LinkInMemory not implemented yet."
link NoLink _ _ _ = return Succeeded
link _ dflags batchAttemptLinking hpt
  | batchAttemptLinking
  = do
      let homeModInfos = eltsUFM hpt
          pkgDeps = concatMap ( map fst
                              . dep_pkgs
                              . mi_deps
                              . hm_iface ) homeModInfos
          ffiMappings = pkgFFIMappings homeModInfos
          linkables = map (expectJust "link" . hm_linkable) homeModInfos
      debugTraceMsg dflags 3 (text "link: linkables are ..." $$ vcat (map ppr linkables))
      if isNoLink (ghcLink dflags) then do
        debugTraceMsg dflags 3
          (text "link(batch): linking omitted (-c flag given).")
        return Succeeded
      else do
        let jarFiles = linkablesToJars linkables
            jarFile = jarFileName dflags
        shouldLink <- linkingNeeded dflags linkables pkgDeps
        if not (gopt Opt_ForceRecomp dflags) && not shouldLink then do
          debugTraceMsg dflags 2
            (text jarFile <+>
             (str "is up to date, linking not required."))
          return Succeeded
        else do
          when (verbosity dflags > 1) $
            compilationProgressMsg dflags ("Linking " ++ jarFile ++ " ...")
          linkGeneric dflags jarFiles pkgDeps
          genFFIMapFile dflags ffiMappings
          debugTraceMsg dflags 3 (text "link: done")
          let numModules = length homeModInfos
              modulesMsg
                | numModules > 1 = show numModules ++ " modules"
                | otherwise = show numModules ++ " module"
          randomSource <- fmap round getPOSIXTime
          let encMessage = getEncouragingMessage randomSource
          compilationProgressMsg dflags $ "\ESC[1m\ESC[32m\x2713 Successfully built "
            ++ modulesMsg ++ ". " ++ encMessage ++ "\ESC[0m\n"
          return Succeeded
  | otherwise
  = do debugTraceMsg dflags 3
         (text "link(batch): upsweep (partially) failed OR" $$
          text "   Main.main not exported; not linking.")
       return Succeeded

getEncouragingMessage :: Int64 -> String
getEncouragingMessage randomSource =
  messages !! fromIntegral (randomSource `rem` (fromIntegral (length messages)))
  where messages = ["Great Hustle!"
                   ,"Happy Hacking!"
                   ,"Way To Go!"
                   ,"Keep Hustling!"
                   ,"Mission Accomplished!"]

linkablesToJars :: [Linkable] -> [FilePath]
linkablesToJars ls = concatMap getOfiles ls
  where getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)

data FFIMapping =
  FFIMapping {
    fmFCQN     :: String,
    fmDataName :: String,
    fmModule   :: String
  }

-- Generate FFI Mapping File
pkgFFIMappings :: [HomeModInfo] -> [FFIMapping]
pkgFFIMappings homeModInfos = concatMap modFFIMappings homeModInfos
  where modFFIMappings hm = catMaybes $ map (getJWTMapping modName) ifaceDecls
          where iface      = hm_iface hm
                modName    = moduleNameString . moduleName $ mi_module iface
                ifaceDecls = map snd $ mi_decls iface

getJWTMapping :: String -> IfaceDecl -> Maybe FFIMapping
getJWTMapping modName (IfaceData { ifName, ifCType })
  | Just (CType _ _ fs) <- ifCType
  = Just $ FFIMapping {
      fmFCQN     = unpackFS fs,
      fmDataName = occNameString ifName,
      fmModule   = modName
    }
getJWTMapping _ _ = Nothing

genFFIMapFile :: DynFlags -> [FFIMapping] -> IO ()
genFFIMapFile dflags ffiMappings =
  unless (null contents) $
    writeFile outputFile contents
  where serializeMapping FFIMapping { fmFCQN, fmDataName, fmModule } =
          fmFCQN ++ "," ++ fmDataName ++ "," ++ fmModule
        contents = unlines $ map serializeMapping ffiMappings
        outputFile = ffiMapFileName dflags

-- -----------------------------------------------------------------------------
-- Compile files in one-shot mode.

compileFiles :: HscEnv -> Phase -> [(String, Maybe Phase)] -> IO [FilePath]
compileFiles hsc_env stop_phase srcs = do
  o_files' <- mapM (compileFile hsc_env stop_phase) non_java_srcs
  let dflags' = foldr addClassPaths (hsc_dflags hsc_env)
              $ class_dirs
              ++ o_files'
      classesDir = fromMaybe "." (objectDir dflags') </> "classes"
  debugTraceMsg dflags' 3 (text $ "compileFiles : start")
  genClassPaths <- if (not (null java_source_srcs))
                   then do
                     createDirectoryIfMissing True classesDir
                     runJavac dflags' $ ["-d", classesDir] ++ java_source_srcs
                   else return []
  extras <- getExtrasFileName dflags'
  let classes = genClassPaths ++ java_class_srcs
  when (not (null classes)) $ do
    debugTraceMsg dflags' 3 (text $ "compileFiles : generating "
                                  ++ extras ++ " with " ++ show classes)
    createJar dflags' extras classes
  let o_files'' = (if null classes then [] else [extras]) ++ o_files'
  debugTraceMsg dflags' 3 (text $ "compileFiles : returning "++ show o_files'')
  return o_files''
  where (java_srcs, non_java_srcs) = partition (isJavaishFilename . fst) srcs
        (java_class_srcs, java_source_srcs) = partition isJavaClassishFilename
                                              $ map fst java_srcs
        class_dirs = map takeDirectory java_class_srcs

getExtrasFileName :: DynFlags -> IO FilePath
getExtrasFileName dflags =
  getOutputFilename StopLn Persistent "__extras" dflags StopLn Nothing

createJar :: DynFlags -> FilePath -> [FilePath] -> IO ()
createJar dflags outputFile classes = do
  -- createEmptyJar outputFile
  pathContents <- forM classes $ \classFile -> do
    contents <- BL.readFile classFile
    let internalPath = classFileCls contents
    relPath <- mkPath $ internalPath ++ ".class"
    return (relPath, BL.toStrict contents)
  addMultiByteStringsToJar' outputFile (compressionMethod dflags) pathContents

oneShot :: HscEnv -> Phase -> [(String, Maybe Phase)] -> IO ()
oneShot hsc_env stop_phase srcs = do
  o_files <- compileFiles hsc_env stop_phase srcs
  doLink (hsc_dflags hsc_env) stop_phase o_files

compileFile :: HscEnv -> Phase -> (FilePath, Maybe Phase) -> IO FilePath
compileFile hsc_env stop_phase (src, mb_phase) = do
   exists <- doesFileExist src
   when (not exists) $
        throwGhcExceptionIO (CmdLineError ("does not exist: " ++ src))

   let
        dflags    = hsc_dflags hsc_env
        split     = gopt Opt_SplitObjs dflags
        mb_o_file = outputFile dflags
        ghc_link  = ghcLink dflags      -- Set by -c or -no-link

        -- When linking, the -o argument refers to the linker's output.
        -- otherwise, we use it as the name for the pipeline's output.
        output
         -- If we are dong -fno-code, then act as if the output is
         -- 'Temporary'. This stops GHC trying to copy files to their
         -- final location.
         | HscNothing <- hscTarget dflags = Temporary
         | StopLn <- stop_phase, not (isNoLink ghc_link) = Persistent
                -- -o foo applies to linker
         | isJust mb_o_file = SpecificFile
                -- -o foo applies to the file we are compiling now
         | otherwise = Persistent

        stop_phase' = case stop_phase of
                        As _ | split -> SplitAs
                        _            -> stop_phase

   ( _, out_file) <- runPipeline stop_phase' hsc_env
                            (src, fmap RealPhase mb_phase) Nothing output
                            Nothing{-no ModLocation-} Nothing
   return out_file


doLink :: DynFlags -> Phase -> [FilePath] -> IO ()
doLink dflags stop_phase o_files
  | not (isStopLn stop_phase)
  = return ()           -- We stopped before the linking phase

  | NoLink <- ghcLink dflags
  = return ()
  | LinkInMemory <- ghcLink dflags
  = return ()
  | otherwise
  = linkGeneric dflags o_files []

-- ---------------------------------------------------------------------------

-- | Run a compilation pipeline, consisting of multiple phases.
--
-- This is the interface to the compilation pipeline, which runs
-- a series of compilation steps on a single source file, specifying
-- at which stage to stop.
--
-- The DynFlags can be modified by phases in the pipeline (eg. by
-- OPTIONS_GHC pragmas), and the changes affect later phases in the
-- pipeline.
runPipeline :: Phase                      -- ^ When to stop
  -> HscEnv                     -- ^ Compilation environment
  -> (FilePath,Maybe PhasePlus) -- ^ Input filename (and maybe -x suffix)
  -> Maybe FilePath             -- ^ original basename (if different from ^^^)
  -> PipelineOutput             -- ^ Output filename
  -> Maybe ModLocation          -- ^ A ModLocation, if this is a Haskell module
  -> Maybe FilePath             -- ^ stub object, if we have one
  -> IO (DynFlags, FilePath)    -- ^ (final flags, output filename)
runPipeline stop_phase hsc_env0 (input_fn, mb_phase)
             mb_basename output maybe_loc maybe_stub_o

    = do let
             dflags0 = hsc_dflags hsc_env0

             -- Decide where dump files should go based on the pipeline output
             dflags = dflags0 { dumpPrefix = Just (basename ++ ".") }
             hsc_env = hsc_env0 {hsc_dflags = dflags}

             (input_basename, suffix) = splitExtension input_fn
             suffix' = drop 1 suffix -- strip off the .
             basename | Just b <- mb_basename = b
                      | otherwise             = input_basename

             -- If we were given a -x flag, then use that phase to start from
             start_phase = fromMaybe (RealPhase (startPhase suffix')) mb_phase

             isHaskell (RealPhase (Unlit _)) = True
             isHaskell (RealPhase (Cpp   _)) = True
             isHaskell (RealPhase (HsPp  _)) = True
             isHaskell (RealPhase (Hsc   _)) = True
             isHaskell (HscOut {})           = True
             isHaskell _                     = False

             isHaskellishFile = isHaskell start_phase

             env = PipeEnv{ pe_isHaskellishFile = isHaskellishFile,
                            stop_phase,
                            src_filename = input_fn,
                            src_basename = basename,
                            src_suffix = suffix',
                            output_spec = output }

         -- We want to catch cases of "you can't get there from here" before
         -- we start the pipeline, because otherwise it will just run off the
         -- end.
         --
         -- There is a partial ordering on phases, where A < B iff A occurs
         -- before B in a normal compilation pipeline.

         let happensBefore' = happensBefore dflags
         case start_phase of
             RealPhase start_phase'
               | start_phase' == StopLn -> return (dflags, input_fn)
               | not (start_phase' `happensBefore'` stop_phase) ->
                 throwGhcExceptionIO $
                   UsageError $ "cannot compile this file to desired target: " ++ input_fn
             _ -> do
               debugTraceMsg dflags 4 (text "Running the pipeline")
               runPipeline' start_phase hsc_env env input_fn maybe_loc maybe_stub_o

         -- If we are compiling a Haskell module, and doing
         -- -dynamic-too, but couldn't do the -dynamic-too fast
         -- path, then rerun the pipeline for the dyn way
         -- let dflags = extractDynFlags hsc_env
         -- -- NB: Currently disabled on Windows (ref #7134, #8228, and #5987)
         -- when (not $ platformOS (targetPlatform dflags) == OSMinGW32) $ do
         --   when isHaskellishFile $ whenCannotGenerateDynamicToo dflags $ do
         --       debugTraceMsg dflags 4
         --           (text "Running the pipeline again for -dynamic-too")
         --       let dflags' = dynamicTooMkDynamicDynFlags dflags
         --       hsc_env' <- newHscEnv dflags'
         --       _ <- runPipeline' start_phase hsc_env' env input_fn
         --                         maybe_loc maybe_stub_o
         --       return ()
         -- return r

runPipeline'
  :: PhasePlus                  -- ^ When to start
  -> HscEnv                     -- ^ Compilation environment
  -> PipeEnv
  -> FilePath                   -- ^ Input filename
  -> Maybe ModLocation          -- ^ A ModLocation, if this is a Haskell module
  -> Maybe FilePath             -- ^ stub object, if we have one
  -> IO (DynFlags, FilePath)    -- ^ (final flags, output filename)
runPipeline' start_phase hsc_env env input_fn
             maybe_loc maybe_stub_o
  = do
  -- Execute the pipeline...
  let state = PipeState{ hsc_env, maybe_loc, maybe_stub_o = maybe_stub_o }

  evalP (pipeLoop start_phase input_fn) env state

-- ---------------------------------------------------------------------------
-- outer pipeline loop

-- | pipeLoop runs phases until we reach the stop phase
pipeLoop :: PhasePlus -> FilePath -> CompPipeline (DynFlags, FilePath)
pipeLoop phase input_fn = do
  env <- getPipeEnv
  dflags <- getDynFlags
  let happensBefore' = happensBefore dflags
      stopPhase = stop_phase env
  case phase of
   RealPhase realPhase | realPhase `eqPhase` stopPhase            -- All done
     -> -- Sometimes, a compilation phase doesn't actually generate any output
        -- (eg. the CPP phase when -fcpp is not turned on).  If we end on this
        -- stage, but we wanted to keep the output, then we have to explicitly
        -- copy the file, remembering to prepend a {-# LINE #-} pragma so that
        -- further compilation stages can tell what the original filename was.
        case output_spec env of
        Temporary ->
            return (dflags, input_fn)
        output ->
            do pst <- getPipeState
               final_fn <- liftIO $ getOutputFilename
                                        stopPhase output (src_basename env)
                                        dflags stopPhase (maybe_loc pst)
               when (final_fn /= input_fn) $ do
                  let msg = ("Copying `" ++ input_fn ++"' to `" ++ final_fn ++ "'")
                      line_prag = Just ("{-# LINE 1 \"" ++ src_filename env ++ "\" #-}\n")
                  liftIO $ copyWithHeader dflags msg line_prag input_fn final_fn
               return (dflags, final_fn)


     | not (realPhase `happensBefore'` stopPhase)
        -- Something has gone wrong.  We'll try to cover all the cases when
        -- this could happen, so if we reach here it is a panic.
        -- eg. it might happen if the -C flag is used on a source file that
        -- has {-# OPTIONS -fasm #-}.
     -> panic ("pipeLoop: at phase " ++ show realPhase ++
           " but I wanted to stop at phase " ++ show stopPhase)

   _
     -> do liftIO $ debugTraceMsg dflags 4
                                  (ptext (sLit "Running phase") <+> ppr phase)
           (next_phase, output_fn) <- runHookedPhase phase input_fn dflags
           r <- pipeLoop next_phase output_fn
           case phase of
               HscOut {} ->
                   whenGeneratingDynamicToo dflags $ do
                       setDynFlags $ dynamicTooMkDynamicDynFlags dflags
                       -- TODO shouldn't ignore result:
                       _ <- pipeLoop phase input_fn
                       return ()
               _ ->
                   return ()
           return r

runHookedPhase :: PhasePlus -> FilePath -> DynFlags
               -> CompPipeline (PhasePlus, FilePath)
runHookedPhase pp input dflags =
  lookupHook runPhaseHook runPhase dflags pp input dflags

-- -----------------------------------------------------------------------------
-- In each phase, we need to know into what filename to generate the
-- output.  All the logic about which filenames we generate output
-- into is embodied in the following function.

phaseOutputFilename :: Phase{-next phase-} -> CompPipeline FilePath
phaseOutputFilename next_phase = do
  PipeEnv{stop_phase, src_basename, output_spec} <- getPipeEnv
  PipeState{maybe_loc, hsc_env} <- getPipeState
  let dflags = hsc_dflags hsc_env
  liftIO $ getOutputFilename stop_phase output_spec
                             src_basename dflags next_phase maybe_loc

getOutputFilename
  :: Phase -> PipelineOutput -> String
  -> DynFlags -> Phase{-next phase-} -> Maybe ModLocation -> IO FilePath
getOutputFilename stop_phase output basename dflags next_phase maybe_location
 | is_last_phase, Persistent   <- output = persistent_fn
 | is_last_phase, SpecificFile <- output = case outputFile dflags of
                                           Just f -> return f
                                           Nothing ->
                                               panic "SpecificFile: No filename"
 | keep_this_output                      = persistent_fn
 | otherwise                             = newTempName dflags TFL_CurrentModule suffix
    where
          hcsuf      = hcSuf dflags
          odir       = objectDir dflags
          osuf       = objectSuf dflags
          keep_hc    = gopt Opt_KeepHcFiles dflags
          keep_s     = gopt Opt_KeepSFiles dflags
          keep_bc    = gopt Opt_KeepLlvmFiles dflags

          myPhaseInputExt HCc       = hcsuf
          myPhaseInputExt MergeStub = osuf
          myPhaseInputExt StopLn    = osuf
          myPhaseInputExt other     = phaseInputExt other

          is_last_phase = next_phase `eqPhase` stop_phase

          -- sometimes, we keep output from intermediate stages
          keep_this_output =
               case next_phase of
                       As _    | keep_s     -> True
                       LlvmOpt | keep_bc    -> True
                       HCc     | keep_hc    -> True
                       _other               -> False

          suffix = myPhaseInputExt next_phase

          -- persistent object files get put in odir
          persistent_fn
             | StopLn <- next_phase = return odir_persistent
             | otherwise            = return persistent

          persistent = basename <.> suffix

          odir_persistent
             | Just loc <- maybe_location = ml_obj_file loc
             | Just d <- odir = d </> persistent
             | otherwise      = persistent

-- -----------------------------------------------------------------------------
-- | Each phase in the pipeline returns the next phase to execute, and the
-- name of the file in which the output was placed.
--
-- We must do things dynamically this way, because we often don't know
-- what the rest of the phases will be until part-way through the
-- compilation: for example, an {-# OPTIONS -fasm #-} at the beginning
-- of a source file can change the latter stages of the pipeline from
-- taking the LLVM route to using the native code generator.
--
runPhase :: PhasePlus   -- ^ Run this phase
         -> FilePath    -- ^ name of the input file
         -> DynFlags    -- ^ for convenience, we pass the current dflags in
         -> CompPipeline (PhasePlus,           -- next phase to run
                          FilePath)            -- output filename

        -- Invariant: the output filename always contains the output
        -- Interesting case: Hsc when there is no recompilation to do
        --                   Then the output filename is still a .o file


-------------------------------------------------------------------------------
-- Unlit phase

runPhase (RealPhase (Unlit sf)) input_fn _dflags
  = do output_fn <- phaseOutputFilename (Cpp sf)

       -- let flags = [ -- The -h option passes the file name for unlit to
       --               -- put in a #line directive
       --               SysTools.Option     "-h"
       --             , SysTools.Option $ escape $ normalise input_fn
       --             , SysTools.FileOption "" input_fn
       --             , SysTools.FileOption "" output_fn
       --             ]
       -- liftIO $ SysTools.runUnlit dflags flags
       liftIO $ do
         input <- readFile input_fn
         writeFile output_fn $ unlit (escape (normalise input_fn)) input
       return (RealPhase (Cpp sf), output_fn)
  where
       -- escape the characters \, ", and ', but don't try to escape
       -- Unicode or anything else (so we don't use Util.charToC
       -- here).  If we get this wrong, then in
       -- Coverage.addTicksToBinds where we check that the filename in
       -- a SrcLoc is the same as the source filenaame, the two will
       -- look bogusly different. See test:
       -- libraries/hpc/tests/function/subdir/tough2.lhs
       escape ('\\':cs) = '\\':'\\': escape cs
       escape ('\"':cs) = '\\':'\"': escape cs
       escape ('\'':cs) = '\\':'\'': escape cs
       escape (c:cs)    = c : escape cs
       escape []        = []

-------------------------------------------------------------------------------
-- Cpp phase : (a) gets OPTIONS out of file
--             (b) runs cpp if necessary

runPhase (RealPhase (Cpp sf)) input_fn dflags0
  = do
       src_opts <- liftIO $ getOptionsFromFile dflags0 input_fn
       (dflags1, unhandled_flags, warns)
           <- liftIO $ parseDynamicFilePragma dflags0 src_opts
       setDynFlags dflags1
       liftIO $ checkProcessArgsResult dflags1 unhandled_flags

       if not (xopt Opt_Cpp dflags1) then do
           -- we have to be careful to emit warnings only once.
           unless (gopt Opt_Pp dflags1) $
               liftIO $ handleFlagWarnings dflags1 warns

           -- no need to preprocess CPP, just pass input file along
           -- to the next phase of the pipeline.
           return (RealPhase (HsPp sf), input_fn)
        else do
            output_fn <- phaseOutputFilename (HsPp sf)
            orig_fn <- fmap src_filename getPipeEnv
            liftIO $ doCpp dflags1 orig_fn input_fn output_fn
            -- re-read the pragmas now that we've preprocessed the file
            -- See #2464,#3457
            src_opts <- liftIO $ getOptionsFromFile dflags0 output_fn
            (dflags2, unhandled_flags, warns)
                <- liftIO $ parseDynamicFilePragma dflags0 src_opts
            liftIO $ checkProcessArgsResult dflags2 unhandled_flags
            unless (gopt Opt_Pp dflags2) $
                liftIO $ handleFlagWarnings dflags2 warns
            -- the HsPp pass below will emit warnings

            setDynFlags dflags2

            return (RealPhase (HsPp sf), output_fn)

-------------------------------------------------------------------------------
-- HsPp phase

runPhase (RealPhase (HsPp sf)) input_fn dflags
  = do
       if not (gopt Opt_Pp dflags) then
           -- no need to preprocess, just pass input file along
           -- to the next phase of the pipeline.
          return (RealPhase (Hsc sf), input_fn)
        else do
            PipeEnv{src_basename, src_suffix} <- getPipeEnv
            let orig_fn = src_basename <.> src_suffix
            output_fn <- phaseOutputFilename (Hsc sf)
            liftIO $ SysTools.runPp dflags
                           ( [ SysTools.Option     orig_fn
                             , SysTools.Option     input_fn
                             , SysTools.FileOption "" output_fn
                             ]
                           )

            -- re-read pragmas now that we've parsed the file (see #3674)
            src_opts <- liftIO $ getOptionsFromFile dflags output_fn
            (dflags1, unhandled_flags, warns)
                <- liftIO $ parseDynamicFilePragma dflags src_opts
            setDynFlags dflags1
            liftIO $ checkProcessArgsResult dflags1 unhandled_flags
            liftIO $ handleFlagWarnings dflags1 warns

            return (RealPhase (Hsc sf), output_fn)

-----------------------------------------------------------------------------
-- Hsc phase

-- Compilation of a single module, in "legacy" mode (_not_ under
-- the direction of the compilation manager).
runPhase (RealPhase (Hsc src_flavour)) input_fn dflags0
 = do   -- normal Hsc mode, not mkdependHS

        PipeEnv{ stop_phase=stop,
                 src_basename=basename,
                 src_suffix=suff } <- getPipeEnv

  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the include path, since this is
  -- what gcc does, and it's probably what you want.
        let current_dir = takeDirectory basename
            paths = includePaths dflags0
            dflags = dflags0 { includePaths = current_dir : paths }

        setDynFlags dflags

  -- gather the imports and module name
        (hspp_buf,mod_name,imps,src_imps) <- liftIO $ do
          do
            buf <- hGetStringBuffer input_fn
            (src_imps,imps,L _ mod_name) <- getImports dflags buf input_fn (basename <.> suff)
            return (Just buf, mod_name, imps, src_imps)

  -- Take -o into account if present
  -- Very like -ohi, but we must *only* do this if we aren't linking
  -- (If we're linking then the -o applies to the linked thing, not to
  -- the object file for one module.)
  -- Note the nasty duplication with the same computation in compileFile above
        location <- getLocation src_flavour mod_name

        let o_file = ml_obj_file location -- The real object file
            hi_file = ml_hi_file location
            dest_file | writeInterfaceOnlyMode dflags
                            = hi_file
                      | otherwise
                            = o_file

  -- Figure out if the source has changed, for recompilation avoidance.
  --
  -- Setting source_unchanged to True means that M.o seems
  -- to be up to date wrt M.hs; so no need to recompile unless imports have
  -- changed (which the compiler itself figures out).
  -- Setting source_unchanged to False tells the compiler that M.o is out of
  -- date wrt M.hs (or M.o doesn't exist) so we must recompile regardless.
        src_timestamp <- liftIO $ getModificationUTCTime (basename <.> suff)

        source_unchanged <- liftIO $
          if not (isStopLn stop)
                -- SourceModified unconditionally if
                --      (a) recompilation checker is off, or
                --      (b) we aren't going all the way to .o file (e.g. ghc -S)
             then return SourceModified
                -- Otherwise look at file modification dates
             else do dest_file_exists <- doesFileExist dest_file
                     if not dest_file_exists
                        then return SourceModified       -- Need to recompile
                        else do t2 <- getModificationUTCTime dest_file
                                if t2 > src_timestamp
                                  then return SourceUnmodified
                                  else return SourceModified

        PipeState{hsc_env=hsc_env'} <- getPipeState

  -- Tell the finder cache about this module
        mod <- liftIO $ addHomeModuleToFinder hsc_env' mod_name location

  -- Make the ModSummary to hand to hscMain
        let
            mod_summary = ModSummary {  ms_mod       = mod,
                                        ms_hsc_src   = src_flavour,
                                        ms_hspp_file = input_fn,
                                        ms_hspp_opts = dflags,
                                        ms_hspp_buf  = hspp_buf,
                                        ms_location  = location,
                                        ms_hs_date   = src_timestamp,
                                        ms_obj_date  = Nothing,
                                        ms_iface_date   = Nothing,
                                        ms_textual_imps = imps,
                                        ms_srcimps      = src_imps }

  -- run the compiler!
        result <- liftIO $ hscCompileOneShot hsc_env'
                               mod_summary source_unchanged

        return (HscOut src_flavour mod_name result,
                panic "HscOut doesn't have an input filename")

runPhase (HscOut src_flavour mod_name result) _ dflags = do
  location <- getLocation src_flavour mod_name
  setModLocation location

  let o_file = ml_obj_file location -- The real object file
      _hsc_lang = hscTarget dflags
      next_phase = StopLn

  case result of
    HscNotGeneratingCode -> do
      return (RealPhase next_phase,
              panic "No output filename from Hsc when no-code")
    HscUpToDate -> do
          liftIO $ touchObjectFile dflags o_file
          -- The .o file must have a later modification date
          -- than the source file (else we wouldn't get Nothing)
          -- but we touch it anyway, to keep 'make' happy (we think).
          return (RealPhase StopLn, o_file)
    HscUpdateBoot ->
      do -- In the case of hs-boot files, generate a dummy .o-boot
          -- stamp file for the benefit of Make
          liftIO $ touchObjectFile dflags o_file
          return (RealPhase next_phase, o_file)
    HscUpdateSig ->
      do -- We need to create a REAL but empty .o file
          -- because we are going to attempt to put it in a library
          PipeState{hsc_env=hsc_env'} <- getPipeState
          let input_fn = expectJust "runPhase" (ml_hs_file location)
              basename = dropExtension input_fn
          liftIO $ compileEmptyStub dflags hsc_env' basename location
          return (RealPhase next_phase, o_file)
    HscRecomp cgguts mod_summary ->
      do output_fn <- phaseOutputFilename next_phase

         PipeState{hsc_env=hsc_env'} <- getPipeState

         (outputFilename, _) <- liftIO $ hscGenHardCode hsc_env' cgguts mod_summary output_fn

         return (RealPhase next_phase, outputFilename)

runPhase (RealPhase other) _input_fn _dflags =
   panic ("runPhase: don't know how to run phase " ++ show other)

-- maybeMergeStub :: CompPipeline Phase
-- maybeMergeStub
--  = do
--      PipeState{maybe_stub_o} <- getPipeState
--      if isJust maybe_stub_o then return MergeStub else return StopLn

getLocation :: HscSource -> ModuleName -> CompPipeline ModLocation
getLocation src_flavour mod_name = do
    dflags <- getDynFlags

    PipeEnv{ src_basename=basename,
             src_suffix=suff } <- getPipeEnv

    -- Build a ModLocation to pass to hscMain.
    -- The source filename is rather irrelevant by now, but it's used
    -- by hscMain for messages.  hscMain also needs
    -- the .hi and .o filenames, and this is as good a way
    -- as any to generate them, and better than most. (e.g. takes
    -- into account the -osuf flags)
    location1 <- liftIO $ mkHomeModLocation2 dflags mod_name basename suff

    -- Boot-ify it if necessary
    let location2 | HsBootFile <- src_flavour = addBootSuffixLocn location1
                  | otherwise                 = location1


    -- Take -ohi into account if present
    -- This can't be done in mkHomeModuleLocation because
    -- it only applies to the module being compiles
    let ohi = outputHi dflags
        location3 | Just fn <- ohi = location2{ ml_hi_file = fn }
                  | otherwise      = location2

    -- Take -o into account if present
    -- Very like -ohi, but we must *only* do this if we aren't linking
    -- (If we're linking then the -o applies to the linked thing, not to
    -- the object file for one module.)
    -- Note the nasty duplication with the same computation in compileFile above
    let expl_o_file = outputFile dflags
        location4 | Just ofile <- expl_o_file
                  , isNoLink (ghcLink dflags)
                  = location3 { ml_obj_file = ofile }
                  | otherwise = location3

    return location4

-----------------------------------------------------------------------------
-- Look for the /* GHC_PACKAGES ... */ comment at the top of a .hc file

-- getHCFilePackages :: FilePath -> IO [UnitId]
-- getHCFilePackages filename =
--   Exception.bracket (openFile filename ReadMode) hClose $ \h -> do
--     l <- hGetLine h
--     case l of
--       '/':'*':' ':'G':'H':'C':'_':'P':'A':'C':'K':'A':'G':'E':'S':rest ->
--           return (map stringToUnitId (words rest))
--       _other ->
--           return []

-- -----------------------------------------------------------------------------
-- Running CPP

doCpp :: DynFlags -> FilePath -> FilePath -> FilePath -> IO ()
doCpp dflags origFile input_fn output_fn = do
    pkg_include_dirs <- getPackageIncludePath dflags []

    -- Default CPP defines in Haskell source
    etaVersionH <- getEtaVersionPathName dflags

    let hscpp_opts = picPOpts dflags
        cmdline_include_paths = includePaths dflags

        include_paths = map ("-I" ++) . nub $
                          cmdline_include_paths ++ pkg_include_dirs

        cpp_prog_args = getOpts dflags opt_P

        verbFlags = getVerbFlags dflags

#ifdef ETA_REPL
        th_defs = [ "-D__GLASGOW_HASKELL_TH__=YES" ]
#else
        th_defs = [ "-D__GLASGOW_HASKELL_TH__=NO" ]
#endif
        hsSourceCppOpts =
          [ "-D__GLASGOW_HASKELL__=" ++ ghcProjectVersionInt
          , "-DETA_VERSION=" ++ cProjectVersionInt
          , "-DETA_BUILD_NUMBER=" ++ cProjectPatchLevel
          , "-include" ++ etaVersionH
          ]
        flags = verbFlags ++ include_paths ++ hsSourceCppOpts
             ++ th_defs ++ hscpp_opts ++ cpp_prog_args

        cppOpts = flags ++ [input_fn, "-o", output_fn]
    liftIO $ do
      mError <- Hpp.runWithArgs cppOpts
      case mError of
        Just err -> throwGhcExceptionIO $ ProgramError (renderHppError origFile err)
        _ -> return ()

-- TODO: Make this more detailed and better
renderHppError :: FilePath -> Hpp.Error -> String
renderHppError origFile err = origFile ++ ":\n    " ++ show err

-- -----------------------------------------------------------------------------
-- Misc.

writeInterfaceOnlyMode :: DynFlags -> Bool
writeInterfaceOnlyMode dflags =
 gopt Opt_WriteInterface dflags &&
 HscNothing == hscTarget dflags

-- | What phase to run after one of the backend code generators has run
hscPostBackendPhase :: DynFlags -> HscSource -> HscTarget -> Phase
hscPostBackendPhase _ HsBootFile _    =  StopLn
hscPostBackendPhase _ HsigFile _      =  StopLn
hscPostBackendPhase dflags _ hsc_lang =
  case hsc_lang of
        HscC -> HCc
        HscAsm | gopt Opt_SplitObjs dflags -> Splitter
               | otherwise                 -> As False
        HscLlvm        -> LlvmOpt
        HscNothing     -> StopLn
        HscInterpreted -> StopLn

touchObjectFile :: DynFlags -> FilePath -> IO ()
touchObjectFile _dflags path = do
  createDirectoryIfMissing True $ takeDirectory path
  exists <- fileExist path
  if exists
    then touchFile path
    else writeFile path ""

haveRtsOptsFlags :: DynFlags -> Bool
haveRtsOptsFlags dflags =
         isJust (rtsOpts dflags) || case rtsOptsEnabled dflags of
                                        RtsOptsSafeOnly -> False
                                        _ -> True

-- | Find out path to @etaversion.h@ file
getEtaVersionPathName :: DynFlags -> IO FilePath
getEtaVersionPathName dflags = do
  dirs  <- getPackageIncludePath dflags $ map toInstalledUnitId [rtsUnitId]
  found <- filterM doesFileExist (map (</> "etaversion.h") dirs)
  case found of
    []    -> throwGhcExceptionIO
               (InstallationError ("etaversion.h missing from " ++ show dirs))
    (x:_) -> return x

-- genJavaBytecode :: HscEnv -> CgGuts -> ModSummary -> FilePath -> IO FilePath
-- genJavaBytecode hsc_env cgguts mod_summary output_filename = do
--   let CgGuts{ -- This is the last use of the ModGuts in a compilation.
--               -- From now on, we just use the bits we need.
--               cg_module   = this_mod,
--               cg_binds    = core_binds,
--               cg_tycons   = tycons,
--               cg_foreign  = foreign_stubs0,
--               cg_dep_pkgs = dependencies,
--               cg_hpc_info = hpc_info } = cgguts
--       dflags = hsc_dflags hsc_env
--       location = ms_location mod_summary
--       data_tycons = filter isDataTyCon tycons
--       -- cg_tycons includes newtypes, for the benefit of External Core,
--       -- but we don't generate any code for newtypes

--   -------------------
--   -- PREPARE FOR CODE GENERATION
--   -- Do saturation and convert to A-normal form
--   prepd_binds <- {-# SCC "CorePrep" #-}
--                   corePrepPgm hsc_env location core_binds data_tycons ;
--   -----------------  Convert to STG ------------------
--   (stg_binds, _)
--       <- {-# SCC "CoreToStg" #-}
--           myCoreToStg dflags this_mod prepd_binds

--   classes <- codeGen hsc_env this_mod data_tycons stg_binds hpc_info
--   let jarContents' = map (classFilePath &&& classFileBS) classes
--   jarContents <- forM jarContents' $ \(a,b) -> do
--     a' <- mkPath a
--     return (a', b)
--   createEmptyJar output_filename
--   addMultiByteStringsToJar' jarContents output_filename
--   return output_filename

-- myCoreToStg :: DynFlags -> Module -> CoreProgram
--             -> IO ( [StgBinding] -- output program
--                   , CollectedCCs) -- cost centre info (declared and used)
-- myCoreToStg dflags this_mod prepd_binds = do
--   stg_binds <- {-# SCC "Core2Stg" #-}
--           coreToStg dflags this_mod prepd_binds
--
--   (stg_binds2, cost_centre_info)
--       <- {-# SCC "Stg2Stg" #-}
--           stg2stg dflags this_mod stg_binds
--
--   return (stg_binds2, cost_centre_info)

linkingNeeded :: DynFlags -> [Linkable] -> [InstalledUnitId] -> IO Bool
linkingNeeded dflags linkables pkgDeps = do
        -- if the modification time on the executable is later than the
        -- modification times on all of the objects and libraries, then omit
        -- linking (unless the -fforce-recomp flag was given).
  let jarFile = jarFileName dflags
      getTime = tryIO . getModificationUTCTime
  eJarTime <- getTime jarFile
  case eJarTime of
    Left _  -> return True
    Right t -> do
        debugTraceMsg dflags 3 (text $ "linkingNeeded: previousJar time = " ++ show t)
        -- TODO: Factor in ldOptions too
        let jarTimes = map linkableTime linkables
        debugTraceMsg dflags 3 (text $ "linkingNeeded: linkablesJar times = " ++
                                show jarTimes)
        if any (t <) jarTimes then
          return True
        else do
          let pkgHSLibs  = [ (libraryDirs c, lib)
                          | Just c <- map (lookupInstalledPackage dflags) pkgDeps
                          , lib <- packageHsLibs dflags c ]
          pkgLibFiles <- mapM (uncurry $ findHSLib dflags) pkgHSLibs
          if any isNothing pkgLibFiles then do
             debugTraceMsg dflags 3 (text $ "linkingNeeded: any isNothing pkgLibFiles")
             return True
          else do
            extras <- getExtrasFileName dflags
            let -- Currently __extras.jar is always generated even when modules are not compiled
                inputJars = jarInputs dflags \\ [extras]
                pkgAndInputJars = catMaybes pkgLibFiles `union` inputJars
            eLibTimes <- mapM getTime pkgAndInputJars
            let (libErrs, libTimes) = splitEithers eLibTimes
            debugTraceMsg dflags 3 (text $ "linkingNeeded: pkgAndInputJars times = " ++
                                show libTimes ++ ",libErrs = " ++ show libErrs)
            if not (null libErrs) || any (t <) libTimes
               then return True
               else checkLinkInfo dflags linkables pkgDeps jarFile

jarFileName :: DynFlags -> FilePath
jarFileName dflags
  | Just s <- outputFile dflags = s <?.> "jar"
  | otherwise = "Run.jar"

ffiMapFileName :: DynFlags -> FilePath
ffiMapFileName dflags
  | Just s <- outputFile dflags = s -<.> "ffimap"
  | otherwise = "Run.ffimap"

(<?.>) :: FilePath -> String -> FilePath
s <?.> ext | null (takeExtension s) = s <.> ext
           | otherwise              = s

checkLinkInfo :: DynFlags -> [Linkable] -> [InstalledUnitId] -> FilePath -> IO Bool
checkLinkInfo  dflags linkables pkg_deps jar_file = do
  let linkablesJars = linkablesToJars linkables
  link_info <- getLinkInfo dflags linkablesJars pkg_deps
  debugTraceMsg dflags 3 $ text ("checkLinkInfo: Link info= " ++ show link_info)
  m_jar_link_info <- extractLinkInfoFromJarFile dflags etaLinkInfoSectionName jar_file
  debugTraceMsg dflags 3 $ text ("checkLinkInfo: Exe link info= " ++ show m_jar_link_info)
  return (Just link_info /= m_jar_link_info)

type LinkInfo = Set Fingerprint

getLinkInfo :: DynFlags -> [FilePath] -> [InstalledUnitId] -> IO LinkInfo
getLinkInfo dflags linkablesJars dep_packages = do
  includedPkgLibJars <- if includePackages then getPackageLibJars dflags dep_packages
                        else return []
  mainAndManifest <- fmap (map fst) $ maybeMainAndManifest dflags isExecutable
  extras <- getExtrasFileName dflags
  mExtrasHash <- getFileHashIfExists extras
  let inputJars = jarInputs dflags
      allJars =  (linkablesJars ++ (includedPkgLibJars `union` inputJars)) \\ [extras]
      linkInfo = (show $ compressionMethod dflags) : (mainAndManifest ++ allJars)
      extrasHash = maybeToList mExtrasHash
  return $ Set.fromList $ map fingerprintString linkInfo ++ extrasHash
  where LinkFlags { isExecutable, includePackages } = getLinkFlags dflags

getFileHashIfExists :: FilePath -> IO (Maybe Fingerprint)
getFileHashIfExists file = do
   exists <- doesFileExist file
   if exists then fmap Just $ getFileHash file
   else return Nothing

getLinkInfoFile :: DynFlags -> [FilePath] -> [InstalledUnitId] -> IO (FilePath, ByteString)
getLinkInfoFile dflags linkablesJars dep_packages = do
  linkInfo <- getLinkInfo dflags linkablesJars dep_packages
  return (etaLinkInfoSectionName, BC.pack . unlines . map show . Set.toList $ linkInfo)

extractLinkInfoFromJarFile :: DynFlags -> String -> FilePath
                           -> IO (Maybe LinkInfo)
extractLinkInfoFromJarFile dflags linkInfoName jarFile = do
  debugTraceMsg dflags 3 (text $ "extractLinkInfoFromJarFile: jarFile=" ++ show jarFile)
  existJar <- doesFileExist jarFile
  let bsToLinkInfo = Set.fromList . map readHexFingerprint . lines . BC.unpack
  if (not existJar) then return Nothing
  else do
    mContent <- getEntryContentFromJar jarFile linkInfoName
    return $ fmap bsToLinkInfo mContent

etaLinkInfoSectionName :: String
etaLinkInfoSectionName = ".eta-link-info"

-- etaFrontend :: ModSummary -> Hsc TcGblEnv
-- etaFrontend mod_summary = do
--   hpm <- hscParse' mod_summary
--   hsc_env <- getHscEnv
--   tcg_env <- tcRnModule' hsc_env mod_summary False hpm
--   return tcg_env

findHSLib :: DynFlags -> [String] -> String -> IO (Maybe FilePath)
findHSLib _dflags dirs lib = do
  found <- filterM doesFileExist (map (</> file) dirs)
  return $
    case found of
      [] -> Nothing
      (x:_) -> Just x
  where file = lib <.> "jar"

linkGeneric :: DynFlags -> [String] -> [InstalledUnitId] -> IO ()
linkGeneric dflags oFiles depPackages = do
    -- TODO: Figure out the right place for this error message
    -- when (haveRtsOptsFlags dflags) $ do
    --   log_action dflags dflags SevInfo noSrcSpan defaultUserStyle
    --       ((text $ "Warning: -rtsopts and -with-rtsopts have no effect with"
    --          ++ " -no-hs-main.") $$
    --        (text $ "    Call Rts.init() from your main() method to set"
    --          ++ " these options."))
    -- TODO: Use conduits to combine the jars
    linkInfoFile <- getLinkInfoFile dflags oFiles depPackages
    mainFiles' <- maybeMainAndManifest dflags isExecutable
    mainFiles <- forM (linkInfoFile : mainFiles') $ \(a, b) -> do
                   a' <- mkPath a
                   return (a', b)
    outJars <- mapM getNonManifestEntries oFiles
    pkgLibJars <- if includePackages then getPackageLibJars dflags depPackages
                  else return []
    extraJars <- mapM getNonManifestEntries pkgLibJars
                  -- TODO: Verify that the right version eta was used
                  -- in the Manifests of the jars being compiled
    inputJars <- mapM getNonManifestEntries (jarInputs dflags)
    start <- getCurrentTime
    debugTraceMsg dflags 3 (text $ "linkGeneric: linkables are: " ++
                            show ( ["mainFiles"]  ++ map (show . fst) mainFiles
                                ++ ["pkgLibJars"] ++ pkgLibJars
                                ++ ["jarInputs"]  ++ jarInputs dflags
                                ++ ["oFiles"]     ++ oFiles) )
    mergeClassesAndJars outputFn (compressionMethod dflags) mainFiles $
      extraJars ++ inputJars ++ outJars
    end <- getCurrentTime
    when (verbosity dflags > 1) $
      putStrLn $ "Link time: " ++ show (diffUTCTime end start)
    -- TODO: Handle frameworks & extra ldInputs
    where LinkFlags { isExecutable, includePackages } = getLinkFlags dflags
          outputFn = jarFileName dflags
          getNonManifestEntries = getEntriesFromJar

data LinkFlags = LinkFlags { isExecutable :: Bool, includePackages :: Bool }

getLinkFlags :: DynFlags -> LinkFlags
getLinkFlags dflags =  uncurry LinkFlags $
  case ghcLink dflags of
    LinkBinary    -> (True, True)
    LinkStaticLib -> (False, False)
    LinkDynLib    -> (True, False)
    other         ->
     panic ("link: GHC not built to link this way: " ++
            show other)


maybeMainAndManifest :: DynFlags -> Bool -> IO [(FilePath, ByteString)]
maybeMainAndManifest dflags isExecutable = do
  when (gopt Opt_NoHsMain dflags && haveRtsOptsFlags dflags) $ do
      log_action dflags dflags SevInfo noSrcSpan defaultUserStyle $
        (text $ "Warning: -rtsopts and -with-rtsopts have no effect with "
             ++ "-no-hs-main.") $$
        (text $ "    Call hs_init_ghc() from your main() function to set these"
             ++ " options.")
  return . catMaybes $ [mainFile, manifestFile]
  where
    mainClass = "eta/main"
    mainClassJava = "eta.main"
    mainFile
      | gopt Opt_NoHsMain dflags || not isExecutable = Nothing
      | otherwise = Just ((classFilePath &&& classFileBS)
                          $ mkRtsMainClass dflags mainClass)
    manifestFile = Just ( "META-INF/MANIFEST.MF"
                        , BC.pack $
                           "Manifest-Version: 1.0\n"
                        ++ "Created-By: eta-" ++ cProjectVersion ++ "\n"
                        ++ maybe "" (const $ "Main-Class: " ++ mainClassJava ++ "\n")
                                 mainFile)

mkRtsMainClass :: DynFlags -> String -> ClassFile
mkRtsMainClass dflags mainClass
  = mkClassFile java7 [Public, Super] mainClass' Nothing [] []
  [
    mkMethodDef mainClass' [Public, Static] "main" [jarray jstring] void $ fold
      [ gload (jarray jstring) 0
      -- TODO: Find main module
      , invokestatic $ mkMethodRef (moduleJavaClass mainMod) "DZCmain" [] (ret closureType)
      , invokestatic $ mkMethodRef (rts "Runtime") "main" [ jarray jstring
                                                          , closureType ] void
      , vreturn ]
  ]
  where mainClass' = T.pack mainClass
        mainMod = mainModIs dflags
