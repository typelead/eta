{-# LANGUAGE CPP, NamedFieldPuns, NondecreasingIndentation #-}
{-# OPTIONS_GHC -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

-----------------------------------------------------------------------------
--
-- GHC Driver
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module GHCVM.Main.DriverPipeline (
        -- Run a series of compilation steps in a pipeline, for a
        -- collection of source files.
   oneShot, compileFile,

        -- Interfaces for the compilation manager (interpreted/batch-mode)
   preprocess,
   compileOne, compileOne',
   link,

        -- Exports for hooks to override runPhase and link
   PhasePlus(..), CompPipeline(..), PipeEnv(..), PipeState(..),
   phaseOutputFilename, getPipeState, getPipeEnv,
   hscPostBackendPhase, getLocation, setModLocation, setDynFlags,
   runPhase, jarFileName,
   linkingNeeded, writeInterfaceOnlyMode
  ) where

import GHCVM.Core.CoreSyn (CoreProgram)
import GHCVM.StgSyn.StgSyn (StgBinding, pprStgBindings)
import GHCVM.Profiling.CostCentre (CollectedCCs)
import GHCVM.SimplStg.SimplStg         ( stg2stg )
import GHCVM.StgSyn.CoreToStg        ( coreToStg )
import GHCVM.Core.CorePrep         ( corePrepPgm )
import GHCVM.Main.SysTools
import qualified GHCVM.Main.SysTools as SysTools
import GHCVM.Types.TyCon ( isDataTyCon )
import GHCVM.BasicTypes.NameEnv

import GHCVM.CodeGen.Main
import GHCVM.CodeGen.Name
import GHCVM.Debug
import GHCVM.CodeGen.Rts
import GHCVM.Parser.Parse
import GHCVM.JAR
import GHCVM.Util
import Codec.JVM

import GHCVM.Iface.MkIface
import GHCVM.Main.PipelineMonad
import GHCVM.Main.Packages
import GHCVM.Main.HeaderInfo
import GHCVM.Main.DriverPhases
import GHCVM.Main.HscMain
import GHCVM.Main.Finder
import GHCVM.Main.HscTypes hiding ( Hsc )
import GHCVM.Utils.Outputable
import GHCVM.BasicTypes.Module
import GHCVM.Utils.UniqFM           ( eltsUFM )
import GHCVM.Main.ErrUtils
import GHCVM.Main.DynFlags
import GHCVM.Utils.Panic
import GHCVM.Utils.Util
import GHCVM.Utils.StringBuffer     ( hGetStringBuffer )
import GHCVM.BasicTypes.BasicTypes       ( SuccessFlag(..) )
import GHCVM.Utils.Maybes           ( expectJust )
import GHCVM.BasicTypes.SrcLoc
import GHCVM.Utils.FastString
-- import LlvmCodeGen      ( llvmFixupAsm )
import GHCVM.Utils.MonadUtils
import GHCVM.Utils.Platform
import GHCVM.TypeCheck.TcRnTypes
import GHCVM.Main.Hooks

import GHCVM.Utils.Exception
import qualified GHCVM.Utils.Exception as Exception
import Data.IORef       ( readIORef )
import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import Data.List        ( isSuffixOf )
import Data.Maybe
import System.Environment
import Data.Char
import Control.Arrow((&&&), first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

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
  --ASSERT2(isJust mb_phase || isHaskellSrcFilename filename, text filename)
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
              --MASSERT(isJust maybe_old_linkable)
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
  empty_stub <- newTempName dflags "c"
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
          linkables = map (expectJust "link" . hm_linkable) homeModInfos
      debugTraceMsg dflags 3 (text "link: linkables are ..." $$ vcat (map ppr linkables))
      if isNoLink (ghcLink dflags) then do
        debugTraceMsg dflags 3
          (text "link(batch): linking omitted (-c flag given).")
        return Succeeded
      else do
        let getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)
            jarFiles = concatMap getOfiles linkables
            jarFile = jarFileName dflags
        shouldLink <- linkingNeeded dflags linkables pkgDeps
        if not (gopt Opt_ForceRecomp dflags) && not shouldLink then do
          debugTraceMsg dflags 2
            (text jarFile <+>
             (str "is up to date, linking not required."))
          return Succeeded
        else do
          compilationProgressMsg dflags ("Linking " ++ jarFile ++ " ...")
          let link = case ghcLink dflags of
                       LinkBinary    -> linkGeneric True
                       LinkStaticLib -> linkGeneric False
                       LinkDynLib    -> linkGeneric False
                       other         ->
                         panic ("link: GHC not built to link this way: " ++
                                show other)
          link dflags jarFiles pkgDeps
          debugTraceMsg dflags 3 (text "link: done")
          return Succeeded
  | otherwise
  = do debugTraceMsg dflags 3
         (text "link(batch): upsweep (partially) failed OR" $$
          text "   Main.main not exported; not linking.")
       return Succeeded

-- -----------------------------------------------------------------------------
-- Compile files in one-shot mode.

oneShot :: HscEnv -> Phase -> [(String, Maybe Phase)] -> IO ()
oneShot hsc_env stop_phase srcs = do
  o_files <- mapM (compileFile hsc_env stop_phase) srcs
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

  | otherwise
  = case ghcLink dflags of
        NoLink        -> return ()
        LinkBinary    -> linkGeneric True  dflags o_files []
        LinkStaticLib -> linkGeneric False dflags o_files []
        LinkDynLib    -> linkGeneric False dflags o_files []
        _         -> panic "doLink: implement"


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
             RealPhase start_phase' ->
                 when (not (start_phase' `happensBefore'` stop_phase)) $
                       throwGhcExceptionIO (UsageError
                                   ("cannot compile this file to desired target: "
                                      ++ input_fn))
             HscOut {} -> return ()

         debugTraceMsg dflags 4 (text "Running the pipeline")
         r <- runPipeline' start_phase hsc_env env input_fn
                           maybe_loc maybe_stub_o

         -- If we are compiling a Haskell module, and doing
         -- -dynamic-too, but couldn't do the -dynamic-too fast
         -- path, then rerun the pipeline for the dyn way
         let dflags = extractDynFlags hsc_env
         -- NB: Currently disabled on Windows (ref #7134, #8228, and #5987)
         when (not $ platformOS (targetPlatform dflags) == OSMinGW32) $ do
           when isHaskellishFile $ whenCannotGenerateDynamicToo dflags $ do
               debugTraceMsg dflags 4
                   (text "Running the pipeline again for -dynamic-too")
               let dflags' = dynamicTooMkDynamicDynFlags dflags
               hsc_env' <- newHscEnv dflags'
               _ <- runPipeline' start_phase hsc_env' env input_fn
                                 maybe_loc maybe_stub_o
               return ()
         return r

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
 | otherwise                             = newTempName dflags suffix
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

runPhase (RealPhase (Unlit sf)) input_fn dflags
  = do
       output_fn <- phaseOutputFilename (Cpp sf)

       let flags = [ -- The -h option passes the file name for unlit to
                     -- put in a #line directive
                     SysTools.Option     "-h"
                   , SysTools.Option $ escape $ normalise input_fn
                   , SysTools.FileOption "" input_fn
                   , SysTools.FileOption "" output_fn
                   ]

       liftIO $ SysTools.runUnlit dflags flags

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
            liftIO $ doCpp dflags1 True{-raw-}
                           input_fn output_fn
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
      hsc_lang = hscTarget dflags
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

-----------------------------------------------------------------------------
-- Cmm phase

runPhase (RealPhase CmmCpp) input_fn dflags
  = do
       output_fn <- phaseOutputFilename Cmm
       liftIO $ doCpp dflags False{-not raw-}
                      input_fn output_fn
       return (RealPhase Cmm, output_fn)

-----------------------------------------------------------------------------
-- Cc phase

-- we don't support preprocessing .c files (with -E) now.  Doing so introduces
-- way too many hacks, and I can't say I've ever used it anyway.

runPhase (RealPhase cc_phase) input_fn dflags
   | any (cc_phase `eqPhase`) [Cc, Ccpp, HCc, Cobjc, Cobjcpp]
   = do
        let platform = targetPlatform dflags
            hcc = cc_phase `eqPhase` HCc

        let cmdline_include_paths = includePaths dflags

        -- HC files have the dependent packages stamped into them
        pkgs <- if hcc then liftIO $ getHCFilePackages input_fn else return []

        -- add package include paths even if we're just compiling .c
        -- files; this is the Value Add(TM) that using ghc instead of
        -- gcc gives you :)
        pkg_include_dirs <- liftIO $ getPackageIncludePath dflags pkgs
        let include_paths = foldr (\ x xs -> ("-I" ++ x) : xs) []
                              (cmdline_include_paths ++ pkg_include_dirs)

        let gcc_extra_viac_flags = extraGccViaCFlags dflags
        let pic_c_flags = picCCOpts dflags

        let verbFlags = getVerbFlags dflags

        -- cc-options are not passed when compiling .hc files.  Our
        -- hc code doesn't not #include any header files anyway, so these
        -- options aren't necessary.
        pkg_extra_cc_opts <- liftIO $
          if cc_phase `eqPhase` HCc
             then return []
             else getPackageExtraCcOpts dflags pkgs

        framework_paths <-
            if platformUsesFrameworks platform
            then do pkgFrameworkPaths <- liftIO $ getPackageFrameworkPath dflags pkgs
                    let cmdlineFrameworkPaths = frameworkPaths dflags
                    return $ map ("-F"++)
                                 (cmdlineFrameworkPaths ++ pkgFrameworkPaths)
            else return []

        let split_objs = gopt Opt_SplitObjs dflags
            split_opt | hcc && split_objs = [ "-DUSE_SPLIT_MARKERS" ]
                      | otherwise         = [ ]

        let cc_opt | optLevel dflags >= 2 = [ "-O2" ]
                   | optLevel dflags >= 1 = [ "-O" ]
                   | otherwise            = []

        -- Decide next phase
        let next_phase = As False
        output_fn <- phaseOutputFilename next_phase

        let
          more_hcc_opts =
                -- on x86 the floating point regs have greater precision
                -- than a double, which leads to unpredictable results.
                -- By default, we turn this off with -ffloat-store unless
                -- the user specified -fexcess-precision.
                (if platformArch platform == ArchX86 &&
                    not (gopt Opt_ExcessPrecision dflags)
                        then [ "-ffloat-store" ]
                        else []) ++

                -- gcc's -fstrict-aliasing allows two accesses to memory
                -- to be considered non-aliasing if they have different types.
                -- This interacts badly with the C code we generate, which is
                -- very weakly typed, being derived from C--.
                ["-fno-strict-aliasing"]

        ghcVersionH <- liftIO $ getGhcVersionPathName dflags

        let gcc_lang_opt | cc_phase `eqPhase` Ccpp    = "c++"
                         | cc_phase `eqPhase` Cobjc   = "objective-c"
                         | cc_phase `eqPhase` Cobjcpp = "objective-c++"
                         | otherwise                  = "c"
        liftIO $ SysTools.runCc dflags (
                -- force the C compiler to interpret this file as C when
                -- compiling .hc files, by adding the -x c option.
                -- Also useful for plain .c files, just in case GHC saw a
                -- -x c option.
                        [ SysTools.Option "-x", SysTools.Option gcc_lang_opt
                        , SysTools.FileOption "" input_fn
                        , SysTools.Option "-o"
                        , SysTools.FileOption "" output_fn
                        ]
                       ++ map SysTools.Option (
                          pic_c_flags

                -- Stub files generated for foreign exports references the runIO_closure
                -- and runNonIO_closure symbols, which are defined in the base package.
                -- These symbols are imported into the stub.c file via RtsAPI.h, and the
                -- way we do the import depends on whether we're currently compiling
                -- the base package or not.
                       ++ (if platformOS platform == OSMinGW32 &&
                              thisPackage dflags == basePackageKey
                                then [ "-DCOMPILING_BASE_PACKAGE" ]
                                else [])

        -- We only support SparcV9 and better because V8 lacks an atomic CAS
        -- instruction. Note that the user can still override this
        -- (e.g., -mcpu=ultrasparc) as GCC picks the "best" -mcpu flag
        -- regardless of the ordering.
        --
        -- This is a temporary hack. See #2872, commit
        -- 5bd3072ac30216a505151601884ac88bf404c9f2
                       ++ (if platformArch platform == ArchSPARC
                           then ["-mcpu=v9"]
                           else [])

                       -- GCC 4.6+ doesn't like -Wimplicit when compiling C++.
                       ++ (if (cc_phase /= Ccpp && cc_phase /= Cobjcpp)
                             then ["-Wimplicit"]
                             else [])

                       ++ (if hcc
                             then gcc_extra_viac_flags ++ more_hcc_opts
                             else [])
                       ++ verbFlags
                       ++ [ "-S" ]
                       ++ cc_opt
                       ++ [ "-D__GLASGOW_HASKELL__=001"
                          , "-include", ghcVersionH
                          ]
                       ++ framework_paths
                       ++ split_opt
                       ++ include_paths
                       ++ pkg_extra_cc_opts
                       ))

        return (RealPhase next_phase, output_fn)

-----------------------------------------------------------------------------
-- Splitting phase

runPhase (RealPhase Splitter) input_fn dflags
  = do  -- tmp_pfx is the prefix used for the split .s files

        split_s_prefix <- liftIO $ SysTools.newTempName dflags "split"
        let n_files_fn = split_s_prefix

        liftIO $ SysTools.runSplit dflags
                          [ SysTools.FileOption "" input_fn
                          , SysTools.FileOption "" split_s_prefix
                          , SysTools.FileOption "" n_files_fn
                          ]

        -- Save the number of split files for future references
        s <- liftIO $ readFile n_files_fn
        let n_files = read s :: Int
            dflags' = dflags { splitInfo = Just (split_s_prefix, n_files) }

        setDynFlags dflags'

        -- Remember to delete all these files
        liftIO $ addFilesToClean dflags'
                                 [ split_s_prefix ++ "__" ++ show n ++ ".s"
                                 | n <- [1..n_files]]

        return (RealPhase SplitAs,
                "**splitter**") -- we don't use the filename in SplitAs

-----------------------------------------------------------------------------
-- As, SpitAs phase : Assembler

-- This is for calling the assembler on a regular assembly file (not split).
runPhase (RealPhase (As with_cpp)) input_fn dflags
  = do
        -- LLVM from version 3.0 onwards doesn't support the OS X system
        -- assembler, so we use clang as the assembler instead. (#5636)
        let whichAsProg | hscTarget dflags == HscLlvm &&
                          platformOS (targetPlatform dflags) == OSDarwin
                        = do
                            -- be careful what options we call clang with
                            -- see #5903 and #7617 for bugs caused by this.
                            llvmVer <- liftIO $ figureLlvmVersion dflags
                            return $ case llvmVer of
                                Just n | n >= 30 -> SysTools.runClang
                                _                -> SysTools.runAs

                        | otherwise = return SysTools.runAs

        as_prog <- whichAsProg
        let cmdline_include_paths = includePaths dflags
        let pic_c_flags = picCCOpts dflags

        next_phase <- maybeMergeStub
        output_fn <- phaseOutputFilename next_phase

        -- we create directories for the object file, because it
        -- might be a hierarchical module.
        liftIO $ createDirectoryIfMissing True (takeDirectory output_fn)

        ccInfo <- liftIO $ getCompilerInfo dflags
        let runAssembler inputFilename outputFilename
                = liftIO $ as_prog dflags
                       ([ SysTools.Option ("-I" ++ p) | p <- cmdline_include_paths ]

                       -- See Note [-fPIC for assembler]
                       ++ map SysTools.Option pic_c_flags

        -- We only support SparcV9 and better because V8 lacks an atomic CAS
        -- instruction so we have to make sure that the assembler accepts the
        -- instruction set. Note that the user can still override this
        -- (e.g., -mcpu=ultrasparc). GCC picks the "best" -mcpu flag
        -- regardless of the ordering.
        --
        -- This is a temporary hack.
                       ++ (if platformArch (targetPlatform dflags) == ArchSPARC
                           then [SysTools.Option "-mcpu=v9"]
                           else [])
                       ++ (if any (ccInfo ==) [Clang, AppleClang, AppleClang51]
                            then [SysTools.Option "-Qunused-arguments"]
                            else [])
                       ++ [ SysTools.Option "-x"
                          , if with_cpp
                              then SysTools.Option "assembler-with-cpp"
                              else SysTools.Option "assembler"
                          , SysTools.Option "-c"
                          , SysTools.FileOption "" inputFilename
                          , SysTools.Option "-o"
                          , SysTools.FileOption "" outputFilename
                          ])

        liftIO $ debugTraceMsg dflags 4 (text "Running the assembler")
        runAssembler input_fn output_fn
        return (RealPhase next_phase, output_fn)


-- This is for calling the assembler on a split assembly file (so a collection
-- of assembly files)
runPhase (RealPhase SplitAs) _input_fn dflags
  = do
        -- we'll handle the stub_o file in this phase, so don't MergeStub,
        -- just jump straight to StopLn afterwards.
        let next_phase = StopLn
        output_fn <- phaseOutputFilename next_phase

        let base_o = dropExtension output_fn
            osuf = objectSuf dflags
            split_odir  = base_o ++ "_" ++ osuf ++ "_split"

        let pic_c_flags = picCCOpts dflags

        -- this also creates the hierarchy
        liftIO $ createDirectoryIfMissing True split_odir

        -- remove M_split/ *.o, because we're going to archive M_split/ *.o
        -- later and we don't want to pick up any old objects.
        fs <- liftIO $ getDirectoryContents split_odir
        liftIO $ mapM_ removeFile $
                map (split_odir </>) $ filter (osuf `isSuffixOf`) fs

        let (split_s_prefix, n) = case splitInfo dflags of
                                  Nothing -> panic "No split info"
                                  Just x -> x

        let split_s   n = split_s_prefix ++ "__" ++ show n <.> "s"

            split_obj :: Int -> FilePath
            split_obj n = split_odir </>
                          takeFileName base_o ++ "__" ++ show n <.> osuf

        let assemble_file n
              = SysTools.runAs dflags (

        -- We only support SparcV9 and better because V8 lacks an atomic CAS
        -- instruction so we have to make sure that the assembler accepts the
        -- instruction set. Note that the user can still override this
        -- (e.g., -mcpu=ultrasparc). GCC picks the "best" -mcpu flag
        -- regardless of the ordering.
        --
        -- This is a temporary hack.
                          (if platformArch (targetPlatform dflags) == ArchSPARC
                           then [SysTools.Option "-mcpu=v9"]
                           else []) ++

                          -- See Note [-fPIC for assembler]
                          map SysTools.Option pic_c_flags ++

                          [ SysTools.Option "-c"
                          , SysTools.Option "-o"
                          , SysTools.FileOption "" (split_obj n)
                          , SysTools.FileOption "" (split_s n)
                          ])

        liftIO $ mapM_ assemble_file [1..n]

        -- Note [pipeline-split-init]
        -- If we have a stub file, it may contain constructor
        -- functions for initialisation of this module.  We can't
        -- simply leave the stub as a separate object file, because it
        -- will never be linked in: nothing refers to it.  We need to
        -- ensure that if we ever refer to the data in this module
        -- that needs initialisation, then we also pull in the
        -- initialisation routine.
        --
        -- To that end, we make a DANGEROUS ASSUMPTION here: the data
        -- that needs to be initialised is all in the FIRST split
        -- object.  See Note [codegen-split-init].

        PipeState{maybe_stub_o} <- getPipeState
        case maybe_stub_o of
            Nothing     -> return ()
            Just stub_o -> liftIO $ do
                     tmp_split_1 <- newTempName dflags osuf
                     let split_1 = split_obj 1
                     copyFile split_1 tmp_split_1
                     removeFile split_1
                     joinObjectFiles dflags [tmp_split_1, stub_o] split_1

        -- join them into a single .o file
        liftIO $ joinObjectFiles dflags (map split_obj [1..n]) output_fn

        return (RealPhase next_phase, output_fn)

-----------------------------------------------------------------------------
-- LlvmOpt phase

runPhase (RealPhase LlvmOpt) input_fn dflags
  = do
    ver <- liftIO $ readIORef (llvmVersion dflags)

    let opt_lvl  = max 0 (min 2 $ optLevel dflags)
        -- don't specify anything if user has specified commands. We do this
        -- for opt but not llc since opt is very specifically for optimisation
        -- passes only, so if the user is passing us extra options we assume
        -- they know what they are doing and don't get in the way.
        optFlag  = if null (getOpts dflags opt_lo)
                       then map SysTools.Option $ words (llvmOpts ver !! opt_lvl)
                       else []
        tbaa | ver < 29                 = "" -- no tbaa in 2.8 and earlier
             | gopt Opt_LlvmTBAA dflags = "--enable-tbaa=true"
             | otherwise                = "--enable-tbaa=false"


    output_fn <- phaseOutputFilename LlvmLlc

    liftIO $ SysTools.runLlvmOpt dflags
               ([ SysTools.FileOption "" input_fn,
                    SysTools.Option "-o",
                    SysTools.FileOption "" output_fn]
                ++ optFlag
                ++ [SysTools.Option tbaa])

    return (RealPhase LlvmLlc, output_fn)
  where
        -- we always (unless -optlo specified) run Opt since we rely on it to
        -- fix up some pretty big deficiencies in the code we generate
        llvmOpts ver = [ "-mem2reg -globalopt"
                       , if ver >= 34 then "-O1 -globalopt" else "-O1"
                         -- LLVM 3.4 -O1 doesn't eliminate aliases reliably (bug #8855)
                       , "-O2"
                       ]

-----------------------------------------------------------------------------
-- LlvmLlc phase

runPhase (RealPhase LlvmLlc) input_fn dflags
  = do
    ver <- liftIO $ readIORef (llvmVersion dflags)

    let opt_lvl = max 0 (min 2 $ optLevel dflags)
        -- iOS requires external references to be loaded indirectly from the
        -- DATA segment or dyld traps at runtime writing into TEXT: see #7722
        rmodel | platformOS (targetPlatform dflags) == OSiOS = "dynamic-no-pic"
               | gopt Opt_PIC dflags                         = "pic"
               | not (gopt Opt_Static dflags)                = "dynamic-no-pic"
               | otherwise                                   = "static"
        tbaa | ver < 29                 = "" -- no tbaa in 2.8 and earlier
             | gopt Opt_LlvmTBAA dflags = "--enable-tbaa=true"
             | otherwise                = "--enable-tbaa=false"

    -- hidden debugging flag '-dno-llvm-mangler' to skip mangling
    let next_phase = case gopt Opt_NoLlvmMangler dflags of
                         False                            -> LlvmMangle
                         True | gopt Opt_SplitObjs dflags -> Splitter
                         True                             -> As False

    output_fn <- phaseOutputFilename next_phase

    -- AVX can cause LLVM 3.2 to generate a C-like frame pointer
    -- prelude, see #9391
    when (ver == 32 && isAvxEnabled dflags) $ liftIO $ errorMsg dflags $ text
      "Note: LLVM 3.2 has known problems with AVX instructions (see trac #9391)"

    liftIO $ SysTools.runLlvmLlc dflags
                ([ SysTools.Option (llvmOpts !! opt_lvl),
                    SysTools.Option $ "-relocation-model=" ++ rmodel,
                    SysTools.FileOption "" input_fn,
                    SysTools.Option "-o", SysTools.FileOption "" output_fn]
                ++ [SysTools.Option tbaa]
                ++ map SysTools.Option fpOpts
                ++ map SysTools.Option abiOpts
                ++ map SysTools.Option sseOpts
                ++ map SysTools.Option (avxOpts ver)
                ++ map SysTools.Option avx512Opts
                ++ map SysTools.Option stackAlignOpts)

    return (RealPhase next_phase, output_fn)
  where
        -- Bug in LLVM at O3 on OSX.
        llvmOpts = if platformOS (targetPlatform dflags) == OSDarwin
                   then ["-O1", "-O2", "-O2"]
                   else ["-O1", "-O2", "-O3"]
        -- On ARMv7 using LLVM, LLVM fails to allocate floating point registers
        -- while compiling GHC source code. It's probably due to fact that it
        -- does not enable VFP by default. Let's do this manually here
        fpOpts = case platformArch (targetPlatform dflags) of
                   ArchARM ARMv7 ext _ -> if (elem VFPv3 ext)
                                      then ["-mattr=+v7,+vfp3"]
                                      else if (elem VFPv3D16 ext)
                                           then ["-mattr=+v7,+vfp3,+d16"]
                                           else []
                   ArchARM ARMv6 ext _ -> if (elem VFPv2 ext)
                                          then ["-mattr=+v6,+vfp2"]
                                          else ["-mattr=+v6"]
                   _                 -> []
        -- On Ubuntu/Debian with ARM hard float ABI, LLVM's llc still
        -- compiles into soft-float ABI. We need to explicitly set abi
        -- to hard
        abiOpts = case platformArch (targetPlatform dflags) of
                    ArchARM _ _ HARD -> ["-float-abi=hard"]
                    ArchARM _ _ _    -> []
                    _                -> []

        sseOpts | isSse4_2Enabled dflags = ["-mattr=+sse42"]
                | isSse2Enabled dflags   = ["-mattr=+sse2"]
                | isSseEnabled dflags    = ["-mattr=+sse"]
                | otherwise              = []

        avxOpts ver | isAvx512fEnabled dflags = ["-mattr=+avx512f"]
                    | isAvx2Enabled dflags    = ["-mattr=+avx2"]
                    | isAvxEnabled dflags     = ["-mattr=+avx"]
                    | ver == 32               = ["-mattr=-avx"] -- see #9391
                    | otherwise               = []

        avx512Opts =
          [ "-mattr=+avx512cd" | isAvx512cdEnabled dflags ] ++
          [ "-mattr=+avx512er" | isAvx512erEnabled dflags ] ++
          [ "-mattr=+avx512pf" | isAvx512pfEnabled dflags ]

        stackAlignOpts =
            case platformArch (targetPlatform dflags) of
              ArchX86_64 | isAvxEnabled dflags -> ["-stack-alignment=32"]
              _                                -> []

-----------------------------------------------------------------------------
-- LlvmMangle phase

-- runPhase (RealPhase LlvmMangle) input_fn dflags
--   = do
--       let next_phase = if gopt Opt_SplitObjs dflags then Splitter else As False
--       output_fn <- phaseOutputFilename next_phase
--       liftIO $ llvmFixupAsm dflags input_fn output_fn
--       return (RealPhase next_phase, output_fn)

-----------------------------------------------------------------------------
-- merge in stub objects

runPhase (RealPhase MergeStub) input_fn dflags
 = do
     PipeState{maybe_stub_o} <- getPipeState
     output_fn <- phaseOutputFilename StopLn
     liftIO $ createDirectoryIfMissing True (takeDirectory output_fn)
     case maybe_stub_o of
       Nothing ->
         panic "runPhase(MergeStub): no stub"
       Just stub_o -> do
         liftIO $ joinObjectFiles dflags [input_fn, stub_o] output_fn
         return (RealPhase StopLn, output_fn)

-- warning suppression
runPhase (RealPhase other) _input_fn _dflags =
   panic ("runPhase: don't know how to run phase " ++ show other)

maybeMergeStub :: CompPipeline Phase
maybeMergeStub
 = do
     PipeState{maybe_stub_o} <- getPipeState
     if isJust maybe_stub_o then return MergeStub else return StopLn

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

getHCFilePackages :: FilePath -> IO [PackageKey]
getHCFilePackages filename =
  Exception.bracket (openFile filename ReadMode) hClose $ \h -> do
    l <- hGetLine h
    case l of
      '/':'*':' ':'G':'H':'C':'_':'P':'A':'C':'K':'A':'G':'E':'S':rest ->
          return (map stringToPackageKey (words rest))
      _other ->
          return []

-- -----------------------------------------------------------------------------
-- Running CPP

doCpp :: DynFlags -> Bool -> FilePath -> FilePath -> IO ()
doCpp dflags raw input_fn output_fn = do
    let hscpp_opts = picPOpts dflags
    let cmdline_include_paths = includePaths dflags

    pkg_include_dirs <- getPackageIncludePath dflags []
    let include_paths = foldr (\ x xs -> "-I" : x : xs) []
                          (cmdline_include_paths ++ pkg_include_dirs)

    let verbFlags = getVerbFlags dflags

    let cpp_prog args | raw       = SysTools.runCpp dflags args
                      | otherwise = SysTools.runCc dflags (SysTools.Option "-E" : args)

    let target_defs = [] -- TODO: Deal with this
          -- [ "-D" ++ HOST_OS     ++ "_BUILD_OS=1",
          --   "-D" ++ HOST_ARCH   ++ "_BUILD_ARCH=1",
          --   "-D" ++ TARGET_OS   ++ "_HOST_OS=1",
          --   "-D" ++ TARGET_ARCH ++ "_HOST_ARCH=1" ]
        -- remember, in code we *compile*, the HOST is the same our TARGET,
        -- and BUILD is the same as our HOST.

    let sse_defs =
          [ "-D__SSE__=1"    | isSseEnabled    dflags ] ++
          [ "-D__SSE2__=1"   | isSse2Enabled   dflags ] ++
          [ "-D__SSE4_2__=1" | isSse4_2Enabled dflags ]

    let avx_defs =
          [ "-D__AVX__=1"  | isAvxEnabled  dflags ] ++
          [ "-D__AVX2__=1" | isAvx2Enabled dflags ] ++
          [ "-D__AVX512CD__=1" | isAvx512cdEnabled dflags ] ++
          [ "-D__AVX512ER__=1" | isAvx512erEnabled dflags ] ++
          [ "-D__AVX512F__=1"  | isAvx512fEnabled  dflags ] ++
          [ "-D__AVX512PF__=1" | isAvx512pfEnabled dflags ]

    backend_defs <- getBackendDefs dflags

#ifdef GHCI
    let th_defs = [ "-D__GLASGOW_HASKELL_TH__=YES" ]
#else
    let th_defs = [ "-D__GLASGOW_HASKELL_TH__=NO" ]
#endif
    -- Default CPP defines in Haskell source
    ghcVersionH <- getGhcVersionPathName dflags
    let hsSourceCppOpts =
          [ "-D__GLASGOW_HASKELL__=001"
          , "-include", ghcVersionH
          ]

    cpp_prog       (   map SysTools.Option verbFlags
                    ++ map SysTools.Option include_paths
                    ++ map SysTools.Option hsSourceCppOpts
                    ++ map SysTools.Option target_defs
                    ++ map SysTools.Option backend_defs
                    ++ map SysTools.Option th_defs
                    ++ map SysTools.Option hscpp_opts
                    ++ map SysTools.Option sse_defs
                    ++ map SysTools.Option avx_defs
        -- Set the language mode to assembler-with-cpp when preprocessing. This
        -- alleviates some of the C99 macro rules relating to whitespace and the hash
        -- operator, which we tend to abuse. Clang in particular is not very happy
        -- about this.
                    ++ [ SysTools.Option     "-x"
                       , SysTools.Option     "assembler-with-cpp"
                       , SysTools.Option     input_fn
        -- We hackily use Option instead of FileOption here, so that the file
        -- name is not back-slashed on Windows.  cpp is capable of
        -- dealing with / in filenames, so it works fine.  Furthermore
        -- if we put in backslashes, cpp outputs #line directives
        -- with *double* backslashes.   And that in turn means that
        -- our error messages get double backslashes in them.
        -- In due course we should arrange that the lexer deals
        -- with these \\ escapes properly.
                       , SysTools.Option     "-o"
                       , SysTools.FileOption "" output_fn
                       ])

getBackendDefs :: DynFlags -> IO [String]
getBackendDefs dflags | hscTarget dflags == HscLlvm = do
    llvmVer <- figureLlvmVersion dflags
    return $ case llvmVer of
               Just n -> [ "-D__GLASGOW_HASKELL_LLVM__="++show n ]
               _      -> []

getBackendDefs _ =
    return []

-- ---------------------------------------------------------------------------
-- join object files into a single relocatable object file, using ld -r

joinObjectFiles :: DynFlags -> [FilePath] -> FilePath -> IO ()
joinObjectFiles dflags o_files output_fn = do
  let mySettings = settings dflags
      ldIsGnuLd = sLdIsGnuLd mySettings
      osInfo = platformOS (targetPlatform dflags)
      ld_r args cc = SysTools.runLink dflags ([
                       SysTools.Option "-nostdlib",
                       SysTools.Option "-Wl,-r"
                     ]
                     ++ (if any (cc ==) [Clang, AppleClang, AppleClang51]
                          then []
                          else [SysTools.Option "-nodefaultlibs"])
                     ++ (if osInfo == OSFreeBSD
                          then [SysTools.Option "-L/usr/lib"]
                          else [])
                        -- gcc on sparc sets -Wl,--relax implicitly, but
                        -- -r and --relax are incompatible for ld, so
                        -- disable --relax explicitly.
                     ++ (if platformArch (targetPlatform dflags) == ArchSPARC
                         && ldIsGnuLd
                            then [SysTools.Option "-Wl,-no-relax"]
                            else [])
                     ++ map SysTools.Option ld_build_id
                     ++ [ SysTools.Option "-o",
                          SysTools.FileOption "" output_fn ]
                     ++ args)

      -- suppress the generation of the .note.gnu.build-id section,
      -- which we don't need and sometimes causes ld to emit a
      -- warning:
      ld_build_id | sLdSupportsBuildId mySettings = ["-Wl,--build-id=none"]
                  | otherwise                     = []

  ccInfo <- getCompilerInfo dflags
  if ldIsGnuLd
     then do
          script <- newTempName dflags "ldscript"
          cwd <- getCurrentDirectory
          let o_files_abs = map (cwd </>) o_files
          writeFile script $ "INPUT(" ++ unwords o_files_abs ++ ")"
          ld_r [SysTools.FileOption "" script] ccInfo
     else if sLdSupportsFilelist mySettings
     then do
          filelist <- newTempName dflags "filelist"
          writeFile filelist $ unlines o_files
          ld_r [SysTools.Option "-Wl,-filelist",
                SysTools.FileOption "-Wl," filelist] ccInfo
     else do
          ld_r (map (SysTools.FileOption "") o_files) ccInfo

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
touchObjectFile dflags path = do
  createDirectoryIfMissing True $ takeDirectory path
  SysTools.touch dflags "Touching object file" path

haveRtsOptsFlags :: DynFlags -> Bool
haveRtsOptsFlags dflags =
         isJust (rtsOpts dflags) || case rtsOptsEnabled dflags of
                                        RtsOptsSafeOnly -> False
                                        _ -> True

-- | Find out path to @ghcversion.h@ file
getGhcVersionPathName :: DynFlags -> IO FilePath
getGhcVersionPathName dflags = do
  dirs <- getPackageIncludePath dflags [rtsPackageKey]

  found <- filterM doesFileExist (map (</> "ghcversion.h") dirs)
  case found of
      []    -> throwGhcExceptionIO (InstallationError ("ghcversion.h missing"))
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

myCoreToStg :: DynFlags -> Module -> CoreProgram
            -> IO ( [StgBinding] -- output program
                  , CollectedCCs) -- cost centre info (declared and used)
myCoreToStg dflags this_mod prepd_binds = do
  stg_binds <- {-# SCC "Core2Stg" #-}
          coreToStg dflags this_mod prepd_binds

  (stg_binds2, cost_centre_info)
      <- {-# SCC "Stg2Stg" #-}
          stg2stg dflags this_mod stg_binds

  return (stg_binds2, cost_centre_info)

linkingNeeded :: DynFlags -> [Linkable] -> [PackageKey] -> IO Bool
linkingNeeded dflags linkables pkgDeps = do
        -- if the modification time on the executable is later than the
        -- modification times on all of the objects and libraries, then omit
        -- linking (unless the -fforce-recomp flag was given).
  let jarFile = jarFileName dflags
  eJarTime <- tryIO $ getModificationUTCTime jarFile
  case eJarTime of
    Left _  -> return True
    Right t -> do
        -- TODO: Factor in ldOptions too
        let jarTimes = map linkableTime linkables
        if any (t <) jarTimes then return True
        else do
          let pkgHSLibs  = [ (libraryDirs c, lib)
                          | Just c <- map (lookupPackage dflags) pkgDeps
                          , lib <- packageHsLibs dflags c ]
          pkgLibFiles <- mapM (uncurry $ findHSLib dflags) pkgHSLibs
          if any isNothing pkgLibFiles then
            return True
          else do
            eLibTimes <- mapM (tryIO . getModificationUTCTime)
                              (catMaybes pkgLibFiles)
            let (libErrs, libTimes) = splitEithers eLibTimes
            return $ not (null libErrs) || any (t <) libTimes

jarFileName :: DynFlags -> FilePath
jarFileName dflags
  | Just s <- outputFile dflags = s <?.> "jar"
  | otherwise = "main.jar"
  where s <?.> ext | null (takeExtension s) = s <.> ext
                   | otherwise              = s

-- ghcvmFrontend :: ModSummary -> Hsc TcGblEnv
-- ghcvmFrontend mod_summary = do
--   hpm <- hscParse' mod_summary
--   hsc_env <- getHscEnv
--   tcg_env <- tcRnModule' hsc_env mod_summary False hpm
--   return tcg_env

findHSLib :: DynFlags -> [String] -> String -> IO (Maybe FilePath)
findHSLib dflags dirs lib = do
  found <- filterM doesFileExist (map (</> file) dirs)
  return $
    case found of
      [] -> Nothing
      (x:_) -> Just x
  where file = lib <.> "jar"

linkGeneric :: Bool -> DynFlags -> [String] -> [PackageKey] -> IO ()
linkGeneric isExecutable dflags oFiles depPackages = do
    when (haveRtsOptsFlags dflags) $ do
      log_action dflags dflags SevInfo noSrcSpan defaultUserStyle
          ((text $ "Warning: -rtsopts and -with-rtsopts have no effect with"
             ++ " -no-hs-main.") $$
           (text $ "    Call hsInit() from your main() method to set"
             ++ " these options."))
    -- TODO: Use conduits to combine the jars
    mainFiles' <- maybeMainAndManifest dflags
    mainFiles <- forM mainFiles' $ \(a, b) -> do
                   a' <- mkPath a
                   return (a', b)
    oFiles  <- concatMapM getFilesFromJar oFiles
    extraFiles <-
          if isExecutable then do
            pkgLibJars <- getPackageLibJars dflags depPackages
            concatMapM getFilesFromJar pkgLibJars
            -- TODO: Verify that the right version ghcvm was used
            --       in the Manifests of the jars being compiled
          else return []
    let files = extraFiles ++ oFiles ++ mainFiles
    linkJars dflags files
    -- TODO: Handle frameworks & extra ldInputs

linkJars :: DynFlags -> [FileAndContents] -> IO ()
linkJars dflags files = do
    let outputFn = jarFileName dflags
    -- fullOutputFn <- if isAbsolute outputFn then return outputFn
    --                 else do d <- getCurrentDirectory
    --                         return $ normalise (d </> outputFn)
    createEmptyJar outputFn
    addMultiByteStringsToJar' files outputFn

maybeMainAndManifest :: DynFlags -> IO [(FilePath, ByteString)]
maybeMainAndManifest dflags = do
  when (gopt Opt_NoHsMain dflags && haveRtsOptsFlags dflags) $ do
      log_action dflags dflags SevInfo noSrcSpan defaultUserStyle $
        (text $ "Warning: -rtsopts and -with-rtsopts have no effect with "
             ++ "-no-hs-main.") $$
        (text $ "    Call hs_init_ghc() from your main() function to set these"
             ++ " options.")
  return . catMaybes $ [mainFile, manifestFile]
  where
    mainClass = "ghcvm/main"
    mainClassJava = "ghcvm.main"
    mainFile
      | gopt Opt_NoHsMain dflags = Nothing
      | otherwise = Just ((classFilePath &&& classFileBS)
                          $ mkRtsMainClass dflags mainClass)
    manifestFile = Just ( "META-INF/MANIFEST.MF"
                        , BC.pack $
                           "Manifest-Version: 1.0\n"
                        -- TODO: Add actual versioning information here
                        ++ "Created-By: ghcvm-0.0.0.1\n"
                        ++ maybe "" (const $ "Main-Class: " ++ mainClassJava)
                                 mainFile)
