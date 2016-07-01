module GHCVM.DriverPipeline
  (runGhcVMPhase,
   linkGhcVM,
   ghcvmCompileOneShot)
where

import MkIface
import Module
import DynFlags
import DriverPipeline
import DriverPhases
import BasicTypes
import CoreSyn (CoreProgram)
import StgSyn (StgBinding, pprStgBindings)
import CostCentre (CollectedCCs)
import SimplStg         ( stg2stg )
import CoreToStg        ( coreToStg )
import CorePrep         ( corePrepPgm )
import Maybes (expectJust)
import Panic
import MonadUtils ( liftIO )
import SysTools
import ErrUtils
import Outputable
import TyCon ( isDataTyCon )
import NameEnv
import HscMain
import HscTypes

import Data.IORef
import Control.Monad(when)
import Control.Arrow((&&&))
import System.Directory
import System.FilePath

import qualified Data.ByteString.Lazy as B

import GHCVM.CodeGen.Main
import GHCVM.CodeGen.Name
import GHCVM.JAR
import Codec.JVM

runGhcVMPhase :: PhasePlus -> FilePath -> DynFlags -> CompPipeline (PhasePlus, FilePath)
runGhcVMPhase realphase@(RealPhase (Unlit _)) = runPhase realphase
runGhcVMPhase realphase@(RealPhase (Cpp _)) = runPhase realphase
runGhcVMPhase realphase@(RealPhase (HsPp _)) = runPhase realphase
runGhcVMPhase realphase@(RealPhase (DriverPhases.Hsc _)) = \fn dflags -> do
  runPhase realphase fn dflags
runGhcVMPhase realphase@(HscOut src_flavour mod_name result) = \_ dflags -> do
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

         outputFilename <- liftIO $ genJavaBytecode hsc_env' cgguts mod_summary output_fn

         return (RealPhase next_phase, outputFilename)

runGhcVMPhase realphase@(RealPhase other) = \_ dflags -> panic $ "runGhcVMPhase: invalid phase " ++ show other

genJavaBytecode :: HscEnv -> CgGuts -> ModSummary -> FilePath -> IO FilePath
genJavaBytecode hsc_env cgguts mod_summary output_filename = do
  let CgGuts{ -- This is the last use of the ModGuts in a compilation.
              -- From now on, we just use the bits we need.
              cg_module   = this_mod,
              cg_binds    = core_binds,
              cg_tycons   = tycons,
              cg_foreign  = foreign_stubs0,
              cg_dep_pkgs = dependencies,
              cg_hpc_info = hpc_info } = cgguts
      dflags = hsc_dflags hsc_env
      location = ms_location mod_summary
      data_tycons = filter isDataTyCon tycons
      -- cg_tycons includes newtypes, for the benefit of External Core,
      -- but we don't generate any code for newtypes

  -------------------
  -- PREPARE FOR CODE GENERATION
  -- Do saturation and convert to A-normal form
  prepd_binds <- {-# SCC "CorePrep" #-}
                  corePrepPgm hsc_env location core_binds data_tycons ;
  -----------------  Convert to STG ------------------
  (stg_binds, _)
      <- {-# SCC "CoreToStg" #-}
          myCoreToStg dflags this_mod prepd_binds

  classes <- codeGen hsc_env this_mod data_tycons stg_binds hpc_info
  let jarContents = map (classFilePath &&& classFileBS) classes
  addMultiByteStringsToJar jarContents output_filename
  return output_filename

dumpStg :: DynFlags -> SDoc -> IO ()
dumpStg dflags = dumpSDoc dflags alwaysQualify Opt_D_dump_stg "STG Syntax:"

touchObjectFile :: DynFlags -> FilePath -> IO ()
touchObjectFile dflags path = do
  createDirectoryIfMissing True $ takeDirectory path
  SysTools.touch dflags "Touching object file" path

compileEmptyStub :: DynFlags -> HscEnv -> FilePath -> ModLocation -> IO ()
compileEmptyStub dflags hsc_env basename location = do
  -- To maintain the invariant that every Haskell file
  -- compiles to object code, we make an empty (but
  -- valid) stub object file for signatures
  empty_stub <- newTempName dflags "c"
  writeFile empty_stub ""
  {- _ <- runPipeline StopLn hsc_env
                  (empty_stub, Nothing)
                  (Just basename)
                  Persistent
                  (Just location)
                  Nothing -}
  return ()

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

linkGhcVM :: GhcLink -> DynFlags -> Bool -> HomePackageTable -> IO SuccessFlag
linkGhcVM _ dflags _ hpt = do
  putStrLn "Linking the generated .class files..."
  return Succeeded

-- Compile Haskell/boot in OneShot mode.
ghcvmCompileOneShot :: HscEnv
                   -> ModSummary
                   -> SourceModified
                   -> IO HscStatus
ghcvmCompileOneShot hsc_env mod_summary src_changed
  = do
    -- One-shot mode needs a knot-tying mutable variable for interface
    -- files. See TcRnTypes.TcGblEnv.tcg_type_env_var.
    type_env_var <- newIORef emptyNameEnv
    let mod = ms_mod mod_summary
        hsc_env' = hsc_env{ hsc_type_env_var = Just (mod, type_env_var) }

        msg what = oneShotMsg hsc_env' what

        skip = do msg UpToDate
                  dumpIfaceStats hsc_env'
                  return HscUpToDate

        compile mb_old_hash reason = runHsc hsc_env' $ do
            liftIO $ msg reason
            tc_result <- genericHscFrontend mod_summary
            guts0 <- hscDesugar' (ms_location mod_summary) tc_result
            dflags <- getDynFlags
            case hscTarget dflags of
                HscNothing -> do
                    when (gopt Opt_WriteInterface dflags) $ liftIO $ do
                        (iface, changed, _details) <- hscSimpleIface hsc_env tc_result mb_old_hash
                        hscWriteIface dflags iface changed mod_summary
                    return HscNotGeneratingCode
                _ ->
                    case ms_hsc_src mod_summary of
                    t | isHsBootOrSig t ->
                        do (iface, changed, _) <- hscSimpleIface' tc_result mb_old_hash
                           liftIO $ hscWriteIface dflags iface changed mod_summary
                           return (case t of
                                    HsBootFile -> HscUpdateBoot
                                    HsigFile -> HscUpdateSig
                                    HsSrcFile -> panic "hscCompileOneShot Src")
                    _ ->
                        do guts <- hscSimplify' guts0
                           (iface, changed, _details, cgguts) <- hscNormalIface' guts mb_old_hash
                           liftIO $ hscWriteIface dflags iface changed mod_summary
                           return $ HscRecomp cgguts mod_summary

        -- XXX This is always False, because in one-shot mode the
        -- concept of stability does not exist.  The driver never
        -- passes SourceUnmodifiedAndStable in here.
        stable = case src_changed of
                     SourceUnmodifiedAndStable -> True
                     _                         -> False

    (recomp_reqd, mb_checked_iface)
        <- {-# SCC "checkOldIface" #-}
           checkOldIface hsc_env' mod_summary src_changed Nothing
    -- save the interface that comes back from checkOldIface.
    -- In one-shot mode we don't have the old iface until this
    -- point, when checkOldIface reads it from the disk.
    let mb_old_hash = fmap mi_iface_hash mb_checked_iface

    case mb_checked_iface of
        Just iface | not (recompileRequired recomp_reqd) ->
            -- If the module used TH splices when it was last compiled,
            -- then the recompilation check is not accurate enough (#481)
            -- and we must ignore it. However, if the module is stable
            -- (none of the modules it depends on, directly or indirectly,
            -- changed), then we *can* skip recompilation. This is why
            -- the SourceModified type contains SourceUnmodifiedAndStable,
            -- and it's pretty important: otherwise ghc --make would
            -- always recompile TH modules, even if nothing at all has
            -- changed. Stability is just the same check that make is
            -- doing for us in one-shot mode.
            if mi_used_th iface && not stable
            then compile mb_old_hash (RecompBecause "TH")
            else skip
        _ ->
            compile mb_old_hash recomp_reqd
