module GHCVM.DriverPipeline
  (runGhcVMPhase,
   linkGhcVM,
   ghcvmFrontend)
where

import TcRnTypes
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
import HscMain hiding (hscParse')
import HscTypes

import Data.IORef
import Control.Monad(when)
import Control.Arrow((&&&))
import System.Directory
import System.FilePath

import qualified Data.ByteString.Lazy as B

import GHCVM.CodeGen.Main
import GHCVM.CodeGen.Name
import GHCVM.Parser.Parse
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

ghcvmFrontend :: ModSummary -> Hsc TcGblEnv
ghcvmFrontend mod_summary = do
  hpm <- hscParse' mod_summary
  hsc_env <- getHscEnv
  tcg_env <- tcRnModule' hsc_env mod_summary False hpm
  return tcg_env
