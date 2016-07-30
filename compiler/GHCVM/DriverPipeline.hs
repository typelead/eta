module GHCVM.DriverPipeline
  (runGhcVMPhase,
   linkGhcVM,
   ghcvmFrontend)
where

import SrcLoc
import UniqFM
import GHCVM.TypeCheck.TcRnTypes
import MkIface
import Module
import DynFlags
import Exception (tryIO)
import Util (getModificationUTCTime, splitEithers)

import DriverPipeline hiding (linkingNeeded)
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

import Data.Maybe(catMaybes, isNothing, isJust)
import Data.IORef
import Control.Monad(when, filterM, forM)
import Control.Arrow((&&&), first)
import System.Directory
import System.FilePath

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

import GHCVM.CodeGen.Main
import GHCVM.CodeGen.Name
import GHCVM.Debug
import GHCVM.CodeGen.Rts
import GHCVM.Parser.Parse
import GHCVM.JAR
import GHCVM.Packages
import GHCVM.Util
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
  let jarContents' = map (classFilePath &&& classFileBS) classes
  jarContents <- forM jarContents' $ \(a,b) -> do
    a' <- mkPath a
    return (a', b)
  createEmptyJar output_filename
  addMultiByteStringsToJar' jarContents output_filename
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
linkGhcVM LinkInMemory _ _ _ = error "LinkInMemory not implemented yet."
linkGhcVM NoLink _ _ _ = return Succeeded
linkGhcVM _ dflags batchAttemptLinking hpt
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

ghcvmFrontend :: ModSummary -> Hsc TcGblEnv
ghcvmFrontend mod_summary = do
  hpm <- hscParse' mod_summary
  hsc_env <- getHscEnv
  tcg_env <- tcRnModule' hsc_env mod_summary False hpm
  return tcg_env

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

haveRtsOptsFlags :: DynFlags -> Bool
haveRtsOptsFlags dflags
  = isJust (rtsOpts dflags)
  || case rtsOptsEnabled dflags of
       RtsOptsSafeOnly -> False
       _ -> True
