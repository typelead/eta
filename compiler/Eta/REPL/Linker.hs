{-# LANGUAGE CPP, NondecreasingIndentation, TupleSections, RecordWildCards, BangPatterns #-}
{-# OPTIONS_GHC -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

--
--  (c) The University of Glasgow 2002-2006
--
-- | The interface to Eta REPL's custom classloader to handle dynamic classloading.
module Eta.REPL.Linker ( getHValueAsInt, showLinkerState,
                linkExpr, linkClasses, linkModules, unload, withExtendedLinkEnv,
                extendLinkEnv, deleteFromLinkEnv, initDynLinker
        ) where

#include "HsVersions.h"

import Eta.REPL
import Eta.REPL.RemoteTypes
import Eta.TypeCheck.TcRnMonad
import Eta.Main.HscTypes
import Eta.Main.Packages
import Eta.BasicTypes.Module
import Eta.BasicTypes.Name
import Eta.BasicTypes.NameEnv
import Eta.Main.DynFlags
import Eta.Utils.Outputable
import Eta.Utils.UniqFM
import Eta.Utils.Util as Util
import Eta.Main.ErrUtils
import Eta.BasicTypes.SrcLoc

import Data.ByteString (ByteString)
import Data.IORef
import Control.Concurrent.MVar

import Eta.Utils.Exception
import Codec.JVM

{- **********************************************************************

                        The Linker's state

  ********************************************************************* -}

{-
The global IORef used for PersistentLinkerState actually contains another MVar.
The reason for this is that we want to allow another loaded copy of the Eta
library to side-effect the PLS and for those changes to be reflected here.

The PersistentLinkerState maps Names to actual closures (for
interpreted code only), for use during linking.
-}

GLOBAL_VAR_M(v_PersistentLinkerState, newMVar (panic "Dynamic linker not initialised"), MVar PersistentLinkerState)
GLOBAL_VAR(v_InitLinkerDone, False, Bool) -- Set True when dynamic linker is initialised

modifyPLS_ :: (PersistentLinkerState -> IO PersistentLinkerState) -> IO ()
modifyPLS_ f = readIORef v_PersistentLinkerState >>= flip modifyMVar_ f

modifyPLS :: (PersistentLinkerState -> IO (PersistentLinkerState, a)) -> IO a
modifyPLS f = readIORef v_PersistentLinkerState >>= flip modifyMVar f

data PersistentLinkerState
   = PersistentLinkerState {
        -- Current global mapping from Names to their true values
        closure_env :: ClosureEnv,
        -- Maps home modules to the path to their linkables
        home_modules_env :: ModuleNameEnv [FilePath],
        last_root_module :: ModuleName
        }

emptyPLS :: DynFlags -> PersistentLinkerState
emptyPLS _ = PersistentLinkerState { closure_env = emptyClosureEnv
                                   , home_modules_env = emptyUFM
                                   , last_root_module = mkModuleName "?"
                                   }

type ClosureEnv = NameEnv (Name, ForeignHValue)

emptyClosureEnv :: ClosureEnv
emptyClosureEnv = emptyNameEnv

extendClosureEnv :: ClosureEnv -> [(Name,ForeignHValue)] -> ClosureEnv
extendClosureEnv cl_env pairs
  = extendNameEnvList cl_env [ (n, (n,v)) | (n,v) <- pairs]

extendLinkEnv :: [(Name,ForeignHValue)] -> IO ()
extendLinkEnv new_bindings =
  modifyPLS_ $ \pls@PersistentLinkerState{..} -> do
    let new_ce = extendClosureEnv closure_env new_bindings
    return $! pls{ closure_env = new_ce }
    -- strictness is important for not retaining old copies of the pls

deleteFromLinkEnv :: [Name] -> IO ()
deleteFromLinkEnv to_remove =
  modifyPLS_ $ \pls -> do
    let ce = closure_env pls
    let new_ce = delListFromNameEnv ce to_remove
    return pls{ closure_env = new_ce }

-- | Get the 'HValue' associated with the given name and return an integer reference.
getHValueAsInt :: HscEnv -> Name -> IO (Maybe Int)
getHValueAsInt hsc_env name = do
  initDynLinker hsc_env
  pls <- modifyPLS $ \pls -> return (pls, pls)
  return $ fmap (foreignRefToInt . snd) $ lookupNameEnv (closure_env pls) name

-- | Temporarily extend the linker state.
withExtendedLinkEnv :: (ExceptionMonad m) =>
                       [(Name,ForeignHValue)] -> m a -> m a
withExtendedLinkEnv new_env action
    = gbracket (liftIO $ extendLinkEnv new_env)
               (\_ -> reset_old_env)
               (\_ -> action)
    where
        -- Remember that the linker state might be side-effected
        -- during the execution of the IO action, and we don't want to
        -- lose those changes (we might have linked a new module or
        -- package), so the reset action only removes the names we
        -- added earlier.
          reset_old_env = liftIO $ do
            modifyPLS_ $ \pls ->
                let cur = closure_env pls
                    new = delListFromNameEnv cur (map fst new_env)
                in return pls{ closure_env = new }


-- | Display the persistent linker state.
showLinkerState :: DynFlags -> IO ()
showLinkerState dflags = do
  pls <- readIORef v_PersistentLinkerState >>= readMVar
  putLogMsg dflags NoReason SevDump noSrcSpan (defaultDumpStyle dflags)
    (vcat [text "----- Linker state -----"
          ,text "Closure Environment:" <+>
           ppr (closure_env pls)])

{- **********************************************************************

                        Initialisation

  ********************************************************************* -}

-- | Initialise the dynamic linker.  This entails
--
--  a) Loading any packages specified on the command line,
--
-- NOTE: This function is idempotent; if called more than once, it does
-- nothing.  This is useful in Template Metaprogramming, where we call it before
-- trying to link.
--
initDynLinker :: HscEnv -> IO ()
initDynLinker hsc_env =
  modifyPLS_ $ \pls0 -> do
    done <- readIORef v_InitLinkerDone
    if done then return pls0
            else do writeIORef v_InitLinkerDone True
                    reallyInitDynLinker hsc_env

reallyInitDynLinker :: HscEnv -> IO PersistentLinkerState
reallyInitDynLinker hsc_env = do
  -- Initialise the linker state
  let dflags    = hsc_dflags hsc_env
      pls0      = emptyPLS dflags
      classpath = classPaths dflags
  preloadDeps <- closeDependencies dflags [toInstalledUnitId etaMetaId]
  preloadPaths <- getPackageLibJars dflags preloadDeps

  let extraClasspath = filter (\s -> not (s `elem` classpath)) preloadPaths

  -- (a) Load packages from the command-line
  addDynamicClassPath hsc_env (classpath ++ extraClasspath)
  setClassInfoPath hsc_env classpath
  return pls0


{- **********************************************************************

                        Link a byte-code expression

  ********************************************************************* -}

-- | Link a single expression, /including/ first linking packages and
-- modules that this expression depends on.
--
-- Raises an IO exception ('ProgramError') if it can't find a compiled
-- version of the dependents to link.
--
linkExpr :: HscEnv -> String -> String -> [ClassFile] -> IO ForeignHValue
linkExpr hsc_env clsName clsMethod classes
  = do {
     -- Initialise the linker (if it's not been done already)
   ; initDynLinker hsc_env
   ; debugTraceMsg (hsc_dflags hsc_env) 3 $
       text ( "Linking expression: className=" ++ clsName
              ++ ", method=" ++ clsMethod )
     -- Take lock for the actual work.
   ; modifyPLS $ \pls -> do {
       ; loadClasses hsc_env $ forceClasses classes
       ; hvref <- newInstance hsc_env clsName clsMethod
       ; fhv <- mkFinalizedHValue hsc_env hvref
       ; return (pls, fhv)
   }}

linkClasses :: HscEnv -> [(String, String, ByteString)] -> IO ()
linkClasses hsc_env classes
  = do {
     -- Initialise the linker (if it's not been done already)
   ; initDynLinker hsc_env

     -- Take lock for the actual work.
   ; modifyPLS $ \pls -> do {
       ; loadClasses hsc_env classes
       ; return (pls, ())
   }}

linkModules :: HscEnv -> ModuleName -> IO ()
linkModules hsc_env root_module = do {
    -- Initialise the linker (if it's not been done already)
  ; initDynLinker hsc_env
  ; debugTraceMsg (hsc_dflags hsc_env) 3 $
      ( text "Linking modules with root: " <+> ppr root_module )
    -- Take lock for the actual work.
  ; modifyPLS $ \pls -> do {
      ; let root_module' = last_root_module pls
            oldModuleEnv = home_modules_env pls
      ; if root_module' /= root_module then do
          addModuleClassPath hsc_env (concat (eltsUFM (newModuleEnv `minusUFM` oldModuleEnv)))
          return (pls { home_modules_env = newModuleEnv
                      , last_root_module = root_module }, ())
        else return (pls, ())
  }}
  where hpt = hsc_HPT hsc_env
        -- TODO: This calculation is simple since it doesn't take into
        --       account dependency information. We may want to make this
        --       more tight in the future (for projects with 100s of modules)
        newModuleEnv = foldHpt (\modInfo moduleEnv ->
                                  case hm_linkable modInfo of
                                    Just linkable ->
                                      addToUFM moduleEnv
                                        (moduleName (mi_module (hm_iface modInfo)))
                                        (linkableObjs linkable)
                                    Nothing -> moduleEnv)
                       emptyUFM hpt

{- **********************************************************************

                Unload some object modules

  ********************************************************************* -}

-- ---------------------------------------------------------------------------
-- | Unloading old objects ready for a new compilation sweep.
--
-- The compilation manager provides us with a list of linkables that it
-- considers \"stable\", i.e. won't be recompiled this time around.  For
-- each of the modules current linked in memory,
--
--   * if the linkable is stable (and it's the same one -- the user may have
--     recompiled the module on the side), we keep it,
--
--   * otherwise, we unload it.
--
--   * we also implicitly unload all temporary bindings at this point.
--
unload :: HscEnv
       -> [Linkable] -- ^ The linkables to *keep*.
       -> IO ()
unload hsc_env linkables
  = mask_ $ do -- mask, so we're safe from Ctrl-C in here

        -- Initialise the linker (if it's not been done already)
        initDynLinker hsc_env

        _new_pls
            <- modifyPLS $ \pls -> do
                 pls1 <- unload_wkr hsc_env linkables pls
                 return (pls1, pls1)

        return ()

unload_wkr :: HscEnv
           -> [Linkable]                -- stable linkables
           -> PersistentLinkerState
           -> IO PersistentLinkerState
-- Does the core unload business
-- (the wrapper blocks exceptions and deals with the PLS get and put)

unload_wkr hsc_env keep_linkables pls@PersistentLinkerState{..} = do
  -- NB. careful strictness here to avoid keeping the old PLS when
  -- we're unloading some code.  -feta-repl-leak-check with the tests in
  -- tests/suite/repl can detect space leaks here.

  let dflags = hsc_dflags hsc_env
      !modules_retained = mkModuleSet $ map linkableModule
                                      $ filter (not . isObjectLinkable) keep_linkables
      newModuleEnv' = listToUFM [ (moduleName (linkableModule jar), classpath)
                                | jar <- keep_linkables,
                                  let classpath = linkableObjs jar,
                                  not (null classpath) ]
      classesToLink = concat [ linkableClasses l |  l <- keep_linkables,
                                                    isInterpretedLinkable l ]

      -- Note that we want to remove all *local*
      -- (i.e. non-isExternal) names too (these are the
      -- temporary bindings from the command line).
      keep_name (n,_) = isExternalName n
                     && nameModule n `elemModuleSet` modules_retained

      closure_env'  = filterNameEnv keep_name closure_env

      !new_pls = pls { closure_env = closure_env'
                     , home_modules_env = newModuleEnv' }

  debugTraceMsg dflags 3 $ text "Unload old objects"
  debugTraceMsg dflags 3
    ( text "Keeping linkables: " <+> (ppr keep_linkables) )
  -- TODO: Reload class linkables as well via linkClasses?

  resetClasses hsc_env
  addModuleClassPath hsc_env (concat (eltsUFM newModuleEnv'))
  -- TODO: Make this more efficient with per-module classloader system
  loadClasses hsc_env classesToLink

  return new_pls
