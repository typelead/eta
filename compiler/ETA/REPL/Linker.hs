{-# LANGUAGE CPP, NondecreasingIndentation, TupleSections, RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

--
--  (c) The University of Glasgow 2002-2006
--
-- | The interface to Eta REPL's custom classloader to handle dynamic classloading.
module ETA.REPL.Linker ( getHValueAsInt, showLinkerState,
                linkExpr, linkClasses, unload, withExtendedLinkEnv,
                extendLinkEnv, deleteFromLinkEnv, initDynLinker
        ) where

#include "HsVersions.h"

import ETA.REPL
import Eta.REPL.RemoteTypes
import ETA.TypeCheck.TcRnMonad
import ETA.Main.HscTypes
import ETA.BasicTypes.Name
import ETA.BasicTypes.NameEnv
import ETA.Main.DynFlags
import ETA.Utils.Outputable
import ETA.Utils.Util as Util
import ETA.Main.ErrUtils
import ETA.BasicTypes.SrcLoc

import Data.IORef
import Control.Concurrent.MVar

import ETA.Utils.Exception
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
        closure_env :: ClosureEnv
        }

emptyPLS :: DynFlags -> PersistentLinkerState
emptyPLS _ = PersistentLinkerState { closure_env = emptyClosureEnv }

type ClosureEnv = NameEnv (Name, ForeignHValue)

emptyClosureEnv :: ClosureEnv
emptyClosureEnv = emptyNameEnv

extendClosureEnv :: ClosureEnv -> [(Name,ForeignHValue)] -> ClosureEnv
extendClosureEnv cl_env pairs
  = extendNameEnvList cl_env [ (n, (n,v)) | (n,v) <- pairs]

extendLinkEnv :: [(Name,ForeignHValue)] -> IO ()
extendLinkEnv new_bindings =
  modifyPLS_ $ \pls -> do
    let ce = closure_env pls
    let new_ce = extendClosureEnv ce new_bindings
    return pls{ closure_env = new_ce }

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
  putLogMsg dflags SevDump noSrcSpan defaultDumpStyle
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

  -- (a) Load packages from the command-line
  addDynamicClassPath hsc_env classpath
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
linkExpr :: HscEnv -> SrcSpan -> [ClassFile] -> IO ForeignHValue
linkExpr hsc_env _span classes
  = do {
     -- Initialise the linker (if it's not been done already)
   ; initDynLinker hsc_env

     -- Take lock for the actual work.
   ; modifyPLS $ \pls -> do {
       ; loadClasses hsc_env classes
       ; let (_:exprCls:_) = classes
             exprClsName = classFileName exprCls
       ; hvref <- newInstance hsc_env exprClsName
       ; fhv <- mkFinalizedHValue hsc_env hvref
       ; return (pls, fhv)
   }}

linkClasses :: HscEnv -> [ClassFile] -> IO ()
linkClasses hsc_env classes
  = do {
     -- Initialise the linker (if it's not been done already)
   ; initDynLinker hsc_env

     -- Take lock for the actual work.
   ; modifyPLS $ \pls -> do {
       ; loadClasses hsc_env classes
       ; return (pls, ())
   }}

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

        -- let dflags = hsc_dflags hsc_env
        -- debugTraceMsg dflags 3 $
        --   text "unload: retaining objs" <+> ppr (objs_loaded new_pls)
        -- debugTraceMsg dflags 3 $
        --   text "unload: retaining bcos" <+> ppr (bcos_loaded new_pls)
        return ()

unload_wkr :: HscEnv
           -> [Linkable]                -- stable linkables
           -> PersistentLinkerState
           -> IO PersistentLinkerState
unload_wkr _hsc_env _keep_linkables pls = return pls
