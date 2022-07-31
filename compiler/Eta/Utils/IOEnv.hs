{-# LANGUAGE DeriveDataTypeable, UndecidableInstances #-}
{-# LANGUAGE CPP #-}

--
-- (c) The University of Glasgow 2002-2006
--
-- The IO Monad with an environment
--
-- The environment is passed around as a Reader monad but
-- as its in the IO monad, mutable references can be used
-- for updating state.
--

module Eta.Utils.IOEnv (
        IOEnv, -- Instance of Monad

        -- Monad utilities
        module Eta.Utils.MonadUtils,

        -- Errors
        failM, failWithM,
        IOEnvFailure(..),

        -- Getting at the environment
        getEnv, setEnv, updEnv,

        runIOEnv, unsafeInterleaveM, uninterruptibleMaskM_,
        tryM, tryAllM, tryMostM, fixM,

        -- I/O operations
        IORef, newMutVar, readMutVar, writeMutVar, updMutVar,
        atomicUpdMutVar, atomicUpdMutVar'
  ) where

import Eta.Main.DynFlags
import Eta.Utils.Exception
import Eta.BasicTypes.Module
import Eta.Utils.Panic

import Data.IORef       ( IORef, newIORef, readIORef, writeIORef, modifyIORef,
                          atomicModifyIORef )
import Data.Typeable
import System.IO.Unsafe ( unsafeInterleaveIO )
import System.IO        ( fixIO )
import Control.Monad
import Eta.Utils.MonadUtils
import Control.Applicative (Alternative(..))
#if __GLASGOW_HASKELL__ > 800
import qualified Control.Monad.Fail as MonadFail
#endif

----------------------------------------------------------------------
-- Defining the monad type
----------------------------------------------------------------------


newtype IOEnv env a = IOEnv (env -> IO a)

unIOEnv :: IOEnv env a -> (env -> IO a)
unIOEnv (IOEnv m) = m

instance Monad (IOEnv m) where
    (>>=)  = thenM
    (>>)   = thenM_
    return = returnM
#if __GLASGOW_HASKELL__ < 800
    fail _ = failM -- Ignore the string
#else
    fail   = MonadFail.fail

instance MonadFail.MonadFail (IOEnv m) where
    fail _ = failM -- Ignore the string
#endif

instance Applicative (IOEnv m) where
    pure = returnM
    IOEnv f <*> IOEnv x = IOEnv (\ env -> f env <*> x env )

instance Functor (IOEnv m) where
    fmap f (IOEnv m) = IOEnv (\ env -> fmap f (m env))

returnM :: a -> IOEnv env a
returnM a = IOEnv (\ _ -> return a)

thenM :: IOEnv env a -> (a -> IOEnv env b) -> IOEnv env b
thenM (IOEnv m) f = IOEnv (\ env -> do { r <- m env ;
                                         unIOEnv (f r) env })

thenM_ :: IOEnv env a -> IOEnv env b -> IOEnv env b
thenM_ (IOEnv m) f = IOEnv (\ env -> do { _ <- m env ; unIOEnv f env })

failM :: IOEnv env a
failM = IOEnv (\ _ -> throwIO IOEnvFailure)

failWithM :: String -> IOEnv env a
failWithM s = IOEnv (\ _ -> ioError (userError s))

data IOEnvFailure = IOEnvFailure
    deriving Typeable

instance Show IOEnvFailure where
    show IOEnvFailure = "IOEnv failure"

instance Exception IOEnvFailure

instance ExceptionMonad (IOEnv a) where
  gcatch act handle =
      IOEnv $ \s -> unIOEnv act s `gcatch` \e -> unIOEnv (handle e) s
  gmask f =
      IOEnv $ \s -> gmask $ \io_restore ->
                             let
                                g_restore (IOEnv m) = IOEnv $ \s -> io_restore (m s)
                             in
                                unIOEnv (f g_restore) s

instance ContainsDynFlags env => HasDynFlags (IOEnv env) where
    getDynFlags = do env <- getEnv
                     return $! extractDynFlags env

instance ContainsModule env => HasModule (IOEnv env) where
    getModule = do env <- getEnv
                   return $ extractModule env

----------------------------------------------------------------------
-- Fundamental combinators specific to the monad
----------------------------------------------------------------------


---------------------------
runIOEnv :: env -> IOEnv env a -> IO a
runIOEnv env (IOEnv m) = m env


---------------------------
{-# NOINLINE fixM #-}
  -- Aargh!  Not inlining fixTc alleviates a space leak problem.
  -- Normally fixTc is used with a lazy tuple match: if the optimiser is
  -- shown the definition of fixTc, it occasionally transforms the code
  -- in such a way that the code generator doesn't spot the selector
  -- thunks.  Sigh.

fixM :: (a -> IOEnv env a) -> IOEnv env a
fixM f = IOEnv (\ env -> fixIO (\ r -> unIOEnv (f r) env))


---------------------------
tryM :: IOEnv env r -> IOEnv env (Either IOEnvFailure r)
-- Reflect UserError exceptions (only) into IOEnv monad
-- Other exceptions are not caught; they are simply propagated as exns
--
-- The idea is that errors in the program being compiled will give rise
-- to UserErrors.  But, say, pattern-match failures in GHC itself should
-- not be caught here, else they'll be reported as errors in the program
-- begin compiled!
tryM (IOEnv thing) = IOEnv (\ env -> tryIOEnvFailure (thing env))

tryIOEnvFailure :: IO a -> IO (Either IOEnvFailure a)
tryIOEnvFailure = try

-- XXX We shouldn't be catching everything, e.g. timeouts
tryAllM :: IOEnv env r -> IOEnv env (Either SomeException r)
-- Catch *all* exceptions
-- This is used when running a Template-Haskell splice, when
-- even a pattern-match failure is a programmer error
tryAllM (IOEnv thing) = IOEnv (\ env -> try (thing env))

tryMostM :: IOEnv env r -> IOEnv env (Either SomeException r)
tryMostM (IOEnv thing) = IOEnv (\ env -> tryMost (thing env))

---------------------------
unsafeInterleaveM :: IOEnv env a -> IOEnv env a
unsafeInterleaveM (IOEnv m) = IOEnv (\ env -> unsafeInterleaveIO (m env))

uninterruptibleMaskM_ :: IOEnv env a -> IOEnv env a
uninterruptibleMaskM_ (IOEnv m) = IOEnv (\ env -> uninterruptibleMask_ (m env))

----------------------------------------------------------------------
-- Alternative/MonadPlus
----------------------------------------------------------------------


#if __GLASGOW_HASKELL__ > 800
instance Alternative (IOEnv env) where
    empty   = IOEnv (const empty)
    m <|> n = IOEnv (\env -> unIOEnv m env <|> unIOEnv n env)

instance MonadPlus (IOEnv env)
#else
instance MonadPlus IO => Alternative (IOEnv env) where
      empty = mzero
      (<|>) = mplus

-- For use if the user has imported Control.Monad.Error from MTL
-- Requires UndecidableInstances
instance MonadPlus IO => MonadPlus (IOEnv env) where
    mzero = IOEnv (const mzero)
    m `mplus` n = IOEnv (\env -> unIOEnv m env `mplus` unIOEnv n env)
#endif

----------------------------------------------------------------------
-- Accessing input/output
----------------------------------------------------------------------

instance MonadIO (IOEnv env) where
    liftIO io = IOEnv (\ _ -> io)

newMutVar :: a -> IOEnv env (IORef a)
newMutVar val = liftIO (newIORef val)

writeMutVar :: IORef a -> a -> IOEnv env ()
writeMutVar var val = liftIO (writeIORef var val)

readMutVar :: IORef a -> IOEnv env a
readMutVar var = liftIO (readIORef var)

updMutVar :: IORef a -> (a -> a) -> IOEnv env ()
updMutVar var upd = liftIO (modifyIORef var upd)

-- | Atomically update the reference.  Does not force the evaluation of the
-- new variable contents.  For strict update, use 'atomicUpdMutVar''.
atomicUpdMutVar :: IORef a -> (a -> (a, b)) -> IOEnv env b
atomicUpdMutVar var upd = liftIO (atomicModifyIORef var upd)

-- | Strict variant of 'atomicUpdMutVar'.
atomicUpdMutVar' :: IORef a -> (a -> (a, b)) -> IOEnv env b
atomicUpdMutVar' var upd = do
  r <- atomicUpdMutVar var upd
  _ <- liftIO . evaluate =<< readMutVar var
  return r

----------------------------------------------------------------------
-- Accessing the environment
----------------------------------------------------------------------

getEnv :: IOEnv env env
{-# INLINE getEnv #-}
getEnv = IOEnv (\ env -> return env)

-- | Perform a computation with a different environment
setEnv :: env' -> IOEnv env' a -> IOEnv env a
{-# INLINE setEnv #-}
setEnv new_env (IOEnv m) = IOEnv (\ _ -> m new_env)

-- | Perform a computation with an altered environment
updEnv :: (env -> env') -> IOEnv env' a -> IOEnv env a
{-# INLINE updEnv #-}
updEnv upd (IOEnv m) = IOEnv (\ env -> m (upd env))
