{-# LANGUAGE GADTs, RecordWildCards, MagicHash, ScopedTypeVariables, CPP,
    UnboxedTuples, GHCForeignImportPrim, UnliftedFFITypes, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Execute Eta REPL messages.
--
-- For details on Remote Eta REPL, see Note [Remote Eta REPL] in
-- compiler/ETA/REPL.hs.
--
module Eta.Serv.Run
  ( run, redirectInterrupts, runTH, runModFinalizerRefs
  ) where

import Eta.Serv.Common
import Eta.Serv.ClassLoader
import Eta.Serv.ClassQuery
import Eta.REPL.Message
import Eta.REPL.RemoteTypes
import Eta.REPL.Utils
import qualified Language.Eta.Meta.Syntax as TH

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import GHC.Exts
import Foreign
import GHC.IO hiding ( bracket )
import System.Mem.Weak  ( deRefWeak )
import System.Exit
import Data.IORef
import Java

-- -----------------------------------------------------------------------------
-- Implement messages

run :: Message a -> IO a
run m = case m of
  AddDynamicClassPath cp -> addDynamicClassPath cp
  AddModuleClassPath cp  -> addModuleClassPath cp
  LoadClasses classNames classes -> loadClasses classNames classes
  NewInstance className methodName -> newInstance className methodName
  ResetClasses -> resetClasses
  FreeHValueRefs rs -> mapM_ freeRemoteRef rs
  EvalStmt opts r -> evalStmt opts r
  EvalString r -> evalString r
  EvalStringToString r s -> evalStringToString r s
  EvalIO r -> evalIO r
  StartTH -> startTH
  SetClassInfoPath paths -> setClassInfoPath paths
  GetClassInfo classes ->  getClassInfo classes
  _other -> error "Eta.REPL.Run.run: Invalid message"

evalStmt :: EvalOpts -> EvalExpr HValueRef -> IO (EvalStatus [HValueRef])
evalStmt opts expr = do
  io <- mkIO expr
  sandboxIO opts $ do
    rs <- unsafeCoerce# (IO $ evalStmt# (unsafeCoerce# io)) :: IO [HValue]
    mapM mkRemoteRef rs
 where
  mkIO (EvalThis href) = localRef href
  mkIO (EvalApp l r) = do
    l' <- mkIO l
    r' <- mkIO r
    mkApp l' r'

foreign import prim "eta.serv.Utils.evalStmt"
  evalStmt# :: Any -> State# s -> (# State# s, Any #)

mkApp :: HValue -> HValue -> IO HValue
mkApp e1 e2 = IO $ \s ->
  case mkApp# (unsafeCoerce# e1) (unsafeCoerce# e2) s of
    (# s1, res #) -> (# s1, unsafeCoerce# res #)

foreign import prim "eta.serv.Utils.mkApp"
  mkApp# :: Any -> Any -> State# s -> (# State# s, Any #)

evalIO :: HValueRef -> IO (EvalResult ())
evalIO r = do
  io <- localRef r
  tryEval (unsafeCoerce# (IO $ evalIO# (unsafeCoerce# io)) :: IO ())

foreign import prim "eta.serv.Utils.evalIO"
  evalIO# :: Any -> State# s -> (# State# s, Any #)

evalString :: HValueRef -> IO (EvalResult String)
evalString r = do
  io <- localRef r
  tryEval $ do
    r <- unsafeCoerce# (IO $ evalString# (unsafeCoerce# io)) :: IO String
    evaluate (force r)

foreign import prim "eta.serv.Utils.evalString"
  evalString# :: Any -> State# s -> (# State# s, Any #)

evalStringToString :: HValueRef -> String -> IO (EvalResult String)
evalStringToString r str = do
  io <- localRef r
  tryEval $ do
    r <- unsafeCoerce#
      (IO $ evalStringToString# (unsafeCoerce# io) (unsafeCoerce# str)) :: IO String
    evaluate (force r)

foreign import prim "eta.serv.Utils.evalStringToString"
  evalStringToString# :: Any -> Any -> State# s -> (# State# s, Any #)

-- When running a computation, we redirect ^C exceptions to the running
-- thread.  ToDo: we might want a way to continue even if the target
-- thread doesn't die when it receives the exception... "this thread
-- is not responding".
--
-- Careful here: there may be ^C exceptions flying around, so we start the new
-- thread blocked (forkIO inherits mask from the parent, #1048), and unblock
-- only while we execute the user's code.  We can't afford to lose the final
-- putMVar, otherwise deadlock ensues. (#1583, #1922, #1946)

sandboxIO :: EvalOpts -> IO a -> IO (EvalStatus a)
sandboxIO opts io = do
  -- We are running in uninterruptibleMask
  _breakMVar <- newEmptyMVar
  statusMVar <- newEmptyMVar
  let runIt = measureAlloc $ tryEval io
  if useSandboxThread opts
      then do
        tid <- forkIO $ do unsafeUnmask runIt >>= putMVar statusMVar
                              -- empty: can't block
        redirectInterrupts tid $ unsafeUnmask $ takeMVar statusMVar
      else
        runIt

--
-- While we're waiting for the sandbox thread to return a result, if
-- the current thread receives an asynchronous exception we re-throw
-- it at the sandbox thread and continue to wait.
--
-- This is for two reasons:
--
--  * So that ^C interrupts runStmt (e.g. in Eta REPL), allowing the
--    computation to run its exception handlers before returning the
--    exception result to the caller of runStmt.
--
--  * clients of the Eta API can terminate a runStmt in progress
--    without knowing the ThreadId of the sandbox thread (#1381)
--
-- NB. use a weak pointer to the thread, so that the thread can still
-- be considered deadlocked by the RTS and sent a BlockedIndefinitely
-- exception.  A symptom of getting this wrong is that conc033(ghci)
-- will hang.
--
redirectInterrupts :: ThreadId -> IO a -> IO a
redirectInterrupts target wait = do
  wtid <- mkWeakThreadId target
  wait `catch` \e -> do
     m <- deRefWeak wtid
     case m of
       Nothing -> wait
       Just target -> do throwTo target (e :: SomeException); wait

measureAlloc :: IO (EvalResult a) -> IO (EvalStatus a)
measureAlloc io = do
  startHeap <- getHeapMemoryUsage
  startNative <- getNativeMemoryUsage
  a <- io
  endHeap   <- getHeapMemoryUsage
  endNative <- getNativeMemoryUsage
  let allocs = fromIntegral $ abs $ (endHeap - startHeap) + (endNative - startNative)
  return (EvalComplete allocs a)

foreign import java unsafe "@static eta.serv.Utils.getHeapMemoryUsage"
  getHeapMemoryUsage :: IO Int64

foreign import java unsafe "@static eta.serv.Utils.getNativeMemoryUsage"
  getNativeMemoryUsage :: IO Int64

-- Exceptions can't be marshaled because they're dynamically typed, so
-- everything becomes a String.
tryEval :: IO a -> IO (EvalResult a)
tryEval io = do
  e <- try io
  bytes <- getOutputBytes
  case e of
    Left ex -> return (EvalException bytes (toSerializableException ex))
    Right a -> do
      return (EvalSuccess bytes a)

getOutputBytes :: IO ByteString
getOutputBytes = getOutputBytes' >>= fromByteArray

foreign import java unsafe "@static eta.serv.REPLClassLoader.getOutputBytes"
  getOutputBytes' :: IO JByteArray

toSerializableException :: SomeException -> SerializableException
toSerializableException ex
  | Just UserInterrupt <- fromException ex  = EUserInterrupt
  | Just (ec::ExitCode) <- fromException ex = (EExitCode ec)
  | otherwise = EOtherException (handleExceptionAsString ex)

startTH :: IO (RemoteRef (IORef ()))
startTH = do
  r <- unsafeCoerce# (IO startTH#)
  mkRemoteRef r

foreign import prim "eta.serv.Utils.startTH"
  startTH# :: State# s -> (# State# s, Any #)

-- | The implementation of the 'RunTH' message
runTH
  :: Pipe -- ^ Pipe leftovers
  -> RemoteRef (IORef ())
      -- ^ The TH state, created by 'startTH'
  -> HValueRef
      -- ^ The splice to run
  -> THResultType
      -- ^ What kind of splice it is
  -> Maybe TH.Loc
      -- ^ The source location
  -> IO ()
      -- ^ Returns an (encoded) result that depends on the THResultType
runTH Pipe{..} rstate rhv ty mb_loc = do
  !qstateref <- localRef rstate
  !hv <- localRef rhv
  !mbLeftovers <- readIORef pipeLeftovers
  let !serializedParts = toByteArray (LB.toStrict (encode (mbLeftovers, ty, mb_loc)))
  IO $ \s ->
    case runTH# (unsafeCoerce# qstateref) (unsafeCoerce# hv) serializedParts s of
      (# s', x #) -> (# s', unsafeCoerce# x #)

foreign import prim "eta.serv.Utils.runTH"
  runTH# :: Any -> Any -> JByteArray -> State# s -> (# State# s, Any #)

runModFinalizerRefs :: Pipe -> RemoteRef (IORef ())
                    -> [RemoteRef ()]
                    -> IO ()
runModFinalizerRefs Pipe{..} rstate qrefs = do
  mbLeftovers <- readIORef pipeLeftovers
  let !serialized = toByteArray (LB.toStrict (encode mbLeftovers))
  qstateref <- localRef rstate
  IO $ \s -> case runModFinalizerRefs# serialized (unsafeCoerce# qstateref) qrefs s of
               (# s', x #) -> (# s', unsafeCoerce# x #)

foreign import prim "eta.serv.Utils.runModFinalizerRefs" runModFinalizerRefs# ::
  JByteArray -> Any -> [RemoteRef ()] -> State# s -> (# State# s, Any #)
