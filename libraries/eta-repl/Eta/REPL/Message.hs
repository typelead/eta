{-# LANGUAGE CPP, GADTs, DeriveGeneric, DeriveDataTypeable,
    StandaloneDeriving, ScopedTypeVariables, PatternSynonyms,
    GeneralizedNewtypeDeriving, ExistentialQuantification, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}

-- |
-- Remote Eta REPL message types and serialization.
--
-- For details on Remote Eta REPL, see Note [Remote Eta REPL] in
-- compiler/ETA/REPL.hs.
--
module Eta.REPL.Message
  ( Message(..), Msg(..)
  , QResult(..), JResult(..)
  , EvalStatus_(..), EvalStatus, EvalResult(..), EvalOpts(..), EvalExpr(..)
  , SerializableException(..)
  , MessageParseFailure(..)
  , fromSerializableException
  , THResultType(..)
  , ResumeContext(..)
  , getMessage, putMessage, debugMessage
  , Pipe(..), remoteCall, readPipe, writePipe
  ) where

import Eta.REPL.RemoteTypes
import Eta.REPL.ClassInfo

import Eta.Location
import Control.Concurrent
import Control.Exception
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Dynamic
import Data.IORef
import GHC.Generics
import System.Exit
import System.IO
import System.IO.Error
import Data.Int

-- -----------------------------------------------------------------------------
-- The RPC protocol between Eta and the interactive server

-- | A @Message a@ is a message that returns a value of type @a@.
-- These are requests sent from Eta to the server.
data Message a where

  -- Exit the iserv process
  Shutdown :: Message ()

  -- Add a list of files to the dynamic classpath of classloader.
  AddDynamicClassPath :: [String] -> Message ()

  -- Sets the classpath of child classloader.
  AddModuleClassPath :: [String] -> Message ()

  -- Load a list of class names and class contents into memory and link them.
  LoadClasses :: [String] -> [ByteString] -> Message ()

  -- Create a new instance of the supplied class
  NewInstance :: String -> String -> Message HValueRef

  -- Resets the child REPLClassLoader
  ResetClasses :: Message ()

  -- Release 'HValueRef's
  FreeHValueRefs :: [HValueRef] -> Message ()

  -- Evaluate a statement
  EvalStmt
    :: EvalOpts
    -> EvalExpr HValueRef {- IO [a] -}
    -> Message (EvalStatus [HValueRef]) {- [a] -}

  -- Evaluate something of type @IO String@
  EvalString
    :: HValueRef {- IO String -}
    -> Message (EvalResult String)

  -- Evaluate something of type @String -> IO String@
  EvalStringToString
    :: HValueRef {- String -> IO String -}
    -> String
    -> Message (EvalResult String)

  -- Evaluate something of type @IO ()@
  EvalIO
   :: HValueRef {- IO a -}
   -> Message (EvalResult ())

  -- Template Metaprogramming

  -- Start a new TH module, return a state token of type QState
  StartTH :: Message (RemoteRef (IORef ()))

  -- Evaluate a TH computation.
  --
  -- Returns a ByteString, because we have to force the result
  -- before returning it to ensure there are no errors lurking
  -- in it. The TH types don't have NFData instances, and even if
  -- they did, we have to serialize the value anyway, so we might
  -- as well serialize it to force it.
  RunTH
   :: RemoteRef (IORef ()) {- IORef QState -}
   -> HValueRef {- e.g. TH.Q TH.Exp -}
   -> THResultType
   -> Maybe Loc
   -> Message (QResult ByteString)

  -- Run the given mod finalizers.
  RunModFinalizers :: RemoteRef (IORef ()) {- IORef QState -}
                   -> [RemoteRef ()]       {- RemoteRef (TH.Q ()) -}
                   -> Message (QResult ())

  -- Set classpath for class info
  SetClassInfoPath :: [String] -> Message ()

  -- Get info of given classes
  GetClassInfo :: [String] -> Message (JResult ([String], [PreClassInfo]))

deriving instance Show (Message a)

-- | Template Metaprogramming return values
data QResult a
  = QDone a
    -- ^ RunTH finished successfully; return value follows
  | QException String
    -- ^ RunTH threw an exception
  | QFail String
    -- ^ RunTH called 'fail'
  deriving (Generic, Show)

instance Binary a => Binary (QResult a)

-- |  return values
data JResult a
  = JDone a
    -- ^ Java finished successfully; return value follows
  | JException String
    -- ^ Class query threw an exception
  deriving (Generic, Show)

instance Binary a => Binary (JResult a)

data EvalOpts = EvalOpts
  { useSandboxThread :: Bool
  , singleStep :: Bool
  , breakOnException :: Bool
  , breakOnError :: Bool
  }
  deriving (Generic, Show)

instance Binary EvalOpts

data ResumeContext a = ResumeContext
  { resumeBreakMVar :: MVar ()
  , resumeStatusMVar :: MVar (EvalStatus a)
  , resumeThreadId :: ThreadId
  }

-- | We can pass simple expressions to EvalStmt, consisting of values
-- and application.  This allows us to wrap the statement to be
-- executed in another function, which is used by GHCi to implement
-- :set args and :set prog.  It might be worthwhile to extend this
-- little language in the future.
data EvalExpr a
  = EvalThis a
  | EvalApp (EvalExpr a) (EvalExpr a)
  deriving (Generic, Show)

instance Binary a => Binary (EvalExpr a)

type EvalStatus a = EvalStatus_ a a

data EvalStatus_ a b
  = EvalComplete Word64 (EvalResult a)
  | EvalBreak Bool
       HValueRef{- AP_STACK -}
       Int {- break index -}
       Int {- uniq of ModuleName -}
       (RemoteRef (ResumeContext b))
      --  (RemotePtr CostCentreStack) -- Cost centre stack
  deriving (Generic, Show)

instance Binary a => Binary (EvalStatus_ a b)

data EvalResult a
  = EvalException ByteString SerializableException
  | EvalSuccess ByteString a
  deriving (Generic, Show)

instance Binary a => Binary (EvalResult a)

-- SomeException can't be serialized because it contains dynamic
-- types.  However, we do very limited things with the exceptions that
-- are thrown by interpreted computations:
--
-- * We print them, e.g. "*** Exception: <something>"
-- * UserInterrupt has a special meaning
-- * In ghc -e, exitWith should exit with the appropriate exit code
--
-- So all we need to do is distinguish UserInterrupt and ExitCode, and
-- all other exceptions can be represented by their 'show' string.
--
data SerializableException
  = EUserInterrupt
  | EExitCode ExitCode
  | EOtherException String
  deriving (Generic, Show)

fromSerializableException :: SerializableException -> SomeException
fromSerializableException EUserInterrupt = toException UserInterrupt
fromSerializableException (EExitCode c) = toException c
fromSerializableException (EOtherException str) = toException (ErrorCall str)

instance Binary SerializableException

instance Binary ExitCode where
  put ExitSuccess      = putWord8 0
  put (ExitFailure ec) = putWord8 1 >> put ec
  get = do
    w <- getWord8
    case w of
      0 -> pure ExitSuccess
      _ -> ExitFailure <$> get

data THResultType = THExp | THPat | THType | THDec | THAnnWrapper
  deriving (Enum, Show, Generic)

instance Binary THResultType

data Msg = forall a . (Binary a, Show a) => Msg (Message a)

instance Binary Loc

getMessage :: Get Msg
getMessage = do
  b <- getWord8
  case b of
    0  -> Msg <$> return Shutdown
    1  -> Msg <$> AddDynamicClassPath <$> get
    2  -> Msg <$> AddModuleClassPath <$> get
    3  -> Msg <$> (LoadClasses <$> get <*> get)
    4  -> Msg <$> (NewInstance <$> get <*> get)
    5  -> return $ Msg ResetClasses
    6  -> Msg <$> FreeHValueRefs <$> get
    7  -> Msg <$> (EvalStmt <$> get <*> get)
    8  -> Msg <$> (EvalString <$> get)
    9  -> Msg <$> (EvalStringToString <$> get <*> get)
    10 -> Msg <$> (EvalIO <$> get)
    11  -> Msg <$> return StartTH
    12 -> Msg <$> (RunTH <$> get <*> get <*> get <*> get)
    13 -> Msg <$> (RunModFinalizers <$> get <*> get)
    14 -> Msg <$> SetClassInfoPath <$> get
    15 -> Msg <$> GetClassInfo <$> get
    _  -> error $ "getMessage: Invalid message tag: " ++ show b

putMessage :: Message a -> Put
putMessage m = case m of
  Shutdown                    -> putWord8 0
  AddDynamicClassPath a       -> putWord8 1  >> put a
  AddModuleClassPath a        -> putWord8 2  >> put a
  LoadClasses a b             -> putWord8 3  >> put a >> put b
  NewInstance a b             -> putWord8 4  >> put a >> put b
  ResetClasses                -> putWord8 5
  FreeHValueRefs val          -> putWord8 6  >> put val
  EvalStmt opts val           -> putWord8 7  >> put opts >> put val
  EvalString val              -> putWord8 8  >> put val
  EvalStringToString str val  -> putWord8 9  >> put str >> put val
  EvalIO val                  -> putWord8 10 >> put val
  StartTH                     -> putWord8 11
  RunTH st q loc ty           -> putWord8 12 >> put st >> put q >> put loc >> put ty
  RunModFinalizers a b        -> putWord8 13 >> put a >> put b
  SetClassInfoPath a          -> putWord8 14 >> put a
  GetClassInfo a              -> putWord8 15 >> put a

debugMessage :: Message a -> String
debugMessage m = case m of
  LoadClasses a _ -> "LoadClasses " ++ show a
  _               -> show m

-- -----------------------------------------------------------------------------
-- Reading/writing messages

data Pipe = Pipe
  { pipeRead :: Handle
  , pipeWrite ::  Handle
  , pipeLeftovers :: IORef (Maybe ByteString)
  }

remoteCall :: Binary a => Pipe -> Message a -> IO a
remoteCall pipe msg = do
  writePipe pipe (putMessage msg)
  readPipe pipe get

writePipe :: Pipe -> Put -> IO ()
writePipe Pipe{..} put
  | LB.null bs = return ()
  | otherwise  = do
    LB.hPut pipeWrite bs
    hFlush pipeWrite
 where
  bs = runPut put

readPipe :: Pipe -> Get a -> IO a
readPipe Pipe{..} get = do
  leftovers <- readIORef pipeLeftovers
  m <- getBin pipeRead get leftovers
  case m of
    Nothing -> throw $
      mkIOError eofErrorType "Eta.REPL.Message.remoteCall" (Just pipeRead) Nothing
    Just (result, new_leftovers) -> do
      writeIORef pipeLeftovers new_leftovers
      return result

data MessageParseFailure = MessageParseFailure String String Int64
  deriving (Show, Typeable)

instance Exception MessageParseFailure

getBin
  :: Handle -> Get a -> Maybe ByteString
  -> IO (Maybe (a, Maybe ByteString))

getBin h get leftover = go leftover (runGetIncremental get)
 where
   go Nothing (Done leftover _ msg) =
     return (Just (msg, if B.null leftover then Nothing else Just leftover))
   go _ Done{} = throwIO (ErrorCall "getBin: Done with leftovers")
   go (Just leftover) (Partial fun) = do
     go Nothing (fun (Just leftover))
   go Nothing (Partial fun) = do
     -- debug "before hGetSome"
     b <- B.hGetSome h (32*1024)
     -- debug $ "hGetSome: " ++ show (B.length b)
     if B.null b
        then return Nothing
        else go Nothing (fun (Just b))
   go _lft (Fail rest off str) =
     throwIO (MessageParseFailure str (show rest) off)
