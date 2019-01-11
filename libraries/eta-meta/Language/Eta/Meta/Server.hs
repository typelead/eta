{-# LANGUAGE CPP, ExistentialQuantification, DeriveGeneric, StandaloneDeriving,
             TupleSections, GADTs, InstanceSigs, ScopedTypeVariables #-}
module Language.Eta.Meta.Server
  (THMessage(..), THMsg(..), QState(..), THResult(..)
  , getTHMessage, putTHMessage, remoteTHCall, debugTHMessage
#ifdef ETA_VERSION
  , startTH, runTH, runModFinalizerRefs
#endif
  )
where

import Eta.Location
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Language.Eta.Meta.Lib.Map (Map)
import qualified Language.Eta.Meta.Lib.Map as M
import Data.Dynamic
import Language.Eta.Meta.Binary ()
import Language.Eta.Meta.Syntax as TH
import GHC.Generics
import Eta.LanguageExtensions ()
import Eta.Serialized
import Eta.REPL.RemoteTypes
import Eta.REPL.Message
import Control.Exception
import Data.Data
import Data.Maybe

#if __GLASGOW_HASKELL__ > 800 || defined(ETA_VERSION)
import qualified Control.Monad.Fail as Fail
#endif

#ifdef ETA_VERSION
import qualified GHC.IO.FD as FD
import GHC.IO.Handle.Internals
import System.IO
import GHC.IO.Handle.Types hiding (getState)
import GHC.Desugar
import Unsafe.Coerce
import System.Posix.Types
import Java hiding (Map)
import Eta.REPL.Utils
import Data.IORef
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LB
import Control.DeepSeq
import Control.Concurrent.MVar
import System.IO.Unsafe
#endif

-- | The server-side Template Metaprogramming state.  This is created by the
-- StartTH message.  A new one is created per module that Eta typechecks.
data QState = QState
  { qsMap        :: Map TypeRep Dynamic
       -- ^ persistent data between splices in a module
  , qsLocation   :: Maybe Loc
       -- ^ location for current splice, if any
  , qsPipe       :: Pipe
       -- ^ pipe to communicate with Eta
  }

instance Show QState where show _ = "<QState>"

data THResult a
  = THException String
  | THComplete a
  deriving (Generic, Show)

instance Binary a => Binary (THResult a)

-- | Messages sent back to GHC from GHCi.TH, to implement the methods
-- of 'Quasi'.  For an overview of how TH works with Remote Eta REPL, see
-- Note [Remote Template Haskell] in GHCi.TH.
data THMessage a where
  NewName :: String -> THMessage (THResult TH.Name)
  Report :: Bool -> String -> THMessage (THResult ())
  LookupName :: Bool -> String -> THMessage (THResult (Maybe TH.Name))
  Reify :: TH.Name -> THMessage (THResult TH.Info)
  ReifyFixity :: TH.Name -> THMessage (THResult (Maybe TH.Fixity))
  ReifyInstances :: TH.Name -> [TH.Type] -> THMessage (THResult [TH.Dec])
  ReifyRoles :: TH.Name -> THMessage (THResult [TH.Role])
  ReifyAnnotations :: TH.AnnLookup -> TypeRep
    -> THMessage (THResult [ByteString])
  ReifyModule :: TH.Module -> THMessage (THResult TH.ModuleInfo)
  ReifyConStrictness :: TH.Name -> THMessage (THResult [TH.DecidedStrictness])

  AddDependentFile :: FilePath -> THMessage (THResult ())
  AddTempFile :: String -> THMessage (THResult FilePath)
  AddModFinalizer :: RemoteRef (TH.Q ()) -> THMessage (THResult ())
  AddCorePlugin :: String -> THMessage (THResult ())
  AddTopDecls :: [TH.Dec] -> THMessage (THResult ())
  IsExtEnabled :: Extension -> THMessage (THResult Bool)
  ExtsEnabled :: THMessage (THResult [Extension])

  StartRecover :: THMessage ()
  EndRecover :: Bool -> THMessage ()

  -- Indicates that this RunTH is finished, and the next message
  -- will be the result of RunTH (a QResult).
  RunTHDone :: THMessage ()

deriving instance Show (THMessage a)

data THMsg = forall a. (Binary a, Show a) => THMsg (THMessage a)

getTHMessage :: Get THMsg
getTHMessage = do
  b <- getWord8
  case b of
    0  -> THMsg <$> NewName <$> get
    1  -> THMsg <$> (Report <$> get <*> get)
    2  -> THMsg <$> (LookupName <$> get <*> get)
    3  -> THMsg <$> Reify <$> get
    4  -> THMsg <$> ReifyFixity <$> get
    5  -> THMsg <$> (ReifyInstances <$> get <*> get)
    6  -> THMsg <$> ReifyRoles <$> get
    7  -> THMsg <$> (ReifyAnnotations <$> get <*> get)
    8  -> THMsg <$> ReifyModule <$> get
    9  -> THMsg <$> ReifyConStrictness <$> get
    10 -> THMsg <$> AddDependentFile <$> get
    -- 11 -> THMsg <$> AddTempFile <$> get
    12 -> THMsg <$> AddTopDecls <$> get
    13 -> THMsg <$> (IsExtEnabled <$> get)
    14 -> THMsg <$> return ExtsEnabled
    15 -> THMsg <$> return StartRecover
    16 -> THMsg <$> EndRecover <$> get
    17 -> return (THMsg RunTHDone)
    18 -> THMsg <$> AddModFinalizer <$> get
    -- 19  -> THMsg <$> AddCorePlugin <$> get
    _  -> error $ "getTHMessage: Invalid tag: " ++ show b

putTHMessage :: THMessage a -> Put
putTHMessage m = case m of
  NewName a                   -> putWord8 0  >> put a
  Report a b                  -> putWord8 1  >> put a >> put b
  LookupName a b              -> putWord8 2  >> put a >> put b
  Reify a                     -> putWord8 3  >> put a
  ReifyFixity a               -> putWord8 4  >> put a
  ReifyInstances a b          -> putWord8 5  >> put a >> put b
  ReifyRoles a                -> putWord8 6  >> put a
  ReifyAnnotations a b        -> putWord8 7  >> put a >> put b
  ReifyModule a               -> putWord8 8  >> put a
  ReifyConStrictness a        -> putWord8 9  >> put a
  AddDependentFile a          -> putWord8 10 >> put a
  AddTempFile a               -> putWord8 11 >> put a
  AddTopDecls a               -> putWord8 12 >> put a
  IsExtEnabled a              -> putWord8 13 >> put a
  ExtsEnabled                 -> putWord8 14
  StartRecover                -> putWord8 15
  EndRecover a                -> putWord8 16 >> put a
  RunTHDone                   -> putWord8 17
  AddModFinalizer a           -> putWord8 18 >> put a
  -- AddForeignFilePath lang a   -> putWord8 19 >> put lang >> put a
  AddCorePlugin a             -> putWord8 20 >> put a

debugTHMessage :: THMessage a -> String
debugTHMessage m = show m

-- | The monad in which we run TH computations on the server
newtype GHCiQ a = GHCiQ { runGHCiQ :: QState -> IO (a, QState) }

-- | The exception thrown by "fail" in the GHCiQ monad
data GHCiQException = GHCiQException QState String
  deriving Show

instance Exception GHCiQException

instance Functor GHCiQ where
  fmap f (GHCiQ s) = GHCiQ $ fmap (\(x,s') -> (f x,s')) . s

instance Applicative GHCiQ where
  f <*> a = GHCiQ $ \s ->
    do (f',s')  <- runGHCiQ f s
       (a',s'') <- runGHCiQ a s'
       return (f' a', s'')
  pure x = GHCiQ (\s -> return (x,s))

instance Monad GHCiQ where
  m >>= f = GHCiQ $ \s ->
    do (m', s')  <- runGHCiQ m s
       (a,  s'') <- runGHCiQ (f m') s'
       return (a, s'')
#if __GLASGOW_HASKELL__ < 800 && !defined(ETA_VERSION)
  fail err = GHCiQ $ \s -> throwIO (GHCiQException s err)
#else
  fail = Fail.fail

instance Fail.MonadFail GHCiQ where
  fail err  = GHCiQ $ \s -> throwIO (GHCiQException s err)
#endif

getState :: GHCiQ QState
getState = GHCiQ $ \s -> return (s,s)

noLoc :: TH.Loc
noLoc = TH.Loc "<no file>" "<no package>" "<no module>" (0,0) (0,0)

instance TH.Quasi GHCiQ where
  qNewName str = ghcCmd (NewName str)
  qRunIO m = GHCiQ $ \s -> fmap (,s) m
  qReport isError msg = ghcCmd (Report isError msg)

  -- See Note [TH recover with -fexternal-interpreter] in TcSplice
  qRecover (GHCiQ h) (GHCiQ a) = GHCiQ $ \s -> (do
    remoteTHCall (qsPipe s) StartRecover
    (r, s') <- a s
    remoteTHCall (qsPipe s) (EndRecover False)
    return (r,s'))
      `catch`
       \GHCiQException{} -> remoteTHCall (qsPipe s) (EndRecover True) >> h s
  qLookupName isType occ = ghcCmd (LookupName isType occ)
  qReify name = ghcCmd (Reify name)
  qReifyFixity name = ghcCmd (ReifyFixity name)
  qReifyInstances name tys = ghcCmd (ReifyInstances name tys)
  qReifyRoles name = ghcCmd (ReifyRoles name)

  -- To reify annotations, we send Eta the AnnLookup and also the
  -- TypeRep of the thing we're looking for, to avoid needing to
  -- serialize irrelevant annotations.
  qReifyAnnotations :: forall a. Data a => TH.AnnLookup -> GHCiQ [a]
  qReifyAnnotations lookup' =
    map (deserializeWithData . B.unpack) <$>
      ghcCmd (ReifyAnnotations lookup' typerep)
    where typerep = typeOf (undefined :: a)

  qReifyModule m = ghcCmd (ReifyModule m)
  qReifyConStrictness name = ghcCmd (ReifyConStrictness name)
  qLocation = fromMaybe noLoc . qsLocation <$> getState
  qAddDependentFile file = ghcCmd (AddDependentFile file)
  -- qAddTempFile suffix = ghcCmd (AddTempFile suffix)
  qAddTopDecls decls = ghcCmd (AddTopDecls decls)
  -- qAddForeignFilePath lang fp = ghcCmd (AddForeignFilePath lang fp)
  qAddModFinalizer fin = GHCiQ (\s -> mkRemoteRef fin >>= return . (, s)) >>=
                         ghcCmd . AddModFinalizer
  -- qAddCorePlugin str = ghcCmd (AddCorePlugin str)
  qGetQ = GHCiQ $ \s ->
    let lookup' :: forall a. Typeable a => Map TypeRep Dynamic -> Maybe a
        lookup' m = fromDynamic =<< M.lookup (typeOf (undefined::a)) m
    in return (lookup' (qsMap s), s)
  qPutQ k = GHCiQ $ \s ->
    return ((), s { qsMap = M.insert (typeOf k) (toDyn k) (qsMap s) })
  qIsExtEnabled x = ghcCmd (IsExtEnabled x)
  qExtsEnabled = ghcCmd ExtsEnabled

-- | Send a 'THMessage' to GHC and return the result.
ghcCmd :: Binary a => THMessage (THResult a) -> GHCiQ a
ghcCmd m = GHCiQ $ \s -> do
  r <- remoteTHCall (qsPipe s) m
  case r of
    THException str -> throwIO (GHCiQException s str)
    THComplete  res -> return  (res, s)

remoteTHCall :: Binary a => Pipe -> THMessage a -> IO a
remoteTHCall pipe msg = do
  writePipe pipe (putTHMessage msg)
  readPipe pipe get

#ifdef ETA_VERSION
-- | Create a new instance of 'QState'
initQState :: Pipe -> QState
initQState p = QState M.empty Nothing p

-- | The implementation of the 'StartTH' message: create
-- a new IORef QState, and return a RemoteRef to it.
startTH :: IO (IORef QState)
startTH = newIORef (initQState (error "startTH: no pipe"))

-- | Runs the mod finalizers.
--
-- The references must be created on the caller process.
runModFinalizerRefs :: JByteArray -> IORef QState
                    -> [RemoteRef ()]
                    -> IO ()
runModFinalizerRefs leftovers' qstateref qs' = do
  serialized <- fromByteArray leftovers'
  let leftovers = decode (LB.fromStrict serialized)
  pipe <- mkPipe leftovers
  qs   <- unsafeCoerce (mapM localRef qs') :: IO [TH.Q ()]
  mapM_ freeRemoteRef qs'
  wrapRunTH pipe $ do
    qstate <- readIORef qstateref
    _ <- runGHCiQ (TH.runQ $ sequence_ qs) qstate { qsPipe = pipe }
    return ()

-- | The implementation of the 'RunTH' message
runTH
  :: IORef QState
      -- ^ The TH state, created by 'startTH'
  -> TH.Q ()
      -- ^ The splice to run
  -> JByteArray -- ^ Serialized form of (Pipe leftovers, THResultType, Maybe TH.Loc)
  -> IO ()
      -- ^ Returns an (encoded) result that depends on the THResultType

runTH rstate hv serialized = do
  serialized' <- fromByteArray serialized
  let (leftovers, ty, mb_loc) = decode (LB.fromStrict serialized')
  pipe <- mkPipe leftovers
  wrapRunTH pipe $ do
    let runTHQ' :: forall a. Binary a => TH.Q a -> IO ByteString
        runTHQ' = runTHQ pipe rstate mb_loc
    case ty of
      THExp  -> runTHQ' (unsafeCoerce hv :: TH.Q TH.Exp)
      THPat  -> runTHQ' (unsafeCoerce hv :: TH.Q TH.Pat)
      THType -> runTHQ' (unsafeCoerce hv :: TH.Q TH.Type)
      THDec  -> runTHQ' (unsafeCoerce hv :: TH.Q [TH.Dec])
      THAnnWrapper ->
        case unsafeCoerce hv of
          AnnotationWrapper thing -> return $!
            LB.toStrict (runPut (put (toSerialized serializeWithData thing)))

-- | Run a Q computation.
runTHQ
  :: Binary a => Pipe -> IORef QState -> Maybe TH.Loc -> TH.Q a
  -> IO ByteString
runTHQ pipe qstateref mb_loc ghciq = do
  qstate <- readIORef qstateref
  let st = qstate { qsLocation = mb_loc, qsPipe = pipe }
  (r,new_state) <- runGHCiQ (TH.runQ ghciq) st
  writeIORef qstateref new_state
  return $! LB.toStrict (runPut (put r))

-- Run some TH code, which may interact with GHC by sending
-- THMessage requests, and then finally send RunTHDone followed by a
-- QResult.  For an overview of how TH works with Eta Serv, see
-- Note [Remote Template Haskell] in libraries/eta-repl/Eta/REPL/TH.hs.
wrapRunTH :: forall a. (Binary a, Show a) => Pipe -> IO a -> IO ()
wrapRunTH pipe ioAction = do
  r <- try ioAction
  writePipe pipe (putTHMessage RunTHDone)
  case r of
    Left e
      | Just (GHCiQException _ err) <- fromException e  ->
          write (QFail err)
      | otherwise -> do
          str <- showException e
          write (QException str)
    Right a -> write (QDone a)
  where write x = writePipe pipe (put x)

-- carefully when showing an exception, there might be other exceptions
-- lurking inside it.  If so, we return the inner exception instead.
showException :: SomeException -> IO String
showException e0 = do
    r <- try $ evaluate (force (show (e0::SomeException)))
    case r of
      Left e -> showException e
      Right str -> return str

mkPipe :: Maybe ByteString -> IO Pipe
mkPipe leftovers' = do
  leftovers <- newIORef leftovers'
  return $ Pipe { pipeRead      = pipeReadHandle
                , pipeWrite     = pipeWriteHandle
                , pipeLeftovers = leftovers }

-- These handles bind to the non-sandboxed versions of stdin & stdout to directly
-- communicate with the Eta compiler.
pipeReadHandle :: Handle
{-# NOINLINE pipeReadHandle #-}
pipeReadHandle = unsafePerformIO $ do
   fd <- mkFD oldStdIn ReadMode
   mkHandle fd "<internal:stdin>" ReadHandle Nothing
                (Just latin1)
                nativeNewlineMode{-translate newlines-}
                (Just stdHandleFinalizer) Nothing

foreign import java unsafe "@static eta.serv.REPLClassLoader.getOldStdIn"
  oldStdIn :: Channel

pipeWriteHandle :: Handle
{-# NOINLINE pipeWriteHandle #-}
pipeWriteHandle = unsafePerformIO $ do
   fd <- mkFD oldStdOut WriteMode
   mkHandle fd "<internal:stdout>" WriteHandle (Just LineBuffering)
                (Just latin1)
                nativeNewlineMode{-translate newlines-}
                (Just stdHandleFinalizer) Nothing

foreign import java unsafe "@static eta.serv.REPLClassLoader.getOldStdOut"
  oldStdOut :: Channel

-- NOTE: Taken from GHC.IO.Handle.FD in base
stdHandleFinalizer :: FilePath -> MVar Handle__ -> IO ()
stdHandleFinalizer fp m = do
  h_ <- takeMVar m
  flushWriteBuffer h_
  case haType h_ of
      ClosedHandle -> return ()
      _other       -> closeTextCodecs h_
  putMVar m (ioe_finalizedHandle fp)

mkFD :: Channel -> IOMode -> IO FD.FD
mkFD c mode = fmap fst $ FD.mkFD c Nothing mode Nothing False
#endif
