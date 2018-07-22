{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , BangPatterns
           , AutoDeriveTypeable
           , MagicHash
  #-}
{-# OPTIONS_GHC -Wno-identities #-}
-- Whether there are identities depends on the platform
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.FD
-- Copyright   :  (c) The University of Glasgow, 1994-2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Raw read/write operations on file descriptors
--
-----------------------------------------------------------------------------

module GHC.IO.FD (
        FD(..), FDType(..),
        openFile, mkFD, release,
        setNonBlockingMode,
        readRawBufferPtr, readRawBufferPtrNoBlock, writeRawBufferPtr,
        fdPath, fdChannel,
        stdin, stdout, stderr,
        rw_flags
    ) where

import GHC.Base
import GHC.Num
import GHC.Real
import GHC.Show
import GHC.Enum
import Data.Typeable

import GHC.IO
import GHC.IO.IOMode
import GHC.IO.Buffer
import GHC.IO.BufferedIO
import qualified GHC.IO.Device
import GHC.IO.Device (SeekMode(..), IODeviceType(..))
import GHC.Conc.IO
import GHC.IO.Exception
#ifdef mingw32_HOST_OS
import GHC.Windows
#endif

import Foreign
import Foreign.C
import qualified System.Posix.Internals
import System.Posix.Internals hiding (FD, setEcho, getEcho)
import System.Posix.Types

#ifdef mingw32_HOST_OS
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

c_DEBUG_DUMP :: Bool
c_DEBUG_DUMP = False

-- -----------------------------------------------------------------------------
-- The file-descriptor IO device

data FD = FD {
  fdFD            :: !FDType,
  fdIsNonBlocking :: Bool
 }
 deriving Typeable

data FDType = FDGeneric !Channel
            | FDFile !Path !FileChannel

instance Show FD where
  show fd = errorWithoutStackTrace "Show[FD]: No Show instance yet."

instance GHC.IO.Device.RawIO FD where
  read             = fdRead
  readNonBlocking  = fdReadNonBlocking
  write            = fdWrite
  writeNonBlocking = fdWriteNonBlocking

instance GHC.IO.Device.IODevice FD where
  ready         = ready
  close         = close
  isTerminal    = isTerminal
  isSeekable    = isSeekable
  seek          = seek
  tell          = tell
  getSize       = getSize
  setSize       = setSize
  setEcho       = setEcho
  getEcho       = getEcho
  setRaw        = setRaw
  devType       = devType
  dup           = dup
  dup2          = dup2

fdPath :: FD -> Maybe Path
fdPath fd
  | FDFile p _ <- fdFD fd
  = Just p
  | otherwise = Nothing

fdChannel :: FD -> Channel
fdChannel fd =
  case fdFD fd of
    FDGeneric ch -> ch
    FDFile _ fc  -> superCast fc

withChannel :: FD -> (Channel -> IO a) -> IO a
withChannel fd act = act (fdChannel fd)

withFileChannel :: String -> FD -> (Path -> FileChannel -> IO a) -> IO a
withFileChannel msg fd act
  | FDFile p fc <- fdFD fd
  = act p fc
  | otherwise = errorWithoutStackTrace $ msg ++ ": Not a file channel"

withFileChannelOrElse :: String -> FD -> a -> (Path -> FileChannel -> IO a) -> IO a
withFileChannelOrElse msg fd def act
  | FDFile p fc <- fdFD fd
  = act p fc
  | otherwise = return def

-- We used to use System.Posix.Internals.dEFAULT_BUFFER_SIZE, which is
-- taken from the value of BUFSIZ on the current platform.  This value
-- varies too much though: it is 512 on Windows, 1024 on OS X and 8192
-- on Linux.  So let's just use a decent size on every platform:
dEFAULT_FD_BUFFER_SIZE :: Int
dEFAULT_FD_BUFFER_SIZE = 8096

instance BufferedIO FD where
  newBuffer _dev state = newByteBuffer dEFAULT_FD_BUFFER_SIZE state
  fillReadBuffer    fd buf = readBuf' fd buf
  fillReadBuffer0   fd buf = readBufNonBlocking fd buf
  flushWriteBuffer  fd buf = writeBuf' fd buf
  flushWriteBuffer0 fd buf = writeBufNonBlocking fd buf

readBuf' :: FD -> Buffer Word8 -> IO (Int, Buffer Word8)
readBuf' fd buf = do
  when c_DEBUG_DUMP $
      puts ("readBuf fd=" ++ show fd ++ " " ++ summaryBuffer buf ++ "\n")
  (r,buf') <- readBuf fd buf
  when c_DEBUG_DUMP $
      puts ("after: " ++ summaryBuffer buf' ++ "\n")
  return (r,buf')

writeBuf' :: FD -> Buffer Word8 -> IO (Buffer Word8)
writeBuf' fd buf = do
  when c_DEBUG_DUMP $
      puts ("writeBuf fd=" ++ show fd ++ " " ++ summaryBuffer buf ++ "\n")
  writeBuf fd buf

-- -----------------------------------------------------------------------------
-- opening files

-- | Open a file and make an 'FD' for it.  Truncates the file to zero
-- size when the `IOMode` is `WriteMode`.
openFile
  :: FilePath -- ^ file to open
  -> IOMode   -- ^ mode in which to open the file
  -> Bool     -- ^ open the file in non-blocking mode?
  -> IO (FD,IODeviceType)

openFile filepath iomode non_blocking = do

  let oflags = case iomode of
                 ReadMode      -> read_flags
                 WriteMode     -> write_flags
                 ReadWriteMode -> rw_flags
                 AppendMode    -> append_flags
  f <- getPath filepath
  fd <- if non_blocking then c_open      f oflags 0o666
                        else c_safe_open f oflags 0o666

  (fD, fd_type) <- mkFD (superCast fd) (Just f) iomode Nothing non_blocking
                  `catchAny` \e -> do _ <- c_close (superCast fd)
                                      throwIO e

  -- we want to truncate() if this is an open in WriteMode, but only
  -- if the target is a RegularFile.  ftruncate() fails on special files
  -- like /dev/null.
  when (iomode == WriteMode && fd_type == RegularFile) $
    setSize fD 0

  return (fD,fd_type)

std_flags, output_flags, read_flags, write_flags, rw_flags,
    append_flags, nonblock_flags :: [StandardOpenOption]
std_flags    = []
output_flags = std_flags    ++ [o_CREATE]
read_flags   = std_flags    ++ [o_READ]
write_flags  = output_flags ++ [o_WRITE]
rw_flags     = output_flags ++ [o_READ, o_WRITE]
append_flags = write_flags  ++ [o_APPEND1]
nonblock_flags = [] -- o_NONBLOCK TODO: Handle this


-- | Make a 'FD' from an existing file descriptor.  Fails if the FD
-- refers to a directory.  If the FD refers to a file, `mkFD` locks
-- the file according to the Haskell 2010 single writer/multiple reader
-- locking semantics (this is why we need the `IOMode` argument too).
mkFD :: Channel
     -> Maybe Path
     -> IOMode
     -> Maybe (IODeviceType, Object)
     -- the results of fdStat if we already know them, or we want
     -- to prevent fdToHandle_stat from doing its own stat.
     -- These are used for:
     --   - we fail if the FD refers to a directory
     --   - if the FD refers to a file, we lock it using (cdev,cino)
     -> Bool
     -> IO (FD,IODeviceType)

mkFD ch mPath iomode mb_stat non_block = do

  (fd_type, key) <-
      case mb_stat of
        Nothing   -> case mPath of
          Just path -> fdStat path
          Nothing   -> return (RawDevice, undefined)
        Just stat -> return stat

  let write
        | iomode == ReadMode = False
        | otherwise = True

  case fd_type of
    Directory -> ioException (IOError Nothing InappropriateType "openFile"
                              "is a directory" Nothing Nothing)

    -- regular files need to be locked
    RegularFile -> do
      locked <- lockFile key write
      when (not locked) $
          ioException (IOError Nothing ResourceBusy "openFile"
                        "file is locked" Nothing Nothing)

    _other_type -> return ()
  let fd
        | Just path <- mPath = FDFile path (unsafeCast ch)
        | otherwise = FDGeneric ch

  return ( FD { fdFD            = fd
              , fdIsNonBlocking = non_block }
         , fd_type )

-- -----------------------------------------------------------------------------
-- Standard file descriptors

stdFD :: Channel -> FD
stdFD ch = FD { fdFD = FDGeneric ch
              , fdIsNonBlocking = False }

foreign import java unsafe "@static eta.base.Utils.getStdIn"  _stdin  :: Channel
foreign import java unsafe "@static eta.base.Utils.getStdErr" _stderr :: Channel

stdin, stdout, stderr :: FD
stdin  = stdFD _stdin
stdout = stdFD _stdout
stderr = stdFD _stderr

-- -----------------------------------------------------------------------------
-- Operations on file descriptors

close :: FD -> IO ()
close fd =
  do -- release the lock *first*, because otherwise if we're preempted
     -- after closing but before releasing, the FD may have been reused.
     -- (#7646)
     release fd

     closeFdWith c_close (fdChannel fd)

release :: FD -> IO ()
release fd
  | Just p <- fdPath fd
  = fdKey p >>= unlockFile >> return ()
  | otherwise = return ()

isSeekable :: FD -> IO Bool
isSeekable fd = do
  t <- devType fd
  return $ t == RegularFile

seek :: FD -> SeekMode -> Integer -> IO ()
seek fd mode off = withFileChannel "seek" fd $ \_ fc ->
     c_lseek fc (fromIntegral off) seektype >> return ()
 where
    seektype :: CInt
    seektype = case mode of
                   AbsoluteSeek -> sEEK_SET
                   RelativeSeek -> sEEK_CUR
                   SeekFromEnd  -> sEEK_END

tell :: FD -> IO Integer
tell fd = withFileChannel "tell" fd $ \_ fc ->
  fromIntegral `fmap` c_lseek fc 0 sEEK_CUR

getSize :: FD -> IO Integer
getSize fd = withFileChannelOrElse "getSize" fd (-1) $ \p _ ->
  fdFileSize p

setSize :: FD -> Integer -> IO ()
setSize fd size = withFileChannel "setSize" fd $ \_ fc ->
  c_ftruncate fc (fromIntegral size) >> return ()

devType :: FD -> IO IODeviceType
devType fd = withFileChannel "devType" fd $ \p _ ->
  do (ty,_) <- fdStat p; return ty

dup :: FD -> IO FD
dup fd = return fd

dup2 :: FD -> FD -> IO FD
dup2 fd _fdto = return fd

setNonBlockingMode :: FD -> Bool -> IO FD
setNonBlockingMode fd set = do
  withChannel fd $ \c -> setNonBlockingFD c set
  return fd { fdIsNonBlocking = set }

ready :: FD -> Bool -> Int -> IO Bool
ready fd write msecs =
  fdReady (fdChannel fd) write (fromIntegral msecs)

foreign import java safe "@static eta.base.Utils.fdReady"
  fdReady :: Channel -> Bool -> CInt -> IO Bool

-- ---------------------------------------------------------------------------
-- Terminal-related stuff

isTerminal :: FD -> IO Bool
isTerminal _ = return False

setEcho :: FD -> Bool -> IO ()
setEcho fd on = System.Posix.Internals.setEcho (errorWithoutStackTrace "setEcho: Not implemented yet.") on
-- setEcho fd on = System.Posix.Internals.setEcho (fdFD fd) on

getEcho :: FD -> IO Bool
getEcho fd = System.Posix.Internals.getEcho (errorWithoutStackTrace "getEcho: Not implemented yet.")
-- getEcho fd = System.Posix.Internals.getEcho (fdFD fd)

setRaw :: FD -> Bool -> IO ()
setRaw fd raw = System.Posix.Internals.setCooked (errorWithoutStackTrace "setRaw: Not implemented yet.") (not raw)
-- setRaw fd raw = System.Posix.Internals.setCooked (fdFD fd) (not raw)

-- -----------------------------------------------------------------------------
-- Reading and Writing

fdRead :: FD -> Ptr Word8 -> Int -> IO Int
fdRead fd ptr bytes
  = do { r <- readRawBufferPtr "GHC.IO.FD.fdRead" fd ptr 0 (fromIntegral bytes)
       ; return (fromIntegral r) }

fdReadNonBlocking :: FD -> Ptr Word8 -> Int -> IO (Maybe Int)
fdReadNonBlocking fd ptr bytes = do
  r <- readRawBufferPtrNoBlock "GHC.IO.FD.fdReadNonBlocking" fd ptr
           0 (fromIntegral bytes)
  case fromIntegral r of
    (-1) -> return (Nothing)
    n    -> return (Just n)


fdWrite :: FD -> Ptr Word8 -> Int -> IO ()
fdWrite fd ptr bytes = do
  res <- writeRawBufferPtr "GHC.IO.FD.fdWrite" fd ptr 0 (fromIntegral bytes)
  let res' = fromIntegral res
  if res' < bytes
     then fdWrite fd (ptr `plusPtr` res') (bytes - res')
     else return ()

-- XXX ToDo: this isn't non-blocking
fdWriteNonBlocking :: FD -> Ptr Word8 -> Int -> IO Int
fdWriteNonBlocking fd ptr bytes = do
  res <- writeRawBufferPtrNoBlock "GHC.IO.FD.fdWriteNonBlocking" fd ptr 0
            (fromIntegral bytes)
  return (fromIntegral res)

-- -----------------------------------------------------------------------------
-- FD operations

-- Low level routines for reading/writing to (raw)buffers:

{-
NOTE [nonblock]:

Unix has broken semantics when it comes to non-blocking I/O: you can
set the O_NONBLOCK flag on an FD, but it applies to the all other FDs
attached to the same underlying file, pipe or TTY; there's no way to
have private non-blocking behaviour for an FD.  See bug #724.

We fix this by only setting O_NONBLOCK on FDs that we create; FDs that
come from external sources or are exposed externally are left in
blocking mode.  This solution has some problems though.  We can't
completely simulate a non-blocking read without O_NONBLOCK: several
cases are wrong here.  The cases that are wrong:

  * reading/writing to a blocking FD in non-threaded mode.
    In threaded mode, we just make a safe call to read().
    In non-threaded mode we call select() before attempting to read,
    but that leaves a small race window where the data can be read
    from the file descriptor before we issue our blocking read().
  * readRawBufferNoBlock for a blocking FD

NOTE [2363]:

In the threaded RTS we could just make safe calls to read()/write()
for file descriptors in blocking mode without worrying about blocking
other threads, but the problem with this is that the thread will be
uninterruptible while it is blocked in the foreign call.  See #2363.
So now we always call fdReady() before reading, and if fdReady
indicates that there's no data, we call threadWaitRead.

-}

readRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO Int
readRawBufferPtr loc !fd buf off len
  | isNonBlocking fd = unsafe_read -- unsafe is ok, it can't block
  | otherwise    = do r <- unsafe_fdReady (fdChannel fd) False 0
                      if r
                        then read
                        else do threadWaitRead (fdChannel fd); read
  where
    do_read call = fromIntegral `fmap` retryOnNonBlock loc call (threadWaitRead (fdChannel fd))
    read        = if threaded then safe_read else unsafe_read
    unsafe_read = do_read (c_read (fdChannel fd) (buf `plusPtr` off) len)
    safe_read   = do_read (c_safe_read (fdChannel fd) (buf `plusPtr` off) len)

-- return: -1 indicates EOF, >=0 is bytes read
readRawBufferPtrNoBlock :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO Int
readRawBufferPtrNoBlock loc !fd buf off len
  | isNonBlocking fd  = unsafe_read -- unsafe is ok, it can't block
  | otherwise    = do r <- unsafe_fdReady (fdChannel fd) False 0
                      if r then safe_read
                           else return 0
       -- XXX see note [nonblock]
 where
   do_read call = do r <- retryOnInterrupt loc call (return (-1))
                     case r of
                       (-1) -> return 0
                       0    -> return (-1)
                       n    -> return (fromIntegral n)
   unsafe_read  = do_read (c_read (fdChannel fd) (buf `plusPtr` off) len)
   safe_read    = do_read (c_safe_read (fdChannel fd) (buf `plusPtr` off) len)

writeRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
writeRawBufferPtr loc !fd buf off len
  | isNonBlocking fd = unsafe_write -- unsafe is ok, it can't block
  | otherwise   = do r <- unsafe_fdReady (fdChannel fd) True 0
                     if r
                       then write
                       else do threadWaitWrite (fdChannel fd); write
  where
    do_write call = fromIntegral `fmap` retryOnNonBlock loc call (threadWaitWrite (fdChannel fd))
    write         = if threaded then safe_write else unsafe_write
    unsafe_write  = do_write (c_write (fdChannel fd) (buf `plusPtr` off) len)
    safe_write    = do_write (c_safe_write (fdChannel fd) (buf `plusPtr` off) len)

writeRawBufferPtrNoBlock :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
writeRawBufferPtrNoBlock loc !fd buf off len
  | isNonBlocking fd = unsafe_write -- unsafe is ok, it can't block
  | otherwise   = do r <- unsafe_fdReady (fdChannel fd) True 0
                     if r then write
                          else return 0
  where
    do_write call = do r <- throwErrnoIfMinus1RetryOnBlock loc call (return (-1))
                       case r of
                         (-1) -> return 0
                         n    -> return (fromIntegral n)
    write         = if threaded then safe_write else unsafe_write
    unsafe_write  = do_write (c_write (fdChannel fd) (buf `plusPtr` off) len)
    safe_write    = do_write (c_safe_write (fdChannel fd) (buf `plusPtr` off) len)

isNonBlocking :: FD -> Bool
isNonBlocking = fdIsNonBlocking

foreign import java unsafe "@static eta.base.Utils.fdReady"
  unsafe_fdReady :: Channel -> Bool -> CInt -> IO Bool

threaded :: Bool
threaded = True

-- -----------------------------------------------------------------------------
-- utils

#ifndef mingw32_HOST_OS
throwErrnoIfMinus1RetryOnBlock  :: String -> IO CSsize -> IO CSsize -> IO CSsize
throwErrnoIfMinus1RetryOnBlock loc f on_block  =
  do
    res <- f
    if (res :: CSsize) == -1
      then do
        err <- getErrno
        if err == eINTR
          then throwErrnoIfMinus1RetryOnBlock loc f on_block
          else if err == eWOULDBLOCK || err == eAGAIN
                 then do on_block
                 else throwErrno loc
      else return res
#endif

-- TODO: Catch java.io.IOException and convert them to Eta-level IOException
--       On InterruptedException, retry
retryOnInterrupt :: (Eq a, Num a) => String -> IO a -> IO a -> IO a
retryOnInterrupt loc act on_block = do
  res <- act
  if res == fromIntegral ((-1) :: Int)
  then on_block
  else return res

-- TODO: Catch java.io.IOException and convert them to Eta-level IOException
retryOnNonBlock :: (Eq a, Num a) => String -> IO a -> IO b -> IO a
retryOnNonBlock loc act on_block = do
  res <- act
  if res == fromIntegral ((-1) :: Int)
  then do _ <- on_block
          retryOnNonBlock loc act on_block
  else return res

-- -----------------------------------------------------------------------------
-- Locking/unlocking

foreign import java unsafe "@static eta.base.Utils.lockFile"
  lockFile   :: Object -> Bool -> IO Bool

foreign import java unsafe "@static eta.base.Utils.unlockFile"
  unlockFile :: Object -> IO Bool
