{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, CApiFFI #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Internals
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (requires POSIX)
--
-- POSIX support layer for the standard libraries.
-- This library is built on *every* platform, including Win32.
--
-- Non-posix compliant in order to support the following features:
--      * S_ISSOCK (no sockets in POSIX)
--
-----------------------------------------------------------------------------

module System.Posix.Internals where

import System.Posix.Types

import Foreign
import Foreign.C

-- import Data.Bits
import Data.Maybe

#if !defined(HTYPE_TCFLAG_T)
import System.IO.Error
#endif

import GHC.Base
import GHC.Num
import GHC.Real
import GHC.IO
import GHC.IO.IOMode
import GHC.IO.Exception
import GHC.IO.Device
#ifndef mingw32_HOST_OS
import {-# SOURCE #-} GHC.IO.Encoding (getFileSystemEncoding)
import qualified GHC.Foreign as GHC
#endif

-- ---------------------------------------------------------------------------
-- Debugging the base package

puts :: String -> IO ()
puts s = withCAStringLen (s ++ "\n") $ \(p, len) -> do
            -- In reality should be withCString, but assume ASCII to avoid loop
            -- if this is called by GHC.Foreign
           _ <- c_write undefined (castPtr p) (fromIntegral len)
           -- _ <- c_write 1 (castPtr p) (fromIntegral len)
           return ()


-- ---------------------------------------------------------------------------
-- Types

type CFLock     = ()
data {-# CTYPE "struct group" #-} CGroup
type CLconv     = ()
type CPasswd    = ()
type CSigaction = ()
data {-# CTYPE "sigset_t" #-} CSigset
type CStat      = ()
type CTermios   = ()
type CTm        = ()
type CTms       = ()
type CUtimbuf   = ()
type CUtsname   = ()

type FD = CInt

-- ---------------------------------------------------------------------------
-- stat()-related stuff

fdFileSize :: FD -> IO Integer
fdFileSize fd =
  allocaBytes sizeof_stat $ \ p_stat -> do
    throwErrnoIfMinus1Retry_ "fileSize" $
        c_fstat fd p_stat
    c_mode <- st_mode p_stat :: IO CMode
    if not (s_isreg c_mode)
        then return (-1)
        else do
      c_size <- st_size p_stat
      return (fromIntegral c_size)

fileType :: FilePath -> IO IODeviceType
fileType file =
  allocaBytes sizeof_stat $ \ p_stat -> do
  withFilePath file $ \p_file -> do
    throwErrnoIfMinus1Retry_ "fileType" $
      c_stat p_file p_stat
    statGetType p_stat

-- NOTE: On Win32 platforms, this will only work with file descriptors
-- referring to file handles. i.e., it'll fail for socket FDs.
fdStat :: FD -> IO (IODeviceType, CDev, CIno)
fdStat fd =
  allocaBytes sizeof_stat $ \ p_stat -> do
    throwErrnoIfMinus1Retry_ "fdType" $
        c_fstat fd p_stat
    ty <- statGetType p_stat
    dev <- st_dev p_stat
    ino <- st_ino p_stat
    return (ty,dev,ino)

fdType :: FD -> IO IODeviceType
fdType fd = do (ty,_,_) <- fdStat fd; return ty

statGetType :: Ptr CStat -> IO IODeviceType
statGetType p_stat = do
  c_mode <- st_mode p_stat :: IO CMode
  case () of
      _ | s_isdir c_mode        -> return Directory
        | s_isfifo c_mode || s_issock c_mode || s_ischr  c_mode
                                -> return Stream
        | s_isreg c_mode        -> return RegularFile
         -- Q: map char devices to RawDevice too?
        | s_isblk c_mode        -> return RawDevice
        | otherwise             -> ioError ioe_unknownfiletype

ioe_unknownfiletype :: IOException
ioe_unknownfiletype = IOError Nothing UnsupportedOperation "fdType"
                        "unknown file type"
                        Nothing
                        Nothing

fdGetMode :: FD -> IO IOMode
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
fdGetMode _ = do
    -- We don't have a way of finding out which flags are set on FDs
    -- on Windows, so make a handle that thinks that anything goes.
    let flags = o_RDWR
#else
fdGetMode fd = do
    flags <- throwErrnoIfMinus1Retry "fdGetMode"
                (c_fcntl_read fd const_f_getfl)
#endif
    let
       wH  = (flags .&. o_WRONLY) /= 0
       aH  = (flags .&. o_APPEND) /= 0
       rwH = (flags .&. o_RDWR) /= 0

       mode
         | wH && aH  = AppendMode
         | wH        = WriteMode
         | rwH       = ReadWriteMode
         | otherwise = ReadMode

    return mode

#ifdef mingw32_HOST_OS
withFilePath :: FilePath -> (CWString -> IO a) -> IO a
withFilePath = withCWString

newFilePath :: FilePath -> IO CWString
newFilePath = newCWString

peekFilePath :: CWString -> IO FilePath
peekFilePath = peekCWString
#else

withFilePath :: FilePath -> (CString -> IO a) -> IO a
newFilePath :: FilePath -> IO CString
peekFilePath :: CString -> IO FilePath
peekFilePathLen :: CStringLen -> IO FilePath

withFilePath fp f = getFileSystemEncoding >>= \enc -> GHC.withCString enc fp f
newFilePath fp = getFileSystemEncoding >>= \enc -> GHC.newCString enc fp
peekFilePath fp = getFileSystemEncoding >>= \enc -> GHC.peekCString enc fp
peekFilePathLen fp = getFileSystemEncoding >>= \enc -> GHC.peekCStringLen enc fp

#endif

-- ---------------------------------------------------------------------------
-- Terminal-related stuff

#if defined(HTYPE_TCFLAG_T)

setEcho :: FD -> Bool -> IO ()
setEcho fd on = do
  tcSetAttr fd $ \ p_tios -> do
    lflag <- c_lflag p_tios :: IO CTcflag
    let new_lflag
         | on        = lflag .|. fromIntegral const_echo
         | otherwise = lflag .&. complement (fromIntegral const_echo)
    poke_c_lflag p_tios (new_lflag :: CTcflag)

getEcho :: FD -> IO Bool
getEcho fd = do
  tcSetAttr fd $ \ p_tios -> do
    lflag <- c_lflag p_tios :: IO CTcflag
    return ((lflag .&. fromIntegral const_echo) /= 0)

setCooked :: FD -> Bool -> IO ()
setCooked fd cooked =
  tcSetAttr fd $ \ p_tios -> do

    -- turn on/off ICANON
    lflag <- c_lflag p_tios :: IO CTcflag
    let new_lflag | cooked    = lflag .|. (fromIntegral const_icanon)
                  | otherwise = lflag .&. complement (fromIntegral const_icanon)
    poke_c_lflag p_tios (new_lflag :: CTcflag)

    -- set VMIN & VTIME to 1/0 respectively
    when (not cooked) $ do
            c_cc <- ptr_c_cc p_tios
            let vmin  = (c_cc `plusPtr` (fromIntegral const_vmin))  :: Ptr Word8
                vtime = (c_cc `plusPtr` (fromIntegral const_vtime)) :: Ptr Word8
            poke vmin  1
            poke vtime 0

tcSetAttr :: FD -> (Ptr CTermios -> IO a) -> IO a
tcSetAttr fd fun = do
     allocaBytes sizeof_termios  $ \p_tios -> do
        throwErrnoIfMinus1Retry_ "tcSetAttr"
           (c_tcgetattr fd p_tios)

        -- Save a copy of termios, if this is a standard file descriptor.
        -- These terminal settings are restored in hs_exit().
        when (fd <= 2) $ do
          p <- get_saved_termios fd
          when (p == nullPtr) $ do
             saved_tios <- mallocBytes sizeof_termios
             copyBytes saved_tios p_tios sizeof_termios
             set_saved_termios fd saved_tios

        -- tcsetattr() when invoked by a background process causes the process
        -- to be sent SIGTTOU regardless of whether the process has TOSTOP set
        -- in its terminal flags (try it...).  This function provides a
        -- wrapper which temporarily blocks SIGTTOU around the call, making it
        -- transparent.
        allocaBytes sizeof_sigset_t $ \ p_sigset -> do
          allocaBytes sizeof_sigset_t $ \ p_old_sigset -> do
             throwErrnoIfMinus1_ "sigemptyset" $
                 c_sigemptyset p_sigset
             throwErrnoIfMinus1_ "sigaddset" $
                 c_sigaddset   p_sigset const_sigttou
             throwErrnoIfMinus1_ "sigprocmask" $
                 c_sigprocmask const_sig_block p_sigset p_old_sigset
             r <- fun p_tios  -- do the business
             throwErrnoIfMinus1Retry_ "tcSetAttr" $
                 c_tcsetattr fd const_tcsanow p_tios
             throwErrnoIfMinus1_ "sigprocmask" $
                 c_sigprocmask const_sig_setmask p_old_sigset nullPtr
             return r

-- TODO: Implement
-- foreign import ccall unsafe "HsBase.h __hscore_get_saved_termios"
get_saved_termios :: CInt -> IO (Ptr CTermios)
get_saved_termios = undefined

-- foreign import ccall unsafe "HsBase.h __hscore_set_saved_termios"
set_saved_termios :: CInt -> (Ptr CTermios) -> IO ()
set_saved_termios = undefined

#else

-- 'raw' mode for Win32 means turn off 'line input' (=> buffering and
-- character translation for the console.) The Win32 API for doing
-- this is GetConsoleMode(), which also requires echoing to be disabled
-- when turning off 'line input' processing. Notice that turning off
-- 'line input' implies enter/return is reported as '\r' (and it won't
-- report that character until another character is input..odd.) This
-- latter feature doesn't sit too well with IO actions like IO.hGetLine..
-- consider yourself warned.
setCooked :: FD -> Bool -> IO ()
setCooked fd cooked = do
  x <- set_console_buffering fd (if cooked then 1 else 0)
  if (x /= 0)
   then ioError (ioe_unk_error "setCooked" "failed to set buffering")
   else return ()

ioe_unk_error :: String -> String -> IOException
ioe_unk_error loc msg
 = ioeSetErrorString (mkIOError OtherError loc Nothing Nothing) msg

-- Note: echoing goes hand in hand with enabling 'line input' / raw-ness
-- for Win32 consoles, hence setEcho ends up being the inverse of setCooked.
setEcho :: FD -> Bool -> IO ()
setEcho fd on = do
  x <- set_console_echo fd (if on then 1 else 0)
  if (x /= 0)
   then ioError (ioe_unk_error "setEcho" "failed to set echoing")
   else return ()

getEcho :: FD -> IO Bool
getEcho fd = do
  r <- get_console_echo fd
  if (r == (-1))
   then ioError (ioe_unk_error "getEcho" "failed to get echoing")
   else return (r == 1)

-- foreign import ccall unsafe "consUtils.h set_console_buffering__"
set_console_buffering :: CInt -> CInt -> IO CInt
set_console_buffering = undefined

-- foreign import ccall unsafe "consUtils.h set_console_echo__"
set_console_echo :: CInt -> CInt -> IO CInt
set_console_echo = undefined

-- foreign import ccall unsafe "consUtils.h get_console_echo__"
get_console_echo :: CInt -> IO CInt
get_console_echo = undefined

-- foreign import ccall unsafe "consUtils.h is_console__"
is_console :: CInt -> IO CInt
is_console = undefined

#endif

-- ---------------------------------------------------------------------------
-- Turning on non-blocking for a file descriptor

setNonBlockingFD :: FD -> Bool -> IO ()
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
setNonBlockingFD fd set = do
  flags <- throwErrnoIfMinus1Retry "setNonBlockingFD"
                 (c_fcntl_read fd const_f_getfl)
  let flags' | set       = flags .|. o_NONBLOCK
             | otherwise = flags .&. complement o_NONBLOCK
  when (flags /= flags') $ do
    -- An error when setting O_NONBLOCK isn't fatal: on some systems
    -- there are certain file handles on which this will fail (eg. /dev/null
    -- on FreeBSD) so we throw away the return code from fcntl_write.
    _ <- c_fcntl_write fd const_f_setfl (fromIntegral flags')
    return ()
#else

-- bogus defns for win32
setNonBlockingFD _ _ = return ()

#endif

-- -----------------------------------------------------------------------------
-- Set close-on-exec for a file descriptor

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
setCloseOnExec :: FD -> IO ()
setCloseOnExec fd = do
  throwErrnoIfMinus1_ "setCloseOnExec" $
    c_fcntl_write fd const_f_setfd const_fd_cloexec
#endif

-- -----------------------------------------------------------------------------
-- foreign imports

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
type CFilePath = CString
#else
type CFilePath = CWString
#endif

-- foreign import ccall unsafe "HsBase.h access"
c_access :: CString -> CInt -> IO CInt
c_access = undefined

-- foreign import ccall unsafe "HsBase.h chmod"
c_chmod :: CString -> CMode -> IO CInt
c_chmod = undefined

-- foreign import ccall unsafe "HsBase.h close"
c_close :: CInt -> IO CInt
c_close = undefined

-- foreign import ccall unsafe "HsBase.h creat"
c_creat :: CString -> CMode -> IO CInt
c_creat = undefined

-- foreign import ccall unsafe "HsBase.h dup"
c_dup :: CInt -> IO CInt
c_dup = undefined

-- foreign import ccall unsafe "HsBase.h dup2"
c_dup2 :: CInt -> CInt -> IO CInt
c_dup2 = undefined

-- foreign import ccall unsafe "HsBase.h __hscore_fstat"
c_fstat :: CInt -> Ptr CStat -> IO CInt
c_fstat = undefined

foreign import java unsafe "@static eta.base.Utils.c_isatty"
  c_isatty :: CInt -> IO CInt

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
-- foreign import ccall unsafe "io.h _lseeki64"
c_lseek :: CInt -> Int64 -> CInt -> IO Int64
c_lseek = undefined
#else
-- We use CAPI as on some OSs (eg. Linux) this is wrapped by a macro
-- which redirects to the 64-bit-off_t versions when large file
-- support is enabled.
-- foreign import capi unsafe "unistd.h lseek"
c_lseek :: CInt -> COff -> CInt -> IO COff
c_lseek = undefined
#endif

-- foreign import ccall unsafe "HsBase.h __hscore_lstat"
lstat :: CFilePath -> Ptr CStat -> IO CInt
lstat = undefined

-- foreign import ccall unsafe "HsBase.h __hscore_open"
c_open :: CFilePath -> CInt -> CMode -> IO CInt
c_open = undefined

-- foreign import ccall safe "HsBase.h __hscore_open"
c_safe_open :: CFilePath -> CInt -> CMode -> IO CInt
c_safe_open = undefined

-- See Note: CSsize
foreign import java unsafe "@static eta.base.Utils.c_read"
  c_read :: Channel -> Ptr Word8 -> CSize -> IO CSsize

-- See Note: CSsize
foreign import java safe "@static eta.base.Utils.c_read"
  c_safe_read :: Channel -> Ptr Word8 -> CSize -> IO CSsize

-- foreign import ccall unsafe "HsBase.h __hscore_stat"
c_stat :: CFilePath -> Ptr CStat -> IO CInt
c_stat = undefined

-- foreign import ccall unsafe "HsBase.h umask"
c_umask :: CMode -> IO CMode
c_umask = undefined

-- See Note: CSsize
foreign import java unsafe "@static eta.base.Utils.c_write"
  c_write :: Channel -> Ptr Word8 -> CSize -> IO CSsize

-- See Note: CSsize
foreign import java safe "@static eta.base.Utils.c_write"
  c_safe_write :: Channel -> Ptr Word8 -> CSize -> IO CSsize

-- foreign import ccall unsafe "HsBase.h __hscore_ftruncate"
c_ftruncate :: CInt -> COff -> IO CInt
c_ftruncate = undefined

-- foreign import ccall unsafe "HsBase.h unlink"
c_unlink :: CString -> IO CInt
c_unlink = undefined

-- foreign import ccall unsafe "HsBase.h getpid"
c_getpid :: IO CPid
c_getpid = undefined

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
-- foreign import capi unsafe "HsBase.h fcntl"
c_fcntl_read  :: CInt -> CInt -> IO CInt
c_fcntl_read = undefined

-- foreign import capi unsafe "HsBase.h fcntl"
c_fcntl_write :: CInt -> CInt -> CLong -> IO CInt
c_fcntl_write = undefined

-- foreign import capi unsafe "HsBase.h fcntl"
c_fcntl_lock  :: CInt -> CInt -> Ptr CFLock -> IO CInt
c_fcntl_lock = undefined

-- foreign import ccall unsafe "HsBase.h fork"
c_fork :: IO CPid
c_fork = undefined

-- foreign import ccall unsafe "HsBase.h link"
c_link :: CString -> CString -> IO CInt
c_link = undefined

-- capi is required at least on Android
-- foreign import capi unsafe "HsBase.h mkfifo"
c_mkfifo :: CString -> CMode -> IO CInt
c_mkfifo = undefined

-- foreign import ccall unsafe "HsBase.h pipe"
c_pipe :: Ptr CInt -> IO CInt
c_pipe = undefined

-- foreign import capi unsafe "signal.h sigemptyset"
c_sigemptyset :: Ptr CSigset -> IO CInt
c_sigemptyset = undefined

-- foreign import capi unsafe "signal.h sigaddset"
c_sigaddset :: Ptr CSigset -> CInt -> IO CInt
c_sigaddset = undefined

-- foreign import capi unsafe "signal.h sigprocmask"
c_sigprocmask :: CInt -> Ptr CSigset -> Ptr CSigset -> IO CInt
c_sigprocmask = undefined

-- capi is required at least on Android
-- foreign import capi unsafe "HsBase.h tcgetattr"
c_tcgetattr :: CInt -> Ptr CTermios -> IO CInt
c_tcgetattr = undefined

-- capi is required at least on Android
-- foreign import capi unsafe "HsBase.h tcsetattr"
c_tcsetattr :: CInt -> CInt -> Ptr CTermios -> IO CInt
c_tcsetattr = undefined

-- foreign import capi unsafe "HsBase.h utime"
c_utime :: CString -> Ptr CUtimbuf -> IO CInt
c_utime = undefined

-- foreign import ccall unsafe "HsBase.h waitpid"
c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid
c_waitpid = undefined
#endif

-- POSIX flags only:
-- foreign import ccall unsafe "HsBase.h __hscore_o_rdonly"
o_RDONLY :: CInt
o_RDONLY = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_o_wronly"
o_WRONLY :: CInt
o_WRONLY = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_o_rdwr"
o_RDWR   :: CInt
o_RDWR = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_o_append"
o_APPEND :: CInt
o_APPEND = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_o_creat"
o_CREAT  :: CInt
o_CREAT = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_o_excl"
o_EXCL   :: CInt
o_EXCL = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_o_trunc"
o_TRUNC  :: CInt
o_TRUNC = undefined

-- non-POSIX flags.
-- foreign import ccall unsafe "HsBase.h __hscore_o_noctty"
o_NOCTTY   :: CInt
o_NOCTTY = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_o_nonblock"
o_NONBLOCK :: CInt
o_NONBLOCK = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_o_binary"
o_BINARY   :: CInt
o_BINARY = undefined

-- foreign import capi unsafe "sys/stat.h S_ISREG"
c_s_isreg  :: CMode -> CInt
c_s_isreg = undefined
-- foreign import capi unsafe "sys/stat.h S_ISCHR"
c_s_ischr  :: CMode -> CInt
c_s_ischr = undefined
-- foreign import capi unsafe "sys/stat.h S_ISBLK"
c_s_isblk  :: CMode -> CInt
c_s_isblk = undefined
-- foreign import capi unsafe "sys/stat.h S_ISDIR"
c_s_isdir  :: CMode -> CInt
c_s_isdir = undefined
-- foreign import capi unsafe "sys/stat.h S_ISFIFO"
c_s_isfifo :: CMode -> CInt
c_s_isfifo = undefined

s_isreg  :: CMode -> Bool
s_isreg cm = c_s_isreg cm /= 0
s_ischr  :: CMode -> Bool
s_ischr cm = c_s_ischr cm /= 0
s_isblk  :: CMode -> Bool
s_isblk cm = c_s_isblk cm /= 0
s_isdir  :: CMode -> Bool
s_isdir cm = c_s_isdir cm /= 0
s_isfifo :: CMode -> Bool
s_isfifo cm = c_s_isfifo cm /= 0

-- foreign import ccall unsafe "HsBase.h __hscore_sizeof_stat"
sizeof_stat :: Int
sizeof_stat = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_st_mtime"
st_mtime :: Ptr CStat -> IO CTime
st_mtime = undefined
#ifdef mingw32_HOST_OS
-- foreign import ccall unsafe "HsBase.h __hscore_st_size"
st_size :: Ptr CStat -> IO Int64
st_size = undefined
#else
-- foreign import ccall unsafe "HsBase.h __hscore_st_size"
st_size :: Ptr CStat -> IO COff
st_size = undefined
#endif
-- foreign import ccall unsafe "HsBase.h __hscore_st_mode"
st_mode :: Ptr CStat -> IO CMode
st_mode = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_st_dev"
st_dev :: Ptr CStat -> IO CDev
st_dev = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_st_ino"
st_ino :: Ptr CStat -> IO CIno
st_ino = undefined

-- foreign import ccall unsafe "HsBase.h __hscore_echo"
const_echo :: CInt
const_echo = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_tcsanow"
const_tcsanow :: CInt
const_tcsanow = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_icanon"
const_icanon :: CInt
const_icanon = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_vmin"
const_vmin   :: CInt
const_vmin = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_vtime"
const_vtime  :: CInt
const_vtime = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_sigttou"
const_sigttou :: CInt
const_sigttou = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_sig_block"
const_sig_block :: CInt
const_sig_block = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_sig_setmask"
const_sig_setmask :: CInt
const_sig_setmask = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_f_getfl"
const_f_getfl :: CInt
const_f_getfl = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_f_setfl"
const_f_setfl :: CInt
const_f_setfl = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_f_setfd"
const_f_setfd :: CInt
const_f_setfd = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_fd_cloexec"
const_fd_cloexec :: CLong
const_fd_cloexec = undefined

#if defined(HTYPE_TCFLAG_T)
-- foreign import ccall unsafe "HsBase.h __hscore_sizeof_termios"
sizeof_termios :: Int
sizeof_termios = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_sizeof_sigset_t"
sizeof_sigset_t :: Int
sizeof_sigset_t = undefined

-- foreign import ccall unsafe "HsBase.h __hscore_lflag"
c_lflag :: Ptr CTermios -> IO CTcflag
c_lflag = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_poke_lflag"
poke_c_lflag :: Ptr CTermios -> CTcflag -> IO ()
poke_c_lflag = undefined
-- foreign import ccall unsafe "HsBase.h __hscore_ptr_c_cc"
ptr_c_cc  :: Ptr CTermios -> IO (Ptr Word8)
ptr_c_cc = undefined
#endif

s_issock :: CMode -> Bool
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
s_issock cmode = c_s_issock cmode /= 0
-- foreign import capi unsafe "sys/stat.h S_ISSOCK"
c_s_issock :: CMode -> CInt
c_s_issock = undefined
#else
s_issock _ = False
#endif

-- foreign import ccall unsafe "__hscore_bufsiz"
dEFAULT_BUFFER_SIZE :: Int
dEFAULT_BUFFER_SIZE = undefined
-- foreign import capi  unsafe "stdio.h value SEEK_CUR"
sEEK_CUR :: CInt
sEEK_CUR = undefined
-- foreign import capi  unsafe "stdio.h value SEEK_SET"
sEEK_SET :: CInt
sEEK_SET = undefined
-- foreign import capi  unsafe "stdio.h value SEEK_END"
sEEK_END :: CInt
sEEK_END = undefined

{-
Note: CSsize

On Win64, ssize_t is 64 bit, but functions like read return 32 bit
ints. The CAPI wrapper means the C compiler takes care of doing all
the necessary casting.

When using ccall instead, when the functions failed with -1, we thought
they were returning with 4294967295, and so didn't throw an exception.
This lead to a segfault in echo001(ghci).
-}
