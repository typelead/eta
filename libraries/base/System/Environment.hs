--{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Environment
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Miscellaneous information about the system environment.
--
-----------------------------------------------------------------------------

module System.Environment
    (
      getArgs,
      getJavaArgs,
      getProgName,
      getExecutablePath,
      getEnv,
      lookupEnv,
      setEnv,
      unsetEnv,
      withArgs,
      withProgName,
      getEnvironment,
  ) where

import Foreign
import Foreign.C
import System.IO.Error (mkIOError)
import Control.Exception.Base (bracket, throwIO)
-- import GHC.IO
import GHC.IO.Exception
import GHC.IO.Encoding (getFileSystemEncoding)
import qualified GHC.Foreign as GHC
import Data.List
import Control.Monad
#ifdef mingw32_HOST_OS
import GHC.Environment
import GHC.Windows
#else
import System.Posix.Internals (withFilePath)
#endif

import GHC.Pack
import System.Environment.ExecutablePath
import Java

-- | Computation 'getArgs' returns a list of the program's command
-- line arguments (not including the program name).
getArgs :: IO [String]
getArgs = do
  jargs <- getJavaArgs
  jsArgs <- javaWith jargs arrayToList
  return $ map unpackCString jsArgs

-- | Computation 'getJavaArgs' returns a list of the program's command
-- line arguments (not including the program name) as a native String[].
foreign import java unsafe "@static eta.base.Utils.getJavaArgs"
  getJavaArgs :: IO JStringArray

{-|
Computation 'getProgName' returns the name of the program as it was
invoked.

However, this is hard-to-impossible to implement on some non-Unix
OSes, so instead, for maximum portability, we just return the leafname
of the program as invoked. Even then there are some differences
between platforms: on Windows, for example, a program invoked as foo
is probably really @FOO.EXE@, and that is what 'getProgName' will return.
-}
getProgName :: IO String
getProgName = return "eta.main"

-- | Computation 'getEnv' @var@ returns the value
-- of the environment variable @var@. For the inverse, POSIX users
-- can use 'System.Posix.Env.putEnv'.
--
-- This computation may fail with:
--
--  * 'System.IO.Error.isDoesNotExistError' if the environment variable
--    does not exist.

getEnv :: String -> IO String
getEnv name = lookupEnv name >>= maybe handleError return
  where handleError = ioe_missingEnvVar name

-- | Return the value of the environment variable @var@, or @Nothing@ if
-- there is no such value.
--
-- For POSIX users, this is equivalent to 'System.Posix.Env.getEnv'.
--
-- @since 4.6.0.0
foreign import java unsafe "@static java.lang.System.getenv"
  lookupEnv :: String -> IO (Maybe String)

ioe_missingEnvVar :: String -> IO a
ioe_missingEnvVar name = ioException (IOError Nothing NoSuchThing "getEnv"
    "no environment variable" Nothing (Just name))

-- | @setEnv name value@ sets the specified environment variable to @value@.
--
-- On Windows setting an environment variable to the /empty string/ removes
-- that environment variable from the environment.  For the sake of
-- compatibility we adopt that behavior.  In particular
--
-- @
-- setEnv name \"\"
-- @
--
-- has the same effect as
--
-- @
-- `unsetEnv` name
-- @
--
-- If you don't care about Windows support and want to set an environment
-- variable to the empty string use @System.Posix.Env.setEnv@ from the @unix@
-- package instead.
--
-- Throws `Control.Exception.IOException` if @name@ is the empty string or
-- contains an equals sign.
--
-- @since 4.7.0.0
setEnv :: String -> String -> IO ()
setEnv key_ value_
  | null key       = throwIO (mkIOError InvalidArgument "setEnv" Nothing Nothing)
  | '=' `elem` key = throwIO (mkIOError InvalidArgument "setEnv" Nothing Nothing)
  | null value     = unsetEnv key
  | otherwise      = setEnv_ key value
  where
    key   = takeWhile (/= '\NUL') key_
    value = takeWhile (/= '\NUL') value_

setEnv_ :: String -> String -> IO ()
setEnv_ k v = putEnv (k ++ "=" ++ v)

-- TODO: If this functionally is REALLY required,
-- we can implement with our own env map.
putEnv :: String -> IO ()
putEnv keyvalue = undefined

-- | @unSet name@ removes the specified environment variable from the
-- environment of the current process.
--
-- Throws `Control.Exception.IOException` if @name@ is the empty string or
-- contains an equals sign.
--
-- @since 4.7.0.0
unsetEnv :: String -> IO ()
unsetEnv key = setEnv_ key ""

{-|
'withArgs' @args act@ - while executing action @act@, have 'getArgs'
return @args@.
-}
withArgs :: [String] -> IO a -> IO a
withArgs xs act = do
   p <- System.Environment.getProgName
   withArgv (p:xs) act

{-|
'withProgName' @name act@ - while executing action @act@,
have 'getProgName' return @name@.
-}
withProgName :: String -> IO a -> IO a
withProgName nm act = do
   xs <- System.Environment.getArgs
   withArgv (nm:xs) act

-- Worker routine which marshals and replaces an argv vector for
-- the duration of an action.

withArgv :: [String] -> IO a -> IO a

-- #ifdef mingw32_HOST_OS
-- -- We have to reflect the updated arguments in the RTS-side variables as
-- -- well, because the RTS still consults them for error messages and the like.
-- -- If we don't do this then ghc-e005 fails.
-- withArgv new_args act = withWin32ProgArgv new_args $ withProgArgv new_args act
-- #else
withArgv = withProgArgv
-- #endif

withProgArgv :: [String] -> IO a -> IO a
withProgArgv new_args act = do
  pName <- System.Environment.getProgName
  existing_args <- System.Environment.getArgs
  bracket (setProgArgv new_args)
          (\argv -> do _ <- setProgArgv (pName:existing_args)
                       freeProgArgv argv)
          (const act)

freeProgArgv :: Ptr CString -> IO ()
freeProgArgv argv = undefined
  --do
  -- size <- lengthArray0 nullPtr argv
  -- sequence_ [ peek (argv `advancePtr` i) >>= free
  --           | i <- [size - 1, size - 2 .. 0]]
  -- free argv

setProgArgv :: [String] -> IO (Ptr CString)
setProgArgv argv = undefined
  --do
  -- enc <- getFileSystemEncoding
  -- vs <- mapM (GHC.newCString enc) argv >>= newArray0 nullPtr
  -- c_setProgArgv (genericLength argv) vs
  -- return vs

c_setProgArgv  :: CInt -> Ptr CString -> IO ()
c_setProgArgv = undefined

-- |'getEnvironment' retrieves the entire environment as a
-- list of @(key,value)@ pairs.
--
-- If an environment entry does not contain an @\'=\'@ character,
-- the @key@ is the whole entry and the @value@ is the empty string.
getEnvironment :: IO [(String, String)]

-- #ifdef mingw32_HOST_OS
-- getEnvironment = bracket c_GetEnvironmentStrings c_FreeEnvironmentStrings $ \pBlock ->
--     if pBlock == nullPtr then return []
--      else go pBlock
--   where
--     go pBlock = do
--         -- The block is terminated by a null byte where there
--         -- should be an environment variable of the form X=Y
--         c <- peek pBlock
--         if c == 0 then return []
--          else do
--           -- Seek the next pair (or terminating null):
--           pBlock' <- seekNull pBlock False
--           -- We now know the length in bytes, but ignore it when
--           -- getting the actual String:
--           str <- peekCWString pBlock
--           fmap (divvy str :) $ go pBlock'

--     -- Returns pointer to the byte *after* the next null
--     seekNull pBlock done = do
--         let pBlock' = pBlock `plusPtr` sizeOf (undefined :: CWchar)
--         if done then return pBlock'
--          else do
--            c <- peek pBlock'
--            seekNull pBlock' (c == (0 :: Word8 ))

-- foreign import WINDOWS_CCONV unsafe "windows.h GetEnvironmentStringsW"
--   c_GetEnvironmentStrings :: IO (Ptr CWchar)

-- foreign import WINDOWS_CCONV unsafe "windows.h FreeEnvironmentStringsW"
--   c_FreeEnvironmentStrings :: Ptr CWchar -> IO Bool
-- #else
getEnvironment = undefined
  --do
   -- pBlock <- getEnvBlock
   -- if pBlock == nullPtr then return []
   --  else do
   --    enc <- getFileSystemEncoding
   --    stuff <- peekArray0 nullPtr pBlock >>= mapM (GHC.peekCString enc)
   --    return (map divvy stuff)

getEnvBlock :: IO (Ptr CString)
getEnvBlock = undefined
-- #endif

divvy :: String -> (String, String)
divvy str =
  case break (=='=') str of
    (xs,[])        -> (xs,[]) -- don't barf (like Posix.getEnvironment)
    (name,_:value) -> (name,value)
