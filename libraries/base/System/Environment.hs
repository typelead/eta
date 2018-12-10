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
import Java.StringBase
import Java.Collections

-- | Computation 'getArgs' returns a list of the program's command
-- line arguments (not including the program name).
getArgs :: IO [String]
getArgs = do
  jargs <- getJavaArgs
  return (map fromJString (fromJava jargs))

-- | Computation 'getJavaArgs' returns a list of the program's command
-- line arguments (not including the program name) as a native String[].
foreign import java unsafe "@static eta.runtime.Runtime.getLocalProgramArguments"
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
getProgName = fmap fromJString getProgramName

foreign import java unsafe "@static eta.runtime.Runtime.getLocalProgramName"
  getProgramName :: IO JString

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
setEnv key value = error $ "setEnv " ++ key ++ "=" ++ value ++
                           ": Cannot modify environment on JVM."

-- | @unSet name@ removes the specified environment variable from the
-- environment of the current process.
--
-- Throws `Control.Exception.IOException` if @name@ is the empty string or
-- contains an equals sign.
--
-- @since 4.7.0.0
unsetEnv :: String -> IO ()
unsetEnv key = setEnv key ""

{-|
'withArgs' @args act@ - while executing action @act@, have 'getArgs'
return @args@.
-}
withArgs :: [String] -> IO a -> IO a
withArgs newArgs act = do
  let newArgs' = toJava (map toJString newArgs)
  existingArgs <- getJavaArgs
  bracket (setArgs newArgs')
          (const (setArgs existingArgs))
          (const act)

foreign import java unsafe "@static eta.runtime.Runtime.setLocalProgramArguments"
  setArgs :: JStringArray -> IO ()

{-|
'withProgName' @name act@ - while executing action @act@,
have 'getProgName' return @name@.
-}
withProgName :: String -> IO a -> IO a
withProgName nm act = do
  progName <- getProgramName
  bracket (setProgName (toJString nm))
          (const (setProgName progName))
          (const act)

foreign import java unsafe "@static eta.runtime.Runtime.setLocalProgramName"
  setProgName :: JString -> IO ()

-- |'getEnvironment' retrieves the entire environment as a
-- list of @(key,value)@ pairs.
--
-- If an environment entry does not contain an @\'=\'@ character,
-- the @key@ is the whole entry and the @value@ is the empty string.
getEnvironment :: IO [(String, String)]
getEnvironment = do
  env <- getenv0
  return $ map (\(k, v) -> (fromJString k, fromJString v))
         $ fromJava env

foreign import java unsafe "@static java.lang.System.getenv" getenv0
  :: IO (Map JString JString)
