{-# LANGUAGE CPP, UnboxedTuples, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- | Primarily, this module consists of an interface to the JVM ClassLoader interface.
module Eta.REPL.ClassLoader
  ( ShouldRetainCAFs(..)
  , addDynamicClassPath
  , setModuleClassPath
  , loadClasses
  , newInstance
  , resetClasses
  )  where

import Eta.REPL.RemoteTypes
import Eta.REPL.Utils
import GHC.Exts
import qualified Data.ByteString as B
import Java

-- ---------------------------------------------------------------------------
-- ClassLoader Linker Interface
-- ---------------------------------------------------------------------------

data ShouldRetainCAFs
  = RetainCAFs
    -- ^ Retain CAFs unconditionally in linked Haskell code.
    -- Note that this prevents any code from being unloaded.
    -- It should not be necessary unless you are GHCi or
    -- hs-plugins, which needs to be able call any function
    -- in the compiled code.
  | DontRetainCAFs
    -- ^ Do not retain CAFs.  Everything reachable from foreign
    -- exports will be retained, due to the StablePtrs
    -- created by the module initialisation code.  unloadObj
    -- frees these StablePtrs, which will allow the CAFs to
    -- be GC'd and the code to be removed.

addDynamicClassPath :: [FilePath] -> IO ()
addDynamicClassPath classpath = do
  let jstrings = map toJava classpath :: [JString]
  j_addDynamicClassPath (toJava jstrings)

foreign import java unsafe "@static eta.serv.REPLClassLoader.addURLs"
  j_addDynamicClassPath     :: JStringArray -> IO ()

setModuleClassPath :: [FilePath] -> IO ()
setModuleClassPath classpath = do
  let jstrings = map toJava classpath :: [JString]
  j_setModuleClassPath (toJava jstrings)

foreign import java unsafe "@static eta.serv.REPLClassLoader.setChildURLs"
  j_setModuleClassPath     :: JStringArray -> IO ()

loadClasses :: [String] -> [B.ByteString] -> IO ()
loadClasses classNames classes = do
  let classNames' = toJava (map toJava classNames :: [JString])
      classes'    = toJava (map toByteBuffer classes)
  j_loadClasses classNames' classes'

foreign import java unsafe "@static eta.serv.REPLClassLoader.loadClasses"
  j_loadClasses             :: JStringArray -> List ByteBuffer -> IO ()

newInstance :: String -> IO HValueRef
newInstance className = do
  obj <- j_newInstance className
  mkRemoteRef $ HValue (unsafeCoerce# obj)

foreign import java unsafe "@static eta.serv.REPLClassLoader.newInstance"
  j_newInstance :: String -> IO Object

foreign import java unsafe "@static eta.serv.REPLClassLoader.resetClasses"
  resetClasses :: IO ()


