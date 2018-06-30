module Eta.REPL.ClassQuery (setClassInfoPath, getClassInfo) where

import Java

import Eta.REPL.Message
import Eta.REPL.ClassInfo

setClassInfoPath :: [String] -> IO ()
setClassInfoPath classpath = do
  let jstrings = map toJava classpath :: [JString]
  j_setClassInfoPath (toJava jstrings)

foreign import java unsafe "@static eta.serv.REPLClassLoader.setClassInfoPath"
  j_setClassInfoPath :: JStringArray -> IO ()

getClassInfo :: [String] -> IO (JResult [ClassInfo])
getClassInfo _ = return (error "getClassInfo: Not implemented yet!")
