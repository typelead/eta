module Eta.REPL.ClassQuery (setClassInfoPath, getClassInfo) where

import Eta.REPL.Message

setClassInfoPath :: [String] -> IO ()
setClassInfoPath _ = return ()

getClassInfo :: [String] -> IO (JResult a)
getClassInfo _ = return (error "getClassInfo: Not implemented yet!")
