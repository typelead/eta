{-# LANGUAGE RankNTypes, RecordWildCards, GADTs, ScopedTypeVariables #-}
module Lib (serv) where

import Eta.Serv.Run
import Eta.REPL.Message
import Eta.REPL.RemoteTypes

import Data.Binary
import System.Exit

type MessageHook = Msg -> IO Msg

serv :: MessageHook -> Pipe -> (forall a. IO a -> IO a) -> IO ()
serv hook pipe@Pipe{..} _restore = loop
 where
  loop = do
    Msg msg <- readPipe pipe getMessage >>= hook
    debug ("eta-serv: " ++ debugMessage msg)
    case msg of
      Shutdown -> exitSuccess
      RunTH st q ty loc -> runTH pipe st q ty loc >> loop
      RunModFinalizers st qrefs -> runModFinalizerRefs pipe st qrefs >> loop
      _other -> run msg >>= reply

  reply :: forall a. (Binary a, Show a) => a -> IO ()
  reply r = do
    debug ("eta-serv: return: " ++ show r)
    writePipe pipe (put r)
    loop
