{-# LANGUAGE RankNTypes, RecordWildCards, GADTs, ScopedTypeVariables #-}
module Lib (serv) where

import Eta.Serv.Run
import Eta.REPL.Message
import Eta.REPL.RemoteTypes

import Data.Binary

type MessageHook = Msg -> IO Msg

serv :: MessageHook -> Pipe -> (forall a. IO a -> IO a) -> IO ()
serv hook pipe@Pipe{..} _restore = loop
 where
  loop = do
    Msg msg <- readPipe pipe getMessage >>= hook
    debug ("eta-serv: " ++ show msg)
    case msg of
      Shutdown -> return ()
      RunTH st q ty loc -> runTH pipe st q ty loc >> loop
      RunModFinalizers st qrefs -> runModFinalizerRefs pipe st qrefs >> loop
      _other -> run msg >>= reply

  reply :: forall a. (Binary a, Show a) => a -> IO ()
  reply r = do
    debug ("eta-serv: return: " ++ show r)
    writePipe pipe (put r)
    loop
