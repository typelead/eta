{-# LANGUAGE CPP, GADTs #-}

-- |
-- The Eta Serv server.
--
-- For details on Remote Eta Serv, see Note [Remote Eta REPL] in
-- compiler/eta-repl/Eta/REPL/TH.hs.
--
import Lib (serv)

import Eta.REPL.RemoteTypes
import Eta.REPL.Message

import Control.Exception
import Control.Monad
import Data.IORef
import System.Environment
import System.Exit
import System.IO

dieWithUsage :: IO a
dieWithUsage = die "usage: eta-serv [-v]"

main :: IO ()
main = do
  args <- getArgs
  verbose <- case args of
    ["-v"] -> return True
    []     -> return False
    _      -> dieWithUsage
  when verbose $ setVerbose
  debug "eta-serv: listening on stdin and writing to stdout."
  hSetEncoding stdin  latin1
  hSetEncoding stdout latin1
  lo_ref <- newIORef Nothing
  let pipe = Pipe{pipeRead = stdin, pipeWrite = stdout, pipeLeftovers = lo_ref}
  uninterruptibleMask $ serv hook pipe
  where hook = return -- empty hook
    -- we cannot allow any async exceptions while communicating, because
    -- we will lose sync in the protocol, hence uninterruptibleMask.
