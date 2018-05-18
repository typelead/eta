{-# LANGUAGE CPP, GADTs #-}

-- |
-- The Eta Serv server.
--
-- For details on Remote Eta Serv, see Note [Remote GHCi] in
-- compiler/eta-repl/Eta/REPL/TH.hs.
--
module Main (main) where

import Lib (serv)

import Eta.REPL.Message

import Control.Exception
import Control.Monad
import Data.IORef
import System.Environment
import System.Exit
import Text.Printf
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
  when verbose $
    printf "eta-serv listening on stdin and writing to stdout."
  lo_ref <- newIORef Nothing
  let pipe = Pipe{pipeRead = stdin, pipeWrite = stdout, pipeLeftovers = lo_ref}
  uninterruptibleMask $ serv verbose hook pipe
  where hook = return -- empty hook
    -- we cannot allow any async exceptions while communicating, because
    -- we will lose sync in the protocol, hence uninterruptibleMask.
