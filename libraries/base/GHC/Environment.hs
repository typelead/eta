{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module GHC.Environment (getFullArgs) where

import Foreign
import Foreign.C
import GHC.Base
import GHC.Real ( fromIntegral )

import GHC.IO.Encoding
import GHC.Num
import qualified GHC.Foreign as GHC

getFullArgs :: IO [String]
getFullArgs = undefined
  -- TODO: Implement
  -- alloca $ \ p_argc ->
  -- alloca $ \ p_argv -> do
  --  getFullProgArgv p_argc p_argv
  --  p    <- fromIntegral `liftM` peek p_argc
  --  argv <- peek p_argv
  --  enc <- getFileSystemEncoding
  --  peekArray (p - 1) (advancePtr argv 1) >>= mapM (GHC.peekCString enc)

getFullProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
getFullProgArgv = undefined
