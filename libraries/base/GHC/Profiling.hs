{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | @since 4.7.0.0
module GHC.Profiling where

import GHC.Base

startProfTimer :: IO ()
startProfTimer = errorWithoutStackTrace "startProfTimer: Not implemented in the Eta RTS."

stopProfTimer :: IO ()
stopProfTimer = errorWithoutStackTrace "stopProfTimer: Not implemented in the Eta RTS."
