{-# LANGUAGE OverloadedStrings #-}
module GHCVM.CodeGen.Object where

import qualified Data.ByteString.Lazy as B

-- NOTE: This package should be updated whenever the packages or names of the RTS objects change
closurePackage :: B.ByteString
closurePackage = "ghcvm/runtime/closure/"

closure :: B.ByteString -> B.ByteString
closure = B.append closurePackage

indStaticObj :: B.ByteString
indStaticObj = closure "StgIndStatic"

closureObj :: B.ByteString
closureObj = closure "StgClosure"
