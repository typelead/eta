{-# LANGUAGE BangPatterns #-}

-- This is a non-exposed internal module
--
-- The code in this module has been ripped from containers-0.5.5.1:Data.Map.Base [1] almost
-- verbatimely to avoid a dependency of 'template-haskell' on the containers package.
--
-- [1] see https://hackage.haskell.org/package/containers-0.5.5.1
--
-- The original code is BSD-licensed and copyrighted by Daan Leijen, Andriy Palamarchuk, et al.

module Language.Eta.Meta.Lib.Map
    ( Map
    , empty
    , insert
    , Eta.REPL.Map.lookup
    ) where

import Eta.REPL.Map
