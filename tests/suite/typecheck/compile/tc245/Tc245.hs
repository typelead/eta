-- Test for trac #2937

{-# LANGUAGE GADTs, TypeFamilies #-}

module Tc245 where

import Tc245A

instance Foo Int where
    data Bar Int x where
        Baz :: Bar Int String
