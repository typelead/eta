{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Eta.Location (Loc(..), CharPos) where

import Data.Data
import GHC.Generics

-----------------------------------------------------
--              Locations
-----------------------------------------------------

data Loc
  = Loc { loc_filename :: String
        , loc_package  :: String
        , loc_module   :: String
        , loc_start    :: CharPos
        , loc_end      :: CharPos }
   deriving( Show, Eq, Ord, Data, Typeable, Generic )

type CharPos = (Int, Int)       -- ^ Line and character position
