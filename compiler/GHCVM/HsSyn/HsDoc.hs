{-# LANGUAGE CPP, DeriveDataTypeable #-}

module GHCVM.HsSyn.HsDoc (
  HsDocString(..),
  LHsDocString,
  ppr_mbDoc
  ) where

#include "HsVersions.h"

import GHCVM.Utils.Outputable
import GHCVM.BasicTypes.SrcLoc
import GHCVM.Utils.FastString

import Data.Data

newtype HsDocString = HsDocString FastString
  deriving (Eq, Show, Data, Typeable)

type LHsDocString = Located HsDocString

instance Outputable HsDocString where
  ppr (HsDocString fs) = ftext fs

ppr_mbDoc :: Maybe LHsDocString -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty

