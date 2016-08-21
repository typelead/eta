{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude, UnliftedFFITypes,
             GHCForeignImportPrim #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IntWord64
-- Copyright   :  (c) The University of Glasgow, 1997-2008
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Primitive operations on Int64# and Word64# on platforms where
-- WORD_SIZE_IN_BITS < 64.
--
-----------------------------------------------------------------------------

-- TODO: Replace all C calls with Java calls
module GHC.IntWord64 (Int64#, Word64#, module GHC.IntWord64) where

import GHC.Prim

foreign import prim "@inline eqWord64"    eqWord64# :: Word64# -> Word64# -> Int#
foreign import prim "@inline neWord64"    neWord64# :: Word64# -> Word64# -> Int#
foreign import prim "@inline ltWord64"    ltWord64# :: Word64# -> Word64# -> Int#
foreign import prim "@inline leWord64"    leWord64# :: Word64# -> Word64# -> Int#
foreign import prim "@inline gtWord64"    gtWord64# :: Word64# -> Word64# -> Int#
foreign import prim "@inline geWord64"    geWord64# :: Word64# -> Word64# -> Int#

foreign import prim "@inline eqInt64"     eqInt64#   :: Int64# -> Int64# -> Int#
foreign import prim "@inline neInt64"     neInt64#   :: Int64# -> Int64# -> Int#
foreign import prim "@inline ltInt64"     ltInt64#   :: Int64# -> Int64# -> Int#
foreign import prim "@inline leInt64"     leInt64#   :: Int64# -> Int64# -> Int#
foreign import prim "@inline gtInt64"     gtInt64#   :: Int64# -> Int64# -> Int#
foreign import prim "@inline geInt64"     geInt64#   :: Int64# -> Int64# -> Int#
foreign import prim "@inline quotInt64"   quotInt64# :: Int64# -> Int64# -> Int64#
foreign import prim "@inline remInt64"    remInt64#  :: Int64# -> Int64# -> Int64#

foreign import prim "@inline plusInt64"   plusInt64#   :: Int64#  -> Int64# -> Int64#
foreign import prim "@inline minusInt64"  minusInt64#  :: Int64#  -> Int64# -> Int64#
foreign import prim "@inline timesInt64"  timesInt64#  :: Int64#  -> Int64# -> Int64#
foreign import prim "@inline negateInt64" negateInt64# :: Int64#  -> Int64#
foreign import prim "@inline quotWord64"  quotWord64#  :: Word64# -> Word64# -> Word64#
foreign import prim "@inline remWord64"   remWord64#   :: Word64# -> Word64# -> Word64#

foreign import prim "@inline and64"       and64# :: Word64# -> Word64# -> Word64#
foreign import prim "@inline or64"        or64#  :: Word64# -> Word64# -> Word64#
foreign import prim "@inline xor64"       xor64# :: Word64# -> Word64# -> Word64#
foreign import prim "@inline not64"       not64# :: Word64# -> Word64#

foreign import prim "@inline uShiftL64"   uncheckedShiftL64#   :: Word64# -> Int# -> Word64#
foreign import prim "@inline uShiftRL64"  uncheckedShiftRL64#  :: Word64# -> Int# -> Word64#
foreign import prim "@inline uIShiftL64"  uncheckedIShiftL64#  :: Int64#  -> Int# -> Int64#
foreign import prim "@inline uIShiftRA64" uncheckedIShiftRA64# :: Int64#  -> Int# -> Int64#
foreign import prim "@inline uIShiftRL64" uncheckedIShiftRL64# :: Int64#  -> Int# -> Int64#

foreign import prim "@inline int64ToWord64"   int64ToWord64# :: Int64#  -> Word64#
foreign import prim "@inline word64ToInt64"   word64ToInt64# :: Word64# -> Int64#
foreign import prim "@inline intToInt64"      intToInt64#    :: Int#    -> Int64#
foreign import prim "@inline int64ToInt"      int64ToInt#    :: Int64#  -> Int#
foreign import prim "@inline wordToWord64"    wordToWord64#  :: Word#   -> Word64#
foreign import prim "@inline word64ToWord"    word64ToWord#  :: Word64# -> Word#
