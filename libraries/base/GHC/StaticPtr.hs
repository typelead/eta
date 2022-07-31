{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StaticPtr
-- Copyright   :  (C) 2014 I/O Tweag
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Symbolic references to values.
--
-- References to values are usually implemented with memory addresses, and this
-- is practical when communicating values between the different pieces of a
-- single process.
--
-- When values are communicated across different processes running in possibly
-- different machines, though, addresses are no longer useful since each
-- process may use different addresses to store a given value.
--
-- To solve such concern, the references provided by this module offer a key
-- that can be used to locate the values on each process. Each process maintains
-- a global table of references which can be looked up with a given key. This
-- table is known as the Static Pointer Table. The reference can then be
-- dereferenced to obtain the value.
--
-- The various communicating processes need to agree on the keys used to refer
-- to the values in the Static Pointer Table, or lookups will fail. Only
-- processes launched from the same program binary are guaranteed to use the
-- same set of keys.
--
-----------------------------------------------------------------------------

module GHC.StaticPtr
  ( StaticPtr
  , deRefStaticPtr
  , StaticKey
  , staticKey
  , unsafeLookupStaticPtr
  , StaticPtrInfo(..)
  , staticPtrInfo
  , staticPtrKeys
  ) where

import Data.Typeable       (Typeable)
import Foreign.C.Types     (CInt(..))
import Foreign.Marshal     (allocaArray, peekArray, withArray)
import Foreign.Ptr         (castPtr)
import GHC.Exts            (addrToAny#)
import GHC.Ptr             (Ptr(..), nullPtr)
import GHC.Fingerprint     (Fingerprint(..))


-- | A reference to a value of type 'a'.
data StaticPtr a = StaticPtr StaticKey StaticPtrInfo a
  deriving Typeable

-- | Dereferences a static pointer.
deRefStaticPtr :: StaticPtr a -> a
deRefStaticPtr (StaticPtr _ _ v) = v

-- | A key for `StaticPtrs` that can be serialized and used with
-- 'unsafeLookupStaticPtr'.
type StaticKey = Fingerprint

-- | The 'StaticKey' that can be used to look up the given 'StaticPtr'.
staticKey :: StaticPtr a -> StaticKey
staticKey (StaticPtr k _ _) = k

-- | Looks up a 'StaticPtr' by its 'StaticKey'.
--
-- If the 'StaticPtr' is not found returns @Nothing@.
--
-- This function is unsafe because the program behavior is undefined if the type
-- of the returned 'StaticPtr' does not match the expected one.
--
unsafeLookupStaticPtr :: StaticKey -> IO (Maybe (StaticPtr a))
unsafeLookupStaticPtr (Fingerprint w1 w2) =
  errorWithoutStackTrace "unsafeLookupStaticPtr: Static pointers not implemented in the Eta RTS."

-- | Miscellaneous information available for debugging purposes.
data StaticPtrInfo = StaticPtrInfo
    { -- | Package key of the package where the static pointer is defined
      spInfoPackageKey  :: String
      -- | Name of the module where the static pointer is defined
    , spInfoModuleName :: String
      -- | An internal name that is distinct for every static pointer defined in
      -- a given module.
    , spInfoName       :: String
      -- | Source location of the definition of the static pointer as a
      -- @(Line, Column)@ pair.
    , spInfoSrcLoc     :: (Int, Int)
    }
  deriving (Show, Typeable)

-- | 'StaticPtrInfo' of the given 'StaticPtr'.
staticPtrInfo :: StaticPtr a -> StaticPtrInfo
staticPtrInfo (StaticPtr _ n _) = n

-- | A list of all known keys.
staticPtrKeys :: IO [StaticKey]
staticPtrKeys =
  errorWithoutStackTrace "staticPtrKeys: Static pointers not implemented in the Eta RTS."
{-# NOINLINE staticPtrKeys #-}
