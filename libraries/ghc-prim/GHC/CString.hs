{-# LANGUAGE MagicHash, NoImplicitPrelude, BangPatterns, UnliftedFFITypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.CString
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC C strings definitions (previously in GHC.Base).
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.CString (
        JString#, getBytesUtf8#, indexStrChar#, strLength,
        unpackCString#, unpackAppendCString#, unpackFoldrCString#,
        unpackCStringUtf8#, unpackNBytes#
    ) where

import GHC.Types
import GHC.JArray
import GHC.Prim

type JString# = Object# JString -- convenience

foreign import java unsafe "getBytes" getBytes :: JString# -> JString# -> JByteArray#

foreign import java unsafe "length" strLength :: JString# -> Int#

getBytesUtf8# :: JString# -> JByteArray#
getBytesUtf8# this = getBytes this "UTF-8"#

indexStrChar# :: JByteArray# -> Int# -> Char#
indexStrChar# bytes n = byte2Char# (indexJByteArray# bytes n)

-----------------------------------------------------------------------------
-- Unpacking C strings}
-----------------------------------------------------------------------------

-- This code is needed for virtually all programs, since it's used for
-- unpacking the strings of error messages.

-- Used to be in GHC.Base, but was moved to ghc-prim because the new generics
-- stuff uses Strings in the representation, so to give representations for
-- ghc-prim types we need unpackCString#

unpackCString# :: JString# -> [Char]
{-# NOINLINE unpackCString# #-}
    -- There's really no point in inlining this, ever, as the loop doesn't
    -- specialise in an interesting But it's pretty small, so there's a danger
    -- that it'll be inlined at every literal, which is a waste
unpackCString# str
  = unpack 0#
  where
    bytes = getBytesUtf8# str
    len = strLength str
    unpack nh
      | isTrue# (nh ==# len) = []
      | True                 = C# (indexStrChar# bytes nh) : unpack (nh +# 1#)

unpackAppendCString# :: JString# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString# #-}
     -- See the NOINLINE note on unpackCString#
unpackAppendCString# str rest
  = unpack 0#
  where
    bytes = getBytesUtf8# str
    len = strLength str
    unpack nh
      | isTrue# (nh ==# len) = rest
      | True       = C# ch : unpack (nh +# 1#)
      where
        !ch = indexStrChar# bytes nh

unpackFoldrCString# :: JString# -> (Char -> a -> a) -> a -> a

-- Usually the unpack-list rule turns unpackFoldrCString# into unpackCString#

-- It also has a BuiltInRule in PrelRules.lhs:
--      unpackFoldrCString# "foo" c (unpackFoldrCString# "baz" c n)
--        =  unpackFoldrCString# "foobaz" c n

{-# NOINLINE unpackFoldrCString# #-}
-- At one stage I had NOINLINE [0] on the grounds that, unlike
-- unpackCString#, there *is* some point in inlining
-- unpackFoldrCString#, because we get better code for the
-- higher-order function call.  BUT there may be a lot of
-- literal strings, and making a separate 'unpack' loop for
-- each is highly gratuitous.  See nofib/real/anna/PrettyPrint.

unpackFoldrCString# str f z
  = unpack 0#
  where
    !bytes = getBytesUtf8# str
    !len = strLength str
    unpack nh
      | isTrue# (nh ==# len) = z
      | True       = C# ch `f` unpack (nh +# 1#)
      where
        !ch = indexStrChar# bytes nh

unpackCStringUtf8# :: JString# -> [Char]
unpackCStringUtf8# str
  = unpack 0#
  where
    !bytes = getBytesUtf8# str
    !len = strLength str
    unpack nh
      | isTrue# (nh ==# len) = []
      | isTrue# (ch `leChar#` '\x7F'#) = C# ch : unpack (nh +# 1#)
      | isTrue# (ch `leChar#` '\xDF'#) =
          C# (chr# (((ord# ch                                  -# 0xC0#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexStrChar# bytes (nh +# 1#)) -# 0x80#))) :
          unpack (nh +# 2#)
      | isTrue# (ch `leChar#` '\xEF'#) =
          C# (chr# (((ord# ch                                  -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexStrChar# bytes (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexStrChar# bytes (nh +# 2#)) -# 0x80#))) :
          unpack (nh +# 3#)
      | True                           =
          C# (chr# (((ord# ch                                  -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                    ((ord# (indexStrChar# bytes (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexStrChar# bytes (nh +# 2#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexStrChar# bytes (nh +# 3#)) -# 0x80#))) :
          unpack (nh +# 4#)
      where
        !ch = indexStrChar# bytes nh

unpackNBytes# :: JString# -> Int# -> [Char]
unpackNBytes# _str 0#   = []
unpackNBytes#  str len# = unpack [] (len# -# 1#)
  where !bytes = getBytesUtf8# str
        !len = strLength str
        unpack acc i#
          | isTrue# (i# <# 0#)  = acc
          | True                = case indexStrChar# bytes i# of
                                    ch -> unpack (C# ch : acc) (i# -# 1#)
