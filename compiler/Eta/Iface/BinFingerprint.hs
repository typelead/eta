{-# LANGUAGE CPP #-}

-- | Computing fingerprints of values serializeable with GHC's "Binary" module.
module Eta.Iface.BinFingerprint
  ( -- * Computing fingerprints
    fingerprintBinMem
  , computeFingerprint
  , putNameLiterally
  ) where

#include "HsVersions.h"

import Eta.Utils.Fingerprint
import Eta.Utils.Binary
import Eta.BasicTypes.Name
import Eta.Utils.Panic
import Eta.Utils.Util

fingerprintBinMem :: BinHandle -> IO Fingerprint
fingerprintBinMem bh = withBinBuffer bh f
  where
    f bs =
        -- we need to take care that we force the result here
        -- lest a reference to the ByteString may leak out of
        -- withBinBuffer.
        let fp = fingerprintByteString bs
        in fp `seq` return fp

computeFingerprint :: (Binary a)
                   => (BinHandle -> Name -> IO ())
                   -> a
                   -> IO Fingerprint
computeFingerprint put_nonbinding_name a = do
    bh <- fmap set_user_data $ openBinMem (3*1024) -- just less than a block
    put_ bh a
    fp <- fingerprintBinMem bh
    return fp
  where
    set_user_data bh =
      setUserData bh $ newWriteState put_nonbinding_name putNameLiterally putFS

-- | Used when we want to fingerprint a structure without depending on the
-- fingerprints of external Names that it refers to.
putNameLiterally :: BinHandle -> Name -> IO ()
putNameLiterally bh name = ASSERT( isExternalName name ) do
    put_ bh $! nameModule name
    put_ bh $! nameOccName name
