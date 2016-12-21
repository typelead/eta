{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
           , GeneralizedNewtypeDeriving
  #-}
{-# LANGUAGE AutoDeriveTypeable, StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Types
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX data types: Haskell equivalents of the types defined by the
-- @\<sys\/types.h>@ C header on a POSIX system.
--
-----------------------------------------------------------------------------

module System.Posix.Types (

  -- * POSIX data types
  CDev(..),
  CIno(..),
  CMode(..),
  COff(..),
  CPid(..),
  CSsize(..),

#if defined(HTYPE_GID_T)
  CGid(..),
#endif
#if defined(HTYPE_NLINK_T)
  CNlink(..),
#endif
#if defined(HTYPE_UID_T)
  CUid(..),
#endif
#if defined(HTYPE_CC_T)
  CCc(..),
#endif
#if defined(HTYPE_SPEED_T)
  CSpeed(..),
#endif
#if defined(HTYPE_TCFLAG_T)
  CTcflag(..),
#endif
#if defined(HTYPE_RLIM_T)
  CRLim(..),
#endif

  Fd(..),
  Channel(..),

#if defined(HTYPE_NLINK_T)
  LinkCount,
#endif
#if defined(HTYPE_UID_T)
  UserID,
#endif
#if defined(HTYPE_GID_T)
  GroupID,
#endif

  ByteCount,
  ClockTick,
  EpochTime,
  FileOffset,
  ProcessID,
  ProcessGroupID,
  DeviceID,
  FileID,
  FileMode,
  Limit
 ) where

import Foreign
import Foreign.C
import Data.Typeable
-- import Data.Bits

import GHC.Base
import GHC.Enum
import GHC.Num
import GHC.Real
-- import GHC.Prim
import GHC.Read
import GHC.Show

#include "CTypes.h"

INTEGRAL_TYPE(CDev,Int32)
INTEGRAL_TYPE(CIno,Word64)
INTEGRAL_TYPE_WITH_CTYPE(CMode,mode_t,Word16)
INTEGRAL_TYPE(COff,Int64)
INTEGRAL_TYPE(CPid,Int32)
INTEGRAL_TYPE(CSsize,Int32)

#if defined(HTYPE_GID_T)
INTEGRAL_TYPE(CGid,HTYPE_GID_T)
#endif
#if defined(HTYPE_NLINK_T)
INTEGRAL_TYPE(CNlink,HTYPE_NLINK_T)
#endif

#if defined(HTYPE_UID_T)
INTEGRAL_TYPE(CUid,HTYPE_UID_T)
#endif
#if defined(HTYPE_CC_T)
ARITHMETIC_TYPE(CCc,HTYPE_CC_T)
#endif
#if defined(HTYPE_SPEED_T)
ARITHMETIC_TYPE(CSpeed,HTYPE_SPEED_T)
#endif
#if defined(HTYPE_TCFLAG_T)
INTEGRAL_TYPE(CTcflag,HTYPE_TCFLAG_T)
#endif
#if defined(HTYPE_RLIM_T)
INTEGRAL_TYPE(CRLim,HTYPE_RLIM_T)
#endif

-- ToDo: blksize_t, clockid_t, blkcnt_t, fsblkcnt_t, fsfilcnt_t, id_t, key_t
-- suseconds_t, timer_t, useconds_t

-- Make an Fd type rather than using CInt everywhere

data {-# CLASS "java.nio.channels.Channel" #-} Channel =
  Channel (Object# Channel)

newtype Fd = Fd Int

instance Show Fd where
  show (Fd i) = show i

instance Eq Fd where
  (==) (Fd i) (Fd j) = i == j
-- INTEGRAL_TYPE(Fd,CInt)

-- nicer names, and backwards compatibility with POSIX library:
#if defined(HTYPE_NLINK_T)
type LinkCount      = CNlink
#endif
#if defined(HTYPE_UID_T)
type UserID         = CUid
#endif
#if defined(HTYPE_GID_T)
type GroupID        = CGid
#endif

type ByteCount      = CSize
type ClockTick      = CClock
type EpochTime      = CTime
type DeviceID       = CDev
type FileID         = CIno
type FileMode       = CMode
type ProcessID      = CPid
type FileOffset     = COff
type ProcessGroupID = CPid
type Limit          = CLong
