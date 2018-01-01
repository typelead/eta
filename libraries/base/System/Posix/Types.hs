{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
           , GeneralizedNewtypeDeriving
           , DataKinds
           , MultiParamTypeClasses
           , TypeFamilies
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
  Limit,
  StandardOpenOption,
  OpenOption,
  OpenOptionArray,
  FileAttribute,
  PosixFilePermission,
  Path,
  FileChannel,
  FileAttributeArray,
  BasicFileAttributes
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
import Java.Utils hiding (Enum)
import Java.Core

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
  deriving (Class, Show)

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

-- Start java.nio.file.StandardOpenOption

data {-# CLASS "java.nio.file.StandardOpenOption" #-}
  StandardOpenOption = StandardOpenOption (Object# StandardOpenOption)
  deriving (Eq, Class)

type instance Inherits StandardOpenOption = '[Object, OpenOption]



-- End java.nio.file.StandardOpenOption

-- Start java.nio.file.OpenOption

data {-# CLASS "java.nio.file.OpenOption" #-}
  OpenOption = OpenOption (Object# OpenOption)
  deriving Class

-- End java.nio.file.OpenOption

-- Start java.nio.file.OpenOption

data {-# CLASS "java.nio.file.OpenOption[]" #-}
  OpenOptionArray = OpenOptionArray (Object# OpenOptionArray)
  deriving Class

-- End java.nio.file.OpenOption

-- Start java.nio.file.attribute.FileAttribute

data {-# CLASS "java.nio.file.attribute.FileAttribute" #-}
  FileAttribute = FileAttribute (Object# FileAttribute)
  deriving Class

-- End java.nio.file.attribute.FileAttribute

-- Start java.nio.file.attribute.FileAttributeArray

data {-# CLASS "java.nio.file.attribute.FileAttribute[]" #-}
  FileAttributeArray = FileAttributeArray (Object# FileAttributeArray)
  deriving Class

-- End java.nio.file.attribute.FileAttributeArray

-- Start java.nio.file.attribute.PosixFilePermission

data {-# CLASS "java.nio.file.attribute.PosixFilePermission" #-}
  PosixFilePermission = PosixFilePermission (Object# PosixFilePermission)
  deriving Class

-- End java.nio.file.attribute.PosixFilePermission

-- Start java.nio.file.Path

data {-# CLASS "java.nio.file.Path" #-}
  Path = Path (Object# Path)
  deriving Class

-- End java.nio.file.Path

-- Start java.nio.channels.FileChannel

data {-# CLASS "java.nio.channels.FileChannel" #-}
  FileChannel = FileChannel (Object# FileChannel)
  deriving Class

type instance Inherits FileChannel = '[Channel]

-- End java.nio.channels.FileChannel

data {-# CLASS "java.nio.file.attribute.BasicFileAttributes" #-}
  BasicFileAttributes = BasicFileAttributes (Object# BasicFileAttributes)
  deriving Class
