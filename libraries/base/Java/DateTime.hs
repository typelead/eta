{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.IO
-- Copyright   :  (c) Jyothsna Srinivas 2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  jyothsnasrinivas17@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Bindings for Java Date Time utilities
--
-----------------------------------------------------------------------------

module Java.DateTime where

import GHC.Base
import GHC.Int
import Java.Array
import Java.Collections
import Java.Primitive
import Java.Utils
import Java.Text

-- Start java.util.Calendar

data {-# CLASS "java.util.Calendar" #-} Calendar = Calendar (Object# Calendar)
  deriving Class

foreign import java unsafe add :: (a <: Calendar) => Int -> Int -> Java a ()

foreign import java unsafe after :: (a <: Calendar) => Object -> Java a Bool

foreign import java unsafe before :: (a <: Calendar) => Object -> Java a Bool

foreign import java unsafe clear :: (a <: Calendar) => Java a ()

foreign import java unsafe "clear" clearInt :: (a <: Calendar) => Int -> Java a ()

foreign import java unsafe clone :: (a <: Calendar) => Java a Object

foreign import java unsafe compareTo :: (a <: Calendar) => Calendar -> Java a Int

foreign import java unsafe complete :: (a <: Calendar) => Java a ()

foreign import java unsafe computeFields :: (a <: Calendar) => Java a ()

foreign import java unsafe computeTime :: (a <: Calendar) => Java a ()

foreign import java unsafe get :: (a <: Calendar) => Int -> Java a Int

foreign import java unsafe getActualMaximum :: (a <: Calendar) => Int -> Java a Int

foreign import java unsafe getActualMinimum :: (a <: Calendar) => Int -> Java a Int

foreign import java unsafe getDisplayName :: (a <: Calendar) => Int -> Int -> Locale -> Java a Int

-- foreign import java unsafe
--   getDisplayNames :: (a <: Calendar) => Int -> Int -> Locale -> Java a (Map JString JInteger)

foreign import java unsafe getFirstDayOfWeek :: (a <: Calendar) => Java a Int

foreign import java unsafe getGreatestMinimum :: (a <: Calendar) => Int -> Java a Int

foreign import java unsafe getLeastMaximum :: (a <: Calendar) => Int -> Java a Int

foreign import java unsafe getMaximum :: (a <: Calendar) => Int -> Java a Int

foreign import java unsafe getMinimalDaysInFirstWeek :: (a <: Calendar) => Java a Int

foreign import java unsafe getMinimum :: (a <: Calendar) => Int -> Java a Int

foreign import java unsafe getTime :: (a <: Calendar) => Java a Date

foreign import java unsafe getTimeInMillis :: (a <: Calendar) => Java a Int64

foreign import java unsafe getTimeZone :: (a <: Calendar) => Java a TimeZone

foreign import java unsafe getWeeksInWeekYear :: (a <: Calendar) => Java a Int

foreign import java unsafe getWeekYear :: (a <: Calendar) => Java a Int

foreign import java unsafe "hashCode" hashCodeCalendar :: (a <: Calendar) => Java a Int

foreign import java unsafe internalGet :: (a <: Calendar) => Int -> Java a Int

foreign import java unsafe isLenient :: (a <: Calendar) => Java a Bool

foreign import java unsafe isSet :: (a <: Calendar) => Int -> Java a Bool

foreign import java unsafe isWeekDateSupported :: (a <: Calendar) => Java a Bool

foreign import java unsafe roll :: (a <: Calendar) => Int -> Bool -> Java a ()

foreign import java unsafe "roll" rollInt :: (a <: Calendar) => Int -> Int -> Java a ()

foreign import java unsafe set :: (a <: Calendar) => Int -> Int -> Java a ()

foreign import java unsafe "set" set2 :: (a <: Calendar) => Int -> Int -> Int -> Java a ()

foreign import java unsafe "set"
  set3 :: (a <: Calendar) => Int -> Int -> Int -> Int -> Int -> Java a ()

foreign import java unsafe "set"
  set4 :: (a <: Calendar) => Int -> Int -> Int -> Int -> Int -> Int -> Java a ()

foreign import java unsafe setFirstDayOfWeek :: (a <: Calendar) => Int -> Java a ()

foreign import java unsafe setLenient :: (a <: Calendar) => Bool -> Java a ()

foreign import java unsafe setMinimalDaysInFirstWeek :: (a <: Calendar) => Int -> Java a ()

foreign import java unsafe setTime :: (a <: Calendar) => Date -> Java a ()

foreign import java unsafe setTimeInMillis :: (a <: Calendar) => Int64 -> Java a ()

foreign import java unsafe setTimeZone :: (a <: Calendar) => TimeZone -> Java a ()

foreign import java unsafe setWeekDate :: (a <: Calendar) => Int -> Int -> Int -> Java a ()

-- End java.util.Calendar

-- Start java.util.Date

data {-# CLASS "java.util.Date" #-} Date = Date (Object# Date)
  deriving Class

foreign import java unsafe "after" afterDate :: (a <: Date) => Date -> Java a Bool

foreign import java unsafe "before" beforeDate :: (a <: Date) => Date -> Java a Bool

foreign import java unsafe "compareTo" compareToDate :: (a <: Date) => Date -> Java a Int

foreign import java unsafe "getTime" getTimeDate :: (a <: Date) => Java a Int64

foreign import java unsafe "setTime" setTimeDate :: (a <: Date) => Java a Int64

-- End java.util.Date

-- Start java.util.TimeZone

data {-# CLASS "java.util.TimeZone" #-} TimeZone = TimeZone (Object# TimeZone)
  deriving Class

foreign import java unsafe "clone" cloneTimeZone :: (a <: TimeZone) => Java a Object

foreign import java unsafe "getDisplayName" getDisplayNameTimeZone :: (a <: TimeZone) => Java a String

foreign import java unsafe "getDisplayName"
  getDisplayNameTimeZone2 :: (a <: TimeZone) => Bool -> Int -> Java a String

foreign import java unsafe "getDisplayName"
  getDisplayNameTimeZone3 :: (a <: TimeZone) => Bool -> Int -> Locale -> Java a String

foreign import java unsafe "getDisplayName"
  getDisplayNameTimeZone4 :: (a <: TimeZone) => Locale -> Java a String

foreign import java unsafe getDSTSavings :: (a <: TimeZone) => Java a Int

foreign import java unsafe getID :: (a <: TimeZone) => Java a String

foreign import java unsafe
  getOffset :: (a <: TimeZone) => Int -> Int -> Int -> Int -> Int -> Int -> Java a Int

foreign import java unsafe "getOffset" getOffset2 :: (a <: TimeZone) => Int64 -> Java a Int

foreign import java unsafe getRawOffset :: (a <: TimeZone) => Java a Int

foreign import java unsafe hasSameRules :: (a <: TimeZone) => TimeZone -> Java a Bool

foreign import java unsafe inDaylightTime :: (a <: TimeZone) => Date -> Java a Bool

foreign import java unsafe observeslightTime :: (a <: TimeZone) => Java a Bool

foreign import java unsafe setID :: (a <: TimeZone) => String -> Java a ()

foreign import java unsafe setRawOffset :: (a <: TimeZone) => Int -> Java a ()

foreign import java unsafe useDaylightTime :: (a <: TimeZone) => Java a Bool

-- End java.util.TimeZone
