module ETA.Utils.Metrics where

import ETA.Utils.Json

import Data.Time
import Data.Time.Calendar
import Data.Time.Format

data Metrics = Metrics { metStartTime :: UTCTime
                       , metMode      :: Mode
                       , metEndTime   :: UTCTime }

emptyMetrics :: Metrics
emptyMetrics = Metrics { metStartTime = defaultUTCTime
                       , metMode      = defaultMode
                       , metEndTime   = defaultUTCTime }

instance ToJson Metrics where
  json Metrics {..} = JSObject [("startTime", json metStartTime)
                               ,("mode",      json metMode)
                               ,("endTime",   json metEndTime)]

defaultUTCTime :: UTCTime
defaultUTCTime = UTCTime (fromGregorian 0 0 0) (fromInteger 0)

instance ToJson UTCTime where
  json = JSString . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

data Mode = MakeMode | OneShotMode | InteractiveMode | EvalMode
  deriving Enum

defaultMode :: Mode
defaultMode = MakeMode

instance ToJson Mode where
  json = JSInt . fromEnum
