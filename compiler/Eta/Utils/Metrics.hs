module Eta.Utils.Metrics where

import Eta.Utils.Json

import Data.Time

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

data Mode = MakeMode | OneShotMode | InteractiveMode | EvalMode
  deriving Enum

defaultMode :: Mode
defaultMode = MakeMode

instance ToJson Mode where
  json = JSInt . fromEnum
