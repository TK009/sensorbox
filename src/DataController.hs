module DataController where

import Data.Acid
-- import qualified Data.Map as Map
import Data.Time.Clock
import Control.Concurrent
import qualified Data.Sequence as Seq

import Subscriptions
import LatestStore
import CallbackSystem
import Shared


-- | Initialize intervalThread
-- startIntervalThread :: _ -> IO ThreadId
startIntervalThread = forkIO . intervalLoop

intervalLoop Shared {sISubDB = intervalSubs, sLatestStore = latestStore} = do
    currentTime <- getCurrentTime
    (mTriggered, mNextTriggerTime) <- update intervalSubs $ LookupNextISub currentTime

    case mTriggered of
        Just ISub {isSubData = subData, isSensors = sensors} -> do
            sensorDatas <- query latestStore $ LookupSensorDatas sensors
            sendCallback subData sensorDatas
        Nothing  -> return ()


-- | Loops and executes interval subscriptions


{-
start :: Shared -> IO () -- TODO
start shared@Shared {sESubDB = eventSubsDB} = do
    undefined
  where
    dataController :: SensorData -> IO () -- InputPusher
    dataController sensorData@SensorData {sdSensor = sensor, sdValue = value} = do
        eventSubs <- query eventSubsDB $ LookupESub sensor

        --let onUpdate = filter eventSubs
        return ()
-}




