module DataController where

import Control.Monad (unless)
import Data.Acid
-- import qualified Data.Map as Map
import Data.Time.Clock
import Control.Concurrent (forkIO, ThreadId)

import Subscriptions
import LatestStore
import CallbackSystem
import Shared
import Duration


-- | Initialize intervalThread
startIntervalThread :: Shared -> IO ThreadId
startIntervalThread = forkIO . intervalLoop

-- | Loops and executes interval subscriptions
intervalLoop :: Shared -> IO ()
intervalLoop Shared {sISubDB = intervalSubs, sLatestStore = latestStore} = loop
  where
    loop :: IO ()
    loop = do
        currentTime <- getCurrentTime
        (mTriggered, mNextTriggerTime) <- update intervalSubs $ LookupNextISub currentTime

        case mTriggered of
            Just ISub {isSubData = subData, isSensors = sensors} -> do
                sensorDatas <- query latestStore $ LookupSensorDatas sensors
                sendCallback subData sensorDatas
            Nothing  -> return ()

        newCurrentTime <- getCurrentTime

        case mNextTriggerTime of
            Just triggerTime ->
                let delay = newCurrentTime `diffUTCTime` triggerTime
                in unless (delay < 0) $   -- skip waiting, we have more triggered already
                    threadDelay delay     -- wait, threadDelay should be accurate enough
            Nothing ->
                threadDelay $ seconds (10 :: Int) -- sleep this interval until we get more ISubs

        loop




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




