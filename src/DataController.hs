module DataController where

import Control.Monad (unless, when)
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




triggerEventsSubs :: Shared -> SensorData -> IO () -- TODO
triggerEventsSubs shared@Shared {sESubDB = eventSubsDB, sLatestStore = latestStore} =
    dataController
  where
    dataController :: SensorData -> IO () -- InputPusher
    dataController newData@SensorData {
            sdSensor = sensor, sdValue = newValue, sdTimestamp = newTime} = do
        eventSubs <- query eventSubsDB $ LookupESub sensor

        let filterSubs event = filter ((== event) . esEvent) eventSubs
            onChangeSubs = filterSubs OnChange
            onUpdateSubs = filterSubs OnUpdate
            onAttachSubs = filterSubs OnAttach
            callback     = flip sendCallback [newData] . esSubData

        -- Update events happen always
        mapM_ callback onUpdateSubs

        mOldData <- query latestStore $ LookupSensorData sensor

        case mOldData of
            Just SensorData {sdValue = oldValue, sdTimestamp = oldTime} ->
                when (oldTime < newTime) $ do
                    when (oldValue /= newValue) $
                        mapM_ callback onChangeSubs
                    update latestStore $ SetSensorData newData

            Nothing ->   -- New data
                mapM_ callback onAttachSubs

        return ()



