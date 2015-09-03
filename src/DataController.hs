{-# LANGUAGE OverloadedStrings #-}
module DataController where

import Control.Monad (unless, when)
import Data.Acid
import qualified Data.Text as Text
import Data.Time.Clock
import Control.Concurrent (forkIO, ThreadId)

import Subscriptions
import LatestStore
import CallbackSystem
import Shared
import Duration
import Protocol (ImmediateResponse, Response (..))


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



-- | trigger events and save data
processData :: Shared -> SensorData -> IO ()
processData Shared {sESubDB = eventSubsDB, sLatestStore = latestStore}
            newData@SensorData { sdSensor = sensor
                               , sdValue = newValue
                               , sdTimestamp = newTime} = do
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

triggerEventSubs :: Shared -> Sensor -> Event -> IO ImmediateResponse
triggerEventSubs Shared {sESubDB = eventSubsDB,
                         sLatestStore = latestStore} sensor event = do

    eventSubs <- query eventSubsDB $ LookupESub sensor

    -- TODO: same line as in processData
    let filteredSubs = filter ((== event) . esEvent) eventSubs

    if null filteredSubs then
        return $ Success 0 -- Event triggered but nobody wanted it

    else do
        mLastData <- query latestStore $ LookupSensorData sensor

        case mLastData of
            Just lastData ->
                let callback = flip sendCallback [lastData] . esSubData
                in do
                    mapM_ callback filteredSubs
                    return $ Success 0
            Nothing ->
                return $ Failure 0 404 $ "No data for sensor: " `Text.append` sensor




