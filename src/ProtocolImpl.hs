{-# LANGUAGE OverloadedStrings #-}
module ProtocolImpl where

import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Acid
import Control.Concurrent.STM (atomically)

import Protocol
import Shared
import DataController (processData, triggerEventSubs)
import Duration
import Subscriptions
import LatestStore


processRequest :: Shared -> Request -> IO ImmediateResponse
processRequest shared@Shared { sISubDB = intervalSubs
                             , sESubDB = eventSubs
                             , sLatestStore = latestStore} = runRequest
  where
    runRequest (Write sensor newData) = do
        currentTime <- getCurrentTime
        processData shared $ parseNewData sensor newData currentTime
        return $ Success 0

    runRequest (Subscribe subType ttl callb sensors meta) = do
        currentTime <- getCurrentTime
        requestID   <- atomically $ getNextID shared

        let expiry   = parseTTL ttl currentTime
            callback = parseCallback callb
            subData  = SubData requestID currentTime expiry callback meta

        case subType of
            OnInterval rawInterval timeUnit ->
                let interval = parseDuration timeUnit rawInterval
                    isub = ISub sensors interval subData
                in update intervalSubs $ AddISub isub

            Event event ->
                let esubs = map (\ sensor -> ESub sensor event subData) sensors
                in mapM_ (update eventSubs . AddESub) esubs

        return $ Success requestID

    runRequest (Cancel requestID) = do
        update intervalSubs $ RemoveISub requestID
        update eventSubs $ RemoveESub requestID
        return $ Success requestID

    runRequest (ForceEvent event sensor) = triggerEventSubs shared sensor event

    runRequest (Erase sensor) = do
        update latestStore $ EraseSensorData sensor
        return $ Success 0

-- | Parse new data applying the given currentTime to the values when needed
parseNewData :: Sensor -> NewSensorData -> UTCTime -> SensorData
parseNewData sensor (Data value) currentTime    =
    SensorData sensor (UntypedData value) currentTime

parseNewData sensor (OldData timestamp value) _ =
    SensorData sensor (UntypedData value) $ parseTimestamp timestamp

-- TODO Validate it
parseCallback :: RCallback -> Callback
parseCallback (ToIP callback)    = IP callback
parseCallback (ToIPRaw callback) = IPRaw callback

-- | parse TTL respective to given currentTime
parseTTL :: TTL -> UTCTime -> UTCTime
parseTTL (TTL time timeUnit) ct = addUTCTime (parseDuration timeUnit time) ct
parseTTL (TTLUntil timestamp) _ = parseTimestamp timestamp

parseDuration :: TimeUnit -> Double -> Duration
parseDuration Secs  = seconds
parseDuration Mins  = minutes
parseDuration Hours = hours
parseDuration Days  = days

parseTimestamp :: Timestamp -> UTCTime
parseTimestamp (UTC time) = time
parseTimestamp (UnixTime value) = posixSecondsToUTCTime $ realToFrac value
