{-# LANGUAGE OverloadedStrings #-}
module ProtocolImpl where

import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Acid
import Control.Concurrent.STM (atomically)
import System.IO (Handle)
import Data.Text.IO (hPutStrLn)
import TextShow

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
            mCallback = parseCallback callb

        case mCallback of
            Nothing -> return $ Failure 0 400 "Invalid callback address."
            Just callback -> do
                let subData  = SubData requestID currentTime expiry callback meta

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


sendResponse :: Handle -> Response -> IO ()
sendResponse handle (Raw responseText) = hPutStrLn handle responseText
sendResponse handle response           = hPutStrLn handle $ showt response


-- | Parse new data applying the given currentTime to the values when needed
parseNewData :: Sensor -> NewSensorData -> UTCTime -> SensorData
parseNewData sensor (Data value) currentTime    =
    SensorData sensor (UntypedData value) currentTime

parseNewData sensor (OldData timestamp value) _ =
    SensorData sensor (UntypedData value) $ parseTimestamp timestamp

-- TODO better validation
parseCallback :: RCallback -> Maybe Callback
parseCallback (ToIP callback)    =
    if hostPortValidate callback
    then Just $ IP callback
    else Nothing
parseCallback (ToIPRaw callback) =
    if hostPortValidate callback
    then Just $ IPRaw callback
    else Nothing

-- | Some simple validation for format "123.123.123.123:1234"
hostPortValidate :: String -> Bool
hostPortValidate hostPort = colons == 1 -- && dots == 3
  where colons = length $ filter (== ':') hostPort
        -- can be a hostname
        -- dots   = length $ filter (== '.') hostPort 

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
