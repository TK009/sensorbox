{-# LANGUAGE OverloadedStrings #-}
module CallbackSystem where


import Control.Arrow ((&&&))
import Network.Simple.TCP (connect, HostName, ServiceName)
import Network.Socket (socketToHandle)
-- import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle, IOMode (..))
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, readTQueue, writeTQueue)
import Control.Concurrent.STM (atomically)
import Control.Exception (try, IOException, displayException)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text

import TextShow


import Subscriptions
import Shared
import Protocol
import Socket (receiveRequest)
import LatestStore (SensorData (..))
import ProtocolImpl (processRequest, sendResponse)


type ConnectionQueue = TQueue (SubData, [SensorData])

-- | One 'TQueue' for connection thread, only the connection thread owning a
-- TQueue should remove it from the Map.
type OpenConnections = TVar (Map Callback ConnectionQueue)

-- | Routes callbacks from shared callbackQueue to
-- right connection handlers or create a new.
startCallbackSystem :: Shared -> IO ThreadId
startCallbackSystem sh = forkIO $ callbackLoop sh

-- | This should be forked.
callbackLoop :: Shared -> IO ()
callbackLoop shared @ Shared {sCallbackQueue = cbQueue} = do
    connectionsVar <- newTVarIO Map.empty

    let loop = do
            cbData @ (SubData {sCallback = callbackAddr}, _) <- atomically $ readTQueue cbQueue

            connQueue <- getConnection shared connectionsVar callbackAddr
            atomically $ writeTQueue connQueue cbData

            loop
    loop


-- | Create a new connection or reuse old connection
getConnection :: Shared -> OpenConnections -> Callback -> IO ConnectionQueue
getConnection shared connectionsVar callbackAddr = do
    conn <- atomically $ do  -- STM monad: Right newConn, Left oldConn
        connections <- readTVar connectionsVar

        let existingConn = Map.lookup callbackAddr connections
        case existingConn of
            Just oldConnection -> return $ Left oldConnection
            Nothing -> do
                newConnection <- newTQueue
                writeTVar connectionsVar $
                    Map.insert callbackAddr newConnection connections
                return $ Right newConnection

    case conn of  -- IO monad
        Left  old -> return old
        Right new -> do
            _ <- forkCallbackAgent new
            return new
  where
    (host, port, isRawResponse) = parseCallback callbackAddr

    forkCallbackAgent handleQueue = do
        putStrLn $ "[DEBUG] Connecting callback " ++ show callbackAddr
        forkIO $ connect host port (newConnFunction handleQueue)

    newConnFunction queue (sock, _) = do
        sockHandle <- socketToHandle sock ReadWriteMode
        handleCallbacks queue sockHandle


    handleCallbacks :: ConnectionQueue -> Handle -> IO ()
    handleCallbacks queue handle = do
        (subData, sensorData) <- atomically $ readTQueue queue
        let reqID = sRequestID subData
            rawResults = case sensorData of
                [single] -> Raw $ showt $ sdValue single
                []       -> Raw ""
                many     -> Raw $ showt $ map (sdSensor &&& sdValue) many
            normalResults = Results reqID sensorData
            results = if isRawResponse then rawResults else normalResults

        -- 1. send
        r1 <- try $ sendResponse handle results

        case r1 of
            Left e ->
                putStrLn $ "[DEBUG] Connection error: " ++
                    displayException (e :: IOException)
            Right _ -> do -- 2. listen response

                -- Special responses for callbacks:
                let rawResponse ""   = return ()
                    rawResponse line = do
                        _ <- processRequest shared $
                            Write (sMetaData subData) (Data $ Text.pack line)
                        return ()

                    normalResponse ""   = return ()
                    normalResponse line =
                        putStrLn $ "[WARN] Received invalid response from callback: " ++
                            show callbackAddr ++ ": " ++ line

                    parseFail = if isRawResponse
                        then rawResponse
                        else normalResponse

                r2 <- try $ receiveRequest shared handle parseFail
                case r2 of
                    Left e ->
                        putStrLn $ "[WARN] Exception while waiting response from callback: " ++
                            show callbackAddr ++ ":" ++ displayException (e :: IOException)
                    Right _ ->
                        return ()
        handleCallbacks queue handle


-- | parses callback, results in (host, port, isRawResponse)
parseCallback :: Callback -> (HostName, ServiceName, Bool)
parseCallback ipPort = case ipPort of
    IP    hostPort -> parseIPStr hostPort False
    IPRaw hostPort -> parseIPStr hostPort True
  where
    parseIPStr str isRaw =
      let (host, port) = break (== ':') str
      in (host, tail port, isRaw)





