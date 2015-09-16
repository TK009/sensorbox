{-# LANGUAGE OverloadedStrings #-}
module CallbackSystem where


import Control.Arrow ((&&&))
-- import Network.Simple.TCP (connect, HostName, ServiceName)
import Network.Socket (socketToHandle, HostName, ServiceName)
-- import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle, IOMode (..))
import Control.Concurrent (forkIO, ThreadId, myThreadId)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar, modifyTVar')
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
import Socket (receiveRequest, forkConnect)
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

            eConnQueue <- try $ getConnection shared connectionsVar callbackAddr
            case eConnQueue of
                Left e ->
                    logException ("[WARN] Giving up, lost data " ++ show cbData) e
                Right connQueue ->
                    atomically $ writeTQueue connQueue cbData

            loop
    loop

  where
    logException :: String -> IOException -> IO ()
    logException msg e = putStrLn $ msg ++
        -- ", with callback: " ++ show cbAddr ++
        ": " ++ displayException e


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

    logException :: String -> IOException -> IO ()
    logException msg e = putStrLn $ msg ++ ", with callback: " ++
            show callbackAddr ++ ": " ++ displayException e

    forkCallbackAgent handleQueue = do
        putStrLn $ "[DEBUG] Connecting to callback " ++ show callbackAddr
        forkConnect host port $ handleCallbacks handleQueue


    -- newConnFunction queue (sock, _) = do
    -- newConnFunction queue sockHandle =
        -- sockHandle <- socketToHandle sock ReadWriteMode
        -- tid <- forkIO $ handleCallbacks queue sockHandle
        -- putStrLn $ "[DEBUG] Forked connection thread " ++ show tid


    handleCallbacks :: ConnectionQueue -> Handle -> IO ()
    handleCallbacks queue handle = do
        originalMsgData @ (subData, sensorData) <- atomically $ readTQueue queue
        let reqID = sRequestID subData
            rawResults = case sensorData of
                [single] -> Raw $ showt $ sdValue single
                []       -> Raw ""
                many     -> Raw $ showt $ map (sdSensor &&& sdValue) many
            normalResults = Results reqID sensorData
            results = if isRawResponse then rawResults else normalResults

        logThread $ "[DEBUG] Sending " ++ show results
        -- 1. send
        r1 <- try $ sendResponse handle results

        case r1 of
            Left e -> do  -- Connection is probably closed
                logException "[DEBUG] Connection error, reloading connection" e

                -- Remove this connection and ...
                atomically $ do
                    modifyTVar' connectionsVar $
                        Map.delete callbackAddr
                    -- Restart this message to reconnect
                    writeTQueue queue originalMsgData

            Right _ -> do -- 2. listen response

                logThread "[DEBUG] Listening callback response... "
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
                        logException "[WARN] Exception while waiting response" e
                    Right _ ->
                        return ()
        handleCallbacks queue handle

logThread :: String -> IO ()
logThread msg = do
    tid <- myThreadId
    putStrLn $ msg ++ " [" ++ show tid ++ "]"


-- | parses callback, results in (host, port, isRawResponse)
parseCallback :: Callback -> (HostName, ServiceName, Bool)
parseCallback ipPort = case ipPort of
    IP    hostPort -> parseIPStr hostPort False
    IPRaw hostPort -> parseIPStr hostPort True
  where
    parseIPStr str isRaw =
      let (host, port) = break (== ':') str
      in (host, tail port, isRaw)





