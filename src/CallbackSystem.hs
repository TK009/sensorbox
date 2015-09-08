module CallbackSystem where


import Control.Arrow ((&&&))
import Network.Simple.TCP (connect, HostName, ServiceName)
import Network.Socket (socketToHandle)
-- import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle, IOMode (..))
import Control.Concurrent.STM.TVar (readTVarIO, modifyTVar')
import Control.Concurrent.STM (atomically)
import Control.Exception (try, IOException, displayException)

import qualified Data.Map as Map


import Subscriptions
import Shared
import Socket (receiveRequest)
import LatestStore (SensorData)
import ProtocolImpl (sendResponse)


type ConnectionQueue = TQueue (SubData, SensorData)

-- | One 'TQueue' for connection thread, only the connection thread owning a
-- TQueue should remove it from the Map.
type OpenConnections = TVar (Map Callback ConnectionQueue)

-- | This should be forked. Routes callbacks from shared callbackQueue to
-- right connection handlers or create a new.
callbackLoop :: Shared -> IO ()
callbackLoop shared @ Shared {sCallbackQueue = cbQueue} = do
    connectionsVar <- newTVarIO Map.empty

    let loop = do
            cbData @ (SubData {sCallback = callbackAddr}, _) <- atomically $ readTQueue cbQueue

            connQueue <- getConnection shared callbackAddr
            atomically $ writeTQueue connQueue 

            loop
    loop


-- | Create a new connection or reuse old connection
getConnection :: Shared -> OpenConnections -> Callback -> IO ConnectionQUeue
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
            forkCallbackAgent new
            return new
  where
    (host, port, isRawResponse) = parseCallback callbackAddr

    handleCallbacks = if isRawResponse then handleRawCallbacks else handleNormalCallbacks

    forkCallbackAgent handleQueue = forkIO $ connect host port (newConnFunction handleQueue)

    newConnFunction queue (sock, _) = do
        sockHandle <- socketToHandle sock ReadWriteMode
        handleCallbacks queue sockHandle

    handleNormalCallback queue handle = do
        (subData, sensorData) <- atomically $ readTQueue queue
        let reqID = sRequestID subData

        -- 1. send
        sendResponse handle $ Results reqID sensorData

        -- 2. listen response
        r <- try $ receiveRequest shared handle
        case r of
            Left e ->
                putStrLn "[WARN] Exception while waiting response from callback: " ++
                    show callbackAddr ++ ":" ++ displayException e
            Right -> -- 3. execute followup request
                -- TODO:




    handleRawCallbac handle = do
        let rawResults = case sensorData of
                [single] -> Raw $ showt $ sdValue single
                many     -> Raw $ showt $ map (sdSensor &&& sdValue) many
                []       -> Raw ""

        sendResponse handle rawResults
        -- TODO: listen response


-- | parses callback, results in (host, port, isRawResponse)
parseCallback :: Callback -> (HostName, ServiceName, Bool)
parseCallback ipPort = case ipPort of
    IP    hostPort -> parseIPStr hostPort False
    IPRaw hostPort -> parseIPStr hostPort True
  where
    parseIPStr str isRaw =
      let (host, port) = break (== ':') str
      in (host, tail port, isRaw)


-------------------------
---- line of death ------

-- | Re-uses connection if it exists, otherwise creates a new
-- withConnection %connectionsVar% %callbackAddr% %normalFunc% %funcForRaw%
withConnection :: OpenConnections
               -> Callback
               -> (Handle -> IO r)
               -> (Handle -> IO r)
               -> IO r
withConnection connectionsVar callbackAddr normalFunc funcForRaw = do
    connections <- readTVarIO connectionsVar

    let existingConn = Map.lookup callbackAddr connections

    case existingConn of
        Just oldConnection -> do -- Try send, if fails then reconnect
            -- TODO: How does the threading sort out? How do we mark connection as busy

            r <- try $ func oldConnection

            case r of
                Left e -> do -- Reconnect
                    putStrLn $ "[DEBUG] Connection error: " ++ displayException (e :: IOException)
                    connect host port newConnFunction

                Right res -> -- Success
                    return res

        Nothing -> -- Connect, get socket, send, save connection
            connect host port newConnFunction

  where


