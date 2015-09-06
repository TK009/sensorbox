module CallbackSystem where


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
import LatestStore (SensorData)

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
    (host, port, isRawResponse) = parseCallback callbackAddr

    func = if isRawResponse then funcForRaw else normalFunc

    -- newConnFunction :: (MonadIO m, MonadMask m)
    --                => (Socket, SockAddr) -> m r
    newConnFunction (sock, _) = do
            sockHandle <- socketToHandle sock ReadWriteMode
            atomically $
                modifyTVar' connectionsVar $
                    Map.insert callbackAddr sockHandle
            func sockHandle


-- | parses callback, results in (host, port, isRawResponse)
parseCallback :: Callback -> (HostName, ServiceName, Bool)
parseCallback ipPort = case ipPort of
    IP    hostPort -> parseIPStr hostPort False
    IPRaw hostPort -> parseIPStr hostPort True
  where
    parseIPStr str isRaw =
      let (host, port) = break (== ':') str
      in (host, tail port, isRaw)

sendCallback :: OpenConnections -> SubData -> [SensorData] -> IO ()
sendCallback conns = undefined

