module CallbackSystem where


import Network.Simple.TCP (connect, HostName, ServiceName, Socket, SockAddr)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadMask)
import qualified Data.Map as Map
import Control.Concurrent.STM.TVar (readTVarIO)


import Subscriptions
import Shared
import LatestStore (SensorData)

-- | Re-uses connection if it exists, otherwise creates a new
withConnection :: (MonadIO m, MonadMask m)
               => OpenConnections
               -> Callback
               -> ((Socket, SockAddr) -> m r)
               -> m r
withConnection connectionsVar callbackAddr func = do
    connections <- liftIO $ readTVarIO connectionsVar

    let existingConn = Map.lookup callbackAddr connections

    case existingConn of
        Just oldConnection -> undefined -- Try send, if fails then reconnect
        Nothing -> connect host port func -- Connect, get socket, send, save connection
  where
    (host, port, isRawResponse) = parseCallback callbackAddr

-- | parses callback, results in (host, port, isRawResponse)
parseCallback :: Callback -> (HostName, ServiceName, Bool)
parseCallback ipPort = case ipPort of
    IP    hostPort -> parseIPStr hostPort False
    IPRaw hostPort -> parseIPStr hostPort True
  where
    parseIPStr str isRaw =
      let (host, port) = break (== ':') str
      in (host, port, isRaw)

sendCallback :: OpenConnections -> SubData -> [SensorData] -> IO ()
sendCallback conns = undefined

