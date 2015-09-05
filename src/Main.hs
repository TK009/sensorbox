
import Data.Acid
import Control.Concurrent.STM.TVar

import qualified Data.Map as Map
import Control.Monad (unless)
import Data.Foldable (maximumBy)
import Data.Function (on)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Network.Simple.TCP (serve, HostPreference (..))

import DataController
import Subscriptions
import LatestStore
import Shared
import Socket

usage :: String
usage = "Usage: sensorbox <listen interface> <listen port>"

main :: IO ()
main = do
    args <- getArgs
    unless (length args == 2) $ do
        putStrLn usage
        exitSuccess

    let interface = head args
        port      = args !! 1

    latestValues <- openLocalState emptyLatestStore
    eventSubs    <- openLocalState emptyEventSubscriptions
    intervalSubs <- openLocalState emptyIntervalSubscriptions

    -- find max requestID to save the next available ID
    currentESubs <- query eventSubs GetAllESubs
    currentISubs <- query intervalSubs GetAllISubs

    let esubID = sRequestID . esSubData
        maxESubID = if null currentESubs
                    then 0
                    else esubID $ maximumBy (compare `on` esubID) currentESubs

        isubID =  sRequestID . isSubData
        maxISubID = if null currentISubs
                    then 0
                    else isubID $ maximumBy (compare `on` isubID) currentISubs

        maxID = max maxESubID maxISubID

    nextReqID <- newTVarIO $ maxID + 1

    emptyConnections <- newTVarIO Map.empty

    let shared = Shared latestValues eventSubs intervalSubs nextReqID emptyConnections

    _ <- startIntervalThread shared

    serve (Host interface) port $ handleConn shared

    return ()



