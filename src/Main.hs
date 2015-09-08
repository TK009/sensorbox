
import Data.Acid
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue

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
import CallbackSystem

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

    putStrLn $ "[INFO] Loaded " ++ show (length currentESubs) ++ " EventSubs and " ++
        show (length currentISubs) ++ " IntervalSubs."

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

    emptyTQ <- newTQueueIO

    let shared = Shared latestValues eventSubs intervalSubs nextReqID emptyTQ

    iTId <- startIntervalThread shared
    putStrLn $ "[INFO] Interval thread started: " ++ show iTId

    csTId <- startCallbackSystem shared
    putStrLn $ "[INFO] Interval thread started: " ++ show csTId

    serve (Host interface) port $ handleConn shared

    putStrLn "[INFO] Main thread exiting..."
    return ()



