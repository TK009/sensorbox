
import Data.Acid
import Control.Concurrent.STM.TVar

import Data.Foldable (maximumBy)
import Data.Function (on)

import DataController
import Subscriptions
import LatestStore
import Shared
import Socket


main :: IO ()
main = do
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

    let shared = Shared latestValues eventSubs intervalSubs nextReqID 

    _ <- startIntervalThread shared

    return ()



