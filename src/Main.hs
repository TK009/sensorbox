
import Data.Acid

import DataController
import Subscriptions
import LatestStore
import Shared


main :: IO ()
main = do
    latestValues <- openLocalState emptyLatestStore
    eventSubs    <- openLocalState emptyEventSubscriptions
    intervalSubs <- openLocalState emptyIntervalSubscriptions

    let shared = Shared latestValues eventSubs intervalSubs

    _ <- startIntervalThread shared

    return ()



