-- implements
--
-- logic for switching pump
-- output for graph

import Data.Acid
import qualified Data.Map as Map

import DataController
import Subscriptions
import LatestStore

main :: IO ()
main = do
    latestValues <- openLocalState (LatestStore Map.empty)
    eventSubs    <- openLocalState (EventSubscriptions Map.empty)
    intervalSubs <- openLocalState (IntervalSubscriptions Map.empty)

    -- let shared = Shared latestValues eventSubs intervalSubs
    currentIntervalSubs <- query intervalSubs GetAllISubs
    startIntervalThread currentIntervalSubs



