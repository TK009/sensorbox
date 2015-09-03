module Shared where

import Data.Acid (AcidState)
import Control.Concurrent.STM       (STM)
import Control.Concurrent.STM.TVar  (TVar, readTVar, writeTVar)

import LatestStore (LatestStore)
import Subscriptions (EventSubscriptions, IntervalSubscriptions, RequestID)

-- | Shared state
data Shared = Shared
    { sLatestStore :: AcidState LatestStore
    , sESubDB :: AcidState EventSubscriptions
    , sISubDB :: AcidState IntervalSubscriptions
    , nextRequestID :: TVar RequestID
    }

getNextID :: Shared -> STM RequestID
getNextID Shared {nextRequestID = nextIDVar} = do
    nextID <- readTVar nextIDVar
    writeTVar nextIDVar $ nextID + 1
    return nextID
