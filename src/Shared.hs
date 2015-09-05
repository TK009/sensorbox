module Shared where

import Data.Acid (AcidState)
import Data.Map.Strict (Map)
import Control.Concurrent.STM       (STM)
import Control.Concurrent.STM.TVar  (TVar, readTVar, writeTVar)
import Network.Simple.TCP (SockAddr)
import System.IO (Handle)

import LatestStore (LatestStore)
import Subscriptions (EventSubscriptions, IntervalSubscriptions, RequestID, Callback)

type OpenConnections = TVar (Map Callback Handle)

-- | Shared state
data Shared = Shared
    { sLatestStore :: AcidState LatestStore
    , sESubDB :: AcidState EventSubscriptions
    , sISubDB :: AcidState IntervalSubscriptions
    , sNextRequestID :: TVar RequestID
    , sConnections :: OpenConnections
    }

getNextID :: Shared -> STM RequestID
getNextID Shared {sNextRequestID = nextIDVar} = do
    nextID <- readTVar nextIDVar
    writeTVar nextIDVar $ nextID + 1
    return nextID


