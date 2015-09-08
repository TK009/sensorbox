module Shared where

import Data.Acid (AcidState)
import Control.Concurrent.STM       (STM, atomically)
import Control.Concurrent.STM.TVar  (TVar, readTVar, writeTVar)
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue)

import LatestStore (LatestStore, SensorData)
import Subscriptions (EventSubscriptions, IntervalSubscriptions, RequestID, SubData)


-- | Shared state
data Shared = Shared
    { sLatestStore :: AcidState LatestStore
    , sESubDB :: AcidState EventSubscriptions
    , sISubDB :: AcidState IntervalSubscriptions
    , sNextRequestID :: TVar RequestID
    , sCallbackQueue :: TQueue (SubData, [SensorData])
    }

getNextID :: Shared -> STM RequestID
getNextID Shared {sNextRequestID = nextIDVar} = do
    nextID <- readTVar nextIDVar
    writeTVar nextIDVar $ nextID + 1
    return nextID

sendCallback :: Shared -> SubData -> [SensorData] -> IO ()
sendCallback Shared {sCallbackQueue = cbQueue} subdata sensordata =
    atomically . writeTQueue cbQueue $ (subdata, sensordata)

