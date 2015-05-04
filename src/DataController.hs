module DataController where

import Data.Acid

import DataPushers
import Subscriptions
import LatestStore

data Shared = Shared
    { sLatestStore :: AcidState LatestStore
    , sESubDB :: AcidState EventSubscriptions
    , sISubDB :: AcidState IntervalSubscriptions
    }

startAgents :: Shared -> IO () -- TODO
startAgents shared@Shared {sESubDB = eventSubsDB} = do
    undefined
  where 
    dataController :: InputPusher  -- SensorData -> IO ()
    dataController sensorData@SensorData {sdSensor = sensor, sdValue = value} = do
        eventSubs <- query eventSubsDB $ LookupESub sensor
        
        --let onUpdate = filter eventSubs
        return ()


        


