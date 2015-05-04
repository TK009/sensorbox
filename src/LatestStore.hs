{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module LatestStore where

import Data.SafeCopy
import Data.Typeable
import Data.Acid
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import qualified Data.Map as Map
import Data.Map (Map)

import DataPushers (Sensor, SensorData (..))

-- * Latest Store datatype

data LatestStore = LatestStore {lsAllData :: !(Map Sensor SensorData)}
    deriving (Show, Typeable)



-- * Queries

lookupSensorData :: Sensor -> Query LatestStore (Maybe SensorData)
lookupSensorData sensor = do
    LatestStore allData <- ask
    return $ Map.lookup sensor allData

setSensorData :: SensorData -> Update LatestStore ()
setSensorData sensorData@SensorData {sdSensor = sensor} = do
    LatestStore allData <- get
    put . LatestStore $ Map.insert sensor sensorData allData


-- * TH stuff

deriveSafeCopy 1 'base ''LatestStore
$(makeAcidic ''LatestStore ['lookupSensorData, 'setSensorData])


