{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LatestStore where

import Data.Maybe (mapMaybe)
import Data.SafeCopy
import Data.Typeable
import Data.Acid
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (UTCTime)

-- * Sensor types

type Sensor = Text

newtype UntypedData = UntypedData Text deriving (Eq, Show)

data SensorData = SensorData
    { sdSensor :: Sensor
    , sdTimestamp :: UTCTime
    , sdValue  :: UntypedData
    }
    deriving (Show, Typeable)


-- * Latest Store datatype

data LatestStore = LatestStore {lsAllData :: !(Map Sensor SensorData)}
    deriving (Show, Typeable)

emptyLatestStore :: LatestStore
emptyLatestStore = LatestStore Map.empty 


-- * Queries

lookupSensorData :: Sensor -> Query LatestStore (Maybe SensorData)
lookupSensorData sensor = do
    LatestStore allData <- ask
    return $ Map.lookup sensor allData

lookupSensorDatas :: [Sensor] -> Query LatestStore [SensorData]
lookupSensorDatas sensors = do
    LatestStore allData <- ask
    return $ mapMaybe (`Map.lookup` allData) sensors

setSensorData :: SensorData -> Update LatestStore ()
setSensorData sensorData@SensorData {sdSensor = sensor} = do
    LatestStore allData <- get
    put . LatestStore $ Map.insert sensor sensorData allData


-- * TH stuff

deriveSafeCopy 1 'base ''UntypedData
deriveSafeCopy 1 'base ''SensorData
deriveSafeCopy 1 'base ''LatestStore
$(makeAcidic ''LatestStore ['lookupSensorData, 'lookupSensorDatas, 'setSensorData])


