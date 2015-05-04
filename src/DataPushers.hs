{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module DataPushers where

import Control.Monad (liftM, forM_)
import Control.Monad.IO.Class (liftIO)

import Data.Typeable
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Text (Text)
import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Map (Map)
import qualified Data.Map as M

import System.Hardware.Arduino

{- class InputAgentConfig a where
--     initialize :: InputAgent a => c -> a
-}

{- type AgentHandle
-- class InputAgent a where
--     loop :: IO AgentHandle
-}


type Sensor = Text

newtype UntypedData = UntypedData Text deriving (Eq, Show)

data TypedData = IntT Int
               | DoubleT Double
               | BoolT Bool
               | GenericT Text
    deriving (Eq, Show, Typeable)

-- For acid-state
deriveSafeCopy 1 'base ''TypedData


class AsTypedData a where
    toTypedData :: a -> TypedData

instance AsTypedData Int where
    toTypedData = IntT

instance AsTypedData Double where
    toTypedData = DoubleT

instance AsTypedData Bool where
    toTypedData = BoolT


toGenericData :: Show a => a -> TypedData
toGenericData = GenericT . T.pack . show


data SensorData = SensorData
    { sdSensor :: Sensor
    , sdTimestamp :: UTCTime
    , sdValue  :: TypedData
    }
    deriving (Show, Typeable)

deriveSafeCopy 1 'base ''SensorData



-- | Design: all configurable data should be contained in `a` and functions in instances
class Agent a where

    -- | Takes `InputPusher` as parameter and should use it to push `SensorData` from `sensors`
    runLoop :: InputPusher -> a -> IO ()

    -- | Max interval (seconds) in which the agent pushes data. Negative if not known.
    interval :: a -> Double
    -- | Max interval in milliseconds
    intervalms :: a -> Int
    intervalms = round . (* 1000) . interval

    -- | The Agent should offer the data of these sensors only
    sensors :: a -> [Sensor]


-- | This function is given to puller Agents to push the data
type InputPusher = SensorData -> IO ()


-- | Puller Agent for arduino board
-- Has default implementation for runLoop of Agent
class Agent a => ArduinoAgent a where
    boardLocation :: a -> FilePath
    pullPushLoop :: InputPusher -> a -> Arduino ()



-- | NOTE: Didn't find any way to re-define runLoop default implementation.
-- So this can be used as default in ArduinoAgent instances.
runLoopArduino :: ArduinoAgent a => InputPusher -> a -> IO ()
runLoopArduino push agent = withArduino True (boardLocation agent) (pullPushLoop push agent)



-- * Implementations
data ArduinoSensor = AnalogRead Pin
                   | DigitalRead Pin
                   | DigitalOutput Pin
                   -- ^ changes and gives output status of a pin as sensordata
                   deriving (Show)
                   -- Should arduino actions be done here or at callback end:
                   -- --| WithAction ArduinoSensor (Data -> Arduino ())
                   -- TODO: Custom shift addon, OneWire

data HouseAgent = HouseAgent
    { haBoardLocation :: FilePath
    , haInterval :: Double
    , haSensors :: Map Sensor ArduinoSensor
    , ledPin :: Pin
    }

instance Agent HouseAgent where
    runLoop = runLoopArduino
    interval = haInterval
    sensors = M.keys . haSensors


instance ArduinoAgent HouseAgent where

    boardLocation = haBoardLocation

    pullPushLoop push houseAgent = initialize >> loop
      where
          initialize = return ()
          loop = do
              ledOn
              sensorDataMap <- M.traverseWithKey handleSensor arduinoSensors
              ledOff

              liftIO $ forM_ sensorDataMap push

              delay $ intervalms houseAgent
              loop

          arduinoSensors = haSensors houseAgent

          led = ledPin houseAgent
          ledOn  = digitalWrite led True
          ledOff = digitalWrite led False

          handleSensor :: Sensor -> ArduinoSensor -> Arduino SensorData
          handleSensor sensor sensortype = do
              value    <- sensorLogic sensortype
              currTime <- liftIO getCurrentTime
              return $ SensorData sensor currTime value

          sensorLogic :: ArduinoSensor -> Arduino TypedData
          sensorLogic (AnalogRead fromPin) = do
              value <- analogRead fromPin
              -- scale the value to [0,1] from [0,1023]
              return . toTypedData $ (fromIntegral value :: Double) / 1023

          sensorLogic (DigitalRead fromPin) =
              liftM toTypedData (digitalRead fromPin)

          sensorLogic (DigitalOutput toPin) = do
              writeValue <- liftIO undefined -- TODO
              case writeValue of
                  (BoolT value) -> do
                      digitalWrite toPin value
                      return . toTypedData $ value
                  _ -> do
                      liftIO $ T.putStrLn "Warning: DigitalOutput should get type BoolT"
                      return undefined -- FIXME







