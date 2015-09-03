module Protocol where

import Data.Time.Clock (UTCTime)
import Subscriptions (Event, RequestID)
import LatestStore (Sensor, UntypedData)


-- TODO: is a version system needed?
-- | Protocol definition:
-- Server receives (<), Server sends (>)
-- - All requests end to ';' or '\n'
-- - Callbacks should reply with a new request or empty request (request end)
-- - This protocol can re-use the same callback or request connection so
--   they can be left open or closed depending the use case.
-- - There is a special case for "raw" subscriptions that doesn't ansver with
--   Response and allows implicit write request.
--
-- Request Sending:
-- < 'Request'
-- > 'Response'
--
-- Callbacks
-- > 'Response'
-- < 'Request' or empty request
-- > ['Response']
--
type Protocol = Request


-- |
-- < Write "Objects/Puistola/Temperature" (Data "24.26")
-- > Success 0
--
-- < Subscribe (OnInterval 10 Secs) (TTL 30 Mins)
--     ["Objects/Puistola/Temperature"] (Callback "localhost" 8094) "For drawing graph"
-- > Success 24
-- >c Results 24 [SensorData "Objects/Puistola/Temperature" "42" 2015-09-02 11:37:59.58345 UTC]
-- <c ;
--
-- < Cancel 34
-- > Failure 34
--
-- < ForceEvent OnUpdate "Objects/Puistola/Light1"
-- > Success 0
--
-- < Subscribe (OnIntervalRaw 20 Secs) (TTL 360 Days)
--     ["Objects/Puistola/VirtualPullCommands/Moisture1"]
--     "Objects/Puistola/Derp"
-- >c DR 6
-- <c 0.241  -- Writes to "Derp"
--  (Success)
--
--
data Request = Write Sensor NewSensorData -- ^ Save a new value [and Sensor]
             | Subscribe SubType TTL [Sensor] MetaData
             | Cancel RequestID        -- ^ Cancel a Subscription
             | ForceEvent Event Sensor -- ^ Mainly for restarting Output device subs
             | Erase Sensor            -- ^ Removes the Sensor

-- | Simpler version for protocol
data NewSensorData = Data Text
                   | OldData Timestamp Text

data Timestamp = UTC UTCTime
               | UnixTime Double  -- Double to allow fractions of seconds

-- | Raw versions will have callback responses as Text values only
-- and will accept responses as write requests to sensor given in MetaData field
data SubType = OnInterval Double TimeUnit
             | OnIntervalRaw Double TimeUnit
             | Event Event
             | EventRaw Event

type MetaData = Text


data Response = Success RequestID
              | Failure RequestID Code Text
              | Results RequestID [SensorData]
              | Raw Text

type Code = Int
data TTL  = TTL Double TimeUnit
          | TTLUntil Timestamp

data TimeUnit = Secs
              | Mins
              | Hours
              | Days
