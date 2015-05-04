{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Subscriptions where

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.SafeCopy
import Data.Typeable
import Data.Acid
import Data.Maybe (maybeToList)
import Data.Text (Text)
-- import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

import DataPushers


-- * Subscription data

-- | Event Subscriptions; Callback when specified event is triggered
data ESub = ESub
    { esSensor :: !Sensor -- ^ What sensor to watch
    , esEvent  :: !Event  -- ^ and on which event to trigger
    , esCallback :: !Callback -- ^ where to send data
    } deriving (Show, Typeable)

-- | Interval Subscription; Callbacks on intervals
data ISub = ISub
    { isSensors  :: ![Sensor]  -- ^ sensors to read
    , isInterval :: !Double    -- ^ In seconds
    , isCallback :: !Callback  -- ^ where to send data
    } deriving (Show, Typeable)

data Event = OnChange  -- ^ Triggers whenever value of a sensor changes
           | OnUpdate  -- ^ Triggers every time an update on value is got (even if same)
           | OnAttach  -- ^ Triggers when a new `Sensor` is attached (the first value)
           deriving (Show, Typeable)

type CallbackFunction = SensorData -> IO InternalCallback

class Callback c where
    runCallback :: CallbackFunction

data Callback = ToLog Text     -- ^ Print data to log/terminal with some text as prefix
              | ToCSV FilePath -- ^ Saves to csv
              | InternalFunction -- ^ internal function with id
              -- ^ do some logic and either send other callback or internal write
              -- TODO: some others; OMI?, DB?,
    deriving (Show, Typeable)



-- | Send another callback or internal write event
data InternalCallback = ReCallback Callback | WriteEvent Sensor TypedData
    deriving (Show, Typeable)

data EventSubscriptions = EventSubscriptions {allESubs :: !(Map Sensor [ESub])}
    deriving (Show, Typeable)

data IntervalSubscriptions = IntervalSubscriptions {allISubs :: [ISub]}
    deriving (Show, Typeable)


-- * Internal Callbacks (custom)



-- * Queries

-- TODO: Is this even needed?
getAllESubs :: Query EventSubscriptions [ESub]
getAllESubs = concat . Map.elems . allESubs <$> ask

getAllISubs :: Query IntervalSubscriptions [ISub]
getAllISubs = allISubs <$> ask

lookupESub :: Sensor -> Query EventSubscriptions [ESub]
lookupESub key = do
    EventSubscriptions esubs <- ask
    return . concat . maybeToList $ Map.lookup key esubs

addESub :: ESub -> Update EventSubscriptions ()
addESub sub@ESub {esSensor = sensor} = do
    EventSubscriptions subs <- get
    put $ EventSubscriptions $ Map.insertWith (++) sensor [sub] subs


addISub :: ISub -> Update IntervalSubscriptions ()
addISub sub = do
    IntervalSubscriptions subs <- get
    put $ IntervalSubscriptions (sub : subs)





-- * TH Deriving stuff
-- | Make instances etc for acid

-- deriveSafeCopy version 'base|'extension ''Type
deriveSafeCopy 1 'base ''ISub
deriveSafeCopy 1 'base ''ESub
deriveSafeCopy 1 'base ''Event
deriveSafeCopy 1 'base ''Callback
deriveSafeCopy 1 'base ''EventSubscriptions
deriveSafeCopy 1 'base ''IntervalSubscriptions

$(makeAcidic ''EventSubscriptions ['getAllESubs, 'lookupESub, 'addESub])
$(makeAcidic ''IntervalSubscriptions ['getAllISubs, 'addISub])

