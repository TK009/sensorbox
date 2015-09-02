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
import Data.Time (UTCTime, NominalDiffTime)
import Data.Int (Int64)

import LatestStore (Sensor)


-- * Subscription data

type RequestID = Int64

-- | Contains common data for some subscription
data SubData = SubData
    { sRequestID :: !RequestID -- ^ Unique ID
    , sStartTime :: !UTCTime   -- ^ When was this sub loaded
    , sExpiry    :: !UTCTime   -- ^ Time to live is limited to this expiration time
    , sCallback  :: !Callback  -- ^ where to send data
    , sMetaData  :: !Text      -- ^ Reason for this subscription or other comment
                         -- Single line is recommended for easy debugging and logging purposes
    } deriving (Show, Typeable)

-- | Event Subscriptions; Callback when specified event is triggered
data ESub = ESub
    { esSensor  :: !Sensor -- ^ What sensor to watch
    , esEvent   :: !Event  -- ^ and on which event to trigger
    , esSubData :: !SubData
    } deriving (Show, Typeable)

-- | Interval Subscription; Callbacks on intervals
data ISub = ISub
    { isSensors  :: ![Sensor]        -- ^ sensors to read
    , isInterval :: !NominalDiffTime -- ^ In seconds
    , isSubData  :: !SubData
    } deriving (Show, Typeable)

data Event = OnChange  -- ^ Triggers whenever value of a sensor changes
           | OnUpdate  -- ^ Triggers every time an update on value is got (even if same)
           | OnAttach  -- ^ Triggers when a new `Sensor` is attached (the first value)
           deriving (Show, Typeable)

data Callback = Callback String  -- ^ Connect and send to this URL
              deriving (Show, Eq, Typeable)


-- -- | Send another callback or internal write event
-- data InternalCallback = ReCallback Callback | WriteEvent Sensor TypedData
--     deriving (Show, Typeable)

data EventSubscriptions = EventSubscriptions {allESubs :: !(Map Sensor [ESub])}
    deriving (Show, Typeable)

type IntervalQueue = Seq.Seq (UTCTime, ISub)
data IntervalSubscriptions = IntervalSubscriptions {allISubs :: !(Map RequestID ISub)}
    deriving (Show, Typeable)



-- * Queries

-- TODO: Is this even needed?
getAllESubs :: Query EventSubscriptions [ESub]
getAllESubs = concat . Map.elems . allESubs <$> ask

getAllISubs :: Query IntervalSubscriptions [ISub]
getAllISubs = Map.elems . allISubs <$> ask

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
    let rId = sRequestID $ isSubData sub
    put $ IntervalSubscriptions $ Map.insert rId sub subs

removeESub :: Sensor -> Update EventSubscriptions ()
removeESub sensor = do
    EventSubscriptions subs <- get
    put $ EventSubscriptions $ Map.delete sensor subs

removeISub :: RequestID -> Update IntervalSubscriptions ()
removeISub rId = do
    IntervalSubscriptions subs <- get
    put $ IntervalSubscriptions $ Map.delete rId subs



-- * TH Deriving stuff
-- | Make instances etc for acid

-- deriveSafeCopy version 'base|'extension ''Type
deriveSafeCopy 1 'base ''ISub
deriveSafeCopy 1 'base ''ESub
deriveSafeCopy 1 'base ''Event
deriveSafeCopy 1 'base ''Callback
deriveSafeCopy 1 'base ''SubData
deriveSafeCopy 1 'base ''EventSubscriptions
deriveSafeCopy 1 'base ''IntervalSubscriptions

$(makeAcidic ''EventSubscriptions ['getAllESubs, 'lookupESub, 'addESub])
$(makeAcidic ''IntervalSubscriptions ['getAllISubs, 'addISub])

