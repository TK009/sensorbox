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
import Data.Sequence ( Seq, (|>), ViewL (..) )
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)
import Data.Int (Int64)
import Control.Monad (guard)

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

-- * Persistent storage

data EventSubscriptions = EventSubscriptions {allESubs :: !(Map Sensor [ESub])}
    deriving (Show, Typeable)

emptyEventSubscriptions :: EventSubscriptions
emptyEventSubscriptions = EventSubscriptions Map.empty

type IntervalQueue = Seq (UTCTime, ISub)
data IntervalSubscriptions = IntervalSubscriptions {allISubs :: !IntervalQueue}
    deriving (Show, Typeable)

emptyIntervalSubscriptions :: IntervalSubscriptions
emptyIntervalSubscriptions = IntervalSubscriptions Seq.empty

-- * Utility

-- | private, sort utility function
sortIntervalQueue :: IntervalQueue -> IntervalQueue
sortIntervalQueue = Seq.unstableSortBy cmp
  where cmp a b = fst a `compare` fst b

-- | Calculate next interval step for ISub from the current time and the ISub
calcNextRun :: UTCTime -> ISub -> UTCTime
calcNextRun currentTime ISub {isInterval = interval, isSubData = subData} =
    let startTime    = sStartTime subData

        numOfCalls  :: Integer
        numOfCalls   = floor $ (currentTime `diffUTCTime` startTime) / interval
        numberOfNext = numOfCalls + 1

        offsetFromStart = interval * realToFrac numberOfNext

    in addUTCTime offsetFromStart currentTime

-- * Queries

-- TODO: Is this even needed?
getAllESubs :: Query EventSubscriptions [ESub]
getAllESubs = concat . Map.elems . allESubs <$> ask

getAllISubs :: Query IntervalSubscriptions [ISub]
getAllISubs = map snd . toList . allISubs <$> ask

lookupESub :: Sensor -> Query EventSubscriptions [ESub]
lookupESub key = do
    EventSubscriptions esubs <- ask
    return . concat . maybeToList $ Map.lookup key esubs


-- | Query with current time and get a triggered interval sub (if any) and
-- the next trigger time (if any).
-- Automatically updates the time and removes if the sub is expired
lookupNextISub :: UTCTime -> Update IntervalSubscriptions (Maybe ISub, Maybe UTCTime)
lookupNextISub currentTime = do
    IntervalSubscriptions subs <- get
    let safeHeadTail :: Seq.ViewL a -> Maybe (a, Seq a)
        safeHeadTail Seq.EmptyL = Nothing
        safeHeadTail (x :< xs)  = Just (x, xs)

        mHeadTail = do
            (first, rest) <- safeHeadTail $ Seq.viewl subs

            let (triggerTime, sub) = first
            guard $ currentTime > triggerTime

            return (sub, rest)

        newSubs = case mHeadTail of
            Nothing -> subs
            Just (sub, rest) ->
                let expiry     = sExpiry . isSubData $ sub
                    newTime    = calcNextRun currentTime sub
                    newSubPair = (newTime, sub)
                in if currentTime > expiry
                    then rest  -- deleted
                    else sortIntervalQueue $ rest |> newSubPair -- re-added
        
        mHeadSub = fst <$> mHeadTail
        mNext    = fst <$> safeHeadTail (Seq.viewl newSubs)
        mNextTriggerTime = fst <$> mNext

    put $ IntervalSubscriptions $ sortIntervalQueue newSubs
    return (mHeadSub, mNextTriggerTime)


addESub :: ESub -> Update EventSubscriptions ()
addESub sub@ESub {esSensor = sensor} = do
    EventSubscriptions subs <- get
    put $ EventSubscriptions $ Map.insertWith (++) sensor [sub] subs


addISub :: ISub -> Update IntervalSubscriptions ()
addISub sub = do
    IntervalSubscriptions subs <- get
    let referenceTime = sStartTime $ isSubData sub
        nextRun = calcNextRun referenceTime sub
        newSubs = subs |> (nextRun, sub)
    put $ IntervalSubscriptions $ sortIntervalQueue newSubs

-- add many:
--    let nextRuns       = map (calcNextRun currentTime) intervalSubs `zip` intervalSubs
--        sortedNextRuns = sortIntervalQueue $ Seq.fromList nextRuns

removeESub :: Sensor -> Update EventSubscriptions ()
removeESub sensor = do
    EventSubscriptions subs <- get
    put $ EventSubscriptions $ Map.delete sensor subs

removeISub :: RequestID -> Update IntervalSubscriptions ()
removeISub rId = do
    IntervalSubscriptions subs <- get
    put $ IntervalSubscriptions $ Seq.filter ((rId ==) . sRequestID . isSubData . snd) subs



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
$(makeAcidic ''IntervalSubscriptions ['getAllISubs, 'addISub, 'lookupNextISub])

