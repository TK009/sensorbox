{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Subscriptions where

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.SafeCopy
import Data.Typeable
import Data.Acid
import Data.Maybe (mapMaybe)
import Data.Text (Text)
-- import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence ( Seq, (|>), ViewL (..) )
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Foldable (toList)
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)
import Data.Int (Int64)
import Control.Monad (guard)
import TextShow.TH

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
           deriving (Show, Typeable, Eq)

data Callback = IP String    -- ^ Connect and send to this URL
              | IPRaw String -- ^ Raw protocol behaviour
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

collectParentsAndSelf :: Sensor -> [Sensor]
collectParentsAndSelf self =
    let splitted = Text.split (== '/') self
        join = Text.intercalate "/"
        reverseParents [] = []
        reverseParents [root] = [root]
        reverseParents path@(_ : pathTail) = (join . reverse) path : reverseParents pathTail
    in reverseParents (reverse splitted)

lookupESub :: Sensor -> Query EventSubscriptions [ESub]
lookupESub deepestKey = do
    EventSubscriptions esubs <- ask
    let lookupPath key = Map.lookup key esubs
    return . concat . mapMaybe lookupPath $ collectParentsAndSelf deepestKey


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

removeESubsForSensor :: Sensor -> Update EventSubscriptions ()
removeESubsForSensor sensor = do
    EventSubscriptions subs <- get
    put $ EventSubscriptions $ Map.delete sensor subs

removeESub :: RequestID -> Update EventSubscriptions ()
removeESub rId = do
    EventSubscriptions subs <- get

    let nonEmptyList :: [a] -> Maybe [a]
        nonEmptyList []    = Nothing
        nonEmptyList stuff = Just stuff

        filterEmpty :: [ESub] -> Maybe [ESub]
        filterEmpty = nonEmptyList . filter ((/= rId) . sRequestID . esSubData)
        filteredSubs = Map.mapMaybe filterEmpty subs

    put $ EventSubscriptions filteredSubs

removeISub :: RequestID -> Update IntervalSubscriptions ()
removeISub rId = do
    IntervalSubscriptions subs <- get
    put $ IntervalSubscriptions $ Seq.filter ((/= rId) . sRequestID . isSubData . snd) subs



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

$(makeAcidic ''EventSubscriptions ['getAllESubs, 'lookupESub, 'addESub,
                                   'removeESubsForSensor, 'removeESub])
$(makeAcidic ''IntervalSubscriptions ['getAllISubs, 'addISub, 'lookupNextISub, 'removeISub])

$(deriveTextShow ''Event)
$(deriveTextShow ''Callback)
$(deriveTextShow ''SubData)
$(deriveTextShow ''ESub)
$(deriveTextShow ''ISub)
$(deriveTextShow ''NominalDiffTime)

