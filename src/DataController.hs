module DataController where

import Data.Acid
-- import qualified Data.Map as Map
import Data.Time.Clock
import Control.Concurrent
import qualified Data.Sequence as Seq

import Subscriptions
import LatestStore

data Shared = Shared
    { sLatestStore :: AcidState LatestStore
    , sESubDB :: AcidState EventSubscriptions
    , sISubDB :: AcidState IntervalSubscriptions
    }


-- | private, sort utility function
sortIntervalQueue :: IntervalQueue -> IntervalQueue
sortIntervalQueue = Seq.unstableSortBy cmp
  where cmp a b = fst a `compare` fst b


-- | Initialize intervalThread
startIntervalThread :: [ISub] -> IO ThreadId
startIntervalThread intervalSubs = do
    currentTime <- getCurrentTime

    let nextRuns       = map (calcNextRun currentTime) intervalSubs `zip` intervalSubs
        sortedNextRuns = sortIntervalQueue $ Seq.fromList nextRuns

    forkIO $ intervalLoop sortedNextRuns


-- | Calculate next interval step for ISub from the current time and the ISub
calcNextRun :: UTCTime -> ISub -> UTCTime
calcNextRun currentTime ISub {isInterval = interval, isSubData = subData} =
    let startTime    = sStartTime subData

        numOfCalls  :: Integer
        numOfCalls   = floor $ (currentTime `diffUTCTime` startTime) / interval
        numberOfNext = numOfCalls + 1

        offsetFromStart = interval * realToFrac numberOfNext

    in addUTCTime offsetFromStart currentTime

-- | Loops and executes interval subscriptions


{-
start :: Shared -> IO () -- TODO
start shared@Shared {sESubDB = eventSubsDB} = do
    undefined
  where
    dataController :: SensorData -> IO () -- InputPusher
    dataController sensorData@SensorData {sdSensor = sensor, sdValue = value} = do
        eventSubs <- query eventSubsDB $ LookupESub sensor

        --let onUpdate = filter eventSubs
        return ()
-}




