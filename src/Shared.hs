module Shared where

import Data.Acid

import LatestStore (LatestStore)
import Subscriptions (EventSubscriptions, IntervalSubscriptions)

-- | Shared state
data Shared = Shared
    { sLatestStore :: AcidState LatestStore
    , sESubDB :: AcidState EventSubscriptions
    , sISubDB :: AcidState IntervalSubscriptions
    }
