module Duration where

-- | This module should be imported qualified

import Data.Time.Clock (NominalDiffTime)
import qualified Control.Concurrent as C (threadDelay)

type Duration = NominalDiffTime

dayMult, hourMult, minuteMult, milliMult, microMult :: Duration

minuteMult = 60
hourMult   = 3600
dayMult    = hourMult * 24

milliMult  = 1000
microMult  = 1000000

-- * Conversion

days, hours, minutes, seconds, millis, micros :: (Real n) => n -> Duration
days    = (* dayMult)    . realToFrac
hours   = (* hourMult)   . realToFrac
minutes = (* minuteMult) . realToFrac
seconds =                  realToFrac
millis  = (/ milliMult)  . realToFrac
micros  = (/ microMult)  . realToFrac


asSeconds  :: (Fractional f) => Duration -> f
asSeconds = realToFrac

asMillis, asMicros :: (Integral i) => Duration -> i
asMillis = round . (* milliMult)
asMicros = round . (* microMult)

-- * Re-defined functions

threadDelay :: Duration -> IO ()
threadDelay = C.threadDelay . asMicros


