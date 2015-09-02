module Duration where

-- | This module should be imported qualified

import Data.Time.Clock (NominalDiffTime)
import qualified Control.Concurrent as C (threadDelay)

type Duration = NominalDiffTime

milliMult, microMult :: Duration

milliMult = 1000
microMult = 1000000

-- * Conversion

seconds, millis, micros :: (Integral i) => i -> Duration
seconds = fromIntegral

millis = (/ milliMult) . fromIntegral

micros = (/ microMult) . fromIntegral



asSeconds  :: (Fractional f) => Duration -> f
asSeconds = realToFrac

asMillis, asMicros :: (Integral i) => Duration -> i

asMillis = round . (* milliMult)

asMicros = round . (* microMult)

-- * Re-defined functions

threadDelay :: Duration -> IO ()
threadDelay = C.threadDelay . asMicros


