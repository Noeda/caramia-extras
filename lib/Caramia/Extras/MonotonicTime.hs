-- | Module for retrieving the current monotonic time.
--

module Caramia.Extras.MonotonicTime
    ( getMonotonicTime )
    where

import Caramia.Prelude
import System.Clock

-- | Returns the current monotonic time in seconds.
getMonotonicTime :: Fractional a => IO a
getMonotonicTime = do
    ts <- getTime Monotonic
    return $ (fromIntegral (sec ts)) +
             (fromIntegral (nsec ts) / 1000000000)

