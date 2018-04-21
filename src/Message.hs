module Message (
 mkMessage, sumMessages
) where

import Control.Monad (liftM2)
import Data.List (sort)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random

import Types

-- | Random number between (0, 1] using the global StdGen
-- (possibly pre-initialised with a cmd-line provided seed)
genNum :: IO Double
genNum = randomRIO (0 + epsilon, 1)
 where epsilon = 2.2204460492503131e-16

-- | Timestamp in nanoseconds
getTimestamp :: IO Int
getTimestamp = (round.  (* 1000000)) `fmap` getPOSIXTime

-- | Construct a message with a random number and timestamp.
mkMessage :: IO NumMessage
mkMessage = liftM2 NumMessage genNum getTimestamp

-- | Sort a list of messages based on the timestamp,
-- and construct the sum of i * d for each message,
-- where i is the ith message in the sorted list,
-- and d is the Double part of the message.
sumMessages :: [NumMessage] -> Double
sumMessages ms = multHelper 0 $ zip [1..] $ sort ms
  where
    multHelper acc [] = acc
    multHelper acc ((n, (NumMessage d _)) : ms) = multHelper (d * n + acc) ms
