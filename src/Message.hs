module Message where

import Data.List (sort)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random

import Types

-- | Random number between (0, 1] using the global StdGen
-- (possibly pre-initialised with a cmd-line provided seed)
genNum :: IO Double
genNum = randomRIO (0 + epsilon, 1)
 where epsilon = 2.2204460492503131e-16

range = (0 + epsilon, 1)
 where epsilon = 2.2204460492503131e-16

genRandoms :: Maybe Int -> IO [Double]
genRandoms seed = case seed of
  Nothing ->  fmap (randomRs range) getStdGen
    -- return $ randomRs range gen
  Just s -> return $ randomRs range $ mkStdGen s

-- | Timestamp in nanoseconds
getTimestamp :: IO Int
getTimestamp = (round.  (* 1000000)) `fmap` getPOSIXTime 

sumMessages :: [Message] -> Double
sumMessages ms = multHelper $ zip [1..] $ sort ms
  where
    multHelper [] = 0
    multHelper ((n, (Message d _)) : ms) = d * n + multHelper ms
