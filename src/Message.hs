module Message where

import System.Random

-- | Random number between (0, 1] using the global StdGen
-- (possibly pre-initialised with a cmd-line provided seed)
genNum :: IO Double
genNum = randomRIO (0 + epsilon, 1)
 where epsilon = 2.2204460492503131e-16

