module Types where

type Host = String
type Port = Integer

-- | A node is a host name or IP address + a port
data EndPoint = EndPoint Host Port
 deriving (Eq, Show)

-- | CLI Options
data Options = Options
  { sendTime :: Int
  , graceTime :: Int
  , seed :: Maybe Int
  , nodesFilePath :: String }


-- In nanoseconds
type Timestamp = Int 

-- | A message contains a random Double and the Timestamp when it was about to be sent
data Message = Message Double Timestamp 
  deriving (Eq, Show)
  
instance Ord Message where
  (Message d1 t1) `compare` (Message d2 t2) = t1 `compare` t2

