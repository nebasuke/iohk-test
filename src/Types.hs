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
