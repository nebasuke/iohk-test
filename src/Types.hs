{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

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


-- | Timestamp in nanoseconds
type Timestamp = Int 

-- | A message contains a random Double and the Timestamp when it was about to be sent
data NumMessage = NumMessage Double Timestamp 
  deriving (Eq, Generic, Show)

instance Binary NumMessage

-- | Ord instance is used to order NumMessages by their sent timestamp.
instance Ord NumMessage where
  (NumMessage d1 t1) `compare` (NumMessage d2 t2) = t1 `compare` t2

-- | A StopMessage is sent to signify receiving messages can now be stopped.
data StopMessage = StopMessage
  deriving (Generic, Typeable)

instance Binary StopMessage
