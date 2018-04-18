module Types where

type Host = String
type Port = Integer

data EndPoint = EndPoint Host Port
 deriving Show
