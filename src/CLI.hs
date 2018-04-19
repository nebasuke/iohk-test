module CLI where

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
  { sendTime :: Int
  , graceTime :: Int
  , seed :: Int
  , nodesFilePath :: String }

cliOptions :: Parser Options
cliOptions = Options
  <$> option auto
    (long "send-for"
    <> metavar "INT"
    <> help "Sending time in seconds")
  <*> option auto
    (long "wait-for"
    <> metavar "INT"
    <> help "Grace period in seconds")
  <*> option auto
    (long "with-seed"
    <> metavar "SEED"
    <> help "Initial seed for RNG in messages")
  <*> strOption
    (long "node-list"
    <> metavar "PATH"
    <> help "File path for node list"
    <> showDefault
    <> value "nodelist.txt")
