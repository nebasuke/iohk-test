module CLI (
  cliInfo, cliOptions
 ) where

import Options.Applicative
import Data.Semigroup ((<>))

import Types

-- | A full description of the runnable command-line parser
cliInfo :: ParserInfo Options
cliInfo = info
  (helper <*> cliOptions)
  (  fullDesc
  <> progDesc "Launch Cloud Haskell with given nodes + exec times, and start messaging"
  <> header "IOHK Cloud Haskell test task - Bas van Gijzel (2018)" )

-- | Command line options defined as a parser
cliOptions :: Parser Options
cliOptions = Options
  <$> option auto
    (long "send-for"
    <> metavar "INT"
    <> help "Sending period in seconds")
  <*> option auto
    (long "wait-for"
    <> metavar "INT"
    <> help "Grace period in seconds")
  <*> optional (option auto
    (long "with-seed"
    <> metavar "INT"
    <> help "Initial seed for RNG in messages"))
  <*> strOption
    (long "node-list"
    <> metavar "PATH"
    <> help "File path for node list"
    <> showDefault
    <> value "nodelist.txt")
