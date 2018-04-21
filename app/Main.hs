{-# LANGUAGE RecordWildCards #-}
module Main where

import Options.Applicative
import System.Exit
import System.IO (stderr, hPutStr, hPutStrLn)
import System.Random
import Text.Parsec

import CLI (cliInfo)
import Message
import NodeListParser (parseEndPoints)
import Types

--------------------------------------------------------------------------
-- Bunch of functions to directly support printing to standard error
-- instead of standard output. 
--------------------------------------------------------------------------
putStrErr :: String -> IO ()
putStrErr = hPutStr stderr

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

printErr :: Show a => a -> IO ()
printErr x = putStrLnErr (show x)


main :: IO ()
main = do
  Options{..} <- customExecParser (prefs showHelpOnError) cliInfo

  -- If we got supplied a seed, set the StdGen globally, once and for all.
  -- Otherwise, we depend on the system initialised StdGen.
  case seed of
    Nothing -> return ()
    Just s -> setStdGen $ mkStdGen s

  t <- getTimestamp
  print t
  t2 <- getTimestamp
  print t2
  input <- readFile nodesFilePath
  endPoints <-
    case (parseEndPoints input) of
      Left err -> putStrLnErr "Parsing error: " >> printErr err >> exitWith (ExitFailure 1)
      Right endPoints -> return endPoints
  -- TODO
  print endPoints
