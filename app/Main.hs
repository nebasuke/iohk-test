module Main where


import System.Exit
import System.IO (stderr, hPutStr, hPutStrLn)
import Text.Parsec

import NodeListParser (parseEndPoints)
import Types

type FileName = String

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
  input <- readFile "nodelist.txt"
  endPoints <-
    case (parseEndPoints input) of
      Left err -> putStrLnErr "Parsing error: " >> printErr err >> exitWith (ExitFailure 1)
      Right endPoints -> return endPoints
  -- TODO
  print endPoints
