{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Backend.P2P (makeNodeId)
import Control.Distributed.Process.Node
import Control.Monad (forever, when)
import Network.Transport (Transport)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (stderr, hPutStr, hPutStrLn)
import System.Random
import Text.Parsec

import CLI (cliInfo)
import Message (mkMessage, sumMessages)
import NodeListParser (parseEndPoints)
import Types


main :: IO ()
main = do
  -- Read options from command line
  Options{..} <- customExecParser (prefs showHelpOnError) cliInfo

  when (sendTime <= 0 || graceTime <= 0) $
    putStrLnErr "Grace time and send time should be > 0" >> exitFailure

  input <- readFile nodesFilePath
  (stopPoint, endPoints, nodeIds) <-
    case (parseEndPoints input) of
      Left err -> putStrLnErr "Parsing error: " >> printErr err >> exitFailure
      Right endPoints -> do
        putStrLnErr "Successfully parsed endpoints, got: " >> printErr endPoints
        when (length endPoints < 2) $
          putStrLnErr "A minimum of three end points is required." >> exitFailure       -- First node of endPoints is dedicated node sending stop messages.
        return $
          (head endPoints, tail endPoints, map epToNodeId (endPoints))

  -- If we got supplied a seed, make a StdGen using the supplied seed
  -- Otherwise, we depend on the system initialised StdGen.
  gen <- case seed of
    Nothing -> getStdGen
    Just s ->  return $ mkStdGen s

  -- Start the sending and receiving process on all nodes
  mapM_ (\ep -> startNode gen ep nodeIds) endPoints

  -- Wait for the sending time supplied at command-line
  liftIO . threadDelay $ sendTime * 1000000

  -- Start sending stop messages.
  -- The first endpoint is used to send StopMessages to every other endpoint
  startStopNode stopPoint graceTime nodeIds


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


--------------------------------------------------------------------------
-- Definitions for sending/receiving/printing and sending stop processes. 
--------------------------------------------------------------------------

-- | Create receiving/sending/printing process per node, given a starting
-- StdGen, the EndPoint to start the Node on and the ids of the other nodes.
startNode :: StdGen -> EndPoint -> [NodeId] -> IO ()
startNode gen (EndPoint host port) nodeIds = do
  transport <- createTransport host (show port) (\ p -> (host, p)) defaultTCPParameters
  case transport of
    -- We are okay with a node not being contactable, and just print to StdErr
    Left ioErr -> putStrLnErr $ "Could not create transport connection for: " ++
                    show host ++ "with port: " ++ show port

    Right t -> do
      node <- newLocalNode t initRemoteTable
      -- Processes should be forked off for each node (rather than waited on)
      forkProcess node $ do
        self <- getSelfPid

        -- Register receiving process per node
        register receiveProcLabel self

        -- Spawn a local process that will broadcast messages to other nodes
        say $ "Starting sender."
        sender <- spawnLocal $ sendProc gen nodeIds
        say $ "Started sender."

        -- Start receiving process, deliberately not spawned/forked
        say $ "Starting receiver."
        receivedMsgs <- receiveProc sender []
        say $ "Done receiving."

        -- Receiving finished, we can now print
        printProc receivedMsgs
      return ()

-- | Start a node that will send stop messages to all node IDs.
startStopNode :: EndPoint -> Int -> [NodeId] -> IO ()
startStopNode (EndPoint host port) graceTime nodeIds = do
  transport <- createTransport host (show port) (\ p -> (host, p)) defaultTCPParameters
  case transport of
    -- We are okay with a node not being contactable, and just print to StdErr
    Left ioErr -> putStrLnErr $ "Could not create transport connection for: " ++
                    show host ++ "with port: " ++ show port

    Right t -> do
      node <- newLocalNode t initRemoteTable
      -- Not forking off this time
      runProcess node $ stopProc graceTime nodeIds

-- | Process that broadcasts a Message (random number + timestamp) to each of
-- the other nodes's receiver processes.
-- The StdGen is recursively updated, for each random number generated.
sendProc :: StdGen -> [NodeId] -> Process ()
sendProc gen nodeIds = do
  (message, newGen) <- liftIO $ mkMessage gen
  mapM_ (\ nId -> nsendRemote nId receiveProcLabel message) nodeIds
  sendProc newGen nodeIds

-- | Receiving process. It recursively gathers all received messages until it
-- receives a StopMessage, after which it returns all gathered Messages (to be printed by another process).
receiveProc :: ProcessId -> [NumMessage] -> Process [NumMessage]
receiveProc sender ms = do
  receiveWait [
    match (\(_ :: StopMessage) -> do
      self <- getSelfPid
      say $ "Received a StopMessage from: " ++ show sender
      kill sender "Sender should die now"
      return ms),
    match (\(nm :: NumMessage) -> do
      -- say $ "Received message: " ++ show nm
      receiveProc sender (nm : ms))]

-- | Given a list of Message, return a tuple of (|ms|, sum (i * m(i)))), where
-- messages are sorted by their time stamps.
printProc :: [NumMessage] -> Process ()
printProc ms = do
  say "Done, printing now"
  liftIO . print $ (length ms, sumMessages ms)

-- | Send a stop message to all nodes.
-- After waiting the given the grace time, kill the program.
stopProc :: Int -> [NodeId] -> Process ()
stopProc graceTime nodeIds = do
  mapM_ (\ nId -> nsendRemote nId receiveProcLabel StopMessage) nodeIds
  liftIO . threadDelay $ graceTime * 1000000

-- | Hopefully unique label, that together with the NodeId should uniquely
-- represent the process that wants to receive messages. 
receiveProcLabel :: String
receiveProcLabel = "iohk-receiver"

-- | Given a node (from nodelist.txt), create a NodeId.
-- NodeId + label will identify the correct process.
epToNodeId :: EndPoint -> NodeId
epToNodeId (EndPoint host port) = makeNodeId $ host ++ ":" ++ (show port)
