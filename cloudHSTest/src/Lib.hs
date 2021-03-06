
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
--{-# CPP #-}

-- | use-haskell
-- The purpose of this project is to provide a baseline demonstration of the use of cloudhaskell in the context of the
-- code complexity measurement individual programming project. The cloud haskell platform provides an elegant set of
-- features that support the construction of a wide variety of multi-node distributed systems commuinication
-- architectures. A simple message passing abstraction forms the basis of all communication.
--
-- This project provides a command line switch for starting the application in master or worker mode. It is implemented
-- using the work-pushing pattern described in http://www.well-typed.com/blog/71/. Comments below describe how it
-- operates. A docker-compose.yml file is provided that supports the launching of a master and set of workers.

module Lib
    ( someFunc
    ) where

-- These imports are required for Cloud Haskell
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node                   (initRemoteTable)
import           Control.Monad
import           Network.Transport.TCP                              (createTransport,
                                                                     defaultTCPParameters)
import           Pipes
import           Pipes.Safe                                         (runSafeT)

import           PrimeFactors
import           System.Environment                                 (getArgs)
import           System.Exit

import           CommandLine
import           Argon

-- this is the work we get workers to do. It could be anything we want. To keep things simple, we'll calculate the
-- number of prime factors for the integer passed.
doWork :: FilePath -> IO Integer
doWork = calculateComplexity

-- | worker function.
-- This is the function that is called to launch a worker. It loops forever, asking for work, reading its message queue
-- and sending the result of runnning numPrimeFactors on the message content (an integer).
worker :: ( ProcessId  -- The processid of the manager (where we send the results of our work)
         , ProcessId) -- the process id of the work queue (where we get our work from)
       -> Process ()
worker (manager, workQueue) = do
    us <- getSelfPid              -- get our process identifier
    liftIO $ putStrLn $ "Starting worker: " ++ show us
    go us
  where
    go :: ProcessId -> Process ()
    go us = do

      send workQueue us -- Ask the queue for work. Note that we send out process id so that a message can be sent to us

      -- Wait for work to arrive. We will either be sent a message with an integer value to use as input for processing,
      -- or else we will be sent (). If there is work, do it, otherwise terminate
      receiveWait
        [ match $ \n  -> do
            liftIO $ putStrLn $ "[Node " ++ (show us) ++ "] given work: " ++ n
            complexity <- liftIO $ doWork n
            send manager complexity
            liftIO $ putStrLn $ "[Node " ++ (show us) ++ "] finished work."
            go us -- note the recursion this function is called again!
        , match $ \ () -> do
            liftIO $ putStrLn $ "Terminating node: " ++ show us
            return ()
        ]

remotable ['worker] -- this makes the worker function executable on a remote node

manager :: String    -- The commit we wish to evaluate
        -> String    -- The file path of the repository
        -> Int       -- The number of files in the commit
        -> [NodeId]   -- The set of cloud haskell nodes we will initalise as workers
        -> Process Integer
manager commit fp n workers = do
  us <- getSelfPid

  -- first, we create a thread that generates the work tasks in response to workers
  -- requesting work.
  liftIO $ issueCommand ("git reset --hard " ++ commit) fp
  
  let haskellFiles = allFiles fp

  liftIO $ putStrLn "Haskell files taken"

  workQueue <- spawnLocal $ do
    -- Return the next bit of work to be done
    --iterate over each returned File (returned as Producers so requires heavy lifting)
    runSafeT $ runEffect $ for haskellFiles (\b -> lift $ lift $ fileWaiting b)
    -- Once all the work is done tell the workers to terminate
    terminateWorkers
  
  -- Next, start worker processes on the given cloud haskell nodes. These will start
  -- asking for work from the workQueue thread immediately.
  forM_ workers $ \ nid -> spawn nid ($(mkClosure 'worker) (us, workQueue))
  liftIO $ putStrLn $ "[Manager] Workers spawned"
  -- wait for all the results from the workers and return the sum total. Look at the implementation, whcih is not simply
  -- summing integer values, but instead is expecting results from workers.
  sumIntegers n

-- Convert File Paths into 'Files waiting', respond with a file to worker requests
fileWaiting :: FilePath -> Process ()
fileWaiting fp = do
  liftIO $ putStrLn $ fp ++ " waiting"
  pid <- expect
  liftIO $ putStrLn $ fp ++ " taken"
  send pid fp

-- Terminate worker nodes
terminateWorkers :: Process ()
terminateWorkers = forever $ do
  pid <- expect
  send pid ()

-- note how this function works: initialised with n, the number range we started the program with, it calls itself
-- recursively, decrementing the integer passed until it finally returns the accumulated value in go:acc. Thus, it will
-- be called n times, consuming n messages from the message queue, corresponding to the n messages sent by workers to
-- the manager message queue.
sumIntegers :: Int -> Process Integer
sumIntegers = go 0
  where
    go :: Integer -> Int -> Process Integer
    go !acc 0 = return acc
    go !acc n = do
      m <- expect
      go (acc + m) (n - 1)

rtable :: RemoteTable
rtable = Lib.__remoteTable initRemoteTable

-- | This is the entrypoint for the program. We deal with program arguments and launch up the cloud haskell code from
-- here.
someFunc :: IO ()
someFunc = do


  args <- getArgs

  case args of
    ["manager", host, port, loc] -> do
      putStrLn "Starting Node as Manager"
      backend <- initializeBackend host port rtable
      startMaster backend $ \workers -> do
        fileCount <- liftIO $ getFileCount loc
        commits   <- liftIO $ getCommits loc fileCount
        result    <- manager (head commits) loc fileCount workers
        liftIO $ print result
        --result  <- map (\commit -> manager commit loc workers) commits
        --liftIO $ mapM_ print result
    ["worker", host, port] -> do
      putStrLn "Starting Node as Worker"
      backend <- initializeBackend host port rtable
      startSlave backend
    _ -> putStrLn "Bad parameters"


  -- create a cloudhaskell node, which must be initialised with a network transport
  -- Right transport <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  -- node <- newLocalNode transport initRemoteTable

  -- runProcess node $ do
  --   us <- getSelfNode
  --   _ <- spawnLocal $ sampleTask (1 :: Int, "using spawnLocal")
  --   pid <- spawn us $ $(mkClosure 'sampleTask) (1 :: Int, "using spawn")
  --   liftIO $ threadDelay 2000000
