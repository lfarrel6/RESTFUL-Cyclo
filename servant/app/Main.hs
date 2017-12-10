{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib
import Db
import Config
import CommandLine

import System.Environment
import System.IO

defaultFilePath  = "../data/"
cloudServicePath = "../cloudHSTest/"
clone r          = "git clone " ++ r
buildProject     = "stack build"
execute          = "stack exec use-cloudhaskell-exe"

execWorker :: Config -> String -> IO ()
execWorker c@Config{..} fp = loop (port+1) fp maxNodes
  where
   loop _ _  0 = putStrLn "Workers created"
   loop p fp n = do
     issueCommand (execute ++ " worker localhost " ++ show p ++ " &") fp
     putStrLn $ "Worker created on " ++ show p
     loop (p+1) fp (n-1)

execManager :: Config -> String -> IO Handle
execManager c@Config{..} fp = issueCommandAndWait (execute ++ " manager localhost " ++ show port ++ " " ++ addr ++ "/" ++ getRepoName repo) fp

main :: IO ()
main = do
  a <- getArgs
  case a of
   [p, r, n]        -> run $ config   p r n
   [p, r, n, s]     -> run $ config'  p r n s
   _         -> putStrLn "Bad Parameters"
  where
   config   p r n      = newConfig (read p :: Int) r (read n :: Int) 2 defaultFilePath
   config'  p r n s    = newConfig (read p :: Int) r (read n :: Int) (read s :: Int) defaultFilePath

run :: Config -> IO ()
run c@Config{..} = do
  emptyDirectory addr
  createStorage addr True
  _       <- issueCommandAndWait (clone repo)    defaultFilePath
  _       <- issueCommandAndWait  buildProject   cloudServicePath
  execWorker  c cloudServicePath
  hdl     <- execManager c cloudServicePath
  loop hdl
  --putStrLn "Get complexity per commit, give to db"
--invoke cloudhaskell program to evaluate the cyclomatic complexity
--  putStrLn "do something"
  where
   loop hdl = do
    str <- hGetLine hdl
    putStrLn $ "Servant: " ++ str
    loop hdl
