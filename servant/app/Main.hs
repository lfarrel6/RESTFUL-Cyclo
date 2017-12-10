{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib
import Db
import Config
import CommandLine

import System.Environment
import System.IO

defaultFilePath = "../data/"

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
  hdl  <- issueCommandAndWait ("git clone " ++ repo) defaultFilePath
  putStrLn "do something"
