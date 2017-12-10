module CommandLine (issueCommandAndWait, issueCommand, createStorage, emptyDirectory, getFileCount, getCommits, calculateComplexity) where

import System.Process
import System.IO
import System.Directory
import Control.Monad

argonDir = "argon/"

issueCommandAndWait :: String -> String -> IO Handle
issueCommandAndWait cmd fp = do
  (_,Just hout,_,phdl) <- createProcess (shell cmd){cwd=Just fp,std_out=CreatePipe}
  waitForProcess phdl >> return hout

issueCommand :: String -> String -> IO Handle
issueCommand cmd fp = do 
  (_,Just hout,_,_) <- createProcess (shell cmd){cwd=Just fp,std_out=CreatePipe}
  return hout

createStorage :: String -> Bool -> IO ()
createStorage fp cps = createDirectoryIfMissing cps fp

emptyDirectory :: String -> IO ()
emptyDirectory fp = do
  exists <- (doesDirectoryExist fp)
  when exists (removeDirectoryRecursive fp)

getFileCount :: String -> IO Int
getFileCount fp = do
  countHdl    <- issueCommand "git rev-list --all --count" fp
  commitCount <- hGetLine countHdl
  let c = read commitCount :: Int
  return c

getCommits :: String -> Int -> IO [String]
getCommits fp c = do
  shaHdl      <- issueCommand "git log --pretty=format:\"%H\"" fp
  commitArray c shaHdl
  where
   commitArray c hdl = replicateM c $ hGetLine hdl

calculateComplexity :: String -> IO Integer
calculateComplexity fp = do
  hdl      <- issueCommand ("stack exec argon " ++ fp) argonDir
  response <- hGetLine hdl
  case words response of
   [_,_,_,n] -> return $ val n
     where
      val = read
   _         -> putStrLn "PatternMatchFail" >> return (-1)
