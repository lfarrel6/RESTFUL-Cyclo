module CommandLine (issueCommandAndWait, issueCommand, createStorage, emptyDirectory) where

import System.Process
import System.IO
import System.Directory
import Control.Monad

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
