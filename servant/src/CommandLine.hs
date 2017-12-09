module CommandLine (issueCommand) where

import System.Process
import System.IO

issueCommand :: String -> IO Handle
issueCommand cmd = do 
  (_,Just hout,_,_) <- createProcess (proc (show cmd) []){std_out=CreatePipe}
  return hout