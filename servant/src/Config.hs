{-# LANGUAGE RecordWildCards #-}

module Config (Config(..),newConfig,getRepoName) where

import Data.List.Split

--track port to run on, repo url, and max nodes
data Config = Config {
  port     :: Int
, repo     :: String
, maxNodes :: Int
, step     :: Int
, addr     :: String
}

instance Show Config where
  show c@Config{..} = "Config{\n\t>Port:       " ++ show port ++ "\n\t>Repo:       " ++ repo ++ "\n\t>Node Limit: " ++ show maxNodes ++ "\n\t>Step:       " ++ show step ++ "\n\t>FilePath:   " ++ addr ++ "\n}"

newConfig :: Int -> String -> Int -> Int -> String -> Config
newConfig p r n s fp = Config { port = p, repo = r, maxNodes = n, step = s, addr = fp }

getRepoName :: String -> String
getRepoName r = last $ (splitOn "/" r)
